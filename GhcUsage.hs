module GhcUsage (frontendPlugin) where

import qualified GHC
import GhcPlugins
import DriverPhases
import GhcMonad
import UniqDFM
import UniqDSet
import Avail
import Outputable

import System.IO
import System.Exit
import Control.Monad
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {
  frontend = usage
  }

usage :: [String] -> [(String, Maybe Phase)] -> Ghc ()
usage _flags args = do
    dflags0 <- getDynFlags
    -- NB: frontend plugin defaults to OneShot
    _ <- GHC.setSessionDynFlags dflags0 { ghcMode = CompManager }
    dflags <- getDynFlags
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget args
    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load GHC.LoadAllTargets
    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    hsc_env <- GHC.getSession
    let ifaces = map hm_iface
               . eltsHpt
               $ hsc_HPT hsc_env
        usages = Map.unionsWith unionUniqDSets
               . map usageModIface
               $ ifaces
    liftIO . printForUser dflags stdout neverQualify
           . vcat
           . vpunctuate (text "")
           . map (renderHeader usages)
           -- . filter ((==HsigFile) . mi_hsc_src)
           $ ifaces
    return ()

vpunctuate :: SDoc   -- ^ The punctuation
           -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
           -> [SDoc] -- ^ Punctuated list
vpunctuate _ []     = []
vpunctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d $$ p) : go e es

usageModIface :: ModIface -> Map ModuleName (UniqDSet OccName)
usageModIface iface = Map.fromList $
    [ (mod_name, mkUniqDSet (map fst occs_w_fp))
    | UsageHomeModule{ usg_mod_name = mod_name
                     , usg_entities = occs_w_fp } <- mi_usages iface ]

renderName :: Name -> SDoc
renderName n = let occ = occName n
               in parenSymOcc occ (ppr occ)

renderField :: FieldLabel -> SDoc
renderField f = ftext (flLabel f)

renderAvailInfo :: AvailInfo -> SDoc
renderAvailInfo a
    = renderName n <>
        if null ns && null fs
            then empty
            else parens . fsep . punctuate comma
               $ map renderName (sortOn (occNameFS . occName) ns)
              ++ map renderField (sortOn flLabel fs)
  where
    n  = availName a
    ns = filter (n /=) (availNonFldNames a)
    fs = availFlds a

renderHeader :: Map ModuleName (UniqDSet OccName) -> ModIface -> SDoc
renderHeader usages iface
    = header <+> ppr mod_name <+> text "(" $$
      nest 4 pp_exports $$
      text ") where"
  where
    mod_name = moduleName (mi_module iface)
    usage = fromMaybe emptyUniqDSet (Map.lookup mod_name usages)
    header | HsigFile <- mi_hsc_src iface = text "signature"
           | otherwise                    = text "module"
    pp_exports = vcat
               . punctuate comma
               . map renderAvailInfo
               . sortOn availSortKey
               $ exports
    exports = filterAvails ((`elementOfUniqDSet` usage) . occName)
            $ mi_exports iface

availSortKey :: AvailInfo -> String
availSortKey a
    -- Fortuitously, capital letters sort first.
    = occNameString . occName $ availName a
