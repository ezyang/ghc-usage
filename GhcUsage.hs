module GhcUsage (frontendPlugin) where

import qualified GHC
import GhcPlugins
import DriverPhases
import GhcMonad
import UniqDFM
import UniqDSet

import Data.List
import System.Exit
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin {
  frontend = usage
  }

usage :: [String] -> [(String, Maybe Phase)] -> Ghc ()
usage _flags args = do
    dflags <- getDynFlags
    -- NB: frontend plugin defaults to OneShot
    _ <- GHC.setSessionDynFlags dflags { ghcMode = CompManager }
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget args
    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load GHC.LoadAllTargets
    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    hsc_env <- GHC.getSession
    let stuff = vcat [ hang (ppr (mi_module iface))
                          2 (ppr (usageModIface iface))
                     | hmi <- eltsUDFM (hsc_HPT hsc_env)
                     , let iface = hm_iface hmi ]
    pprTrace "mod_graph" stuff $ return ()
    return ()

usageModIface :: ModIface -> Map ModuleName (UniqDSet OccName)
usageModIface iface = Map.fromList $
    [ (mod_name, mkUniqDSet (map fst occs_w_fp))
    | UsageHomeModule{ usg_mod_name = mod_name
                     , usg_entities = occs_w_fp } <- mi_usages iface ]
