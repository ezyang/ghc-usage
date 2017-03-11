# ghc-usage

`ghc-usage` is a simple frontend plugin which outputs the minimum export
list for every module and signature necessary to compile the package.
This is useful for taking a large signature and paring it down to
precisely the identifiers you use.

For example, suppose you have:

```
signature A where
    f :: Int
    g :: Bool
module B where
    h = f
```

Clearly, only `f` is actually used.  Thus, `ghc-usage` will output:

```
signature A (
    f
) where
```

The benefit of having a reduced signature is that you allow more
implementations (because you require fewer function implementations.)

This tool also outputs minimal export lists for modules, which might
be useful if you're trying to make modules more abstract/find
unnecessary exports. (Though, if this is a library, obviously
there might be external clients using these libraries!)

## How to use

`ghc-usage` is only compatible with GHC 8.2 and later.

Install using `cabal install ghc-usage`, and then run
`ghc-usage --interactive A B C`.  If you are working in a Cabal
project, you can run `cabal repl -w ghc-usage` or `cabal new-repl -w usage`
to get the correct command line arguments.

GHC 8.2 is probably not the default `ghc` executable in your path.
In this case, you will need to also pass `--with-ghc-pkg=/path/to/ghc-pkg-8.2`;
otherwise Cabal will complain about a version mismatch.
