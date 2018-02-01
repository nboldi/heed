
## How to use

Exporting one module:

```
ghc -ffrontend Language.Haskell.Heed.Export.Plugin -ffrontend-opt <dbfile> -j1
```

Exporting a cabal project:

```
cabal build --with-ghc=heed-ghc --ghc-options="-plugin-package heed-export -ffrontend-opt almafa.db -j1"
```

Exporting a stack project:

Add to stack.yaml:

```
- location: https://github.com/nboldi/heed/archive/master.zip
  subdirs:
   - core
   - export
```

Add to (one of the) cabal files:

```
flag HeedExport
  Description: Enable heed export
  Default:     False

-- ...

if flag(HeedExport)
  build-depends:       base >=4.10 && <4.11 -- ...
                     , heed-export
else
  build-depends:       base >=4.10 && <4.11 -- ...
```

Then build and run the repl:

```
stack build --flag DemoProject:HeedExport
stack repl --with-ghc C:/Users/nboldi/AppData/Roaming/local/bin/heed-ghc --ghc-options -ffrontend-opt --ghc-options almafa.db --ghc-options -j1
```