# CHANGELOG

### 0.8.1

- Removed the `either` dependency. It was not needed anymore!

## 0.8.0

- Added support for either-4.5. This support breaks compatibility as we don’t have `EitherT`
  anymore. If you used it, please fall back to `ExceptT`, which has the same interface.

### 0.7.1

- Added support for either 4.4.1.

### 0.7

- Added support for GHC 7.10.

### 0.6.0.2

- Fixed compilation error. That was due to the change regarding
monad-control-1.0.

### 0.6.0.1

- Fixed bug about monad-control and type / data families.

### 0.6

- monad-control 1.0.0.1 qualified.

### 0.5

- license is now BSD3!;
- enhanced the documentation in all modules;
- added README.md.

### 0.4.0.2

- added the changelog in the package description (.cabal).

### 0.4.0.1

- added the *source-repository head* field in the .cabal file;
- added the *bug-reports* field in the .cabal file;
- change *author* and *maintainer* format.

### 0.4

- actually, lower bound is better for now; using mtl-2.1.

### 0.3

- now using lower-bound mtl’s version 0.2.2.1 for Control.Monad.Except.

### 0.2.4

- added `MonadExcept` instances.
