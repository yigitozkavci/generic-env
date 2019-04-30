# generic-env
generic-env lets you produce your generic type from a given subset of environment variables.

# Usage
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Example1 where

import GenericEnv
import GHC.Generics
import System.Environment

data AppEnv = AppEnv { name :: String, ver :: String } deriving (Generic, Show)

main :: IO ()
main = do
  setEnv "APP_NAME" "wow"
  setEnv "APP_VER" "1.0"
  print =<< fromEnv @AppEnv (withPrefix "APP_")
```

```
> main
Right (AppEnv {name = "wow", ver = "1.0"})
```

Let's change our version from String to Float:
```haskell
data AppEnv = AppEnv { name :: String, ver :: Float } deriving (Generic, Show)
```
and run the same code:
```
> main
Right (AppEnv {name = "wow", ver = 1.0})
```

Next, try making the version an integer:
```haskell
data AppEnv = AppEnv { name :: String, ver :: Int } deriving (Generic, Show)
```

```
> main
Left "Could not parse value of type 'Int' for the field named 'ver' from environment variable 'APP_VER=1.0'"
```

# LICENSE
MIT License
