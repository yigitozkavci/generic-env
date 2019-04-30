{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenericEnv
  ( fromEnv
  , EnvOptions (..)
  , defaultEnvOptions
  , withPrefix
  ) where

import GHC.TypeLits
import Data.Kind
import GHC.Generics
import Data.Proxy
import qualified Data.Map as M
import qualified Data.Text as T
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import Data.Typeable (Typeable, typeRep)
import Control.Applicative ((<|>), liftA2)

type WrongTypeExpectationErr = 'Text "Wrong type expectation for environment. You should expect a product type with record syntax."
type NonRecordSyntaxErr = 'Text "Only record-syntaxed product types can represent environments (we need the field names)"

type family ValidField (rep :: Type -> Type) :: Constraint where
  ValidField (M1 s ('MetaSel ('Just field_name) su ss ds) (K1 k field_val)) =
    ()
  ValidField (M1 s ('MetaSel 'Nothing su ss ds) (K1 k field_val)) =
    TypeError NonRecordSyntaxErr
  ValidField (M1 s ('MetaSel ('Just field_name) su ss ds) (K1 k field_val) :*: rest) =
    ValidField rest
  ValidField _ =
    TypeError WrongTypeExpectationErr

type family ValidRep (rep :: Type -> Type) :: Constraint where
  ValidRep (M1 d m1 (M1 c m2 v)) = ValidField v
  ValidRep _ = TypeError WrongTypeExpectationErr

withPrefix :: String -> EnvOptions
withPrefix prefix = defaultEnvOptions { envKeyPrefix = prefix }

data EnvOptions = EnvOptions
  { modifyFieldNames :: String -> String
  , envKeyPrefix :: String
  }

defaultEnvOptions :: EnvOptions
defaultEnvOptions =
  EnvOptions
    { modifyFieldNames = id
    , envKeyPrefix = ""
    }

fromEnv :: forall e. (Generic e, Gen (Rep e)) => EnvOptions -> IO (Either String e)
fromEnv opts = do
  env_map <- M.fromList <$> getEnvironment
  pure $ to <$> gen opts env_map

class Gen f where
  gen :: EnvOptions -> M.Map String String -> Either String (f a)

instance (Gen1 v, ValidRep (M1 d m1 (M1 c m2 v))) => Gen (M1 d m1 (M1 c m2 v)) where
  gen xs = fmap (fmap (M1 . M1)) $ gen1 xs

class Gen1 f where
  gen1 :: EnvOptions -> M.Map String String -> Either String (f a)

-- | Tries to read any readable type and also strings.
readAny :: (Show a, Read a) => String -> Maybe a
readAny = liftA2 (<|>) readMaybe (readMaybe . show)

toUpper :: String -> String
toUpper = T.unpack . T.toUpper . T.pack

-- | Creates a record-syntaxed field with a "known" name.
mkField
  :: forall field_val field_name s su ss ds k a.
     (Show field_val, Typeable field_val, Read field_val, KnownSymbol field_name)
  => Proxy field_name
  -> EnvOptions
  -> M.Map String String
  -> Either String (M1 s ('MetaSel ('Just field_name) su ss ds) (K1 k field_val) a)
mkField proxy (EnvOptions { modifyFieldNames, envKeyPrefix }) env_map =
  let field_name = symbolVal proxy in
  let key = envKeyPrefix <> toUpper (modifyFieldNames field_name) in
  case M.lookup key env_map of
    Just envVal ->
      case readAny envVal :: Maybe field_val of
        Nothing ->
          Left $ "Could not parse value of type "
              <> "'" <> show (typeRep (Proxy @field_val)) <> "'"
              <> " for the field named '" <> field_name <> "'"
              <> " from environment variable "
              <> "'" <> key <> "=" <> envVal <> "'"
        Just readEnvVal ->
          Right $ M1 $ (K1 readEnvVal)
    Nothing ->
      Left $ "Environment variable with key "
          <> key
          <> " could not be found"

-- | Base case for product type field recursion.
instance (Show field_val
         , Typeable field_val
         , Read field_val
         , KnownSymbol field_name
         ) => Gen1 (M1 s ('MetaSel ('Just field_name) su ss ds) (K1 k field_val)) where
  gen1 = mkField (Proxy @field_name)

-- | A single step for product type field recursioni
instance (Show field_val
         , Typeable field_val
         , Read field_val
         , Gen1 b
         , KnownSymbol field_name
         ) => Gen1 (M1 s ('MetaSel ('Just field_name) su ss ds) (K1 k field_val) :*: b) where
  gen1 opts env_map = (:*:) <$> mkField (Proxy @field_name) opts env_map
                            <*> gen1 opts env_map

