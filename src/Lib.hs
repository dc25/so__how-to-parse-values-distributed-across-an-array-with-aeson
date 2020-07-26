{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson (eitherDecode, FromJSON, Object, parseJSON, withArray, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.HashMap.Strict as HashMap (empty, unions)
import Data.Function ((&))
import Data.Foldable (toList)

data Abc = Abc
  { name :: String
  , a :: Maybe String
  , b :: Maybe String
  } deriving (Generic, Show)

instance FromJSON Abc where
  parseJSON =
   withObject "Abc" $ \v -> do
    extra <- getExtra v
    Abc <$> v .: "name" <*> extra .:? "this_string_A" <*> extra .:? "this_string_B"

getExtra :: Object -> Parser Object
getExtra v = do
  mextra <- v .:? "extra"
  case mextra of
    Just vv -> vv & withArray "Abc.extra" (\arr -> do
      let vallst = toList arr
      objlst <- traverse (withObject "Abc.extra[..]" pure) vallst
      return $ HashMap.unions objlst)
    Nothing -> return HashMap.empty

example :: BL.ByteString
example = "{\"name\": \"xyz1\", \"extra\": [{\"this_string_A\": \"Hello\"}, {\"this_string_B\": \"World\"}]}"

test :: Either String Abc
test = eitherDecode example
