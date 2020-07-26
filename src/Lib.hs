{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson 
import Data.Aeson.Types 
import GHC.Generics 
import Data.Map as DM

data Abc = Abc
  { name :: String
  , a :: Maybe String
  , b :: Maybe String
  } deriving (Generic, Show)

instance FromJSON Abc where
  parseJSON =
   withObject "Abc" $ \v -> do
    name <- v.: "name"
    extra0 <- v.: "extra" :: Parser [Map String String]
    let extra = Prelude.foldr union empty extra0
        a = DM.lookup "this_string_A" extra
        b = DM.lookup "this_string_B" extra
    return $ Abc name a b

test :: Either String Abc
test = eitherDecode exampleJson

exampleJson = "{ \"name\": \"xyz1\", \"extra\": [ { \"this_string_A\": \"Hello\" }, { \"this_string_B\": \"World\" } ] }"
