{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ShoeJSON where

import Data.Aeson
import Data.Text
import GHC.Generics


data ShoeJSON =
     ShoeJSON
     { description :: !Text
       , color :: !Text
       , size :: Double
       , photo :: !Text
      } deriving (Show, Generic)


instance FromJSON ShoeJSON
instance ToJSON ShoeJSON
