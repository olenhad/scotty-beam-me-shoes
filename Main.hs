{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Web.Scotty

import Data.Monoid (mconcat)

import Data.Aeson
import Data.Text
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics

import Database.MongoDB
import qualified Data.Bson as B

import qualified Shoe as S
import qualified ShoeJSON as J

data APIFailure =
     APIFailure {reason :: String} deriving (Show, Generic)

instance ToJSON APIFailure


createShoe jsonShoe =
  case (eitherDecode jsonShoe) :: Either String J.ShoeJSON  of
  Left e -> Web.Scotty.json $ APIFailure e
  Right shoe -> do r <- liftIO $ S.createShoe shoe
                   Web.Scotty.json r


main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  post "/shoe" $ do
    shoeJ <- body
    createShoe shoeJ
