{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Web.Scotty

import Data.Monoid (mconcat)

import Data.Aeson
import Data.Text.Lazy as L
import Data.ByteString.Lazy
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics

import Database.MongoDB
import qualified Data.Bson as B

import qualified Shoe as S
import qualified ShoeJSON as J
import qualified Views as V

data APIFailure =
     APIFailure {reason :: String} deriving (Show, Generic)

instance ToJSON APIFailure


createShoe jsonShoe =
  case (eitherDecode jsonShoe) :: Either String J.ShoeJSON  of
  Left e -> Web.Scotty.json $ APIFailure e
  Right shoe -> do r <- liftIO $ S.createShoe shoe
                   Web.Scotty.json r

allShoes = do s <- liftIO S.findShoes
              html $ L.pack $ V.renderAllShoes s

shoeById id = do s <- liftIO $ S.findShoeById id
                 case s of
                      Left m -> html $ mconcat ["Error! ", L.pack m]
                      Right s -> html $ L.pack $ V.renderSingleShoe s


main = scotty 3000 $ do
  get "/test/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  post "/shoes" $ do
    shoeJ <- body
    createShoe shoeJ
  get "/shoes" $ do
    allShoes
  get "/shoes/:id" $ do
    id <- param "id"
    shoeById id
