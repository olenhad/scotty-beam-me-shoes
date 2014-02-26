{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Web.Scotty
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)
import Data.Aeson
import Data.Text.Lazy as L
import Data.ByteString.Lazy
import Data.ByteString.Lazy as BL

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


render :: (Either String a) -> (a -> ActionM ()) -> ActionM ()
render res act =
       case res of
            Left err -> Web.Scotty.json $ APIFailure err
            Right s -> act s

createShoe :: BL.ByteString -> ActionM ()
createShoe jsonShoe =
    case (eitherDecode jsonShoe) :: Either String J.ShoeJSON  of
       Left e -> Web.Scotty.json $ APIFailure e
       Right shoe -> do res <- liftIO $ S.createShoe shoe
                        render res Web.Scotty.json

allShoes :: ActionM ()
allShoes = do ss <- liftIO S.findShoes
              render ss $ html . L.pack . V.renderAllShoes


shoeById :: String -> ActionM ()
shoeById id = do s <- liftIO $ S.findShoeById id
                 render s $ html . L.pack . V.renderSingleShoe


main = scotty 3000 $ do
  post "/shoes" $ do
    shoeJ <- body
    createShoe shoeJ
  get "/shoes" $ do
    allShoes
  get "/shoes/:id" $ do
    id <- param "id"
    shoeById id
  middleware $ staticPolicy (noDots >-> addBase "static")
