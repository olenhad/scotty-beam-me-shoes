{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Shoe where

import Database.MongoDB
import qualified Data.Bson as B
import Data.Aeson
import Data.Text
import Data.Maybe

import qualified ShoeJSON as J


data Shoe =
     Shoe { description :: !Text
          , color :: !Text
          , size :: Double
          , photo :: !Text
          , shoeId :: Maybe ObjectId
          } deriving (Show)



shoeToBSON shoe =
   let base = ["description" =: description shoe,
               "color" =: color shoe,
               "size" =: size shoe,
               "photo" =: photo shoe]
   in case shoeId shoe of
      Nothing -> base
      Just id -> merge base ["_id" =: id]



pluckString k d =
  case valueAt k d of
      B.String s -> s
      _ -> error "Not a string!"


pluckNum k d =
  case valueAt k d of
      B.Float s -> s
      _ -> error "Not a number!"

pluckOid k d =
  case valueAt k d of
      B.ObjId s -> s
      _ -> error "Not an oid!"




shoeFromBSON b = Shoe {description = pluckString "description" b,
                       color = pluckString "color" b,
                       size = pluckNum "size" b,
                       photo = pluckString "photo" b,
                       shoeId = Just $ pluckOid "_id" b}


shoeFromJShoe js = Shoe {description = J.description js,
                        color = J.color js,
                        size = J.size js,
                        photo = J.photo js,
                        shoeId = Nothing}

runA pipe act = access pipe master "zalora" act

createShoe :: J.ShoeJSON -> IO String
createShoe js =
           let bsonShoe = shoeToBSON $ shoeFromJShoe js
           in
           do pipe <- runIOE $ connect $ host "127.0.0.1"
              id <- runA pipe $ insert "shoes" bsonShoe
              putStrLn $ show id
              return $ show id
