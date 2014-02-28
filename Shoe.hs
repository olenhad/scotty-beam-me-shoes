{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Shoe where

import Database.MongoDB
import qualified Database.MongoDB as DB
import qualified Data.Bson as B
import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Text.Read
import qualified ShoeJSON as J
import qualified Codec.Binary.Base64 as Base64
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Either

data Shoe =
     Shoe { description :: !Text
          , color :: !Text
          , size :: Double
          , photo :: FilePath
          , shoeId :: Maybe ObjectId
          } deriving (Show)


shoeToBSON :: Shoe -> [Field]
shoeToBSON shoe =
   let base = ["description" =: description shoe,
               "color" =: color shoe,
               "size" =: size shoe,
               "photo" =: photo shoe]
   in case shoeId shoe of
      Nothing -> base
      Just id -> merge base ["_id" =: id]


pluckString :: Label -> Document -> Text
pluckString k d =
  case valueAt k d of
      B.String s -> s
      _ -> error "Not a string!"

pluckNum :: Label -> Document -> Double
pluckNum k d =
  case valueAt k d of
      B.Float s -> s
      _ -> error "Not a number!"

pluckOid :: Label -> Document -> ObjectId
pluckOid k d =
  case valueAt k d of
      B.ObjId s -> s
      _ -> error "Not an oid!"



shoeFromBSON :: Document -> Shoe
shoeFromBSON b = Shoe {description = pluckString "description" b,
                       color = pluckString "color" b,
                       size = pluckNum "size" b,
                       photo = unpack $ pluckString "photo" b,
                       shoeId = Just $ pluckOid "_id" b}

shoeFromJShoe :: J.ShoeJSON -> Shoe
shoeFromJShoe js = Shoe {description = J.description js,
                        color = J.color js,
                        size = J.size js,
                        photo = "",
                        shoeId = Nothing}




sharedPipe :: IO Pipe
sharedPipe = runIOE $ connect $ host "127.0.0.1"


runA :: (MonadIO m) => Action m a -> m (Either String a)
runA act = do pipe <- liftIO sharedPipe
              res <- access pipe master "zalora" act
              case res of
                   Right a -> return $ Right a
                   Left f -> return $ Left "DB Error"



createShoe :: J.ShoeJSON -> IO (Either String String)
createShoe js =
           do id <- genObjectId

              let fname = ((show id) ++ ".jpg")
              let shoe = shoeFromJShoe js
              let bsonShoe = shoeToBSON $ shoe {photo = fname}

              putStrLn ("Trying to save file at " ++ fname)
              imgRes <- serialiseImg fname $ unpack $ J.photo js
              res <- runA $ insert "shoes" bsonShoe
              return $ liftM show res


findShoes :: IO (Either String [Shoe])
findShoes =
          do shoes <- runA (DB.find (select [] "shoes") >>= rest)
             return $ liftM (Prelude.map shoeFromBSON) shoes


findShoeById :: String -> IO (Either String Shoe)
findShoeById id =
    case (readEither id :: Either String ObjectId) of
         Left e ->  do return $ Left e
         Right oid ->
               do shoe <- runA $ DB.findOne $ select ["_id" =: oid] "shoes"
                  return $ case shoe of
                       Right (Just s) -> Right $ shoeFromBSON s
                       _ -> Left "No Result"


serialiseImg :: FilePath -> String -> IO (Either String String)
serialiseImg filename img =
    case Base64.decode img of
         Just ws -> do BL.writeFile ("static/img/" ++ filename) $ runPut $ mapM_ putWord8 ws
                       return $  Right "success"
         Nothing -> do return $ Left "Error in decoding base64"
