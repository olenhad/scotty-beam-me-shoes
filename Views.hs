{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Views where

import Control.Monad (forM_)
import Data.Monoid

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Pretty as R
import qualified Shoe as S

import Data.Text


maybeToString (Just id) = show id
maybeToString Nothing = ""

aShoe shoe = H.div $ do
      H.p $ H.toHtml $ S.description shoe
      H.p $ H.toHtml $ mconcat [(pack "color : "),(S.color shoe)]
      H.a H.! A.href (H.toValue $ mconcat ["/shoes/", maybeToString $ S.shoeId shoe]) $ "link"

allShoes shoes = H.docTypeHtml $ do
    H.head $ do
        H.title "Shoes"
    H.body $ do
        H.h1 "All Shoes"
        H.ul $ forM_ shoes aShoe

renderAllShoes shoes = R.renderHtml $ allShoes shoes

renderSingleShoe shoe =
   R.renderHtml $
     H.docTypeHtml $ do
         H.head $ do
             H.title "Shoes"
         H.body $ do
             aShoe shoe
             H.img H.! A.src (H.toValue ("/img/" ++ S.photo shoe))
