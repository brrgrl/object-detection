{-# language OverloadedStrings #-}

module Main where

import qualified Web.Scotty as S
import qualified Data.Time.Clock as C
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as HTTP


data Image
  = Image
  { iTime :: C.UTCTime
  , iLabel :: TL.Text
  , iId :: TL.Text
  , iObjects :: [TL.Text]
  }

type Images = M.Map Integer Image


main :: IO ()
main = do
  S.scotty 3000 myApp

myApp :: S.ScottyM ()
myApp = do
  -- Our main page, which will display all of the bulletins
  S.get "/" $ do
    S.text $ "Hello"
  -- A page for a specific post
  S.get "/images/:id" $ do
    S.status HTTP.notFound404
    error "Not yet implemented"

  S.get "/images/:objects" $ do
    S.status HTTP.notFound404
    error "Not yet implemented"

  -- A page for creating a new post
  S.get "/images" $ do
    S.status HTTP.notFound404
    error "Not yet implemented"

  -- A request to submit a new page
  S.post "/images" $ do
    S.status HTTP.notFound404
    error "Not yet implemented"
