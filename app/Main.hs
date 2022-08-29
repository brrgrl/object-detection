{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}

import Control.Concurrent
-- import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (withSocketsDo)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import qualified Database.SQLite.Simple as DB
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type Message = String

initDB :: FilePath -> IO ()
initDB dbfile = DB.withConnection dbfile $ \conn ->
  DB.execute_ conn
    "create table if not exists messages (msg text not null)"

type ImageAPI = "images" :>
                (     QueryParam "objects" String :> Get '[JSON] [Image]
                 :<|> Get '[JSON] [Image]
                 :<|> Capture "id" Int :> Get '[JSON] Image
                 :<|> ReqBody '[JSON] PostImage :> Post '[JSON] Image
                )


data Image = Image
  { label :: String
  , path :: String
  , id :: Int
  , tags :: [String]
  } deriving (Eq, Show, Generic)

data PostImage = PostImage
  { name :: String
  , image :: String
  , detect :: Bool
  } deriving (Eq, Show, Generic, FromJSON)


instance ToJSON Image

family :: Image
family = Image "happy family" "family.png" 1 ["mom", "dad", "dog", "cat"]

bob :: Image
bob = Image "bob" "bob.png" 2 ["man", "computer"]

upload1 :: Image
upload1 = Image "unnamed1" "unnamed1.png" 2 ["potato"]


images :: [Image]
images = [family, bob]

uploadImage :: PostImage -> Image
uploadImage img = Image label' path' id' tags'

  where label' = name img
        id' = 3
        path' = image img
        tags' = if detect img
                   then ["random"]
                   else []



server :: Server ImageAPI
server = matchImages
    :<|> allImages
    :<|> imageId
    :<|> postImage

  where allImages :: Handler [Image]
        allImages = return images

        matchImages :: Maybe String -> Handler [Image]
        matchImages Nothing    = return images
        matchImages (Just obj) = return [family]

        imageId :: Int -> Handler Image
        imageId n = return bob

        postImage :: PostImage -> Handler Image
        postImage body = return (uploadImage body)

imageAPI :: Proxy ImageAPI
imageAPI = Proxy


app :: Application
app = serve imageAPI server

main :: IO ()
main = run 8081 app
