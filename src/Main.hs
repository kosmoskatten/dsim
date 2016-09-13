{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Concurrent.STM
import Data.Aeson
import Data.Aeson.Types
import Data.Map (Map)
import Data.Text (Text)
import Network.Hive
import Text.Printf (printf)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Self = Self
  { nextIp :: TVar Int
  , mmeMap :: TVar MmeMap
  }

type MmeMap = Map Text [Text]

main :: IO ()
main = do
  self <- Self <$> newTVarIO 1
               <*> newTVarIO Map.empty
  hive defaultHiveConfig $ do

    -- Match a GET on the site root. Redirect to index.html.
    match GET <!> None ==> redirectTo "index.html"

    match POST </> "api" </> "0.1" </> "mme" <!> None
          ==> (createMme self =<< bodyByteString)

    -- The match all clause will do static serving.
    matchAll ==> serveDirectory "."

createMme :: Self -> LBS.ByteString -> Handler HandlerResponse
createMme self input = do
  case decodeInput of
    Just name -> do
      inserted <- liftIO $ (atomically $ maybeInsert name)
      if inserted
        then do
          logInfo $ printf "createMme: create %s" (show name)
          respondJSON Created $ object [ name .= mkMmeUrl name ]
        else do
          logInfo $ printf "createMme: %s already created" (show name)
          respondText Conflict "Already created"
    Nothing   -> do
      logInfo $ printf "createMme: Cannot decode %s" (show input)
      respondText BadRequest "Cannot decode JSON"
    where
      decodeInput :: Maybe Text
      decodeInput = do
        result <- decode input
        flip parseMaybe result $ \obj ->
          obj .: "name"

      maybeInsert :: Text -> STM Bool
      maybeInsert name = do
        mmes <- readTVar (mmeMap self)
        case Map.lookup name mmes of
          Just _  -> return False
          Nothing -> do
            ip <- readTVar (nextIp self)
            modifyTVar (nextIp self) (+1)
            writeTVar (mmeMap self) (Map.insert name [mkMmeIp ip] mmes)
            return True

mkMmeUrl :: Text -> Text
mkMmeUrl name = "/api/0.1/mme/" `T.append` name

mkMmeIp :: Int -> Text
mkMmeIp n = "192.168.1." `T.append` (T.pack $ show n)
