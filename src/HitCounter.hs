{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans (ActionT, ScottyT, get, html, param, scottyT)

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = (m', n)
  where n = (+1) $ fromMaybe 0 $ M.lookup k m
        m' = M.insert k n m

incrementCount :: Text
               -> IORef (M.Map Text Integer)
               -> IO Integer
incrementCount k countsRef = do
  countsMap <- readIORef countsRef
  let (countsMap', n) = bumpBoomp k countsMap
  writeIORef countsRef countsMap'
  return n

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    pre <- lift $ asks prefix
    let key' = mappend pre unprefixed
    countsRef <- lift $ asks counts
    newInteger <- liftIO $ incrementCount key' countsRef
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show (newInteger :: Integer)
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  countsRef <- newIORef M.empty
  let config = Config
        { counts = countsRef
        , prefix = TL.pack prefixArg
        }
      runR rma = runReaderT rma config
  scottyT 3000 runR app
