{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (getResolvedConfig, createCacClient, getCacClient,
                                     getFullConfigState, getCacLastModified, cacStartPolling, getDefaultConfig)
import           Control.Concurrent
import           Prelude

main :: IO ()
main = do
    createCacClient "dev" 10 "http://localhost:8080" >>= \case
        Left err -> putStrLn err
        Right _  -> pure ()
    threadId <- forkOS (cacStartPolling "dev")
    print threadId
    getCacClient "dev" >>= \case
        Left err     -> putStrLn err
        Right client -> do
            config       <- getFullConfigState client
            lastModified <- getCacLastModified client
            overrides    <- getResolvedConfig client "{\"country\": \"India\"}" ["country_image_url", "hyperpay_version"]
            defaults     <- getDefaultConfig client ["country_image_url", "hyperpay_version"]
            print config
            print lastModified
            print overrides
            print defaults
            threadDelay 1000000000
    pure ()
