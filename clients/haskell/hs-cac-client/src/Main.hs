{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (cacStartPolling, createCacClient,
                                     getCacClient, getCacLastModified,
                                     getDefaultConfig,
                                     getFullConfigStateWithFilter,
                                     getResolvedConfig)
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
            config          <- getFullConfigStateWithFilter client Nothing Nothing
            lastModified    <- getCacLastModified client
            overrides       <- getResolvedConfig client "{\"country\": \"India\"}" $ Just ["country_image_url", "hyperpay_version"]
            defaults        <- getDefaultConfig client $ Just ["country_image_url", "hyperpay_version"]
            filteredConfig  <- getFullConfigStateWithFilter client (Just "{\"os\": \"android\"}") (Just "hyperpay")
            print config
            print lastModified
            print overrides
            print defaults
            print filteredConfig
            threadDelay 1000000000
    pure ()
