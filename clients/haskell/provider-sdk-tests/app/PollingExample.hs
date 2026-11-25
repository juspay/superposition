{-# LANGUAGE OverloadedStrings #-}

-- | Minimal example that exercises the Superposition Haskell provider's
-- polling refresh against a real deployment.
--
-- Run with:
--
-- @
--   cabal run --project-file=cabal.project.ci polling-example
-- @
--
-- By default this talks to the staging deployment
-- workspace. Every field can be overridden with environment variables:
--
-- @
--   SUPERPOSITION_ENDPOINT   default: http://localhost:8080
--   SUPERPOSITION_ORG        default: org
--   SUPERPOSITION_WORKSPACE  default: workspace
--   SUPERPOSITION_TOKEN      default: 12345678
--   POLL_INTERVAL_SECONDS    default: 5
--   WATCH_SECONDS            default: 30
-- @
--
-- The provider only starts polling after 'initialize'. At 'LevelDebug' the
-- provider logs a @Running refresh.@ line for every poll, which makes it easy
-- to see that polling is actually happening. If you edit one of the resolved
-- configs in Superposition while the watch loop is running, the next poll picks
-- up the new value.
module Main (main) where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (finally)
import           Data.Aeson                   qualified as Aeson
import           Data.Aeson.Key               qualified as Key
import           Data.Aeson.KeyMap            qualified as KeyMap
import           Data.ByteString.Lazy.Char8   qualified as LBS
import           Data.OpenFeature.EvaluationContext qualified as EC
import           Data.OpenFeature.FeatureProvider qualified as OF
import           Data.OpenFeature.SuperpositionProvider qualified as P
import           Data.Text                    (Text)
import           Data.Text                    qualified as T
import           Data.Text.Encoding           qualified as TE
import           GHC.IO.Encoding              (setLocaleEncoding, utf8)
import           Network.URI                  qualified as URI
import           System.Environment           (lookupEnv)
import           System.IO                    (BufferMode (LineBuffering),
                                               hSetBuffering, stdout)

defaultEndpoint :: Text
defaultEndpoint = "http://localhost:8080/"

parseEndpoint :: Text -> URI.URI
parseEndpoint value =
  case URI.parseURI (T.unpack value) of
    Just uri -> uri
    Nothing  -> error ("Invalid endpoint URI: " ++ T.unpack value)

envOrDefault :: String -> Text -> IO Text
envOrDefault name fallback = maybe fallback T.pack <$> lookupEnv name

envInt :: String -> Int -> IO Int
envInt name fallback = maybe fallback read <$> lookupEnv name

evalCtx :: [(Text, Aeson.Value)] -> EC.EvaluationContext
evalCtx = foldr (uncurry EC.withCustomField) EC.defaultContext

-- | Resolve a string, retrying until the first poll has populated the cache.
resolveStringWithRetries :: OF.FeatureProvider p => p -> Int -> Text -> EC.EvaluationContext -> IO String
resolveStringWithRetries provider attempts key ctx
  | attempts <= 0 = do
      result <- OF.resolveStringValue provider key ctx
      case result of
        Right details -> pure (T.unpack (OF.value details))
        Left err      -> error ("Could not resolve " ++ T.unpack key ++ ": " ++ show err)
  | otherwise = do
      result <- OF.resolveStringValue provider key ctx
      case result of
        Right details -> pure (T.unpack (OF.value details))
        Left _        -> threadDelay 1000000 >> resolveStringWithRetries provider (attempts - 1) key ctx

-- | Print every key/value pair in the fully resolved config. This uses
-- 'resolveAllConfig' so it also shows keys that the example does not ask for
-- explicitly (for example a newly added default config).
printAllConfig :: P.SuperpositionProvider -> IO ()
printAllConfig provider = do
  result <- P.resolveAllConfig provider EC.defaultContext
  case result of
    Left err -> putStrLn ("  [resolveAllConfig error] " ++ err)
    Right json ->
      case (Aeson.eitherDecodeStrict' (TE.encodeUtf8 (T.pack json)) :: Either String Aeson.Value) of
        Left err -> putStrLn ("  [decode error] " ++ err ++ ": " ++ json)
        Right (Aeson.Object obj) ->
          case KeyMap.toList obj of
            [] -> putStrLn "  (empty config)"
            kvs -> mapM_ (\(k, v) -> putStrLn ("  " ++ Key.toString k ++ " = " ++ LBS.unpack (Aeson.encode v))) kvs
        Right other -> putStrLn ("  " ++ LBS.unpack (Aeson.encode other))

-- | Print the values this workspace resolves for a few representative contexts.
dumpSnapshot :: P.SuperpositionProvider -> IO ()
dumpSnapshot provider = do
  let defaultCtx = EC.defaultContext
      visaInrCtx =
        evalCtx
          [ ("merchant_id", Aeson.String "QKRSBX"),
            ("payment_method", Aeson.String "VISA"),
            ("order_currency", Aeson.String "INR")
          ]
      mastercardInrCtx =
        evalCtx
          [ ("merchant_id", Aeson.String "QKRSBX"),
            ("payment_method", Aeson.String "MASTERCARD"),
            ("order_currency", Aeson.String "INR")
          ]
  merchant <- resolveStringWithRetries provider 30 "acquirer_merchant_id" defaultCtx
  visaMerchant <- resolveStringWithRetries provider 30 "acquirer_merchant_id" visaInrCtx
  mcMerchant <- resolveStringWithRetries provider 30 "merchant_id" mastercardInrCtx
  mcAcquirer <- resolveStringWithRetries provider 30 "acquirer_name" mastercardInrCtx
  putStrLn "Resolved config:"
  putStrLn ("  default                acquirer_merchant_id = " ++ merchant)
  putStrLn ("  QKRSBX/VISA/INR        acquirer_merchant_id = " ++ visaMerchant)
  putStrLn ("  QKRSBX/MASTERCARD/INR  merchant_id = " ++ mcMerchant ++ ", acquirer_name = " ++ mcAcquirer)
  putStrLn "All resolved keys:"
  printAllConfig provider

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering

  endpointValue <- envOrDefault "SUPERPOSITION_ENDPOINT" defaultEndpoint
  orgId <- envOrDefault "SUPERPOSITION_ORG" "org"
  workspaceId <- envOrDefault "SUPERPOSITION_WORKSPACE" "workspace"
  token <- envOrDefault "SUPERPOSITION_TOKEN" "12345678"
  pollInterval <- envInt "POLL_INTERVAL_SECONDS" 5
  watchSeconds <- envInt "WATCH_SECONDS" 30
  let endpoint = parseEndpoint endpointValue

  putStrLn "=== Superposition Haskell provider polling example ==="
  putStrLn ("Endpoint:  " ++ T.unpack endpointValue)
  putStrLn ("Org:       " ++ T.unpack orgId)
  putStrLn ("Workspace: " ++ T.unpack workspaceId)
  putStrLn ("Poll:      every " ++ show pollInterval ++ "s")
  putStrLn ""

  let options =
        P.defaultProviderOptions
          { P.orgId = orgId,
            P.workspaceId = workspaceId,
            P.endpoint = endpoint,
            P.token = token,
            P.refreshOptions = P.Poll pollInterval,
            -- No experiments in this workspace; adding an experiments poll
            -- would just log an empty list every interval.
            P.experimentationRefreshOptions = Nothing,
            -- LevelDebug prints "Running refresh." for every poll.
            P.logLevel = P.LevelDebug
          }

  provider <- either (error . T.unpack) pure =<< P.newSuperpositionProvider options
  flip finally (P.closeSuperpositionProvider provider) $ do
    OF.initialize provider EC.defaultContext
    putStrLn "Provider initialized; waiting for the first poll..."

    putStrLn ""
    putStrLn "--- snapshot at startup ---"
    dumpSnapshot provider

    putStrLn ""
    putStrLn ("--- watching for " ++ show watchSeconds ++ "s ---")
    putStrLn "Edit a config in Superposition now; the next poll will pick it up."

    let tick = watchSeconds `div` pollInterval
        loop 0 = pure ()
        loop n = do
          threadDelay (pollInterval * 1000000)
          value <- resolveStringWithRetries provider 1 "acquirer_merchant_id" EC.defaultContext
          putStrLn ("[poll] acquirer_merchant_id = " ++ value)
          printAllConfig provider
          loop (n - 1)
    loop tick

    putStrLn ""
    putStrLn "--- snapshot after watching ---"
    dumpSnapshot provider
    putStrLn ""
    putStrLn "Done. Polling is working if \"Running refresh.\" appeared every interval."
