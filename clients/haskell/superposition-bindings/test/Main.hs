{-# OPTIONS_GHC -with-rtsopts="-N1" #-}

module Main (main) where

import FFI.Superposition qualified as FFI
import Test.HUnit qualified as HUnit

main :: IO HUnit.Counts
main = do
  HUnit.runTestTT $
    HUnit.TestList
      [ HUnit.TestLabel "Valid Call" $ HUnit.TestCase validCall,
        HUnit.TestLabel "In-Valid Call" $ HUnit.TestCase invalidCall,
        HUnit.TestLabel "Parse TOML - Valid" $ HUnit.TestCase parseTomlValid,
        HUnit.TestLabel "Parse TOML - Invalid Syntax" $ HUnit.TestCase parseTomlInvalidSyntax,
        HUnit.TestLabel "Parse TOML - Missing Section" $ HUnit.TestCase parseTomlMissingSection,
        HUnit.TestLabel "Parse TOML - Missing Position" $ HUnit.TestCase parseTomlMissingPosition
      ]

validCall :: IO ()
validCall = do
  let orig = "{\"k1\":\"v1\"}"
      params =
        FFI.defaultResolveParams
          { FFI.defaultConfig = Just orig,
            FFI.context = Just "[]",
            FFI.overrides = Just "{}",
            FFI.dimensionInfo = Just "{}",
            FFI.query = Just "{}"
          }
  result <- FFI.getResolvedConfig params
  case result of
    Right cfg -> HUnit.assertEqual ("Resolved config is in-correct: " ++ cfg) cfg orig
    Left e -> HUnit.assertFailure $ "Recieved error in valid call: " ++ e

invalidCall :: IO ()
invalidCall = do
  result <- FFI.getResolvedConfig FFI.defaultResolveParams
  case result of
    Right _ -> HUnit.assertFailure $ "Expected error, recieved: " ++ show result
    Left e -> HUnit.assertBool "Error should not be empty." (not $ null e)

-- TOML parsing tests
exampleToml :: String
exampleToml = unlines
  [ "[default-configs]"
  , "per_km_rate = { \"value\" = 20.0, \"schema\" = { \"type\" = \"number\" } }"
  , "surge_factor = { \"value\" = 0.0, \"schema\" = { \"type\" = \"number\" } }"
  , ""
  , "[dimensions]"
  , "city = { position = 1, schema = { \"type\" = \"string\", \"enum\" = [\"Bangalore\", \"Delhi\"] } }"
  , "vehicle_type = { position = 2, schema = { \"type\" = \"string\", \"enum\" = [ \"auto\", \"cab\", \"bike\", ] } }"
  , "hour_of_day = { position = 3, schema = { \"type\" = \"integer\", \"minimum\" = 0, \"maximum\" = 23 }}"
  , ""
  , "[[overrides]]"
  , "_context_ = {vehicle_type=\"cab\" }"
  , "per_km_rate = 25.0"
  , ""
  , "[[overrides]]"
  , "_context_ = {vehicle_type=\"bike\" }"
  , "per_km_rate = 15.0"
  , ""
  , "[[overrides]]"
  , "_context_ = {vehicle_type=\"bike\", city = \"Bangalore\" }"
  , "per_km_rate = 22.0"
  , ""
  , "[[overrides]]"
  , "_context_ = {vehicle_type=\"cab\", city = \"Delhi\", hour_of_day = 18 }"
  , "per_km_rate = 5.0"
  , ""
  , "[[overrides]]"
  , "_context_ = {vehicle_type=\"cab\", city = \"Delhi\", hour_of_day = 18 }"
  , "per_km_rate = 6.0"
  ]

parseTomlValid :: IO ()
parseTomlValid = do
  result <- FFI.parseTomlConfig exampleToml
  case result of
    Right _val -> HUnit.assertBool "Valid TOML should parse successfully" True
    Left e -> HUnit.assertFailure $ "Failed to parse valid TOML: " ++ e

parseTomlInvalidSyntax :: IO ()
parseTomlInvalidSyntax = do
  let invalidToml = "[invalid toml content ][["
  result <- FFI.parseTomlConfig invalidToml
  case result of
    Right _ -> HUnit.assertFailure "Expected error for invalid TOML syntax"
    Left e -> do
      HUnit.assertBool "Error message should contain TOML" ("TOML" `elem` words e)
      HUnit.assertBool "Error should not be empty" (not $ null e)

parseTomlMissingSection :: IO ()
parseTomlMissingSection = do
  let invalidToml = "[dimensions]\ncity = { position = 1, schema = { \"type\" = \"string\" } }"
  result <- FFI.parseTomlConfig invalidToml
  case result of
    Right _ -> HUnit.assertFailure "Expected error for missing default-config section"
    Left e -> HUnit.assertBool "Error should not be empty" (not $ null e)

parseTomlMissingPosition :: IO ()
parseTomlMissingPosition = do
  let invalidToml = unlines
        [ "[default-config]"
        , "key1 = { value = 10, schema = { type = \"integer\" } }"
        , ""
        , "[dimensions]"
        , "city = { schema = { \"type\" = \"string\" } }"
        ]
  result <- FFI.parseTomlConfig invalidToml
  case result of
    Right _ -> HUnit.assertFailure "Expected error for missing position field"
    Left e -> HUnit.assertBool "Error should not be empty" (not $ null e)
