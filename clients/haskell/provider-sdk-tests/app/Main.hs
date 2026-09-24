{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Aeson qualified as Aeson
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.OpenFeature.EvaluationContext qualified as EC
import Data.OpenFeature.EvaluationDetails qualified as ED
import Data.OpenFeature.FeatureProvider qualified as OF
import Data.OpenFeature.SuperpositionProvider qualified as P
import Data.Text qualified as T
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Io.Superposition.Command.CreateContext qualified as CC
import Io.Superposition.Command.CreateDefaultConfig qualified as CDC
import Io.Superposition.Command.CreateDimension qualified as CD
import Io.Superposition.Command.CreateExperiment qualified as CE
import Io.Superposition.Command.CreateOrganisation qualified as CO
import Io.Superposition.Command.CreateWorkspace qualified as CW
import Io.Superposition.Command.RampExperiment qualified as RE
import Io.Superposition.Model.ContextPut qualified as CP
import Io.Superposition.Model.CreateContextInput qualified as CCI
import Io.Superposition.Model.CreateDefaultConfigInput qualified as CDCI
import Io.Superposition.Model.CreateDimensionInput qualified as CDI
import Io.Superposition.Model.CreateExperimentInput qualified as CEI
import Io.Superposition.Model.CreateExperimentOutput qualified as CEO
import Io.Superposition.Model.CreateOrganisationInput qualified as COI
import Io.Superposition.Model.CreateOrganisationOutput qualified as COO
import Io.Superposition.Model.CreateWorkspaceInput qualified as CWI
import Io.Superposition.Model.DimensionType qualified as DT
import Io.Superposition.Model.RampExperimentInput qualified as REI
import Io.Superposition.Model.Unit qualified as U
import Io.Superposition.Model.Variant qualified as V
import Io.Superposition.Model.VariantType qualified as VT
import Io.Superposition.Model.WorkspaceStatus qualified as WS
import Io.Superposition.SuperpositionClient qualified as SDK
import Network.HTTP.Client qualified as HTTP
import Network.URI qualified as URI

workspaceId :: T.Text
workspaceId = "hsprovidertest"

unit :: U.Unit
unit = expectRight "unit" $ U.build (pure ())

endpointUri :: URI.URI
endpointUri = expectJust "endpoint uri" $ URI.parseURI "http://localhost:8080"

expectJust :: String -> Maybe a -> a
expectJust _ (Just a) = a
expectJust label Nothing = error label

expectRight :: String -> Either T.Text a -> a
expectRight _ (Right a) = a
expectRight label (Left e) = error (label ++ ": " ++ T.unpack e)

expectOk :: (Show e) => String -> IO (Either e a) -> IO a
expectOk label action = do
  result <- action
  case result of
    Right a -> pure a
    Left e -> error (label ++ " failed: " ++ show e)

-- | Retry an action that may return 'Left' (e.g. while the provider's
-- polling refresh fetches the first config), every second.
withRetries :: Int -> IO (Either e a) -> IO (Either e a)
withRetries attempts action = go attempts
  where
    go 0 = action
    go n = do
      result <- action
      case result of
        Right _ -> pure result
        Left _ -> threadDelay 1000000 >> go (n - 1)

mkClient :: HTTP.Manager -> SDK.SuperpositionClient
mkClient manager =
  expectRight "build client" $ SDK.build $ do
    SDK.setBearerauth (Just $ SDK.BearerAuth "12345678")
    SDK.setEndpointuri endpointUri
    SDK.setHttpmanager manager

createOrganisation :: SDK.SuperpositionClient -> IO T.Text
createOrganisation client = do
  response <- expectOk "create organisation" $ CO.createOrganisation client $ do
    COI.setName "hstestorg"
    COI.setAdminEmail "admin@hstestorg.com"
  putStrLn $
    "Organisation created successfully: "
      ++ T.unpack (COO.name response)
      ++ " with ID: "
      ++ T.unpack (COO.id' response)
  pure (COO.id' response)

createWorkspace :: SDK.SuperpositionClient -> T.Text -> IO ()
createWorkspace client orgId = do
  _ <- expectOk "create workspace" $ CW.createWorkspace client $ do
    CWI.setOrgId orgId
    CWI.setWorkspaceName workspaceId
    CWI.setWorkspaceAdminEmail "test@tests.com"
    CWI.setWorkspaceStatus (Just WS.ENABLED)
    CWI.setAllowExperimentSelfApproval (Just True)
    CWI.setAutoPopulateControl (Just False) -- disable auto populate control for testing experiment
    CWI.setEnableContextValidation (Just True)
    CWI.setEnableChangeReasonValidation (Just True)
  putStrLn "Workspace created!"

createDimensions :: SDK.SuperpositionClient -> T.Text -> IO ()
createDimensions client orgId = do
  putStrLn "Creating dimensions:"
  mapM_ createOne dimensions
  where
    createOne (dim, pos, schema, desc, dtype) = do
      _ <- expectOk ("create dimension " ++ show dim) $ CD.createDimension client $ do
        CDI.setOrgId orgId
        CDI.setWorkspaceId workspaceId
        CDI.setDimension dim
        CDI.setPosition pos
        CDI.setSchema schema
        CDI.setDescription desc
        CDI.setChangeReason ("adding " <> dim <> " dimension")
        CDI.setDimensionType dtype
      putStrLn ("  - Created dimension: " ++ T.unpack dim)
    dimensions :: [(T.Text, Int32, Map.Map T.Text Aeson.Value, T.Text, Maybe DT.DimensionType)]
    dimensions =
      [ ( "name",
          1,
          Map.fromList [("type", Aeson.String "string")],
          "customer name dimension",
          Just (DT.Regular unit)
        ),
        ( "city",
          2,
          Map.fromList [("type", Aeson.String "string")],
          "city dimension",
          Just (DT.Regular unit)
        ),
        ( "customers",
          1,
          Map.fromList
            [ ("type", Aeson.String "string"),
              ( "enum",
                Aeson.toJSON (["platinum", "gold", "otherwise"] :: [T.Text])
              ),
              ( "definitions",
                Aeson.object
                  [ "platinum"
                      Aeson..= Aeson.object
                        [ "in"
                            Aeson..= Aeson.toJSON
                              [ Aeson.object ["var" Aeson..= ("name" :: T.Text)],
                                Aeson.toJSON (["Agush", "Sauyav"] :: [T.Text])
                              ]
                        ],
                    "gold"
                      Aeson..= Aeson.object
                        [ "in"
                            Aeson..= Aeson.toJSON
                              [ Aeson.object ["var" Aeson..= ("name" :: T.Text)],
                                Aeson.toJSON (["Angit", "Bhrey"] :: [T.Text])
                              ]
                        ]
                  ]
              )
            ],
          "customers dimension",
          Just (DT.LocalCohort "name")
        )
      ]

createDefaultConfigs :: SDK.SuperpositionClient -> T.Text -> IO ()
createDefaultConfigs client orgId = do
  putStrLn "Creating default configs:"
  mapM_ createOne configs
  where
    createOne (k, val, schema, desc) = do
      _ <- expectOk ("create default config " ++ show k) $ CDC.createDefaultConfig client $ do
        CDCI.setOrgId orgId
        CDCI.setWorkspaceId workspaceId
        CDCI.setKey k
        CDCI.setValue val
        CDCI.setSchema schema
        CDCI.setDescription desc
        CDCI.setChangeReason ("adding " <> k <> " config")
      putStrLn ("  - Created config: " ++ T.unpack k)
    configs :: [(T.Text, Aeson.Value, Map.Map T.Text Aeson.Value, T.Text)]
    configs =
      [ ( "price",
          Aeson.Number 10000,
          Map.fromList
            [ ("type", Aeson.String "number"),
              ("minimum", Aeson.Number 0)
            ],
          "price as a positive number"
        ),
        ( "currency",
          Aeson.String "Rupee",
          Map.fromList
            [ ("type", Aeson.String "string"),
              ( "enum",
                Aeson.toJSON (["Rupee", "Dollar", "Euro"] :: [T.Text])
              )
            ],
          "currency as an enum"
        )
      ]

createOverrides :: SDK.SuperpositionClient -> T.Text -> IO ()
createOverrides client orgId = do
  putStrLn "Creating overrides:"
  mapM_ createOne overrides
  where
    mkRequest ctx ovr desc =
      expectRight "context put" $ CP.build $ do
        CP.setContext (Map.fromList ctx)
        CP.setOverride (Map.fromList ovr)
        CP.setDescription (Just desc)
        CP.setChangeReason "testing"
    createOne (ctx, ovr, desc) = do
      _ <- expectOk ("create override " ++ show desc) $ CC.createContext client $ do
        CCI.setOrgId orgId
        CCI.setWorkspaceId workspaceId
        CCI.setRequest (mkRequest ctx ovr desc)
      putStrLn ("  - Created override: " ++ T.unpack desc)
    overrides :: [([(T.Text, Aeson.Value)], [(T.Text, Aeson.Value)], T.Text)]
    overrides =
      [ ([("city", "Boston")], [("currency", "Dollar")], "Bostonian"),
        ([("city", "Berlin")], [("currency", "Euro")], "Berlin"),
        ([("customers", "platinum")], [("price", Aeson.Number 5000)], "platinum customer"),
        ([("customers", "gold")], [("price", Aeson.Number 8000)], "gold customers"),
        ([("name", "karbik")], [("price", Aeson.Number 1)], "edge case customer karbik")
      ]

createExperiments :: SDK.SuperpositionClient -> T.Text -> IO ()
createExperiments client orgId = do
  putStrLn "Creating experiments:"
  expOut <- expectOk "create experiment" $ CE.createExperiment client $ do
    CEI.setOrgId orgId
    CEI.setWorkspaceId workspaceId
    CEI.setName "testexperiment"
    CEI.setContext (Map.fromList [("city", "Bangalore")])
    CEI.setVariants
      [ expectRight "control variant" $ V.build $ do
          V.setId' "testexperiment-control"
          V.setVariantType VT.CONTROL
          -- Note: Using a different price to distinguish from default
          V.setOverrides (Map.fromList [("price", Aeson.Number 8000)]),
        expectRight "experimental variant" $ V.build $ do
          V.setId' "testexperiment-experimental"
          V.setVariantType VT.EXPERIMENTAL
          V.setOverrides (Map.fromList [("price", Aeson.Number 7000)])
      ]
    CEI.setDescription "test experimentation"
    CEI.setChangeReason "a reason"
  let expId = CEO.id' expOut
  putStrLn ("  - Created experiment: " ++ T.unpack expId)
  _ <- expectOk "ramp experiment" $ RE.rampExperiment client $ do
    REI.setOrgId orgId
    REI.setWorkspaceId workspaceId
    REI.setId' expId
    REI.setChangeReason "ramp the experiment"
    REI.setTrafficPercentage 50
  putStrLn "  - Ramped experiment to 50% traffic"

setupWithSDK :: SDK.SuperpositionClient -> T.Text -> IO ()
setupWithSDK client orgId = do
  putStrLn "\n=== Setting up test environment ===\n"
  createWorkspace client orgId
  createDimensions client orgId
  createDefaultConfigs client orgId
  createOverrides client orgId
  createExperiments client orgId
  putStrLn "\n=== Setup complete ===\n"

evalCtx :: [(T.Text, Aeson.Value)] -> EC.EvaluationContext
evalCtx = foldr (uncurry EC.withCustomField) EC.defaultContext

doubleValue :: OF.ResolutionDetails Double -> Double
doubleValue = OF.value

checkValue :: (Eq a, Show a) => String -> a -> IO (Either ED.EvaluationError (OF.ResolutionDetails a)) -> IO ()
checkValue label expected action = do
  result <- withRetries 30 action
  case result of
    Left err -> error (label ++ ": resolution error: " ++ show err)
    Right details
      | OF.value details == expected -> putStrLn ("  ✓ " ++ label ++ " passed")
      | otherwise ->
          error (label ++ ": expected " ++ show expected ++ ", got " ++ show (OF.value details))

runDemo :: T.Text -> IO ()
runDemo orgId = do
  let options =
        P.defaultProviderOptions
          { P.orgId = orgId,
            P.workspaceId = workspaceId,
            P.endpoint = endpointUri,
            P.token = "12345678",
            P.refreshOptions = P.Poll 1,
            P.experimentationRefreshOptions = Just (P.Poll 1),
            P.logLevel = P.LevelWarn
          }
  putStrLn "\n=== Starting OpenFeature tests ===\n"
  provider <- expectOk "create provider" (P.newSuperpositionProvider options)
  putStrLn "Provider created successfully"
  OF.initialize provider EC.defaultContext
  putStrLn "Provider initialized successfully\n"
  flip finally (P.closeSuperpositionProvider provider) $ do
    putStrLn "Test 1: Default values (no context)"
    checkValue "Default price is 10000" 10000 (OF.resolveDoubleValue provider "price" EC.defaultContext)
    checkValue "Default currency is Rupee" "Rupee" (OF.resolveStringValue provider "currency" EC.defaultContext)

    putStrLn "Test 2: Platinum customer - Agush (no city)"
    let agushCtx = evalCtx [("name", "Agush")]
    checkValue "Price is 5000 (platinum customer)" 5000 (OF.resolveDoubleValue provider "price" agushCtx)
    checkValue "Currency is default Rupee" "Rupee" (OF.resolveStringValue provider "currency" agushCtx)

    putStrLn "Test 3: Platinum customer - Sauyav with city Boston"
    let sauyavBostonCtx = evalCtx [("name", "Sauyav"), ("city", "Boston")]
    checkValue "Price is 5000" 5000 (OF.resolveDoubleValue provider "price" sauyavBostonCtx)
    checkValue "Currency is Dollar" "Dollar" (OF.resolveStringValue provider "currency" sauyavBostonCtx)

    putStrLn "Test 4: Regular customer - John (no city)"
    let johnCtx = evalCtx [("name", "John")]
    checkValue "Price is default 10000" 10000 (OF.resolveDoubleValue provider "price" johnCtx)
    checkValue "Currency is default Rupee" "Rupee" (OF.resolveStringValue provider "currency" johnCtx)

    putStrLn "Test 5: Platinum customer - Sauyav with city Berlin"
    let sauyavBerlinCtx = evalCtx [("name", "Sauyav"), ("city", "Berlin")]
    checkValue "Price is 5000" 5000 (OF.resolveDoubleValue provider "price" sauyavBerlinCtx)
    checkValue "Currency is Euro in Berlin" "Euro" (OF.resolveStringValue provider "currency" sauyavBerlinCtx)

    putStrLn "Test 6: Regular customer - John with city Boston"
    let johnBostonCtx = evalCtx [("name", "John"), ("city", "Boston")]
    checkValue "Price is default 10000" 10000 (OF.resolveDoubleValue provider "price" johnBostonCtx)
    checkValue "Currency is Dollar in Boston" "Dollar" (OF.resolveStringValue provider "currency" johnBostonCtx)

    putStrLn "Test 7: Edge case customer - karbik (specific override)"
    let karbikCtx = evalCtx [("name", "karbik")]
    checkValue "Price is 1 for karbik" 1 (OF.resolveDoubleValue provider "price" karbikCtx)
    checkValue "Currency is default Rupee" "Rupee" (OF.resolveStringValue provider "currency" karbikCtx)

    putStrLn "Test 8: Edge case customer - karbik with city Boston"
    let karbikBostonCtx = evalCtx [("name", "karbik"), ("city", "Boston")]
    checkValue "Price is 1 for karbik" 1 (OF.resolveDoubleValue provider "price" karbikBostonCtx)
    checkValue "Currency is Dollar in Boston" "Dollar" (OF.resolveStringValue provider "currency" karbikBostonCtx)

    putStrLn "Test 9: Experiment case: Bangalore pricing"
    let bangaloreCtx = EC.withTargetingKey "test" $ evalCtx [("city", "Bangalore")]
    price <- withRetries 30 (OF.resolveDoubleValue provider "price" bangaloreCtx)
    case price of
      Left err -> error ("Experiment price: resolution error: " ++ show err)
      Right details
        | doubleValue details `elem` [8000, 7000] ->
            putStrLn "  ✓ Price is either 8000 (control) or 7000 (experimental) in Bangalore"
        | otherwise ->
            error ("Experiment price: expected one of [8000, 7000], got " ++ show (doubleValue details))
    checkValue "Currency is Rupee in Bangalore" "Rupee" (OF.resolveStringValue provider "currency" bangaloreCtx)
    putStrLn "  ✓ Experiment Test passed"
  putStrLn "\n=== All tests passed! ===\n"

main :: IO ()
main = do
  -- The runner locale may be plain C; never let unicode output crash the tests.
  setLocaleEncoding utf8
  putStrLn "Starting Superposition OpenFeature demo and tests (Haskell)..."
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let client = mkClient manager
  orgId <- createOrganisation client
  setupWithSDK client orgId
  runDemo orgId
