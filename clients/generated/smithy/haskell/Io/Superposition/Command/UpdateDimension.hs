module Io.Superposition.Command.UpdateDimension (
    UpdateDimensionError(..),
    updateDimension
) where
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Bifunctor
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Either
import qualified Data.Function
import qualified Data.Functor
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.Model.UpdateDimensionInput
import qualified Io.Superposition.Model.UpdateDimensionOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data UpdateDimensionError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON UpdateDimensionError
instance Data.Aeson.FromJSON UpdateDimensionError

serUpdateDimensionPAYLOAD:: Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput -> Network.HTTP.Client.RequestBody
serUpdateDimensionPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "schema" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.schema input,
        "autocomplete_function_name" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.autocomplete_function_name input,
        "change_reason" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.change_reason input,
        "function_name" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.function_name input,
        "description" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.description input,
        "position" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.position input,
        "dependencies" Data.Aeson..= Io.Superposition.Model.UpdateDimensionInput.dependencies input
        ]
    

serUpdateDimensionHEADER :: Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput -> Network.HTTP.Types.Header.RequestHeaders
serUpdateDimensionHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.UpdateDimensionInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.UpdateDimensionInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serUpdateDimensionLABEL :: Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInput -> Data.ByteString.ByteString
serUpdateDimensionLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "dimension",
        (Io.Superposition.Model.UpdateDimensionInput.dimension input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
        ]
    

updateDimension :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateDimensionInput.UpdateDimensionInputBuilder () -> IO (Data.Either.Either UpdateDimensionError Io.Superposition.Model.UpdateDimensionOutput.UpdateDimensionOutput)
updateDimension client inputB = do
    let inputE = Io.Superposition.Model.UpdateDimensionInput.build inputB
        baseUri = Io.Superposition.SuperpositionClient.endpointUri client
        httpManager = Io.Superposition.SuperpositionClient.httpManager client
        requestE = Network.HTTP.Client.requestFromURI @(Data.Either.Either Control.Exception.SomeException) baseUri
    
    case (inputE, requestE) of
        (Data.Either.Left err, _) -> return $ Data.Either.Left (BuilderError err)
        (_, Data.Either.Left err) -> return $ Data.Either.Left (RequestError $ Data.Text.pack $ show err)
        (Data.Either.Right input, Data.Either.Right req) -> do
            response <- Network.HTTP.Client.httpLbs (toRequest input req) httpManager
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodPut
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serUpdateDimensionLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serUpdateDimensionPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serUpdateDimensionHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.UpdateDimensionOutput.UpdateDimensionOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    schemaDocumentE :: Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "schema") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    mandatoryDocumentE :: Data.Maybe.Maybe Bool <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "mandatory") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    dependenciesDocumentE :: [] Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "dependencies") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    dependency_graphDocumentE :: Data.Map.Map Data.Text.Text Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "dependency_graph") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    autocomplete_function_nameDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "autocomplete_function_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    function_nameDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "function_name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    dependentsDocumentE :: [] Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "dependents") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    positionDocumentE :: Integer <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "position") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    dimensionDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "dimension") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.UpdateDimensionOutput.build $ do
        Io.Superposition.Model.UpdateDimensionOutput.setSchema schemaDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setMandatory mandatoryDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setLastModifiedAt last_modified_atDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setDependencies dependenciesDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setDependencyGraph dependency_graphDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setAutocompleteFunctionName autocomplete_function_nameDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setFunctionName function_nameDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setDependents dependentsDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setPosition positionDocumentE
        Io.Superposition.Model.UpdateDimensionOutput.setDimension dimensionDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


