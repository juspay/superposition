module Io.Superposition.Command.UpdateDefaultConfig (
    UpdateDefaultConfigError(..),
    updateDefaultConfig
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.Model.UpdateDefaultConfigInput
import qualified Io.Superposition.Model.UpdateDefaultConfigOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data UpdateDefaultConfigError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serUpdateDefaultConfigPAYLOAD:: Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput -> Network.HTTP.Client.RequestBody
serUpdateDefaultConfigPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "schema" Data.Aeson..= Io.Superposition.Model.UpdateDefaultConfigInput.schema input,
        "change_reason" Data.Aeson..= Io.Superposition.Model.UpdateDefaultConfigInput.change_reason input,
        "function_name" Data.Aeson..= Io.Superposition.Model.UpdateDefaultConfigInput.function_name input,
        "description" Data.Aeson..= Io.Superposition.Model.UpdateDefaultConfigInput.description input,
        "value" Data.Aeson..= Io.Superposition.Model.UpdateDefaultConfigInput.value input
        ]
    

serUpdateDefaultConfigHEADER :: Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput -> Network.HTTP.Types.Header.RequestHeaders
serUpdateDefaultConfigHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.UpdateDefaultConfigInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.UpdateDefaultConfigInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serUpdateDefaultConfigLABEL :: Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInput -> Data.ByteString.ByteString
serUpdateDefaultConfigLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "default-config",
        (Io.Superposition.Model.UpdateDefaultConfigInput.key input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
        ]
    

updateDefaultConfig :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateDefaultConfigInput.UpdateDefaultConfigInputBuilder () -> IO (Data.Either.Either UpdateDefaultConfigError Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput)
updateDefaultConfig client inputB = do
    let inputE = Io.Superposition.Model.UpdateDefaultConfigInput.build inputB
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
                Network.HTTP.Client.path = serUpdateDefaultConfigLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serUpdateDefaultConfigPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serUpdateDefaultConfigHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.UpdateDefaultConfigOutput.UpdateDefaultConfigOutput
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
        
    
    valueDocumentE :: Data.Aeson.Value <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "value") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    keyDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "key") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.UpdateDefaultConfigOutput.build $ do
        Io.Superposition.Model.UpdateDefaultConfigOutput.setSchema schemaDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setFunctionName function_nameDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setValue valueDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setKey keyDocumentE
        Io.Superposition.Model.UpdateDefaultConfigOutput.setLastModifiedAt last_modified_atDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


