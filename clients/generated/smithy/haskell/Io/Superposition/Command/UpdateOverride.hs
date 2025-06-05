module Io.Superposition.Command.UpdateOverride (
    UpdateOverrideError(..),
    updateOverride
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
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.Model.UpdateOverrideInput
import qualified Io.Superposition.Model.UpdateOverrideOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data UpdateOverrideError =
    ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serUpdateOverridePAYLOAD:: Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput -> Network.HTTP.Client.RequestBody
serUpdateOverridePAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $
    Data.Aeson.encode $
    Io.Superposition.Model.UpdateOverrideInput.request input

serUpdateOverrideHEADER :: Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput -> Network.HTTP.Types.Header.RequestHeaders
serUpdateOverrideHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.UpdateOverrideInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.UpdateOverrideInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        config_tagsHeader = (Io.Superposition.Model.UpdateOverrideInput.config_tags input
                    Data.Functor.<&> Io.Superposition.Utility.toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-config-tags", Data.Text.Encoding.encodeUtf8 x)]
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader,
            config_tagsHeader
            ]
        
    

serUpdateOverrideLABEL :: Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInput -> Data.ByteString.ByteString
serUpdateOverrideLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "context",
        "overrides"
        ]
    

updateOverride :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.UpdateOverrideInput.UpdateOverrideInputBuilder () -> IO (Data.Either.Either UpdateOverrideError Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput)
updateOverride client inputB = do
    let inputE = Io.Superposition.Model.UpdateOverrideInput.build inputB
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
                Network.HTTP.Client.path = serUpdateOverrideLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serUpdateOverridePAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serUpdateOverrideHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.UpdateOverrideOutput.UpdateOverrideOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    change_reasonDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    override_idDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "override_id") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    weightDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "weight") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    descriptionDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "description") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_atDocumentE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "created_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    id'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "id") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    overrideDocumentE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "override") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_byDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "last_modified_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    valueDocumentE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "value") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Maybe.Maybe Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.UpdateOverrideOutput.build $ do
        Io.Superposition.Model.UpdateOverrideOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setOverrideId override_idDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setWeight weightDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setId' id'DocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setOverride overrideDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setValue valueDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.UpdateOverrideOutput.setLastModifiedAt last_modified_atDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


