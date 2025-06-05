module Io.Superposition.Command.CreateWebhook (
    CreateWebhookError(..),
    createWebhook
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
import qualified Io.Superposition.Model.CreateWebhookInput
import qualified Io.Superposition.Model.CreateWebhookOutput
import qualified Io.Superposition.Model.HttpMethod
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.Version
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data CreateWebhookError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serCreateWebhookPAYLOAD:: Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput -> Network.HTTP.Client.RequestBody
serCreateWebhookPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "change_reason" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.change_reason input,
        "method" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.method input,
        "name" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.name input,
        "description" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.description input,
        "version" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.version input,
        "enabled" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.enabled input,
        "url" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.url input,
        "events" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.events input,
        "custom_headers" Data.Aeson..= Io.Superposition.Model.CreateWebhookInput.custom_headers input
        ]
    

serCreateWebhookHEADER :: Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput -> Network.HTTP.Types.Header.RequestHeaders
serCreateWebhookHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.CreateWebhookInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.CreateWebhookInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serCreateWebhookLABEL :: Io.Superposition.Model.CreateWebhookInput.CreateWebhookInput -> Data.ByteString.ByteString
serCreateWebhookLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "webhook"
        ]
    

createWebhook :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.CreateWebhookInput.CreateWebhookInputBuilder () -> IO (Data.Either.Either CreateWebhookError Io.Superposition.Model.CreateWebhookOutput.CreateWebhookOutput)
createWebhook client inputB = do
    let inputE = Io.Superposition.Model.CreateWebhookInput.build inputB
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
        method = Network.HTTP.Types.Method.methodPost
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serCreateWebhookLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serCreateWebhookPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serCreateWebhookHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.CreateWebhookOutput.CreateWebhookOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    methodDocumentE :: Io.Superposition.Model.HttpMethod.HttpMethod <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "method") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    max_retriesDocumentE :: Integer <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "max_retries") responseObject
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
        
    
    versionDocumentE :: Io.Superposition.Model.Version.Version <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "version") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    created_byDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "created_by") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    enabledDocumentE :: Bool <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "enabled") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    urlDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "url") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_modified_atDocumentE :: Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "last_modified_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    change_reasonDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "change_reason") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    last_triggered_atDocumentE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "last_triggered_at") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    nameDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "name") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    eventsDocumentE :: [] Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "events") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    custom_headersDocumentE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "custom_headers") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.CreateWebhookOutput.build $ do
        Io.Superposition.Model.CreateWebhookOutput.setMethod methodDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setMaxRetries max_retriesDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setVersion versionDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setEnabled enabledDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setUrl urlDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setLastModifiedAt last_modified_atDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setLastTriggeredAt last_triggered_atDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setName nameDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setEvents eventsDocumentE
        Io.Superposition.Model.CreateWebhookOutput.setCustomHeaders custom_headersDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


