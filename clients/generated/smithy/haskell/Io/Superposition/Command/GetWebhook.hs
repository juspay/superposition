module Io.Superposition.Command.GetWebhook (
    GetWebhookError(..),
    getWebhook
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
import qualified Io.Superposition.Model.GetWebhookInput
import qualified Io.Superposition.Model.GetWebhookOutput
import qualified Io.Superposition.Model.HttpMethod
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.Version
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data GetWebhookError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serGetWebhookHEADER :: Io.Superposition.Model.GetWebhookInput.GetWebhookInput -> Network.HTTP.Types.Header.RequestHeaders
serGetWebhookHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.GetWebhookInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.GetWebhookInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serGetWebhookLABEL :: Io.Superposition.Model.GetWebhookInput.GetWebhookInput -> Data.ByteString.ByteString
serGetWebhookLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "webhook",
        (Io.Superposition.Model.GetWebhookInput.name input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
        ]
    

getWebhook :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetWebhookInput.GetWebhookInputBuilder () -> IO (Data.Either.Either GetWebhookError Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput)
getWebhook client inputB = do
    let inputE = Io.Superposition.Model.GetWebhookInput.build inputB
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
        method = Network.HTTP.Types.Method.methodGet
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serGetWebhookLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = (serGetWebhookHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.GetWebhookOutput.GetWebhookOutput
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
        
    
    Io.Superposition.Model.GetWebhookOutput.build $ do
        Io.Superposition.Model.GetWebhookOutput.setMethod methodDocumentE
        Io.Superposition.Model.GetWebhookOutput.setMaxRetries max_retriesDocumentE
        Io.Superposition.Model.GetWebhookOutput.setDescription descriptionDocumentE
        Io.Superposition.Model.GetWebhookOutput.setCreatedAt created_atDocumentE
        Io.Superposition.Model.GetWebhookOutput.setLastModifiedBy last_modified_byDocumentE
        Io.Superposition.Model.GetWebhookOutput.setVersion versionDocumentE
        Io.Superposition.Model.GetWebhookOutput.setCreatedBy created_byDocumentE
        Io.Superposition.Model.GetWebhookOutput.setEnabled enabledDocumentE
        Io.Superposition.Model.GetWebhookOutput.setUrl urlDocumentE
        Io.Superposition.Model.GetWebhookOutput.setLastModifiedAt last_modified_atDocumentE
        Io.Superposition.Model.GetWebhookOutput.setChangeReason change_reasonDocumentE
        Io.Superposition.Model.GetWebhookOutput.setLastTriggeredAt last_triggered_atDocumentE
        Io.Superposition.Model.GetWebhookOutput.setName nameDocumentE
        Io.Superposition.Model.GetWebhookOutput.setEvents eventsDocumentE
        Io.Superposition.Model.GetWebhookOutput.setCustomHeaders custom_headersDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


