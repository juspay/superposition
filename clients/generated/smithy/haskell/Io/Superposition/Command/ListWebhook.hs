module Io.Superposition.Command.ListWebhook (
    ListWebhookError(..),
    listWebhook
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
import qualified Data.Int
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListWebhookInput
import qualified Io.Superposition.Model.ListWebhookOutput
import qualified Io.Superposition.Model.WebhookResponse
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ListWebhookError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serListWebhookQUERY :: Io.Superposition.Model.ListWebhookInput.ListWebhookInput -> Data.ByteString.ByteString
serListWebhookQUERY input =
    let
        staticParams = [
            ]
        
        all'Query = Io.Superposition.Model.ListWebhookInput.all' input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("all", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        countQuery = Io.Superposition.Model.ListWebhookInput.count input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("count", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Io.Superposition.Model.ListWebhookInput.page input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("page", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ all'Query ++ countQuery ++ pageQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serListWebhookHEADER :: Io.Superposition.Model.ListWebhookInput.ListWebhookInput -> Network.HTTP.Types.Header.RequestHeaders
serListWebhookHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ListWebhookInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ListWebhookInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serListWebhookLABEL :: Io.Superposition.Model.ListWebhookInput.ListWebhookInput -> Data.ByteString.ByteString
serListWebhookLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "webhook"
        ]
    

listWebhook :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListWebhookInput.ListWebhookInputBuilder () -> IO (Data.Either.Either ListWebhookError Io.Superposition.Model.ListWebhookOutput.ListWebhookOutput)
listWebhook client inputB = do
    let inputE = Io.Superposition.Model.ListWebhookInput.build inputB
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
                Network.HTTP.Client.path = serListWebhookLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serListWebhookQUERY input
                , Network.HTTP.Client.requestHeaders = (serListWebhookHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ListWebhookOutput.ListWebhookOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: [] Io.Superposition.Model.WebhookResponse.WebhookResponse <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "data") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    total_pagesDocumentE :: Data.Int.Int64 <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "total_pages") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    total_itemsDocumentE :: Data.Int.Int64 <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "total_items") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.ListWebhookOutput.build $ do
        Io.Superposition.Model.ListWebhookOutput.setData' data'DocumentE
        Io.Superposition.Model.ListWebhookOutput.setTotalPages total_pagesDocumentE
        Io.Superposition.Model.ListWebhookOutput.setTotalItems total_itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


