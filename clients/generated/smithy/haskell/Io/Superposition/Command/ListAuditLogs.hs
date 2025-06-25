module Io.Superposition.Command.ListAuditLogs (
    ListAuditLogsError(..),
    listAuditLogs
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
import qualified Io.Superposition.Model.AuditLogFull
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListAuditLogsInput
import qualified Io.Superposition.Model.ListAuditLogsOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ListAuditLogsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serListAuditLogsQUERY :: Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInput -> Data.ByteString.ByteString
serListAuditLogsQUERY input =
    let
        staticParams = [
            ]
        
        all'Query = Io.Superposition.Model.ListAuditLogsInput.all' input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("all", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        tablesQuery = Io.Superposition.Model.ListAuditLogsInput.tables input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("table", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        from_dateQuery = Io.Superposition.Model.ListAuditLogsInput.from_date input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("from_date", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        to_dateQuery = Io.Superposition.Model.ListAuditLogsInput.to_date input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("to_date", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        countQuery = Io.Superposition.Model.ListAuditLogsInput.count input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("count", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        actionQuery = Io.Superposition.Model.ListAuditLogsInput.action input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("action", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Io.Superposition.Model.ListAuditLogsInput.page input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("page", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        usernameQuery = Io.Superposition.Model.ListAuditLogsInput.username input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("username", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ all'Query ++ tablesQuery ++ from_dateQuery ++ to_dateQuery ++ countQuery ++ actionQuery ++ pageQuery ++ usernameQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serListAuditLogsHEADER :: Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInput -> Network.HTTP.Types.Header.RequestHeaders
serListAuditLogsHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ListAuditLogsInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ListAuditLogsInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serListAuditLogsLABEL :: Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInput -> Data.ByteString.ByteString
serListAuditLogsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "audit"
        ]
    

listAuditLogs :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListAuditLogsInput.ListAuditLogsInputBuilder () -> IO (Data.Either.Either ListAuditLogsError Io.Superposition.Model.ListAuditLogsOutput.ListAuditLogsOutput)
listAuditLogs client inputB = do
    let inputE = Io.Superposition.Model.ListAuditLogsInput.build inputB
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
                Network.HTTP.Client.path = serListAuditLogsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serListAuditLogsQUERY input
                , Network.HTTP.Client.requestHeaders = (serListAuditLogsHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ListAuditLogsOutput.ListAuditLogsOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: Data.Maybe.Maybe ([] Io.Superposition.Model.AuditLogFull.AuditLogFull) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "data") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    total_pagesDocumentE :: Data.Maybe.Maybe Integer <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "total_pages") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    total_itemsDocumentE :: Data.Maybe.Maybe Integer <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "total_items") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.ListAuditLogsOutput.build $ do
        Io.Superposition.Model.ListAuditLogsOutput.setData' data'DocumentE
        Io.Superposition.Model.ListAuditLogsOutput.setTotalPages total_pagesDocumentE
        Io.Superposition.Model.ListAuditLogsOutput.setTotalItems total_itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


