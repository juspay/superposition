module Io.Superposition.Command.ListDefaultConfigs (
    ListDefaultConfigsError(..),
    listDefaultConfigs
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.DefaultConfigFull
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListDefaultConfigsInput
import qualified Io.Superposition.Model.ListDefaultConfigsOutput
import qualified Io.Superposition.Model.ResourceNotFound
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ListDefaultConfigsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | ResourceNotFound Io.Superposition.Model.ResourceNotFound.ResourceNotFound
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ListDefaultConfigsError
instance Data.Aeson.FromJSON ListDefaultConfigsError

serListDefaultConfigsQUERY :: Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInput -> Data.ByteString.ByteString
serListDefaultConfigsQUERY input =
    let
        staticParams = [
            ]
        
        all'Query = Io.Superposition.Model.ListDefaultConfigsInput.all' input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("all", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        countQuery = Io.Superposition.Model.ListDefaultConfigsInput.count input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("count", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Io.Superposition.Model.ListDefaultConfigsInput.page input
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
    

serListDefaultConfigsHEADER :: Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInput -> Network.HTTP.Types.Header.RequestHeaders
serListDefaultConfigsHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ListDefaultConfigsInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ListDefaultConfigsInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serListDefaultConfigsLABEL :: Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInput -> Data.ByteString.ByteString
serListDefaultConfigsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "default-config"
        ]
    

listDefaultConfigs :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListDefaultConfigsInput.ListDefaultConfigsInputBuilder () -> IO (Data.Either.Either ListDefaultConfigsError Io.Superposition.Model.ListDefaultConfigsOutput.ListDefaultConfigsOutput)
listDefaultConfigs client inputB = do
    let inputE = Io.Superposition.Model.ListDefaultConfigsInput.build inputB
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
                Network.HTTP.Client.path = serListDefaultConfigsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serListDefaultConfigsQUERY input
                , Network.HTTP.Client.requestHeaders = (serListDefaultConfigsHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ListDefaultConfigsOutput.ListDefaultConfigsOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: Data.Maybe.Maybe ([] Io.Superposition.Model.DefaultConfigFull.DefaultConfigFull) <-
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
        
    
    Io.Superposition.Model.ListDefaultConfigsOutput.build $ do
        Io.Superposition.Model.ListDefaultConfigsOutput.setData' data'DocumentE
        Io.Superposition.Model.ListDefaultConfigsOutput.setTotalPages total_pagesDocumentE
        Io.Superposition.Model.ListDefaultConfigsOutput.setTotalItems total_itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


