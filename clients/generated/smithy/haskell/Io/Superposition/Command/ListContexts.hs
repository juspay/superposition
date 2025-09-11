module Io.Superposition.Command.ListContexts (
    ListContextsError(..),
    listContexts
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
import qualified Io.Superposition.Model.ContextResponse
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListContextsInput
import qualified Io.Superposition.Model.ListContextsOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ListContextsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ListContextsError
instance Data.Aeson.FromJSON ListContextsError

serListContextsQUERY :: Io.Superposition.Model.ListContextsInput.ListContextsInput -> Data.ByteString.ByteString
serListContextsQUERY input =
    let
        staticParams = [
            ]
        
        all'Query = Io.Superposition.Model.ListContextsInput.all' input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("all", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        sort_onQuery = Io.Superposition.Model.ListContextsInput.sort_on input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("sort_on", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        prefixQuery = Io.Superposition.Model.ListContextsInput.prefix input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("prefix", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        countQuery = Io.Superposition.Model.ListContextsInput.count input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("count", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        plaintextQuery = Io.Superposition.Model.ListContextsInput.plaintext input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("plaintext", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Io.Superposition.Model.ListContextsInput.page input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("page", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        sort_byQuery = Io.Superposition.Model.ListContextsInput.sort_by input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("sort_by", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        last_modified_byQuery = Io.Superposition.Model.ListContextsInput.last_modified_by input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("last_modified_by", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        created_byQuery = Io.Superposition.Model.ListContextsInput.created_by input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("created_by", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ all'Query ++ sort_onQuery ++ prefixQuery ++ countQuery ++ plaintextQuery ++ pageQuery ++ sort_byQuery ++ last_modified_byQuery ++ created_byQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serListContextsHEADER :: Io.Superposition.Model.ListContextsInput.ListContextsInput -> Network.HTTP.Types.Header.RequestHeaders
serListContextsHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ListContextsInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ListContextsInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serListContextsLABEL :: Io.Superposition.Model.ListContextsInput.ListContextsInput -> Data.ByteString.ByteString
serListContextsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "context",
        "list"
        ]
    

listContexts :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListContextsInput.ListContextsInputBuilder () -> IO (Data.Either.Either ListContextsError Io.Superposition.Model.ListContextsOutput.ListContextsOutput)
listContexts client inputB = do
    let inputE = Io.Superposition.Model.ListContextsInput.build inputB
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
                Network.HTTP.Client.path = serListContextsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serListContextsQUERY input
                , Network.HTTP.Client.requestHeaders = (serListContextsHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ListContextsOutput.ListContextsOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextResponse.ContextResponse) <-
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
        
    
    Io.Superposition.Model.ListContextsOutput.build $ do
        Io.Superposition.Model.ListContextsOutput.setData' data'DocumentE
        Io.Superposition.Model.ListContextsOutput.setTotalPages total_pagesDocumentE
        Io.Superposition.Model.ListContextsOutput.setTotalItems total_itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


