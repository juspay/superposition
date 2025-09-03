module Io.Superposition.Command.ListExperiment (
    ListExperimentError(..),
    listExperiment
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ExperimentResponse
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.ListExperimentInput
import qualified Io.Superposition.Model.ListExperimentOutput
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ListExperimentError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON ListExperimentError
instance Data.Aeson.FromJSON ListExperimentError

serListExperimentQUERY :: Io.Superposition.Model.ListExperimentInput.ListExperimentInput -> Data.ByteString.ByteString
serListExperimentQUERY input =
    let
        staticParams = [
            ]
        
        all'Query = Io.Superposition.Model.ListExperimentInput.all' input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("all", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        experiment_nameQuery = Io.Superposition.Model.ListExperimentInput.experiment_name input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("experiment_name", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        from_dateQuery = Io.Superposition.Model.ListExperimentInput.from_date input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("from_date", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        countQuery = Io.Superposition.Model.ListExperimentInput.count input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("count", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        experiment_idsQuery = Io.Superposition.Model.ListExperimentInput.experiment_ids input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("experiment_ids", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        global_experiments_onlyQuery = Io.Superposition.Model.ListExperimentInput.global_experiments_only input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("global_experiments_only", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        sort_byQuery = Io.Superposition.Model.ListExperimentInput.sort_by input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("sort_by", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        experiment_group_idsQuery = Io.Superposition.Model.ListExperimentInput.experiment_group_ids input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("experiment_group_ids", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        created_byQuery = Io.Superposition.Model.ListExperimentInput.created_by input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("created_by", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        sort_onQuery = Io.Superposition.Model.ListExperimentInput.sort_on input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("sort_on", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        to_dateQuery = Io.Superposition.Model.ListExperimentInput.to_date input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("to_date", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Io.Superposition.Model.ListExperimentInput.page input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("page", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        statusQuery = Io.Superposition.Model.ListExperimentInput.status input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("status", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ all'Query ++ experiment_nameQuery ++ from_dateQuery ++ countQuery ++ experiment_idsQuery ++ global_experiments_onlyQuery ++ sort_byQuery ++ experiment_group_idsQuery ++ created_byQuery ++ sort_onQuery ++ to_dateQuery ++ pageQuery ++ statusQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serListExperimentHEADER :: Io.Superposition.Model.ListExperimentInput.ListExperimentInput -> Network.HTTP.Types.Header.RequestHeaders
serListExperimentHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ListExperimentInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ListExperimentInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serListExperimentLABEL :: Io.Superposition.Model.ListExperimentInput.ListExperimentInput -> Data.ByteString.ByteString
serListExperimentLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "experiments"
        ]
    

listExperiment :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ListExperimentInput.ListExperimentInputBuilder () -> IO (Data.Either.Either ListExperimentError Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput)
listExperiment client inputB = do
    let inputE = Io.Superposition.Model.ListExperimentInput.build inputB
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
                Network.HTTP.Client.path = serListExperimentLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serListExperimentQUERY input
                , Network.HTTP.Client.requestHeaders = (serListExperimentHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ListExperimentOutput.ListExperimentOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: [] Io.Superposition.Model.ExperimentResponse.ExperimentResponse <-
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
        
    
    Io.Superposition.Model.ListExperimentOutput.build $ do
        Io.Superposition.Model.ListExperimentOutput.setData' data'DocumentE
        Io.Superposition.Model.ListExperimentOutput.setTotalPages total_pagesDocumentE
        Io.Superposition.Model.ListExperimentOutput.setTotalItems total_itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


