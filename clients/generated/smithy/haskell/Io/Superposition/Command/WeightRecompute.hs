module Io.Superposition.Command.WeightRecompute (
    WeightRecomputeError(..),
    weightRecompute
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
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.WeightRecomputeInput
import qualified Io.Superposition.Model.WeightRecomputeOutput
import qualified Io.Superposition.Model.WeightRecomputeResponse
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data WeightRecomputeError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON WeightRecomputeError
instance Data.Aeson.FromJSON WeightRecomputeError

serWeightRecomputeHEADER :: Io.Superposition.Model.WeightRecomputeInput.WeightRecomputeInput -> Network.HTTP.Types.Header.RequestHeaders
serWeightRecomputeHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.WeightRecomputeInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.WeightRecomputeInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        config_tagsHeader = (Io.Superposition.Model.WeightRecomputeInput.config_tags input
                    Data.Functor.<&> Io.Superposition.Utility.toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-config-tags", Data.Text.Encoding.encodeUtf8 x)]
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader,
            config_tagsHeader
            ]
        
    

serWeightRecomputeLABEL :: Io.Superposition.Model.WeightRecomputeInput.WeightRecomputeInput -> Data.ByteString.ByteString
serWeightRecomputeLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "context",
        "weight",
        "recompute"
        ]
    

weightRecompute :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.WeightRecomputeInput.WeightRecomputeInputBuilder () -> IO (Data.Either.Either WeightRecomputeError Io.Superposition.Model.WeightRecomputeOutput.WeightRecomputeOutput)
weightRecompute client inputB = do
    let inputE = Io.Superposition.Model.WeightRecomputeInput.build inputB
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
                Network.HTTP.Client.path = serWeightRecomputeLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = (serWeightRecomputeHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.WeightRecomputeOutput.WeightRecomputeOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: Data.Maybe.Maybe ([] Io.Superposition.Model.WeightRecomputeResponse.WeightRecomputeResponse) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "data") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.WeightRecomputeOutput.build $ do
        Io.Superposition.Model.WeightRecomputeOutput.setData' data'DocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


