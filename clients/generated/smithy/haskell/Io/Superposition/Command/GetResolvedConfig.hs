module Io.Superposition.Command.GetResolvedConfig (
    GetResolvedConfigError(..),
    getResolvedConfig
) where
import qualified Control.Exception
import qualified Data.Aeson
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.GetResolvedConfigInput
import qualified Io.Superposition.Model.GetResolvedConfigOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data GetResolvedConfigError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetResolvedConfigError
instance Data.Aeson.FromJSON GetResolvedConfigError

serGetResolvedConfigPAYLOAD:: Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput -> Network.HTTP.Client.RequestBody
serGetResolvedConfigPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "context" Data.Aeson..= Io.Superposition.Model.GetResolvedConfigInput.context input
        ]
    

serGetResolvedConfigQUERY :: Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput -> Data.ByteString.ByteString
serGetResolvedConfigQUERY input =
    let
        staticParams = [
            ]
        
        show_reasoningQuery = Io.Superposition.Model.GetResolvedConfigInput.show_reasoning input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("show_reasoning", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        prefixQuery = Io.Superposition.Model.GetResolvedConfigInput.prefix input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("prefix", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        context_idQuery = Io.Superposition.Model.GetResolvedConfigInput.context_id input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("context_id", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        versionQuery = Io.Superposition.Model.GetResolvedConfigInput.version input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("version", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ show_reasoningQuery ++ prefixQuery ++ context_idQuery ++ versionQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serGetResolvedConfigHEADER :: Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput -> Network.HTTP.Types.Header.RequestHeaders
serGetResolvedConfigHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.GetResolvedConfigInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        merge_strategyHeader = (Io.Superposition.Model.GetResolvedConfigInput.merge_strategy input
                    Data.Functor.<&> Io.Superposition.Utility.toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-merge-strategy", Data.Text.Encoding.encodeUtf8 x)]
        
        org_idHeader = (Io.Superposition.Model.GetResolvedConfigInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            merge_strategyHeader,
            org_idHeader
            ]
        
    

serGetResolvedConfigLABEL :: Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInput -> Data.ByteString.ByteString
serGetResolvedConfigLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "config",
        "resolve"
        ]
    

getResolvedConfig :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetResolvedConfigInput.GetResolvedConfigInputBuilder () -> IO (Data.Either.Either GetResolvedConfigError Io.Superposition.Model.GetResolvedConfigOutput.GetResolvedConfigOutput)
getResolvedConfig client inputB = do
    let inputE = Io.Superposition.Model.GetResolvedConfigInput.build inputB
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
                Network.HTTP.Client.path = serGetResolvedConfigLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serGetResolvedConfigQUERY input
                , Network.HTTP.Client.requestBody = serGetResolvedConfigPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serGetResolvedConfigHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.GetResolvedConfigOutput.GetResolvedConfigOutput
deserializeResponse response = do
    audit_idHeaderE :: Data.Maybe.Maybe Data.Text.Text <-
        (findHeader "x-audit-id" Data.Functor.<&> Io.Superposition.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    versionHeaderE :: Data.Maybe.Maybe Data.Text.Text <-
        (findHeader "x-config-version" Data.Functor.<&> Io.Superposition.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    last_modifiedHeaderE :: Data.Maybe.Maybe Data.Time.UTCTime <-
        (findHeader "last-modified" Data.Functor.<&> Io.Superposition.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    configPayloadE :: Data.Maybe.Maybe Data.Aeson.Value <- do
        Data.Aeson.decode (Network.HTTP.Client.responseBody response)
                Data.Function.& Data.Either.Right
        
    
    Io.Superposition.Model.GetResolvedConfigOutput.build $ do
        Io.Superposition.Model.GetResolvedConfigOutput.setAuditId audit_idHeaderE
        Io.Superposition.Model.GetResolvedConfigOutput.setVersion versionHeaderE
        Io.Superposition.Model.GetResolvedConfigOutput.setLastModified last_modifiedHeaderE
        Io.Superposition.Model.GetResolvedConfigOutput.setConfig configPayloadE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


