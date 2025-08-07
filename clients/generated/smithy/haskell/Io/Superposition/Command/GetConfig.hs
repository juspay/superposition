module Io.Superposition.Command.GetConfig (
    GetConfigError(..),
    getConfig
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.ContextPartial
import qualified Io.Superposition.Model.GetConfigInput
import qualified Io.Superposition.Model.GetConfigOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data GetConfigError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetConfigError
instance Data.Aeson.FromJSON GetConfigError

serGetConfigPAYLOAD:: Io.Superposition.Model.GetConfigInput.GetConfigInput -> Network.HTTP.Client.RequestBody
serGetConfigPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "context" Data.Aeson..= Io.Superposition.Model.GetConfigInput.context input
        ]
    

serGetConfigQUERY :: Io.Superposition.Model.GetConfigInput.GetConfigInput -> Data.ByteString.ByteString
serGetConfigQUERY input =
    let
        staticParams = [
            ]
        
        prefixQuery = Io.Superposition.Model.GetConfigInput.prefix input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("prefix", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        versionQuery = Io.Superposition.Model.GetConfigInput.version input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Io.Superposition.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("version", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ prefixQuery ++ versionQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serGetConfigHEADER :: Io.Superposition.Model.GetConfigInput.GetConfigInput -> Network.HTTP.Types.Header.RequestHeaders
serGetConfigHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.GetConfigInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.GetConfigInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serGetConfigLABEL :: Io.Superposition.Model.GetConfigInput.GetConfigInput -> Data.ByteString.ByteString
serGetConfigLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "config"
        ]
    

getConfig :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetConfigInput.GetConfigInputBuilder () -> IO (Data.Either.Either GetConfigError Io.Superposition.Model.GetConfigOutput.GetConfigOutput)
getConfig client inputB = do
    let inputE = Io.Superposition.Model.GetConfigInput.build inputB
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
                Network.HTTP.Client.path = serGetConfigLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serGetConfigQUERY input
                , Network.HTTP.Client.requestBody = serGetConfigPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serGetConfigHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.GetConfigOutput.GetConfigOutput
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
        
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    contextsDocumentE :: Data.Maybe.Maybe ([] Io.Superposition.Model.ContextPartial.ContextPartial) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "contexts") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    overridesDocumentE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text (Data.Map.Map Data.Text.Text Data.Aeson.Value)) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "overrides") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    default_configsDocumentE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Aeson.Value) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "default_configs") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.GetConfigOutput.build $ do
        Io.Superposition.Model.GetConfigOutput.setAuditId audit_idHeaderE
        Io.Superposition.Model.GetConfigOutput.setVersion versionHeaderE
        Io.Superposition.Model.GetConfigOutput.setLastModified last_modifiedHeaderE
        Io.Superposition.Model.GetConfigOutput.setContexts contextsDocumentE
        Io.Superposition.Model.GetConfigOutput.setOverrides overridesDocumentE
        Io.Superposition.Model.GetConfigOutput.setDefaultConfigs default_configsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


