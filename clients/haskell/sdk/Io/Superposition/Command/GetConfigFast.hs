module Io.Superposition.Command.GetConfigFast (
    GetConfigFastError(..),
    getConfigFast
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
import qualified Io.Superposition.Model.GetConfigFastInput
import qualified Io.Superposition.Model.GetConfigFastOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data GetConfigFastError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON GetConfigFastError
instance Data.Aeson.FromJSON GetConfigFastError

serGetConfigFastHEADER :: Io.Superposition.Model.GetConfigFastInput.GetConfigFastInput -> Network.HTTP.Types.Header.RequestHeaders
serGetConfigFastHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.GetConfigFastInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.GetConfigFastInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serGetConfigFastLABEL :: Io.Superposition.Model.GetConfigFastInput.GetConfigFastInput -> Data.ByteString.ByteString
serGetConfigFastLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "config",
        "fast"
        ]
    

getConfigFast :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.GetConfigFastInput.GetConfigFastInputBuilder () -> IO (Data.Either.Either GetConfigFastError Io.Superposition.Model.GetConfigFastOutput.GetConfigFastOutput)
getConfigFast client inputB = do
    let inputE = Io.Superposition.Model.GetConfigFastInput.build inputB
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
                Network.HTTP.Client.path = serGetConfigFastLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = (serGetConfigFastHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.GetConfigFastOutput.GetConfigFastOutput
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
        
    
    Io.Superposition.Model.GetConfigFastOutput.build $ do
        Io.Superposition.Model.GetConfigFastOutput.setAuditId audit_idHeaderE
        Io.Superposition.Model.GetConfigFastOutput.setVersion versionHeaderE
        Io.Superposition.Model.GetConfigFastOutput.setLastModified last_modifiedHeaderE
        Io.Superposition.Model.GetConfigFastOutput.setConfig configPayloadE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


