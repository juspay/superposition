module Io.Superposition.Command.ApplicableVariants (
    ApplicableVariantsError(..),
    applicableVariants
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
import qualified Io.Superposition.Model.ApplicableVariantsInput
import qualified Io.Superposition.Model.ApplicableVariantsOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.Model.Variant
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data ApplicableVariantsError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serApplicableVariantsPAYLOAD:: Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput -> Network.HTTP.Client.RequestBody
serApplicableVariantsPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "identifier" Data.Aeson..= Io.Superposition.Model.ApplicableVariantsInput.identifier input,
        "context" Data.Aeson..= Io.Superposition.Model.ApplicableVariantsInput.context input
        ]
    

serApplicableVariantsHEADER :: Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput -> Network.HTTP.Types.Header.RequestHeaders
serApplicableVariantsHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.ApplicableVariantsInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.ApplicableVariantsInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serApplicableVariantsLABEL :: Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInput -> Data.ByteString.ByteString
serApplicableVariantsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "experiments",
        "applicable-variants"
        ]
    

applicableVariants :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.ApplicableVariantsInput.ApplicableVariantsInputBuilder () -> IO (Data.Either.Either ApplicableVariantsError Io.Superposition.Model.ApplicableVariantsOutput.ApplicableVariantsOutput)
applicableVariants client inputB = do
    let inputE = Io.Superposition.Model.ApplicableVariantsInput.build inputB
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
                Network.HTTP.Client.path = serApplicableVariantsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serApplicableVariantsPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serApplicableVariantsHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.ApplicableVariantsOutput.ApplicableVariantsOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    data'DocumentE :: [] Io.Superposition.Model.Variant.Variant <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "data") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Io.Superposition.Model.ApplicableVariantsOutput.build $ do
        Io.Superposition.Model.ApplicableVariantsOutput.setData' data'DocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


