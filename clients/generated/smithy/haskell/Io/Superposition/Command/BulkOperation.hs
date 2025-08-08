module Io.Superposition.Command.BulkOperation (
    BulkOperationError(..),
    bulkOperation
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.BulkOperationInput
import qualified Io.Superposition.Model.BulkOperationOut
import qualified Io.Superposition.Model.BulkOperationOutput
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data BulkOperationError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON BulkOperationError
instance Data.Aeson.FromJSON BulkOperationError

serBulkOperationPAYLOAD:: Io.Superposition.Model.BulkOperationInput.BulkOperationInput -> Network.HTTP.Client.RequestBody
serBulkOperationPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $
    Data.Aeson.encode $
    Io.Superposition.Model.BulkOperationInput.bulk_operation input

serBulkOperationHEADER :: Io.Superposition.Model.BulkOperationInput.BulkOperationInput -> Network.HTTP.Types.Header.RequestHeaders
serBulkOperationHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.BulkOperationInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.BulkOperationInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        config_tagsHeader = (Io.Superposition.Model.BulkOperationInput.config_tags input
                    Data.Functor.<&> Io.Superposition.Utility.toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-config-tags", Data.Text.Encoding.encodeUtf8 x)]
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader,
            config_tagsHeader
            ]
        
    

serBulkOperationLABEL :: Io.Superposition.Model.BulkOperationInput.BulkOperationInput -> Data.ByteString.ByteString
serBulkOperationLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "context",
        "bulk-operations"
        ]
    

bulkOperation :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.BulkOperationInput.BulkOperationInputBuilder () -> IO (Data.Either.Either BulkOperationError Io.Superposition.Model.BulkOperationOutput.BulkOperationOutput)
bulkOperation client inputB = do
    let inputE = Io.Superposition.Model.BulkOperationInput.build inputB
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
                Network.HTTP.Client.path = serBulkOperationLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serBulkOperationPAYLOAD input
                , Network.HTTP.Client.requestHeaders = (serBulkOperationHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.BulkOperationOutput.BulkOperationOutput
deserializeResponse response = do
    
    bulk_operation_outputPayloadE :: Data.Maybe.Maybe Io.Superposition.Model.BulkOperationOut.BulkOperationOut <- do
        Data.Aeson.decode (Network.HTTP.Client.responseBody response)
                Data.Function.& Data.Either.Right
        
    
    Io.Superposition.Model.BulkOperationOutput.build $ do
        Io.Superposition.Model.BulkOperationOutput.setBulkOperationOutput bulk_operation_outputPayloadE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


