module Io.Superposition.Command.DeleteFunction (
    DeleteFunctionError(..),
    deleteFunction
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
import qualified Io.Superposition.Model.DeleteFunctionInput
import qualified Io.Superposition.Model.DeleteFunctionOutput
import qualified Io.Superposition.Model.FunctionNotFound
import qualified Io.Superposition.Model.InternalServerError
import qualified Io.Superposition.SuperpositionClient
import qualified Io.Superposition.Utility
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data DeleteFunctionError =
    InternalServerError Io.Superposition.Model.InternalServerError.InternalServerError
    | FunctionNotFound Io.Superposition.Model.FunctionNotFound.FunctionNotFound
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON DeleteFunctionError
instance Data.Aeson.FromJSON DeleteFunctionError

serDeleteFunctionHEADER :: Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInput -> Network.HTTP.Types.Header.RequestHeaders
serDeleteFunctionHEADER input =
    let 
        workspace_idHeader = (Io.Superposition.Model.DeleteFunctionInput.workspace_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-tenant", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        org_idHeader = (Io.Superposition.Model.DeleteFunctionInput.org_id input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
                    Data.Function.& \x -> [("x-org-id", Data.Text.Encoding.encodeUtf8 x)]
                    Data.Function.& Data.Maybe.Just
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            workspace_idHeader,
            org_idHeader
            ]
        
    

serDeleteFunctionLABEL :: Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInput -> Data.ByteString.ByteString
serDeleteFunctionLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "function",
        (Io.Superposition.Model.DeleteFunctionInput.function_name input
                    Data.Function.& Io.Superposition.Utility.toRequestSegment)
        
        ]
    

deleteFunction :: Io.Superposition.SuperpositionClient.SuperpositionClient -> Io.Superposition.Model.DeleteFunctionInput.DeleteFunctionInputBuilder () -> IO (Data.Either.Either DeleteFunctionError Io.Superposition.Model.DeleteFunctionOutput.DeleteFunctionOutput)
deleteFunction client inputB = do
    let inputE = Io.Superposition.Model.DeleteFunctionInput.build inputB
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
        method = Network.HTTP.Types.Method.methodDelete
        token = Data.Text.Encoding.encodeUtf8 $ Io.Superposition.SuperpositionClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serDeleteFunctionLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = (serDeleteFunctionHEADER input) ++ [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Io.Superposition.Model.DeleteFunctionOutput.DeleteFunctionOutput
deserializeResponse response = do
    
    
    Io.Superposition.Model.DeleteFunctionOutput.build $ do
        pure ()
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


