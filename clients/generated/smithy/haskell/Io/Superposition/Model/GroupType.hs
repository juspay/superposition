module Io.Superposition.Model.GroupType (
    GroupType(..)
) where
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Utility

-- Enum implementation for GroupType
data GroupType =
    UserCreated
    | SystemGenerated
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON GroupType where
    toJSON UserCreated = Data.Aeson.String $ Data.Text.pack "UserCreated"
    toJSON SystemGenerated = Data.Aeson.String $ Data.Text.pack "SystemGenerated"

instance Io.Superposition.Utility.RequestSegment GroupType where
    toRequestSegment UserCreated = "UserCreated"
    toRequestSegment SystemGenerated = "SystemGenerated"

instance Data.Aeson.FromJSON GroupType where
    parseJSON = Data.Aeson.withText "GroupType" $ \v ->
        case v of
            "UserCreated" -> pure UserCreated
            "SystemGenerated" -> pure SystemGenerated
            _ -> fail $ "Unknown value for GroupType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment GroupType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "UserCreated" -> Data.Either.Right UserCreated
        Data.Either.Right "SystemGenerated" -> Data.Either.Right SystemGenerated
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    


