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
    USER_CREATED
    | SYSTEM_GENERATED
    deriving (
        GHC.Generics.Generic,
        Data.Eq.Eq,
        GHC.Show.Show
    )

instance Data.Aeson.ToJSON GroupType where
    toJSON USER_CREATED = Data.Aeson.String $ Data.Text.pack "USER_CREATED"
    toJSON SYSTEM_GENERATED = Data.Aeson.String $ Data.Text.pack "SYSTEM_GENERATED"

instance Io.Superposition.Utility.RequestSegment GroupType where
    toRequestSegment USER_CREATED = "USER_CREATED"
    toRequestSegment SYSTEM_GENERATED = "SYSTEM_GENERATED"

instance Data.Aeson.FromJSON GroupType where
    parseJSON = Data.Aeson.withText "GroupType" $ \v ->
        case v of
            "USER_CREATED" -> pure USER_CREATED
            "SYSTEM_GENERATED" -> pure SYSTEM_GENERATED
            _ -> fail $ "Unknown value for GroupType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.ResponseSegment GroupType where
    fromResponseSegment b = case (Data.Text.Encoding.decodeUtf8' b) of
        Data.Either.Right "USER_CREATED" -> Data.Either.Right USER_CREATED
        Data.Either.Right "SYSTEM_GENERATED" -> Data.Either.Right SYSTEM_GENERATED
        Data.Either.Right s -> Data.Either.Left $ "Not a valid enum constructor: " <> s
        Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
    


