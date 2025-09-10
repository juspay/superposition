module Io.Superposition.Model.GroupType (
    GroupType(..)
) where
import qualified Data.Aeson
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

instance Data.Aeson.FromJSON GroupType where
    parseJSON = Data.Aeson.withText "GroupType" $ \v ->
        case v of
            "USER_CREATED" -> pure USER_CREATED
            "SYSTEM_GENERATED" -> pure SYSTEM_GENERATED
            _ -> fail $ "Unknown value for GroupType: " <> Data.Text.unpack v
        
    

instance Io.Superposition.Utility.SerDe GroupType where
    serializeElement USER_CREATED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "USER_CREATED"
    serializeElement SYSTEM_GENERATED = Data.Text.Encoding.encodeUtf8 $ Data.Text.pack "SYSTEM_GENERATED"
    deSerializeElement bs = case Data.Text.Encoding.decodeUtf8 bs of
        "USER_CREATED" -> Right USER_CREATED
        "SYSTEM_GENERATED" -> Right SYSTEM_GENERATED
        e -> Left ("Failed to de-serialize GroupType, encountered unknown variant: " ++ (show bs))
    


