module Io.Superposition.Model.AuditLogFull (
    setTableName,
    setUserName,
    setTimestamp,
    setAction,
    setOriginalData,
    setNewData,
    setQuery,
    build,
    AuditLogFullBuilder,
    AuditLogFull,
    table_name,
    user_name,
    timestamp,
    action,
    original_data,
    new_data,
    query
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified GHC.Generics
import qualified GHC.Show

data AuditLogFull = AuditLogFull {
    table_name :: Data.Maybe.Maybe Data.Text.Text,
    user_name :: Data.Maybe.Maybe Data.Text.Text,
    timestamp :: Data.Maybe.Maybe Data.Time.UTCTime,
    action :: Data.Maybe.Maybe Data.Text.Text,
    original_data :: Data.Maybe.Maybe Data.Aeson.Value,
    new_data :: Data.Maybe.Maybe Data.Aeson.Value,
    query :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON AuditLogFull where
    toJSON a = Data.Aeson.object [
        "table_name" Data.Aeson..= table_name a,
        "user_name" Data.Aeson..= user_name a,
        "timestamp" Data.Aeson..= timestamp a,
        "action" Data.Aeson..= action a,
        "original_data" Data.Aeson..= original_data a,
        "new_data" Data.Aeson..= new_data a,
        "query" Data.Aeson..= query a
        ]
    


instance Data.Aeson.FromJSON AuditLogFull where
    parseJSON = Data.Aeson.withObject "AuditLogFull" $ \v -> AuditLogFull
        Data.Functor.<$> (v Data.Aeson..: "table_name")
        Control.Applicative.<*> (v Data.Aeson..: "user_name")
        Control.Applicative.<*> (v Data.Aeson..: "timestamp")
        Control.Applicative.<*> (v Data.Aeson..: "action")
        Control.Applicative.<*> (v Data.Aeson..: "original_data")
        Control.Applicative.<*> (v Data.Aeson..: "new_data")
        Control.Applicative.<*> (v Data.Aeson..: "query")
    



data AuditLogFullBuilderState = AuditLogFullBuilderState {
    table_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    user_nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    timestampBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    actionBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    original_dataBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    new_dataBuilderState :: Data.Maybe.Maybe Data.Aeson.Value,
    queryBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: AuditLogFullBuilderState
defaultBuilderState = AuditLogFullBuilderState {
    table_nameBuilderState = Data.Maybe.Nothing,
    user_nameBuilderState = Data.Maybe.Nothing,
    timestampBuilderState = Data.Maybe.Nothing,
    actionBuilderState = Data.Maybe.Nothing,
    original_dataBuilderState = Data.Maybe.Nothing,
    new_dataBuilderState = Data.Maybe.Nothing,
    queryBuilderState = Data.Maybe.Nothing
}

newtype AuditLogFullBuilder a = AuditLogFullBuilder {
    runAuditLogFullBuilder :: AuditLogFullBuilderState -> (AuditLogFullBuilderState, a)
}

instance Data.Functor.Functor AuditLogFullBuilder where
    fmap f (AuditLogFullBuilder g) =
        AuditLogFullBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative AuditLogFullBuilder where
    pure a = AuditLogFullBuilder (\s -> (s, a))
    (AuditLogFullBuilder f) <*> (AuditLogFullBuilder g) = AuditLogFullBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad AuditLogFullBuilder where
    (AuditLogFullBuilder f) >>= g = AuditLogFullBuilder (\s ->
        let (s', a) = f s
            (AuditLogFullBuilder h) = g a
        in h s')

setTableName :: Data.Maybe.Maybe Data.Text.Text -> AuditLogFullBuilder ()
setTableName value =
   AuditLogFullBuilder (\s -> (s { table_nameBuilderState = value }, ()))

setUserName :: Data.Maybe.Maybe Data.Text.Text -> AuditLogFullBuilder ()
setUserName value =
   AuditLogFullBuilder (\s -> (s { user_nameBuilderState = value }, ()))

setTimestamp :: Data.Maybe.Maybe Data.Time.UTCTime -> AuditLogFullBuilder ()
setTimestamp value =
   AuditLogFullBuilder (\s -> (s { timestampBuilderState = value }, ()))

setAction :: Data.Maybe.Maybe Data.Text.Text -> AuditLogFullBuilder ()
setAction value =
   AuditLogFullBuilder (\s -> (s { actionBuilderState = value }, ()))

setOriginalData :: Data.Maybe.Maybe Data.Aeson.Value -> AuditLogFullBuilder ()
setOriginalData value =
   AuditLogFullBuilder (\s -> (s { original_dataBuilderState = value }, ()))

setNewData :: Data.Maybe.Maybe Data.Aeson.Value -> AuditLogFullBuilder ()
setNewData value =
   AuditLogFullBuilder (\s -> (s { new_dataBuilderState = value }, ()))

setQuery :: Data.Maybe.Maybe Data.Text.Text -> AuditLogFullBuilder ()
setQuery value =
   AuditLogFullBuilder (\s -> (s { queryBuilderState = value }, ()))

build :: AuditLogFullBuilder () -> Data.Either.Either Data.Text.Text AuditLogFull
build builder = do
    let (st, _) = runAuditLogFullBuilder builder defaultBuilderState
    table_name' <- Data.Either.Right (table_nameBuilderState st)
    user_name' <- Data.Either.Right (user_nameBuilderState st)
    timestamp' <- Data.Either.Right (timestampBuilderState st)
    action' <- Data.Either.Right (actionBuilderState st)
    original_data' <- Data.Either.Right (original_dataBuilderState st)
    new_data' <- Data.Either.Right (new_dataBuilderState st)
    query' <- Data.Either.Right (queryBuilderState st)
    Data.Either.Right (AuditLogFull { 
        table_name = table_name',
        user_name = user_name',
        timestamp = timestamp',
        action = action',
        original_data = original_data',
        new_data = new_data',
        query = query'
    })


