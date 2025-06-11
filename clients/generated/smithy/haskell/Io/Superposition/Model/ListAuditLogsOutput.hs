module Io.Superposition.Model.ListAuditLogsOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListAuditLogsOutputBuilder,
    ListAuditLogsOutput,
    total_pages,
    total_items,
    data'
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.AuditLogFull

data ListAuditLogsOutput = ListAuditLogsOutput {
    total_pages :: Data.Maybe.Maybe Integer,
    total_items :: Data.Maybe.Maybe Integer,
    data' :: Data.Maybe.Maybe ([] Io.Superposition.Model.AuditLogFull.AuditLogFull)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListAuditLogsOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListAuditLogsOutput where
    parseJSON = Data.Aeson.withObject "ListAuditLogsOutput" $ \v -> ListAuditLogsOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListAuditLogsOutputBuilderState = ListAuditLogsOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Integer,
    total_itemsBuilderState :: Data.Maybe.Maybe Integer,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.AuditLogFull.AuditLogFull)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListAuditLogsOutputBuilderState
defaultBuilderState = ListAuditLogsOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListAuditLogsOutputBuilder a = ListAuditLogsOutputBuilder {
    runListAuditLogsOutputBuilder :: ListAuditLogsOutputBuilderState -> (ListAuditLogsOutputBuilderState, a)
}

instance Data.Functor.Functor ListAuditLogsOutputBuilder where
    fmap f (ListAuditLogsOutputBuilder g) =
        ListAuditLogsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListAuditLogsOutputBuilder where
    pure a = ListAuditLogsOutputBuilder (\s -> (s, a))
    (ListAuditLogsOutputBuilder f) <*> (ListAuditLogsOutputBuilder g) = ListAuditLogsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListAuditLogsOutputBuilder where
    (ListAuditLogsOutputBuilder f) >>= g = ListAuditLogsOutputBuilder (\s ->
        let (s', a) = f s
            (ListAuditLogsOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Maybe.Maybe Integer -> ListAuditLogsOutputBuilder ()
setTotalPages value =
   ListAuditLogsOutputBuilder (\s -> (s { total_pagesBuilderState = value }, ()))

setTotalItems :: Data.Maybe.Maybe Integer -> ListAuditLogsOutputBuilder ()
setTotalItems value =
   ListAuditLogsOutputBuilder (\s -> (s { total_itemsBuilderState = value }, ()))

setData' :: Data.Maybe.Maybe ([] Io.Superposition.Model.AuditLogFull.AuditLogFull) -> ListAuditLogsOutputBuilder ()
setData' value =
   ListAuditLogsOutputBuilder (\s -> (s { data'BuilderState = value }, ()))

build :: ListAuditLogsOutputBuilder () -> Data.Either.Either Data.Text.Text ListAuditLogsOutput
build builder = do
    let (st, _) = runListAuditLogsOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListAuditLogsOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })


