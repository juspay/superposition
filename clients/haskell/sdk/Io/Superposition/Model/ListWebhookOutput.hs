module Io.Superposition.Model.ListWebhookOutput (
    setTotalPages,
    setTotalItems,
    setData',
    build,
    ListWebhookOutputBuilder,
    ListWebhookOutput,
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
import qualified Data.Int
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Io.Superposition.Model.WebhookResponse

data ListWebhookOutput = ListWebhookOutput {
    total_pages :: Data.Int.Int64,
    total_items :: Data.Int.Int64,
    data' :: [] Io.Superposition.Model.WebhookResponse.WebhookResponse
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON ListWebhookOutput where
    toJSON a = Data.Aeson.object [
        "total_pages" Data.Aeson..= total_pages a,
        "total_items" Data.Aeson..= total_items a,
        "data" Data.Aeson..= data' a
        ]
    


instance Data.Aeson.FromJSON ListWebhookOutput where
    parseJSON = Data.Aeson.withObject "ListWebhookOutput" $ \v -> ListWebhookOutput
        Data.Functor.<$> (v Data.Aeson..: "total_pages")
        Control.Applicative.<*> (v Data.Aeson..: "total_items")
        Control.Applicative.<*> (v Data.Aeson..: "data")
    



data ListWebhookOutputBuilderState = ListWebhookOutputBuilderState {
    total_pagesBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    total_itemsBuilderState :: Data.Maybe.Maybe Data.Int.Int64,
    data'BuilderState :: Data.Maybe.Maybe ([] Io.Superposition.Model.WebhookResponse.WebhookResponse)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ListWebhookOutputBuilderState
defaultBuilderState = ListWebhookOutputBuilderState {
    total_pagesBuilderState = Data.Maybe.Nothing,
    total_itemsBuilderState = Data.Maybe.Nothing,
    data'BuilderState = Data.Maybe.Nothing
}

newtype ListWebhookOutputBuilder a = ListWebhookOutputBuilder {
    runListWebhookOutputBuilder :: ListWebhookOutputBuilderState -> (ListWebhookOutputBuilderState, a)
}

instance Data.Functor.Functor ListWebhookOutputBuilder where
    fmap f (ListWebhookOutputBuilder g) =
        ListWebhookOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ListWebhookOutputBuilder where
    pure a = ListWebhookOutputBuilder (\s -> (s, a))
    (ListWebhookOutputBuilder f) <*> (ListWebhookOutputBuilder g) = ListWebhookOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ListWebhookOutputBuilder where
    (ListWebhookOutputBuilder f) >>= g = ListWebhookOutputBuilder (\s ->
        let (s', a) = f s
            (ListWebhookOutputBuilder h) = g a
        in h s')

setTotalPages :: Data.Int.Int64 -> ListWebhookOutputBuilder ()
setTotalPages value =
   ListWebhookOutputBuilder (\s -> (s { total_pagesBuilderState = Data.Maybe.Just value }, ()))

setTotalItems :: Data.Int.Int64 -> ListWebhookOutputBuilder ()
setTotalItems value =
   ListWebhookOutputBuilder (\s -> (s { total_itemsBuilderState = Data.Maybe.Just value }, ()))

setData' :: [] Io.Superposition.Model.WebhookResponse.WebhookResponse -> ListWebhookOutputBuilder ()
setData' value =
   ListWebhookOutputBuilder (\s -> (s { data'BuilderState = Data.Maybe.Just value }, ()))

build :: ListWebhookOutputBuilder () -> Data.Either.Either Data.Text.Text ListWebhookOutput
build builder = do
    let (st, _) = runListWebhookOutputBuilder builder defaultBuilderState
    total_pages' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookOutput.ListWebhookOutput.total_pages is a required property.") Data.Either.Right (total_pagesBuilderState st)
    total_items' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookOutput.ListWebhookOutput.total_items is a required property.") Data.Either.Right (total_itemsBuilderState st)
    data'' <- Data.Maybe.maybe (Data.Either.Left "Io.Superposition.Model.ListWebhookOutput.ListWebhookOutput.data' is a required property.") Data.Either.Right (data'BuilderState st)
    Data.Either.Right (ListWebhookOutput { 
        total_pages = total_pages',
        total_items = total_items',
        data' = data''
    })


