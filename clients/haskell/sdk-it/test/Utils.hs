module Utils where

expectRight :: Either a b -> b
expectRight (Right b) = b
expectRight _ = undefined

expectJust :: Maybe a -> a
expectJust (Just a) = a
expectJust _ = undefined
