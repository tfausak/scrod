{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module LegendaryChainsaw.Extra.Read where

import qualified Control.Monad.Catch as Exception
import qualified Data.Typeable as Typeable
import qualified GHC.Stack as Stack
import qualified LegendaryChainsaw.Extra.Either as Either
import qualified LegendaryChainsaw.Extra.Maybe as Maybe
import qualified LegendaryChainsaw.Spec as Spec
import qualified Text.Read as Read

readM :: forall a m . (Stack.HasCallStack, Exception.MonadThrow m, Read a, Typeable.Typeable a) => String -> m a
readM string =
  let proxy = Typeable.Proxy :: Typeable.Proxy a
  in Either.throw
    . Maybe.note (userError $ "invalid " <> show (Typeable.typeRep proxy) <> ": " <> show string)
    $ Read.readMaybe string

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = do
  Spec.named s 'readM $ do
    Spec.it s "succeeds with valid input" $ do
      Spec.assertEq s (readM "True") $ Just True

    Spec.it s "fails with invalid input" $ do
      Spec.assertEq s (readM "invalid") (Nothing :: Maybe Bool)
