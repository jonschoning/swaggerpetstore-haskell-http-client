{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PropJSON where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Monoid ((<>))
import Data.Typeable (Proxy(..), typeOf, Typeable)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.Hspec.QuickCheck (prop)

import SwaggerPetstore.MimeTypes

import ApproxEq

-- * Type Aliases

type ArbitraryJSON a = ArbitraryRoundtrip FromJSON ToJSON a

type ArbitraryMime mime a = ArbitraryRoundtrip (MimeUnrender mime) (MimeRender mime) a

type ArbitraryRoundtrip from to a = (from a, to a, Arbitrary' a)

type Arbitrary' a = (Arbitrary a, Show a, Typeable a)

-- * Mime

propMime
  :: forall a b mime.
     (ArbitraryMime mime a, Testable b)
  => mime -> String -> (a -> a -> b) -> Proxy a -> Spec
propMime m eqDescr eq _ =
  prop
    (show (typeOf (undefined :: a)) <> " " <> show (typeOf (undefined :: mime)) <> " roundtrip " <> eqDescr) $
  \(x :: a) ->
     let rendered = mimeRender' m x
         actual = mimeUnrender' m rendered
         expected = Right x
         failMsg =
           "ACTUAL: " <> show actual <> "\nRENDERED: " <> BL8.unpack rendered
     in counterexample failMsg $
        either reject property (eq <$> actual <*> expected)
  where
    reject = property . const rejected

propJSONMimeEq :: (ArbitraryJSON a, Eq a) => Proxy a -> Spec
propJSONMimeEq = propMime MimeJSON "(EQ)" (==)

-- * JSON

propJSON
  :: forall a b.
     (ArbitraryJSON a, Testable b)
  => String -> (a -> a -> b) -> Proxy a -> Spec
propJSON eqDescr eq _ =
  prop
    (show (typeOf (undefined :: a)) <> " FromJSON/ToJSON roundtrip " <> eqDescr) $
  \(x :: a) ->
     let actual = parseEither parseJSON (toJSON x)
         expected = Right x
         failMsg =
           "ACTUAL: " <> show actual <> "\nJSON: " <> BL8.unpack (encode x)
     in counterexample failMsg $
        either reject property (eq <$> actual <*> expected)
  where
    reject = property . const rejected

propJSONEq
  :: (ArbitraryJSON a, Eq a)
  => Proxy a -> Spec
propJSONEq = propJSON "(Eq)" (==)

propJSONApproxEq
  :: (ArbitraryJSON a, ApproxEq a)
  => Proxy a -> Spec
propJSONApproxEq = propJSON "(ApproxEq)" (==~)
