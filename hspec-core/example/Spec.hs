{-# LANGUAGE QuasiQuotes #-}
module Main (main, spec) where

import Prelude
import Test.Hspec.Core.Spec
import Test.Hspec.Core.Runner
import Test.Hspec.Expectations
import Test.QuickCheck

import Data.Yaml.TH
import qualified Data.Yaml as Yaml
import Data.Aeson (encode, decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB
import           Data.Aeson.Encode.Pretty

import Text.Read
import Data.Maybe


main :: IO ()
main = hspec spec

prettyJson :: Bool -> String -> String -> Maybe (String, String)
prettyJson unicode actual expected = case (readMaybe actual, readMaybe expected) of
  (Just x, Just y) -> Just (enc x, enc y)
  _ -> case (readMaybe actual >>= dec , readMaybe expected >>= dec) of
    (Just x, Just y) -> Just (enc x, enc y)
  where
    dec :: String -> Maybe Value
    dec = decode . LB.fromStrict . T.encodeUtf8 . T.pack

    enc :: Value -> String
    -- enc = T.unpack . T.decodeUtf8 . Yaml.encode
    -- enc = T.unpack . T.decodeUtf8 . LB.toStrict . encodePretty
    enc = T.unpack . T.decodeUtf8 . LB.toStrict . encodePretty' defConfig { confIndent = Spaces 2 }

usePrettyJson :: SpecWith a
usePrettyJson = modifyConfig $ \ c -> c {
  configPrettyPrintFunction = \ unicode x y -> fromMaybe (configPrettyPrintFunction c unicode x y) (prettyJson unicode x y)
}

spec :: Spec
spec = do
  usePrettyJson
  describe "reverse" $ do
    it "reverses a list" $ do
      [yamlQQ|
      name: Joe
      age: 23
      |] `shouldBe` [yamlQQ|
      name: Joe
      age: 42
      |]

    it "reverses a list" $ do
      encode [yamlQQ|
      name: Joe
      age: 23
      |] `shouldBe` encode [yamlQQ|
      name: Joe
      age: 42
      |]

    it "gives the original list, if applied twice" $ property $
      \xs -> (reverse . reverse) xs == (xs :: [Int])
