{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Text.GLTF.Loader

import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "fromByteString" $ do
    it "basic check" $ fromByteString "Sean" `shouldBe` Left ImpossibleError

