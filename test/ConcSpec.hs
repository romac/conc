
module ConcSpec where

import Test.Hspec
import Test.QuickCheck

import Conc.Tree

spec :: Spec
spec = describe "test" $ do
  context "when doing a dummy test" $ do
    it "works" $ property $
      \x -> toList (append (singleton True) x) == [True, x]

  context "when doing a advanced test" $ do
    it "works" $ property $
      \xs -> toList (foldl append empty xs) == (xs :: [Int])

  context "when doing a very advanced test" $ do
    it "works" $ property $
      \xs -> toList (fromList xs) == (xs :: [Int])

