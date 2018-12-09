import Test.Hspec
import Test.QuickCheck

import Fixpoint

main :: IO ()
main = hspec $ do
    describe "fixpoint of a constant" $ do
        it "the fixpoint of a constant function is that constant" $ do
            fixpoint (\x -> 3) `shouldBe` 3
        it "the fixpoint of any constant is that constant" $ do
            property $ \n -> fixpoint (\x -> n) == (n :: Int)

    describe "fixpoint in recursion" $ do
        it "the fixpoint of fac' f n = n * f (n -1 ) is the factorial" $ do
            let fac' f 0 = 1
                fac' f n = n * f (n - 1)
                factorial n = product [1..n]
            property $ \n -> (n :: Int) > 0 ==> fixpoint fac' n == factorial n
