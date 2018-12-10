import Test.Hspec
import Test.QuickCheck

import Fixpoint (fixpoint, fixpoint')

main :: IO ()
main = hspec $ do
    desc_constant fixpoint
    desc_factorial fixpoint
    desc_fibonacci fixpoint
    desc_constant fixpoint'
    desc_factorial fixpoint'
    desc_fibonacci fixpoint'

desc_constant fixp = describe "fixpoint of a constant function" $ do
    it "the fixpoint of a constant function is that constant" $ do
        fixp (\x -> 3) `shouldBe` 3
        fixp (const 2) `shouldBe` 2
    it "the fixpoint of any constant is that constant" $ do
        property $ \n -> fixp (\x -> n) == (n :: Int)

desc_factorial fixp = describe "fixpoint in recursion" $ do
    let fac n = product [1..n]
    it "the fixpoint of \\f n = n * f (n - 1) is the factorial" $ do
        let f' f 0 = 1
            f' f n = n * f (n - 1)
            fac' = fixp f'
        property $ \n -> (n :: Int) > 0 ==> fac' n == fac n

desc_fibonacci fixp = describe "fixpoint fibonacci" $ do
    let fibs = scanl (+) 0 (1:fibs)
    it "the fixpoint of a fibonacci sequence" $ do
        let fibs' = fixp (scanl (+) 0 . (1:))
        property $ \n -> (n :: Int) > 0 ==> (fibs !! n) == (fibs' !! n)
