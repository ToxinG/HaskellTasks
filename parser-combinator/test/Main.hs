module Main where

import Test.Tasty.Hspec
import Test.Tasty

import Parser

main :: IO ()
main = hspecTestTree >>= \unitTests ->
        let allTests = testGroup "" [unitTests]
        in defaultMain allTests

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "Homework" spec

spec :: Spec
spec = describe "Parser" $ do
           it "ok (empty string)" $
               runParser ok "" `shouldBe` Just ((),"")
           it "ok (string)" $
               runParser ok "sampletext" `shouldBe` Just ((),"sampletext")
           it "ok (list)" $
               runParser ok [1 :: Int, 2] `shouldBe` Just ((),[1,2])
           it "eof (empty string)" $
               runParser eof "" `shouldBe` Just ((),"")
           it "eof (string)" $
               runParser eof "sampletext" `shouldBe` Nothing
           it "satisfy (int)" $
               runParser (satisfy (== 0)) [0 :: Int, 1] `shouldBe` Just (0,[1])
           it "satisfy (another int)" $
               runParser (satisfy (== 0)) [10 :: Int, 1] `shouldBe` Nothing
           it "element (list)" $
               runParser (element 0) [0 :: Int, 0, 1] `shouldBe` Just (0,[0,1])
           it "element (wrong string)" $
               runParser (element 'a') "baaa" `shouldBe` Nothing
           it "balancedBrackets (correct)" $
               runParser balancedBrackets "(()())" `shouldBe` Just ("ok","")
           it "balancedBrackets (incorrect 1)" $
               runParser balancedBrackets "()())" `shouldBe` Nothing
           it "balancedBrackets (incorrect 2)" $
               runParser balancedBrackets "(())())" `shouldBe` Nothing
           it "signedInt (no sign)" $
               runParser signedInt "13 " `shouldBe` Just (13," ")
           it "signedInt (plus)" $
               runParser signedInt "+13 " `shouldBe` Just (13," ")
           it "signedInt (minus)" $
               runParser signedInt "-13 " `shouldBe` Just (-13," ")
           it "listListParser (list list)" $
               runParser listListParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10],[5,-7,2]],"")