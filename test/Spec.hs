{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import Skill
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Skill.getSkill" $ do
    it "correctly identifies skill level for valid Elo values" $ do
      getSkill 1000 `shouldBe` Just 0
      getSkill 1400 `shouldBe` Just 1
      getSkill 1111 `shouldBe` Just 0
      getSkill 1788 `shouldBe` Just 1
      getSkill 1800 `shouldBe` Just 2
      getSkill 2170 `shouldBe` Just 2
      getSkill 2200 `shouldBe` Just 3
      getSkill 2800 `shouldBe` Just 3
    it "gives Nothing for irrelevant Elo" $ do
      getSkill 900 `shouldBe` Nothing
      getSkill 1099 `shouldBe` Nothing
      getSkill 3600 `shouldBe` Nothing
      getSkill 4000 `shouldBe` Nothing
