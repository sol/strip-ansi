module Data.String.ANSI.StripSpec (spec) where

import           Test.Hspec

import           System.Console.ANSI

import           Data.String.ANSI.Strip

withColor :: Color -> String -> String
withColor color string =  set <> string <> reset
  where
    set = setSGRCode [SetColor Foreground Dull color]
    reset = setSGRCode []

spec :: Spec
spec = do
  describe "stripAnsi" $ do
    it "removes ANSI color sequences" $ do
      stripAnsi ("some " <> withColor Green "colorized" <> " text") `shouldBe` "some colorized text"

    it "removes DEC private mode sequences" $ do
      stripAnsi (hideCursorCode <> "some text" <> showCursorCode) `shouldBe` "some text"
