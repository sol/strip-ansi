{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Data.String.ANSI.Strip (stripAnsi) where

import Data.Char (isDigit)

-- | Remove terminal sequences.
stripAnsi :: String -> String
stripAnsi = go
  where
    go :: String -> String
    go = \ case
      '\ESC' : '[' :       (dropNumericParameters -> c : xs) | isCommand c -> go xs
      '\ESC' : '[' : '?' : (dropNumericParameters -> c : xs) | isCommand c -> go xs
      '\ESC' : ']' : c : xs | isDigit c -> go (seekStringTerminator xs)
      x : xs -> x : go xs
      xs@[] -> xs

seekStringTerminator :: String -> String
seekStringTerminator = \ case
  '\BEL' : xs -> xs
  '\ESC' : '\\' : xs -> xs
  _ : xs -> seekStringTerminator xs
  xs@[] -> xs

dropNumericParameters :: String -> String
dropNumericParameters = dropWhile (`elem` "0123456789;")

isCommand :: Char -> Bool
isCommand = (`elem` commands)
  where
    commands :: String
    commands = ['A'..'Z'] ++ ['a'..'z']
