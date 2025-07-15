{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Data.String.ANSI.Strip where

-- | Remove terminal sequences.
stripAnsi :: String -> String
stripAnsi = go
  where
    go :: String -> String
    go = \ case
      '\ESC' : '[' :       (dropNumericParameters -> c : xs) | isCommand c -> go xs
      '\ESC' : '[' : '?' : (dropNumericParameters -> c : xs) | isCommand c -> go xs
      x : xs -> x : go xs
      xs@[] -> xs

    dropNumericParameters :: String -> String
    dropNumericParameters = dropWhile (`elem` "0123456789;")

    isCommand :: Char -> Bool
    isCommand = (`elem` commands)

    commands :: String
    commands = ['A'..'Z'] ++ ['a'..'z']
