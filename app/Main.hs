module Main where

import DayOne
import DayTwo

main :: IO () -- <- performs IO actions and does not return a value
{-
main = do
  word <- getLine
  print (isPalindrome word)
  print (isPalindrome (ignoreSpaces(word)))
  --print (isPalindrome ignoreCaps'.ignoreSpaces (word))
  print (isPalindrome (ignoreCapsAndSpaces (word)))
-}

main = do
  word <- getLine
-- String    IO String
  print (checkInput word)
  print (checkInput' word)


-- with de-sugaring

main2 =
  -- IO String -> (String -> Maybe String) -> (Show a => a -> IO ())
  getLine >>= (\x -> print $ checkInput' x)
  