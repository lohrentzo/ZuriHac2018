{-
module DayOne
    ( isPalindrome,
      ignoreSpaces,
      ignoreCaps
    ) where
-}

module DayOne where

import Data.Char (toLower)

-- reasoning about typeclasses (Eq)
isPalindrome :: String -> Bool     -- String -> Bool 
isPalindrome xs = xs == reverse xs -- Eq a => [a] -> Bool

-- with values we can inspect the type
-- with types we can inspect the kind of :info

{-
String:
 recursively defined
 are lists -> a lot of functions
 can be infinite
-}

-- product type
-- data User = User username password -- <- error?!

-- sum type
data Bool' = False' | True'

-- sum-product type
data Maybe' a = Nothing' | Just' a

-- trying to redefine List using different symbols/operators
--data List' a = Nil | a : List' a

-- :t words
-- :t concat

ignoreSpaces :: String -> String
--ignoreSpaces xs = concat $ words xs
ignoreSpaces xs = (concat.words) xs -- more efficient

ignoreCaps :: [Char] -> [Char]
ignoreCaps [] = []
ignoreCaps (x:xs) = (toLower x) : ignoreCaps xs

-- we can use map
pam :: (a -> b) -> [a] -> [b]
pam f [] = []
pam f (x:xs) = f x : pam f xs

ignoreCaps' :: [Char] -> [Char]
ignoreCaps' xs = pam toLower xs

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = f x && all' f xs

-- alternative syntax using case of
lla :: (a -> Bool) -> [a] -> Bool
lla f [] = True
lla f (x:xs) =
  case (f x) of
    False -> False
    True -> lla f xs

-- introducing folds
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = foldr' f (f x acc) xs

foldLla :: (a -> Bool) -> [a] -> Bool
foldLla f xs =
  foldr (\x y -> f x && y) True xs

ignoreCapsAndSpaces = ignoreCaps' . ignoreSpaces

{-
data PippoData a = PippoDataBase {nonno::a, nonna::String}
newtype Pippo a = PippoData a

instance Eq a => Eq (PippoData a) where
  x == y = nonno x == nonno y && nonna x == nonna y

instance Eq a
-}



 -- EOF Day One
