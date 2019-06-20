module DayTwo where

import DayOne
import Data.Char (isAlpha)

import Control.Monad
import Data.Monoid

isWord :: String -> Bool
isWord = lla isAlpha

checkInput :: String -> Bool
checkInput xs =
    case (isWord . ignoreCapsAndSpaces) xs of
        False -> False
        True -> isPalindrome xs

-- Introducing Maybe
-- data Maybe a = Nothing | Just a
-- data Either a b = Right a | Left b

isPalindrome' :: String -> Maybe String
isPalindrome' "" = Nothing
isPalindrome' xs =
    case xs == reverse xs of
        False -> Just "Not a palindrome"
        True -> Just "Palindrome"

isWord' :: String -> Maybe String
isWord' xs = case go xs of
  False -> Nothing
  True -> Just xs
  where
    go = lla isAlpha

checkInput' :: String -> Maybe String
checkInput' xs = case go xs of
  Nothing -> Nothing
  Just xs' -> isPalindrome' xs'
  where
    go = isWord' . ignoreCapsAndSpaces

---------
-- Monad and other typeclasses
-- word <- getLine
-- getLine :: IO String

-- Functor

maybeMap :: (a->b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

-- we could use Either but let's move on to typeclasses

-- data [] a = Nothing | a : [a]
-- Maybe a = Nothing | Just a

-- fmap :: Functor f => (a -> b) -> f a -> f b

-- Functor can work only one functions with one parameter
-- for example it does not work with Either (look at bi-functor)

-- data Either a b = Right a | Left b
data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
    -- fmap :: fmap :: Functor f => (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> (Either x) a -> (Either x) b
    fmap f (Right' a) = Right' (f a)
    fmap f (Left' x) = Left' x -- x means that the argument is part of an Either' via partial application

-- with Either we can't touch Left / the first type ctor

-- Functor for Sum and Product types
-- Sum type
data Choice a b = This a | That b deriving (Show)

instance Functor (Choice a) where
    fmap f (This x) = This x
    fmap f (That b) = That (f b)

-- Product type
data (Pair a) b  = Pair a b deriving (Show)

instance Functor (Pair a) where
    -- fmap :: fmap :: Functor f => (a -> b) -> f a -> f b
    -- fmap (b -> c) -> (Pair x) b -> Pair x c
    fmap f (Pair x b) = Pair x (f b)

{-
Now, we have a function that needs a String and we want to 


main = do 
    word <- getLine
    print (checkInput word) -- <- checkInput returns String
-}

data Sum' a b = First' a | Second' b

instance Functor (Sum' a) where
    fmap _ (First' a) = First' a
    -- fdfmap :: Functor f => (a -> b) -> f a -> f b
    fmap f (Second' b) = Second' (f b)

instance Applicative (Sum' a) where
    pure = Second'
    (<*>) _ (First' a) = First' a
    (<*>) (First' a) _ = First' a
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    (<*>) (Second' f) (Second' a) = Second' (f a)   

instance Monad (Sum' a) where
    return = pure
    (>>=) (First' a) _ = First' a
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    (>>=) (Second' a) f = f a

-- Compare bind and flip-bind
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (=<<) :: Monad m => (a -> m b) -> m a -> m (m b)

bind0 f g = (join . fmap f) g -- -> (f <<=)
bind f = join.fmap f

-- join :: Monad m => m (m a) -> m a
-- we can implment bind with (join . fmap)

-- Monoid -> binary, associative with an identity value
{-
identity -> mempty
aggregation -> mappend
-}

-- data Monoid a => Possibly a = LolNope | Yup a

{-
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
-}

{-
instance Monoid a => Monoid (Possibly a) where
    mempty = LolNope
    mappend a LolNope = a
    mappend LolNope b = b
    mappend (Yup x) (Yup y) = Yup (x `mappend` y) -- or a way to combine a and b
-}

data Multi a = One a | Two a a 
    deriving (Show, Eq)

instance Functor (Multi) where
    fmap f (One x) = One (f x)
    fmap f (Two x y) = Two (f x) (f y)

