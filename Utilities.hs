module Utilities
        ( (.-)
        , (<!>)
        , try
        , untilFailure
        , toMaybe
        , maybeToBool
        , changeAtIndex
        , applyAtIndex
        , insertAt
        , safeListIndex
        ) where

-- module for all the generic stuff I been using

import Data.Maybe
import Control.Applicative

-- concatenate operations with this operator
(.-) :: a -> (a -> b) -> b
(.-) x f = f x

-- try to apply a function that might fail, fall back to current value if it
-- does
try :: (a -> Maybe a) -> a -> a
try f x = fromMaybe x (f x)

-- keep applying a function until the function fails
untilFailure :: (a -> Maybe a) -> a -> Maybe a
untilFailure f x = case f x of
        Just y  -> untilFailure f y
        Nothing -> Just x

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing  = False

changeAtIndex :: Int -> a -> [a] -> [a]
changeAtIndex n = applyAtIndex n . const

-- assumes we are in bounds of array
applyAtIndex :: Int -> (a -> a) -> [a] -> [a]
applyAtIndex 0 f (y:ys) = f y:ys
applyAtIndex n f (y:ys) = y:(applyAtIndex (n-1) f ys)

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x:ys
insertAt n x (y:ys) = y:insertAt (n-1) x ys

safeListIndex :: Int -> [a] -> Maybe a
safeListIndex 0 (x:xs) = Just x
safeListIndex n [] = Nothing
safeListIndex n (x:xs) = if n < 0 then Nothing else safeListIndex (n-1) xs

-- stolen from https://hackage.haskell.org/package/tomland-1.3.3.0/docs/src/Toml.Codec.Types.          html#%3C%21%3E
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a
{-# INLINE (<!>) #-}
