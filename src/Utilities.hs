module Utilities
        ( (<!>)
        , applyAtIndex
        , changeAtIndex
        , swapAdjacent
        , removeAtIndex
        , firstNumberNotInList
        , insertAt
        , maybeToBool
        , safeListIndex
        , toMaybe
        , try
        , untilFailure
        ) where

-- module for all the generic stuff I been using

import qualified Data.Set
import Data.Maybe
import Control.Applicative
import Control.Monad

-- try to apply a function that might fail, fall back to current value if it
-- does
try :: (a -> Maybe a) -> a -> a
try = ap fromMaybe

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
applyAtIndex _ _ _ = undefined

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x:ys
insertAt n x (y:ys) = y:insertAt (n-1) x ys
insertAt _ _ _ = undefined

safeListIndex :: Int -> [a] -> Maybe a
safeListIndex 0 (x:_) = Just x
safeListIndex _ [] = Nothing
safeListIndex n (_:xs) = if n < 0 then Nothing else safeListIndex (n-1) xs


swapAdjacent :: Int -> [a] -> Maybe [a]
swapAdjacent 0 (x:y:xs) = Just (y:x:xs)
swapAdjacent n (x:xs) = do xs' <- swapAdjacent (n-1) xs
                           return (x:xs')
swapAdjacent _ _ = Nothing

removeAtIndex :: Int -> [a] -> Maybe [a]
removeAtIndex 0 (_:xs) = Just xs
removeAtIndex n (x:xs) = do xs' <- removeAtIndex (n-1) xs
                            return (x:xs')
removeAtIndex _ _ = Nothing

firstNumberNotInList :: [Int] -> Int
firstNumberNotInList l = f 0
     where s = Data.Set.fromList l
           f n = if Data.Set.member n s then f (n+1) else n

-- stolen from https://hackage.haskell.org/package/tomland-1.3.3.0/docs/src/Toml.Codec.Types.          html#%3C%21%3E
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
(<!>) = liftM2 (<|>)
{-# INLINE (<!>) #-}
