{-# LANGUAGE OverloadedStrings #-}
module Movements
  ( (.-)
  , previousHole
  , nextHole
  , selectFirst
  , selectNext
  , selectPrev
  , selectLast
  , nextHole'
  , goup) where

import SymbolData

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- move to Maybe based thing, so that we can compose successful operations

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

-- movements: the underlying ast doesn't change, just the position in it

previousHole :: Zipper -> Zipper
previousHole = try previousHole'

nextHole :: Zipper -> Zipper
nextHole = try nextHole'

selectFirst :: Zipper -> Zipper
selectFirst = try selectFirst'

selectNext :: Zipper -> Zipper
selectNext = try selectNext'

selectPrev :: Zipper -> Zipper
selectPrev = try selectPrev'

selectLast :: Zipper -> Zipper
selectLast = try selectLast'

goup :: Zipper -> Zipper
goup = try goup'

selectLast' :: Zipper -> Maybe Zipper
selectLast' z = selectFirst' z >>= (untilFailure selectNext')

zipperOnHole :: Zipper -> Bool
zipperOnHole z = termUnderCursor z == Term UnknownTerm []

nextHole' :: Zipper -> Maybe Zipper
nextHole' w = (searchStart w >>= searchDownRight) <|> searchUpRight w
   where searchStart = if zipperOnHole w then selectNext' else Just

searchDownRight :: Zipper -> Maybe Zipper
searchDownRight z = (searchChildrenRight z) <|> (selectNext' z >>= searchDownRight)
searchChildrenRight :: Zipper -> Maybe Zipper
searchChildrenRight z = if zipperOnHole z then Just z else selectFirst' z >>= searchDownRight
searchUpRight :: Zipper -> Maybe Zipper
searchUpRight z = (parent >>= selectNext' >>= searchDownRight) <|> (parent >>= searchUpRight)
  where parent = goup' z

previousHole' :: Zipper -> Maybe Zipper
previousHole' w = (selectPrev' w >>= searchDownLeft) <|> searchUpLeft w

searchDownLeft :: Zipper -> Maybe Zipper
searchDownLeft z = searchChildrenLeft z <|> (selectPrev' z >>= searchDownLeft)
searchChildrenLeft :: Zipper -> Maybe Zipper
searchChildrenLeft z = if zipperOnHole z then Just z else selectLast' z >>= searchDownLeft
searchUpLeft :: Zipper -> Maybe Zipper
searchUpLeft z = (parent >>= selectPrev' >>= searchDownLeft) <|> (parent >>= searchUpLeft)
   where parent = goup' z

selectFirst' :: Zipper -> Maybe Zipper
selectFirst' z@(Zipper t p) = case termUnderCursor z of
        Term _ [] -> Nothing
        Term _ _ -> Just (Zipper t (p ++ [0]))

selectPrev' :: Zipper -> Maybe Zipper
selectPrev' (Zipper t@(Term _ ts) [x]) = if x == 0 then Nothing else Just (Zipper t [x - 1])
selectPrev' (Zipper t@(Term _ ts) (x:xs)) = do (Zipper _ xs') <- selectPrev' (Zipper (ts!!x) xs)
                                               return (Zipper t (x:xs'))
selectPrev' _ = Nothing

selectNext' :: Zipper -> Maybe Zipper
selectNext' (Zipper t@(Term _ ts) [x]) = if x == length ts - 1 then Nothing else Just (Zipper t [x + 1])
selectNext' (Zipper t@(Term _ ts) (x:xs)) = do (Zipper _ xs') <- selectNext' (Zipper (ts!!x) xs)
                                               return (Zipper t (x:xs'))
selectNext' _ = Nothing

goup' :: Zipper -> Maybe Zipper
goup' (Zipper t []) = Nothing
goup' (Zipper t [_]) = Nothing
goup' (Zipper t p) = Just (Zipper t (init p))
