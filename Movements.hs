{-# LANGUAGE OverloadedStrings #-}
module Movements
  ( selectFirst
  , selectNext
  , selectPrev
  , selectLast
  , selectFirst'
  , selectNext'
  , selectPrev'
  , selectLast'
  , nextLeaf
  , prevLeaf
  , previousHole''
  , nextHole''
  , goToTop
  , goUp) where

import AST
import Utilities

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- movements: the underlying ast doesn't change, just the position in it

selectFirst :: Tree a => Zipper a -> Zipper a
selectFirst = try selectFirst'

selectNext :: Tree a => Zipper a -> Zipper a
selectNext = try selectNext'

selectPrev :: Tree a => Zipper a -> Zipper a
selectPrev = try selectPrev'

selectLast :: Tree a => Zipper a -> Zipper a
selectLast = try selectLast'

prevLeaf :: Tree a => Zipper a -> Zipper a
prevLeaf = try prevLeaf'

nextLeaf :: Tree a => Zipper a -> Zipper a
nextLeaf = try nextLeaf'

goUp :: Tree a => Zipper a -> Zipper a
goUp = try goUp'

goToTop :: Tree a => Zipper a -> Zipper a
goToTop = try (untilFailure goUp')

selectLast' :: Tree a => Zipper a -> Maybe (Zipper a)
selectLast' z = selectFirst' z >>= (untilFailure selectNext')

nextLeaf' :: Tree a => Zipper a -> Maybe (Zipper a)
nextLeaf' = nextHole'' (\(t,p) -> isJust (fmap isLeaf (treeUnderCursor p t)))

prevLeaf' :: Tree a => Zipper a -> Maybe (Zipper a)
prevLeaf' = previousHole'' (\(t,p) -> isJust (fmap isLeaf (treeUnderCursor p t)))

nextHole'' :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
nextHole'' f z = (searchStart z >>= searchDownRight f) <|> searchUpRight f z
   where searchStart = if f z then selectNext' else Just

searchDownRight :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchDownRight f z = (searchChildrenRight f z) <|> (selectNext' z >>= searchDownRight f)
searchChildrenRight :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchChildrenRight f z = if f z then Just z else selectFirst' z >>= searchDownRight f
searchUpRight :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchUpRight f z = (parent >>= selectNext' >>= searchDownRight f) <|> (parent >>= searchUpRight f)
  where parent = goUp' z


previousHole'' :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
previousHole'' f z = (selectPrev' z >>= searchDownLeft f) <|> searchUpLeft f z

searchDownLeft :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchDownLeft f z = searchChildrenLeft f z <|> (selectPrev' z >>= searchDownLeft f)
searchChildrenLeft :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchChildrenLeft f z = if f z then Just z else selectLast' z >>= searchDownLeft f
searchUpLeft :: Tree a => (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchUpLeft f z = (parent >>= selectPrev' >>= searchDownLeft f) <|> (parent >>= searchUpLeft f)
   where parent = goUp' z

selectFirst' :: Tree a => Zipper a -> Maybe (Zipper a)
selectFirst' = attemptPathManipulation selectFirst''

selectNext' :: Tree a => Zipper a -> Maybe (Zipper a)
selectNext' = attemptPathManipulation selectNext''

selectPrev' :: Tree a => Zipper a -> Maybe (Zipper a)
selectPrev' = attemptPathManipulation selectPrev''

goUp' :: Tree a => Zipper a -> Maybe (Zipper a)
goUp' = attemptPathManipulation goUp''

attemptPathManipulation :: Tree a => (Path -> Path) -> Zipper a -> Maybe (Zipper a)
attemptPathManipulation m (t, p) = toMaybe (validatePath t p') (t, p')
        where p' = m p

-- simple path manipulations. we then validate these

selectFirst'' :: Path -> Path
selectFirst'' p =  p ++ [0]

selectPrev'' :: Path -> Path
selectPrev'' p = init p ++ [last p - 1]

selectNext'' :: Path -> Path
selectNext'' p = init p ++ [last p + 1]

goUp'' :: Path -> Path
goUp'' = init
