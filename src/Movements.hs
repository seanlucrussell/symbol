{-# LANGUAGE TypeSynonymInstances #-}
module Movements
  ( selectFirst
  , selectNext
  , selectPrev
  , selectLast
  , nextLeaf
  , prevLeaf
  , Movement
  , goUp) where

import AST
import Utilities

import Data.Maybe

type Movement a = a -> Path -> Maybe Path

searchForward :: Tree a => (a -> Bool) -> Movement a
searchForward test tree path = listToMaybe (filter (>path) (search test tree))

searchBackward :: Tree a => (a -> Bool) -> Movement a
searchBackward test tree path = listToMaybe (reverse (filter (<path) (search test tree)))

nextLeaf :: Tree a => Movement a
nextLeaf = searchForward isLeaf

prevLeaf :: Tree a => Movement a
prevLeaf = searchBackward isLeaf

selectLast :: Tree a => Movement a
selectLast t p = selectFirst t p >>= untilFailure (selectNext t)

selectFirst :: Tree a => Movement a
selectFirst = attemptPathManipulation selectFirst''

selectNext :: Tree a => Movement a
selectNext = attemptPathManipulation selectNext''

selectPrev :: Tree a => Movement a
selectPrev = attemptPathManipulation selectPrev''

goUp :: Tree a => Movement a
goUp = attemptPathManipulation goUp''

attemptPathManipulation :: Tree a => (Path -> Path) -> Movement a
attemptPathManipulation m t p = toMaybe (validatePath t (m p)) (m p)

-- simple path manipulations. we then validate these

selectFirst'' :: Path -> Path
selectFirst'' p =  p ++ [0]

selectPrev'' :: Path -> Path
selectPrev'' [] = []
selectPrev'' p = init p ++ [last p - 1]

selectNext'' :: Path -> Path
selectNext'' [] = []
selectNext'' p = init p ++ [last p + 1]

goUp'' :: Path -> Path
goUp'' [] = []
goUp'' p = init p
