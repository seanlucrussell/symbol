{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Transformations
  ( selectFirst
  , selectNext
  , selectPrev
  , nextLeaf
  , prevLeaf
  , Transformation
  , goUp
  , transformAtPoint
  ) where

import AST
import Utilities

import Data.List

type Transformation a = a -> Path -> Maybe (a, Path)

transformAtPoint :: Tree a => a -> Transformation a
transformAtPoint replacement tree path = fmap (,path) (replaceAtPoint replacement path tree)

searchForward :: Tree a => (a -> Bool) -> Transformation a
searchForward test tree path = fmap (tree,) (find (>path) (search test tree))

searchBackward :: Tree a => (a -> Bool) -> Transformation a
searchBackward test tree path = fmap (tree,) (find (<path) (reverse (search test tree)))

nextLeaf :: Tree a => Transformation a
nextLeaf = searchForward isLeaf

prevLeaf :: Tree a => Transformation a
prevLeaf = searchBackward isLeaf

selectFirst :: Tree a => Transformation a
selectFirst = attemptPathManipulation selectFirst''

selectNext :: Tree a => Transformation a
selectNext = attemptPathManipulation selectNext''

selectPrev :: Tree a => Transformation a
selectPrev = attemptPathManipulation selectPrev''

goUp :: Tree a => Transformation a
goUp = attemptPathManipulation goUp''

attemptPathManipulation :: Tree a => (Path -> Path) -> Transformation a
attemptPathManipulation m t p = fmap (t,) (toMaybe (validatePath t (m p)) (m p))

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
