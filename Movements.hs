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
  -- , nextLeaf
  -- , prevLeaf
  -- , previousHole''
  -- , nextHole''
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

selectFirst :: Tree a => a -> Path -> Path
selectFirst = try . selectFirst'

selectNext :: Tree a => a -> Path -> Path
selectNext = try . selectNext'

selectPrev :: Tree a => a -> Path -> Path
selectPrev = try . selectPrev'

selectLast :: Tree a => a -> Path -> Path
selectLast = try . selectLast'

-- prevLeaf :: Tree a => a -> Path -> Path
-- prevLeaf = try . prevLeaf'

-- nextLeaf :: Tree a => a -> Path -> Path
-- nextLeaf = try . nextLeaf'

goUp :: Tree a => a -> Path -> Path
goUp = try . goUp'

goToTop :: Tree a => a -> Path -> Path
goToTop = try . untilFailure . goUp'

selectLast' :: Tree a => a -> Path -> Maybe Path
selectLast' t p = selectFirst' t p >>= (untilFailure (selectNext' t))

-- nextLeaf' :: Tree a => (a, Path) -> Maybe ((a, Path))
-- nextLeaf' = nextHole'' (\(t,p) -> isJust (fmap isLeaf (treeUnderCursor p t)))

-- prevLeaf' :: Tree a => (a, Path) -> Maybe ((a, Path))
-- prevLeaf' = previousHole'' (\(t,p) -> isJust (fmap isLeaf (treeUnderCursor p t)))

-- nextHole'' :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- nextHole'' f z = (searchStart z >>= searchDownRight f) <|> searchUpRight f z
--    where searchStart = if f z then selectNext' else Just

-- searchDownRight :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchDownRight f z = (searchChildrenRight f z) <|> (selectNext' z >>= searchDownRight f)
-- searchChildrenRight :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchChildrenRight f z = if f z then Just z else selectFirst' z >>= searchDownRight f
-- searchUpRight :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchUpRight f z = (parent >>= selectNext' >>= searchDownRight f) <|> (parent >>= searchUpRight f)
--   where parent = goUp' z


-- previousHole'' :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- previousHole'' f z = (selectPrev' z >>= searchDownLeft f) <|> searchUpLeft f z

-- searchDownLeft :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchDownLeft f z = searchChildrenLeft f z <|> (selectPrev' z >>= searchDownLeft f)
-- searchChildrenLeft :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchChildrenLeft f z = if f z then Just z else selectLast' z >>= searchDownLeft f
-- searchUpLeft :: Tree a => ((a, Path) -> Bool) -> (a, Path) -> Maybe ((a, Path))
-- searchUpLeft f (t,p) = (parent >>= selectPrev' t >>= searchDownLeft f) <|> (parent >>= searchUpLeft f)
--    where parent = goUp' t p

selectFirst' :: Tree a => a -> Path -> Maybe Path
selectFirst' = attemptPathManipulation selectFirst''

selectNext' :: Tree a => a -> Path -> Maybe Path
selectNext' = attemptPathManipulation selectNext''

selectPrev' :: Tree a => a -> Path -> Maybe Path
selectPrev' = attemptPathManipulation selectPrev''

goUp' :: Tree a => a -> Path -> Maybe Path
goUp' = attemptPathManipulation goUp''

attemptPathManipulation :: Tree a => (Path -> Path) -> a -> Path -> Maybe Path
attemptPathManipulation m t p = toMaybe (validatePath t (m p)) (m p)

-- simple path manipulations. we then validate these

selectFirst'' :: Path -> Path
selectFirst'' p =  p ++ [0]

selectPrev'' :: Path -> Path
selectPrev'' p = init p ++ [last p - 1]

selectNext'' :: Path -> Path
selectNext'' p = init p ++ [last p + 1]

goUp'' :: Path -> Path
goUp'' = init
