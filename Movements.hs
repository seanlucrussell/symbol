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
  , searchUpRight
  , searchUpLeft
  , previousHole''
  , nextHole''
  , goToTop
  , goUp) where

import AST
import Utilities

import qualified Data.Text as T

import Data.List
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- movements: the underlying ast doesn't change, just the position in it

selectFirst :: Zipper a -> Zipper a
selectFirst = try selectFirst'

selectNext :: Zipper a -> Zipper a
selectNext = try selectNext'

selectPrev :: Zipper a -> Zipper a
selectPrev = try selectPrev'

selectLast :: Zipper a -> Zipper a
selectLast = try selectLast'

prevLeaf :: Zipper a -> Zipper a
prevLeaf = try prevLeaf'

nextLeaf :: Zipper a -> Zipper a
nextLeaf = try nextLeaf'

goUp :: Zipper a -> Zipper a
goUp = try goUp'

goToTop :: Zipper a -> Zipper a
goToTop = try (untilFailure goUp')

selectLast' :: Zipper a -> Maybe (Zipper a)
selectLast' z = selectFirst' z >>= (untilFailure selectNext')

-- searching for term can be done in a language agnostic way
-- searchNext :: (Tree a -> Bool) -> Tree a -> Path -> Maybe Path
-- searchPrev :: (Tree a -> Bool) -> Tree a -> Path -> Maybe Path
-- then we just make a simple specialization
-- nextHole :: Tree Token -> Path -> Maybe Path
-- nextLeaf :: Zipper a -> Maybe Path
-- nextLeaf z = searchStart z >>= searchDown <|> searchUp z

nextLeaf' :: Zipper a -> Maybe (Zipper a)
nextLeaf' = nextHole'' (\x -> case termUnderCursor x of
                                Tree _ [] -> True
                                _ -> False)

prevLeaf' :: Zipper a -> Maybe (Zipper a)
prevLeaf' = previousHole'' (\x -> case termUnderCursor x of
                                Tree _ [] -> True
                                _ -> False)

nextHole'' :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
nextHole'' f z = (searchStart z >>= searchDownRight f) <|> searchUpRight f z
   where searchStart = if f z then selectNext' else Just

searchDownRight :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchDownRight f z = (searchChildrenRight f z) <|> (selectNext' z >>= searchDownRight f)
searchChildrenRight :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchChildrenRight f z = if f z then Just z else selectFirst' z >>= searchDownRight f
searchUpRight :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchUpRight f z = (parent >>= selectNext' >>= searchDownRight f) <|> (parent >>= searchUpRight f)
  where parent = goUp' z


previousHole'' :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
previousHole'' f z = (selectPrev' z >>= searchDownLeft f) <|> searchUpLeft f z

searchDownLeft :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchDownLeft f z = searchChildrenLeft f z <|> (selectPrev' z >>= searchDownLeft f)
searchChildrenLeft :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchChildrenLeft f z = if f z then Just z else selectLast' z >>= searchDownLeft f
searchUpLeft :: (Zipper a -> Bool) -> Zipper a -> Maybe (Zipper a)
searchUpLeft f z = (parent >>= selectPrev' >>= searchDownLeft f) <|> (parent >>= searchUpLeft f)
   where parent = goUp' z

-- check if a given path is valid
validatePath :: Tree a -> Path -> Bool
validatePath _ [] = False
validatePath t ps = validatePath' t ps
  where validatePath' (Tree _ []) (p:ps) = False
        validatePath' (Tree _ ts) (p:ps) = validatePath'' ts p
                where validatePath'' (x:xs) 0 = validatePath' x ps
                      validatePath'' (x:xs) n = if n > 0 then validatePath'' xs (n-1) else False
                      validatePath'' [] n = False
        validatePath' (Tree _ _) [] = True

selectFirst' :: Zipper a -> Maybe (Zipper a)
selectFirst' = attemptPathManipulation selectFirst''

selectNext' :: Zipper a -> Maybe (Zipper a)
selectNext' = attemptPathManipulation selectNext''

selectPrev' :: Zipper a -> Maybe (Zipper a)
selectPrev' = attemptPathManipulation selectPrev''

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' = attemptPathManipulation goUp''

attemptPathManipulation :: (Path -> Path) -> Zipper a -> Maybe (Zipper a)
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
