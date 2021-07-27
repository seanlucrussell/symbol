{-# LANGUAGE OverloadedStrings #-}
module Movements
  ( (.-)
  , previousHole
  , nextHole
  , selectFirst
  , selectNext
  , selectPrev
  , selectLast
  , previousHole'
  , nextHole'
  , selectFirst'
  , selectNext'
  , selectPrev'
  , selectLast'
  , goToTop
  , goUp) where

import SymbolData
import Utilities

import qualified Data.Text as T

import Data.List
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

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

goUp :: Zipper -> Zipper
goUp = try goUp'

goToTop :: Zipper -> Zipper
goToTop = try (untilFailure goUp')

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
  where parent = goUp' z

previousHole' :: Zipper -> Maybe Zipper
previousHole' w = (selectPrev' w >>= searchDownLeft) <|> searchUpLeft w

searchDownLeft :: Zipper -> Maybe Zipper
searchDownLeft z = searchChildrenLeft z <|> (selectPrev' z >>= searchDownLeft)
searchChildrenLeft :: Zipper -> Maybe Zipper
searchChildrenLeft z = if zipperOnHole z then Just z else selectLast' z >>= searchDownLeft
searchUpLeft :: Zipper -> Maybe Zipper
searchUpLeft z = (parent >>= selectPrev' >>= searchDownLeft) <|> (parent >>= searchUpLeft)
   where parent = goUp' z

-- check if a given path is valid
validatePath :: Term -> Path -> Bool
validatePath _ [] = False
validatePath t ps = validatePath' t ps
  where validatePath' (Term _ []) (p:ps) = False
        validatePath' (Term _ ts) (p:ps) = validatePath'' ts p
                where validatePath'' (x:xs) 0 = validatePath' x ps
                      validatePath'' (x:xs) n = if n > 0 then validatePath'' xs (n-1) else False
                      validatePath'' [] n = False
        validatePath' (Term _ _) [] = True

selectFirst' :: Zipper -> Maybe Zipper
selectFirst' = attemptPathManipulation selectFirst''

selectNext' :: Zipper -> Maybe Zipper
selectNext' = attemptPathManipulation selectNext''

selectPrev' :: Zipper -> Maybe Zipper
selectPrev' = attemptPathManipulation selectPrev''

goUp' :: Zipper -> Maybe Zipper
goUp' = attemptPathManipulation goUp''

attemptPathManipulation :: (Path -> Path) -> Zipper -> Maybe Zipper
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
