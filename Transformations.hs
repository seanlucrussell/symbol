{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , replaceWithTermAndSelectNext
  , replaceWithTerm) where

import SymbolData
import Movements
import TypeChecker
import Utilities

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

insertBefore :: Zipper -> Zipper
insertBefore (Term Program t, p:ps) = (Term Program (insertAt p blankAssignment t), p:ps)

insertAfter :: Zipper -> Zipper
insertAfter (Term Program t, p:ps) = (Term Program (insertAt (p+1) blankAssignment t), p:ps)

replaceWithTerm :: Term -> Zipper -> Zipper
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Term -> Zipper -> Zipper
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))


replaceWithTerm' :: Term -> Zipper -> Maybe Zipper
replaceWithTerm' t (x, p) = if validateZipper replaced then Just replaced else Nothing
        where replaced = (replaceWithTerm'' t p x, p)

replaceWithTerm'' :: Term -> [Int] -> Term -> Term
replaceWithTerm'' t [] _ = t
replaceWithTerm'' t (p:ps) (Term x ts) = Term x (applyAtIndex p (replaceWithTerm'' t ps) ts)

searchForNamedVariables :: Zipper -> [Term]
searchForNamedVariables z = filter (/= blankUnknown) (searchAbove (goUp z) ++ prev)
        where prev = case goToTop z of
                          (_, [0]) -> []
                          _ -> searchBefore (selectPrev (goToTop z))
              searchAbove z = case termUnderCursor z of
                                   Term FunctionTerm [a, _, _] -> a:searchAbove (goUp z)
                                   Term AssignmentTerm [_, _, _] -> []
                                   _ -> searchForNamedVariables (goUp z)
              searchBefore z@(_, [0]) = case termUnderCursor z of
                                                   Term AssignmentTerm [a, _, _] -> [a]
              searchBefore z@(_, _) = case termUnderCursor z of
                                                 Term AssignmentTerm [a, _, _] -> a:searchBefore (selectPrev z)

functionCalls :: Zipper -> [Term]
functionCalls _ = []
-- this is a dumb way of doing things!!! do it right!!! when you have more
-- time!!!
-- functionCalls z = concat [ [(ApplicationTerm x UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm) UnknownTerm)] | x <- searchForNamedVariables z]


standardTerms :: [Term]
standardTerms = [ blankTrue 
                , blankFalse 
                , blankFunction 
                , blankConditional 
                , blankFunctionType 
                , blankBoolType 
                , blankAssignment
                , blankUnknown ]

allPossibleTerms :: Zipper -> [Term]
allPossibleTerms z = (reverse (searchForNamedVariables z)) ++ (functionCalls z) ++ standardTerms

termTypeChecks :: Zipper -> Term -> Bool
termTypeChecks z t = case replaceWithTerm' t z of
                Just z' -> validateZipper z'
                Nothing -> False
                        

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
