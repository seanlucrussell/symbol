{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , replaceWithTermAndSelectNext
  , replaceWithTerm) where

import AST
import Movements
import TypeChecker
import Utilities

import SymbolData
import SymbolMovements

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

insertBefore :: Zipper Token -> Zipper Token
insertBefore (Term Program t, p:ps) = (Term Program (insertAt p blankAssignment t), p:ps)

insertAfter :: Zipper Token -> Zipper Token
insertAfter (Term Program t, p:ps) = (Term Program (insertAt (p+1) blankAssignment t), p:ps)

replaceWithTerm :: Term Token -> Zipper Token -> Zipper Token
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Term Token -> Zipper Token -> Zipper Token
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))

replaceWithTerm' :: Term Token -> Zipper Token -> Maybe (Zipper Token)
replaceWithTerm' t (x, p) = toMaybe (validateZipper replaced) replaced
        where replaced = (replaceWithTerm'' t p x, p)

searchForNamedVariables :: Zipper Token -> [Term Token]
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

functionCalls :: Zipper Token -> [Term a]
functionCalls _ = []
-- this is a dumb way of doing things!!! do it right!!! when you have more
-- time!!!
-- functionCalls z = concat [ [(ApplicationTerm x UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm) UnknownTerm)] | x <- searchForNamedVariables z]


standardTerms :: [Term Token]
standardTerms = [ blankTrue 
                , blankFalse 
                , blankFunction 
                , blankConditional 
                , blankFunctionType 
                , blankBoolType 
                , blankAssignment
                , blankUnknown ]

allPossibleTerms :: Zipper Token -> [Term Token]
allPossibleTerms z = (reverse (searchForNamedVariables z)) ++ (functionCalls z) ++ standardTerms

termTypeChecks :: Zipper Token -> Term Token -> Bool
termTypeChecks z t = case replaceWithTerm' t z of
                Just z' -> validateZipper z'
                Nothing -> False
                        

possibleTerms :: Zipper Token -> [Term Token]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)

-- language agnostic

replaceWithTerm'' :: Term a -> [Int] -> Term a -> Term a
replaceWithTerm'' t [] _ = t
replaceWithTerm'' t (p:ps) (Term x ts) = Term x (applyAtIndex p (replaceWithTerm'' t ps) ts)

