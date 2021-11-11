{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , updateSymbolTable
  , replaceWithTermAndSelectNext
  , replaceWithTerm) where

import AST
import Movements
import TypeChecker
import Utilities

import SymbolData
import SymbolMovements

import qualified Data.Text as T
import qualified Data.Map as M

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

findValidAssignmentId :: Tree Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Tree Token -> [Int]
findAllIds (Tree (IdentifierTerm i) ts) = i:join (fmap findAllIds ts)
findAllIds (Tree _ ts) = join (fmap findAllIds ts)

insertBefore :: Zipper Token -> Zipper Token
insertBefore (t'@(Tree Program t), p:ps) = (Tree Program (insertAt p blankAssignment t), p:ps)
-- insertBefore (t'@(Tree Program t), p:ps) = (Tree Program (insertAt p (newAssignment (findValidAssignmentId t')) t), p:ps)

insertAfter :: Zipper Token -> Zipper Token
insertAfter (t'@(Tree Program t), p:ps) = (Tree Program (insertAt (p+1) blankAssignment t), p:ps)

replaceWithTerm :: Tree Token -> Zipper Token -> Zipper Token
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Tree Token -> Zipper Token -> Zipper Token
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))

replaceWithTerm' :: Tree Token -> Zipper Token -> Maybe (Zipper Token)
replaceWithTerm' t (x, p) = toMaybe (validateZipper replaced) replaced
        where replaced = (replaceWithTerm'' t p x, p)

searchForNamedVariables :: Zipper Token -> [Tree Token]
searchForNamedVariables z = filter (/= blankUnknown) (searchAbove (goUp z) ++ prev)
        where prev = case goToTop z of
                          (_, [0]) -> []
                          _ -> searchBefore (selectPrev (goToTop z))
              searchAbove z = case termUnderCursor z of
                                   Tree FunctionTerm [a, _, _] -> a:searchAbove (goUp z)
                                   Tree AssignmentTerm [_, _, _] -> []
                                   _ -> searchAbove (goUp z)
              searchBefore z@(_, [0]) = case termUnderCursor z of
                                                   Tree AssignmentTerm [a, _, _] -> [a]
              searchBefore z@(_, _) = case termUnderCursor z of
                                                 Tree AssignmentTerm [a, _, _] -> a:searchBefore (selectPrev z)

-- get all named variables
-- for each variable:
--      if variable is function type, then add (ApplicationTerm id unknownTerm):recurse
-- what do we do about terms of unknow type? also need to do some work to get
-- type checker to support this
--
-- for now, do it this way. BUT! this is a dumb way of doing things!!! do it
-- right!!! when you have more time!!!
functionCalls :: Zipper Token -> [Tree Token]
functionCalls z = concat [ fmap (app x) [1..5] | x <- searchForNamedVariables z]
        where app x 0 = x
              app x n = Tree ApplicationTerm [app x (n-1), blankUnknown]



standardTerms :: [Tree Token]
standardTerms = [ blankTrue 
                , blankFalse 
                , blankConditional 
                , blankApplication
                , blankFunction
                , blankFunctionType 
                , blankBoolType 
                , blankAssignment
                , blankUnknown ]

allPossibleTerms :: Zipper Token -> [Tree Token]
allPossibleTerms z = (reverse (searchForNamedVariables z)) ++ (functionCalls z) ++ standardTerms

termTypeChecks :: Zipper Token -> Tree Token -> Bool
termTypeChecks z t = case replaceWithTerm' t z of
                Just z' -> validateZipper z'
                Nothing -> False
                        

possibleTerms :: Zipper Token -> [Tree Token]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)

updateSymbolTable :: Zipper Token -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable z t s = case tokenUnderCursor z of
        IdentifierTerm i -> Just (M.insert i t s)
        _ -> Nothing

-- language agnostic

replaceWithTerm'' :: Tree a -> [Int] -> Tree a -> Tree a
replaceWithTerm'' t [] _ = t
replaceWithTerm'' t (p:ps) (Tree x ts) = Tree x (applyAtIndex p (replaceWithTerm'' t ps) ts)
