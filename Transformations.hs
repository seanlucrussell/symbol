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
import ASTUtilities

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

insertAfter :: Zipper Token -> Zipper Token
insertAfter (t'@(Tree Program t), p:ps) = (Tree Program (insertAt (p+1) blankAssignment t), p:ps)

replaceWithTerm :: Tree Token -> Zipper Token -> Zipper Token
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Tree Token -> Zipper Token -> Zipper Token
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))

replaceWithTerm' :: Tree Token -> Zipper Token -> Maybe (Zipper Token)
replaceWithTerm' t (x, p) = toMaybe (validateZipper replaced) replaced
        where replaced = (replaceAtPoint t p x, p)

searchForNamedVariables :: Tree Token -> [Tree Token]
searchForNamedVariables = searchTree test
        where test (Tree (IdentifierTerm _) []) = True
              test _ = False

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
arityOfIdentifier :: Token -> Tree Token -> Maybe Integer
arityOfIdentifier token tree = findIdentifierDefinition token tree >>= f
        where f :: Tree Token -> Maybe Integer
              f (Tree FunctionTypeTerm [_, t]) = do n <- f t
                                                    return (n+1)
              f (Tree BoolTypeTerm []) = Just 0
              f (Tree FunctionTerm [_, t, _]) = f t
              f (Tree AssignmentTerm [_, t, _]) = f t
              f _ = Nothing

-- to compute arity, we need to find the original definition of the term. this
-- could be a function or it could be an assignment
findIdentifierDefinition :: Token -> Tree Token -> Maybe (Tree Token)
findIdentifierDefinition (IdentifierTerm id) tree = listToMaybe (searchTree test tree)
        where test (Tree FunctionTerm [Tree (IdentifierTerm id') [], _, _]) = id == id'
              test (Tree AssignmentTerm [Tree (IdentifierTerm id') [], _, _]) = id == id'
              test  _ = False
findIdentifierDefinition _ _ = Nothing

functionCalls :: Tree Token -> [Tree Token]
functionCalls t = concat [ fmap (app x) [0..(n (extractToken x))] | x <- searchForNamedVariables t]
        where app x 0 = x
              app x n = Tree ApplicationTerm [app x (n-1), blankUnknown]
              n x = case arityOfIdentifier x t of
                Just m -> m
                Nothing -> 0

standardTerms :: [Tree Token]
standardTerms = [ blankTrue 
                , blankFalse 
                , blankFunction
                , blankConditional 
                , blankApplication
                , blankFunctionType 
                , blankBoolType 
                , blankUnknown ]

allPossibleTerms :: Tree Token -> [Tree Token]
allPossibleTerms t = functionCalls t ++ standardTerms

termTypeChecks :: Zipper Token -> Tree Token -> Bool
termTypeChecks z t = isJust (replaceWithTerm' t z)

possibleTerms :: Zipper Token -> [Tree Token]
possibleTerms (t,p) = filter (termTypeChecks (t,p)) (allPossibleTerms t)

updateSymbolTable :: Zipper Token -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable z t s = case tokenUnderCursor z of
        IdentifierTerm i -> Just (M.insert i t s)
        _ -> Nothing
