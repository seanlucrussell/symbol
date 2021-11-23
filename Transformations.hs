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

findValidAssignmentId :: Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Token -> [Int]
findAllIds (IdentifierTerm i) = [i]
findAllIds t = join (fmap findAllIds (children t))

insertBefore :: Zipper Token -> Zipper Token
insertBefore = error "Need to redefine this"
-- insertBefore (t'@(Tree Program t), p:ps) = (Tree Program (insertAt p blankAssignment t), p:ps)

insertAfter :: Zipper Token -> Zipper Token
insertAfter = error "Need to redefine this"
-- insertAfter (t'@(Tree Program t), p:ps) = (Tree Program (insertAt (p+1) blankAssignment t), p:ps)

replaceWithTerm :: Token -> Zipper Token -> Zipper Token
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Token -> Zipper Token -> Zipper Token
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))

replaceWithTerm' :: Token -> Zipper Token -> Maybe (Zipper Token)
replaceWithTerm' t (x, p) = do replaced <- replaceAtPoint t p x
                               if validateProgram replaced
                               then return (replaced,p)
                               else Nothing

searchForNamedVariables :: Token -> [Token]
searchForNamedVariables = searchTree test
        where test (IdentifierTerm _) = True
              test _ = False

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
arityOfIdentifier :: Token -> Token -> Maybe Integer
arityOfIdentifier token tree = findIdentifierDefinition token tree >>= f
        where f :: Token -> Maybe Integer
              f (FunctionTypeTerm _ t) = fmap (+1) (f t)
              -- f (FunctionTypeTerm _ t) = do n <- f t
              --                               return (n+1)
              f BoolTypeTerm = Just 0
              f (FunctionTerm _ t _) = f t
              f (AssignmentTerm _ t _) = f t
              f _ = Nothing

-- to compute arity, we need to find the original definition of the term. this
-- could be a function or it could be an assignment
findIdentifierDefinition :: Token -> Token -> Maybe Token
findIdentifierDefinition (IdentifierTerm id) tree = listToMaybe (searchTree test tree)
        where test (FunctionTerm (IdentifierTerm id')  _ _) = id == id'
              test (AssignmentTerm (IdentifierTerm id') _ _) = id == id'
              test  _ = False
findIdentifierDefinition _ _ = Nothing

functionCalls :: Token -> [Token]
functionCalls t = concat [ fmap (app x) [0..(n x)] | x <- searchForNamedVariables t]
        where app x 0 = x
              app x n = ApplicationTerm (app x (n-1)) UnknownTerm
              n x = case arityOfIdentifier x t of
                Just m -> m
                Nothing -> 0

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , blankFunction
                , blankConditional 
                , blankApplication
                , blankFunctionType 
                , BoolTypeTerm
                , UnknownTerm ]

allPossibleTerms :: Token -> [Token]
allPossibleTerms t = functionCalls t ++ standardTerms

termTypeChecks :: Zipper Token -> Token -> Bool
termTypeChecks z t = isJust (replaceWithTerm' t z)

possibleTerms :: Zipper Token -> [Token]
possibleTerms (t,p) = filter (termTypeChecks (t,p)) (allPossibleTerms t)

updateSymbolTable :: Zipper Token -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable (t',p) t s = case treeUnderCursor p t' of
        Just (IdentifierTerm i) -> Just (M.insert i t s)
        _ -> Nothing
