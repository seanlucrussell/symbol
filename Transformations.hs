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

insertBefore :: (Token, Path) -> (Token, Path)
insertBefore (t, p:ps) = case newToken of
                Just t' -> (t', 1+p:ps)
                Nothing -> (t, p:ps)
        where newToken = update t (insertAt p blankAssignment (children t))

insertAfter :: (Token, Path) -> (Token, Path)
insertAfter (t, p:ps) = case newToken of
                Just t' -> (t', p:ps)
                Nothing -> (t, p:ps)
        where newToken = update t (insertAt (p+1) blankAssignment (children t))

replaceWithTerm :: Token -> (Token, Path) -> (Token, Path)
replaceWithTerm replacement = try (replaceWithTerm' replacement)

replaceWithTermAndSelectNext :: Token -> (Token, Path) -> (Token, Path)
replaceWithTermAndSelectNext replacement = try (replaceWithTerm' replacement >=> (nextHole' <!> return))

replaceWithTerm' :: Token -> (Token, Path) -> Maybe ((Token, Path))
replaceWithTerm' t (x, p) = do replaced <- replaceAtPoint t p x
                               if validateProgram replaced
                               then return (replaced,p)
                               else Nothing

searchForNamedVariables :: Token -> [Token]
searchForNamedVariables = mapMaybe extractIdentifier . allIdentifierDefinitions

allIdentifierDefinitions :: Token -> [Token]
allIdentifierDefinitions tree = searchTree test tree
        where test (FunctionTerm (IdentifierTerm _)  _ _) = True
              test (AssignmentTerm (IdentifierTerm _) _ _) = True
              test  _ = False

extractIdentifier :: Token -> Maybe Token
extractIdentifier (FunctionTerm i _ _) = Just i
extractIdentifier (AssignmentTerm i _ _) = Just i
extractIdentifier _ = Nothing

extractType :: Token -> Maybe Token
extractType (FunctionTerm _ t _) = Just t
extractType (AssignmentTerm _ t _) = Just t
extractType _ = Nothing

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
arityOfIdentifier :: Token -> Token -> Maybe Integer
arityOfIdentifier token tree = findIdentifierDefinition token tree >>= extractType >>= f
        where f :: Token -> Maybe Integer
              f (FunctionTypeTerm _ t) = fmap (+1) (f t)
              f BoolTypeTerm = Just 0
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

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = isJust (replaceWithTerm' t (t',p))

possibleTerms :: (Token, Path) -> [Token]
possibleTerms (t,p) = filter (termTypeChecks t p) (allPossibleTerms t)

updateSymbolTable :: (Token, Path) -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable (t',p) t s = case treeUnderCursor p t' of
        Just (IdentifierTerm i) -> Just (M.insert i t s)
        _ -> Nothing
