{-# LANGUAGE OverloadedStrings #-}
module STLC.Transformations 
  ( possibleTerms
  , updateSymbolTable
  ) where

import AST
import Movements
import Utilities
import Transformations

import STLC.TypeChecker
import STLC.Data
import STLC.Movements

import qualified Data.Text as T
import qualified Data.Map as M

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

namedVariables :: Token -> [Token]
namedVariables = mapMaybe extractIdentifier . allIdentifierDefinitions

allIdentifierDefinitions :: Token -> [Token]
allIdentifierDefinitions tree = searchTree test tree
        where test (Function (Identifier _)  _ _) = True
              test (Assignment (Identifier _) _ _) = True
              test  _ = False

extractIdentifier :: Token -> Maybe Token
extractIdentifier (Function i _ _) = Just i
extractIdentifier (Assignment i _ _) = Just i
extractIdentifier _ = Nothing

extractType :: Token -> Maybe Token
extractType (Function _ t _) = Just t
extractType (Assignment _ t _) = Just t
extractType _ = Nothing

typeToArity :: Token -> Maybe Integer
typeToArity (FunctionType _ t) = fmap (+1) (typeToArity t)
typeToArity BoolType = Just 0
typeToArity _ = Nothing

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
identifierArity :: Token -> Token -> Maybe Integer
identifierArity id tree = findIdentifierDefinition id tree >>= extractType >>= typeToArity

-- to compute arity, we need to find the original definition of the term. this
-- could be a function or it could be an assignment
findIdentifierDefinition :: Token -> Token -> Maybe Token
findIdentifierDefinition id tree = listToMaybe (searchTree test tree)
        where test t = fromMaybe False (fmap (==id) (extractIdentifier t))

functionCalls :: Token -> [Token]
functionCalls t = concat [ fmap (app x) [0..(n x)] | x <- namedVariables t]
        where app x 0 = x
              app x n = Application (app x (n-1)) Unknown
              n x = fromMaybe 0 (identifierArity x t)

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , Function Unknown Unknown Unknown
                , Conditional Unknown Unknown Unknown
                , FunctionType Unknown Unknown
                , BoolType
                , Unknown ]

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = fromMaybe False (fmap validateProgram (replaceAtPoint t p t'))

possibleTerms :: Token -> Path -> [Token]
possibleTerms t p = filter (termTypeChecks t p) (functionCalls t ++ standardTerms)

updateSymbolTable :: Token -> Path -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable t' p t s = case treeUnderCursor p t' of
        Just (Identifier i) -> Just (M.insert i t s)
        _ -> Nothing
