{-# LANGUAGE OverloadedStrings #-}
module STLC.Transformations 
  ( possibleTerms
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
identifierArity :: Path -> Token -> Token -> Maybe Integer
identifierArity path id tree = findIdentifierDefinition path id tree >>= extractType >>= typeToArity

findIdentifierDefinition :: Path -> Token -> Token -> Maybe Token
findIdentifierDefinition path (Identifier n) tree = contextAtPoint path tree >>= safeListIndex n
findIdentifierDefinition _ _ _ = Nothing

contextAtPoint :: Path -> Token -> Maybe [Token]
contextAtPoint [] t = Just []
contextAtPoint (n:ns) (Program a) = do child <- select n (Program a)
                                       childContext <- contextAtPoint ns child
                                       return (reverse (fmap extractContext (take n a)) ++ childContext)
        where extractContext (Assignment _ t _) = t
              extractContext t = error ("Non-assignment found at top level: " ++ show t)
contextAtPoint (2:ns) (Function _ t b) = fmap (t:) (contextAtPoint ns b)
contextAtPoint (n:ns) t = do child <- select n t
                             contextAtPoint ns child

functionCalls :: Path -> Token -> [Token]
functionCalls p t = concat [ fmap (app i) [0..(n x)] | (x,i) <- zip context [0..]]
        where app x 0 = Identifier x
              app x n = Application (app x (n-1)) Unknown
              n x = fromMaybe 0 (identifierArity p x t)
              context = fromMaybe [] (contextAtPoint p t)

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , Function (Name Nothing) Unknown Unknown
                , Conditional Unknown Unknown Unknown
                , FunctionType Unknown Unknown
                , BoolType
                , Unknown ]

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = fromMaybe False (fmap validateProgram (replaceAtPoint t p t'))

possibleTerms :: Token -> Path -> [Token]
possibleTerms t p = filter (termTypeChecks t p) (functionCalls p t ++ standardTerms)
