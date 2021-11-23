{-# LANGUAGE OverloadedStrings #-}
module SymbolData
  ( Token
    ( IdentifierTerm
    , FunctionTerm
    , ApplicationTerm
    , TrueTerm
    , FalseTerm
    , ConditionalTerm
    , UnknownTerm
    , FunctionTypeTerm
    , BoolTypeTerm
    , AssignmentTerm
    , Program)
  , SymbolTable
  , blankFunction
  , blankConditional
  , blankApplication
  , blankFunctionType
  , blankAssignment
  , newAssignment
  ) where

import AST

import Data.Map
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Token = IdentifierTerm Int
           | FunctionTerm Token Token Token
           | ApplicationTerm Token Token
           | TrueTerm
           | FalseTerm
           | ConditionalTerm Token Token Token
           | UnknownTerm
           | FunctionTypeTerm Token Token
           | BoolTypeTerm
           | AssignmentTerm Token Token Token
           | Program [Token]
           deriving (Eq,Show)

instance Tree Token where
   children (FunctionTerm a b c) = [a,b,c]
   children (ApplicationTerm a b) = [a,b]
   children (ConditionalTerm a b c) = [a,b,c]
   children (FunctionTypeTerm a b) = [a,b]
   children (AssignmentTerm a b c) = [a,b,c]
   children (Program ts) = ts
   children _ = []
   
   update (FunctionTerm _ _ _) [a,b,c] = Just (FunctionTerm a b c)
   update (ApplicationTerm _ _) [a,b] = Just (ApplicationTerm a b)
   update (ConditionalTerm _ _ _) [a,b,c] = Just (ConditionalTerm a b c)
   update (FunctionTypeTerm _ _) [a,b] = Just (FunctionTypeTerm a b)
   update (AssignmentTerm _ _ _) [a,b,c] = Just (AssignmentTerm a b c)
   update (Program _) ts = Just (Program ts)
   update _ _ = Nothing

blankFunction = FunctionTerm UnknownTerm UnknownTerm UnknownTerm
blankConditional = ConditionalTerm UnknownTerm UnknownTerm UnknownTerm
blankFunctionType = FunctionTypeTerm UnknownTerm UnknownTerm
blankApplication = ApplicationTerm UnknownTerm UnknownTerm
blankAssignment = AssignmentTerm UnknownTerm UnknownTerm UnknownTerm

newAssignment i = AssignmentTerm (IdentifierTerm i) UnknownTerm UnknownTerm

type SymbolTable = Map Int T.Text
