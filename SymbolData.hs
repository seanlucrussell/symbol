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
  , blankIdentifier
  , blankTrue
  , blankFalse
  , blankUnknown
  , blankFunction
  , blankConditional
  , blankApplication
  , blankFunctionType
  , blankBoolType
  , blankAssignment
  , newAssignment
  , initialSymbolTable
  , initialZipper) where

import AST

import Data.Map
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Token = IdentifierTerm Int
           | FunctionTerm
           | ApplicationTerm
           | TrueTerm
           | FalseTerm
           | ConditionalTerm
           | UnknownTerm
           | FunctionTypeTerm
           | BoolTypeTerm
           | AssignmentTerm
           | Program
           deriving (Eq,Show)

blankIdentifier t = Tree (IdentifierTerm t) []
blankTrue = Tree TrueTerm []
blankFalse = Tree FalseTerm []
blankUnknown = Tree UnknownTerm []
blankFunction = Tree FunctionTerm [blankUnknown, blankUnknown, blankUnknown]
blankConditional = Tree ConditionalTerm [blankUnknown, blankUnknown, blankUnknown]
blankFunctionType = Tree FunctionTypeTerm [blankUnknown, blankUnknown]
blankApplication = Tree ApplicationTerm [blankUnknown, blankUnknown]
blankBoolType = Tree BoolTypeTerm []
blankAssignment = Tree AssignmentTerm [blankUnknown, blankUnknown, blankUnknown]

newAssignment i = Tree AssignmentTerm [Tree (IdentifierTerm i) [], blankUnknown, blankUnknown]


type SymbolTable = Map Int T.Text

initialSymbolTable :: SymbolTable
initialSymbolTable = empty

initialZipper :: Zipper Token
initialZipper = (Tree Program [blankAssignment], [0])
