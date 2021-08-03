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

blankIdentifier t = Term (IdentifierTerm t) []
blankTrue = Term TrueTerm []
blankFalse = Term FalseTerm []
blankUnknown = Term UnknownTerm []
blankFunction = Term FunctionTerm [blankUnknown, blankUnknown, blankUnknown]
blankConditional = Term ConditionalTerm [blankUnknown, blankUnknown, blankUnknown]
blankFunctionType = Term FunctionTypeTerm [blankUnknown, blankUnknown]
blankApplication = Term ApplicationTerm [blankUnknown, blankUnknown]
blankBoolType = Term BoolTypeTerm []
blankAssignment = Term AssignmentTerm [blankUnknown, blankUnknown, blankUnknown]

newAssignment i = Term AssignmentTerm [Term (IdentifierTerm i) [], blankUnknown, blankUnknown]


type SymbolTable = Map Int T.Text

initialSymbolTable :: SymbolTable
initialSymbolTable = empty

initialZipper :: Zipper Token
initialZipper = (Term Program [blankAssignment], [0])
