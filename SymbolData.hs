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
  , z) where

import AST

import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Token = IdentifierTerm T.Text
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

z :: Zipper Token
z = (Term Program [blankAssignment], [0])
