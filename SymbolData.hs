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
  , Zipper
  , Path
  , Term (Term)
  , zipperToTerm
  , termUnderCursor
  , tokenUnderCursor
  , blankIdentifier
  , blankTrue
  , blankFalse
  , blankUnknown
  , blankFunction
  , blankConditional
  , blankFunctionType
  , blankBoolType
  , blankAssignment
  , z) where
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

data Term = Term Token [Term] deriving (Eq,Show)

type Path = [Int]
type Zipper = (Term, Path)

termUnderCursor :: Zipper -> Term
termUnderCursor (t, []) = t
termUnderCursor ((Term _ ts), (x:xs)) = termUnderCursor (ts!!x, xs)

extractToken :: Term -> Token
extractToken (Term t _) = t

tokenUnderCursor :: Zipper -> Token
tokenUnderCursor = extractToken . termUnderCursor

zipperToTerm :: Zipper -> Term
zipperToTerm (t, _) = t

blankIdentifier t = Term (IdentifierTerm t) []
blankTrue = Term TrueTerm []
blankFalse = Term FalseTerm []
blankUnknown = Term UnknownTerm []
blankFunction = Term FunctionTerm [blankUnknown, blankUnknown, blankUnknown]
blankConditional = Term ConditionalTerm [blankUnknown, blankUnknown, blankUnknown]
blankFunctionType = Term FunctionTypeTerm [blankUnknown, blankUnknown]
blankBoolType = Term BoolTypeTerm []
blankAssignment = Term AssignmentTerm [blankUnknown, blankUnknown, blankUnknown]

z :: Zipper
z = (Term Program [blankAssignment], [0])
