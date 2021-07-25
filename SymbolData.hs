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
  , Zipper (Zipper)
  , Term (Term)
  , zipperToTerm
  , termUnderCursor
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

data Term =  Term Token [Term]
          deriving (Eq,Show)

data Zipper = Zipper Term [Int] deriving (Eq,Show)

termUnderCursor :: Zipper -> Term
termUnderCursor (Zipper t []) = t
termUnderCursor (Zipper (Term _ ts) (x:xs)) = termUnderCursor (Zipper (ts!!x) xs)

zipperToTerm :: Zipper -> Term
zipperToTerm (Zipper t _) = t

z = Zipper (Term Program [(Term AssignmentTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []])]) [0]
