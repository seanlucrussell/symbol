{-# LANGUAGE OverloadedStrings #-}
module SymbolData
  ( Term
    ( IdentifierTerm
    , FunctionTerm
    , ApplicationTerm
    , BooleanLiteralTerm
    , ConditionalTerm
    , UnknownTerm
    , FnTypeTerm
    , BoolTypeTerm
    , Assignment
    , Program
    , TypeAbstractionTerm
    , TypeApplicationTerm
    , UniversalTypeTerm
    , TypeVariableTerm)
  , Container
    ( TopLevel
    , FunctionArg
    , FunctionArgType
    , FunctionBody
    , ApplicationFn
    , ApplicationArg
    , ConditionalCond
    , ConditionalOptOne
    , ConditionalOptTwo
    , AssignmentId
    , AssignmentType
    , AssignmentVal
    , FnTypeArg
    , FnTypeRet)
  , Zipper (Zipper)
  , zipperToTerm
  , z) where
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Term = IdentifierTerm T.Text
          | FunctionTerm Term Term Term
          | ApplicationTerm Term Term
          | TypeAbstractionTerm Term Term
          | TypeApplicationTerm Term Term
          | BooleanLiteralTerm Bool
          | ConditionalTerm Term Term Term
          | UnknownTerm
          | UniversalTypeTerm Term Term
          | TypeVariableTerm T.Text
          | FnTypeTerm Term Term
          | BoolTypeTerm
          | Assignment Term Term Term
          | Program [Term]
          deriving (Eq,Show)

data Container = TopLevel [Term] [Term]
               | FunctionArg Term Term Container
               | FunctionArgType Term Term Container
               | FunctionBody Term Term Container
               | ApplicationFn Term Container
               | ApplicationArg Term Container
               | ConditionalCond Term Term Container
               | ConditionalOptOne Term Term Container
               | ConditionalOptTwo Term Term Container
               | AssignmentId Term Term Container
               | AssignmentType Term Term Container
               | AssignmentVal Term Term Container
               | FnTypeArg Term Container
               | FnTypeRet Term Container
               deriving (Eq,Show)

data Zipper = Zipper Term Container deriving (Eq,Show)

goUp :: Term -> Container -> (Term, Maybe Container)
goUp t (TopLevel as bs) = (Program ((reverse as) ++ [t] ++ bs), Nothing)
goUp t (FunctionArg a b c) = (FunctionTerm t a b, Just c)
goUp t (FunctionArgType a b c) = (FunctionTerm a t b, Just c)
goUp t (FunctionBody a b c) = (FunctionTerm a b t, Just c)
goUp t (ApplicationFn a c) = (ApplicationTerm t a, Just c)
goUp t (ApplicationArg a c) = (ApplicationTerm a t, Just c)
goUp t (ConditionalCond a b c) = (ConditionalTerm t a b, Just c)
goUp t (ConditionalOptOne a b c) = (ConditionalTerm a t b, Just c)
goUp t (ConditionalOptTwo a b c) = (ConditionalTerm a b t, Just c)
goUp t (AssignmentId a b c) = (Assignment t a b, Just c)
goUp t (AssignmentType a b c) = (Assignment a t b, Just c)
goUp t (AssignmentVal a b c) = (Assignment a b t, Just c)
goUp t (FnTypeArg a c) = (FnTypeTerm t a, Just c)
goUp t (FnTypeRet a c) = (FnTypeTerm a t, Just c)

zipperToTerm :: Zipper -> Term
zipperToTerm (Zipper t c) = goToTop t (Just c)
        where goToTop t' (Just c') = let (t'',c'') = goUp t' c' in goToTop t'' c''
              goToTop t' Nothing = t'

z = Zipper (Assignment UnknownTerm UnknownTerm UnknownTerm) (TopLevel [] [])
