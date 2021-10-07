module SymbolUtilities
    ( validIdentifier
    , overIdentifier
    ) where

import AST
import SymbolData
import Movements
import Utilities

import Control.Monad

validIdentifier :: Term Token -> Term Token
validIdentifier t = Term (IdentifierTerm (findValidAssignmentId t)) []

findValidAssignmentId :: Term Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Term Token -> [Int]
findAllIds (Term (IdentifierTerm i) ts) = i:join (fmap findAllIds ts)
findAllIds (Term _ ts) = join (fmap findAllIds ts)

overIdentifier :: Zipper Token -> Bool
overIdentifier (t,p) = case tokenUnderCursor (goUp (t,p)) of
        FunctionTerm -> Prelude.last p == 0
        AssignmentTerm -> Prelude.last p == 0
        _ -> False
