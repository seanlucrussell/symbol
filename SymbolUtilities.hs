module SymbolUtilities
    ( validIdentifier
    , overIdentifier
    ) where

import AST
import SymbolData
import Movements
import Utilities

import Control.Monad

validIdentifier :: Tree Token -> Tree Token
validIdentifier t = Tree (IdentifierTerm (findValidAssignmentId t)) []

findValidAssignmentId :: Tree Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Tree Token -> [Int]
findAllIds (Tree (IdentifierTerm i) ts) = i:join (fmap findAllIds ts)
findAllIds (Tree _ ts) = join (fmap findAllIds ts)

overIdentifier :: Zipper Token -> Bool
overIdentifier (t,p) = case tokenUnderCursor (goUp (t,p)) of
        FunctionTerm -> Prelude.last p == 0
        AssignmentTerm -> Prelude.last p == 0
        _ -> False
