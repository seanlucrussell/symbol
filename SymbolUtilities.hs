module SymbolUtilities
    ( validIdentifier
    , overIdentifier
    ) where

import AST
import SymbolData
import Movements
import Utilities

import Control.Monad

validIdentifier :: Token -> Token
validIdentifier t = (IdentifierTerm (findValidAssignmentId t))

findValidAssignmentId :: Token -> Int
findValidAssignmentId t = firstNumberNotInList (findAllIds t)

findAllIds :: Token -> [Int]
findAllIds (IdentifierTerm i) = [i]
findAllIds t = join (fmap findAllIds (children t))

-- Includes places where the identifier is still an UnknownToken
overIdentifier :: Zipper Token -> Bool
overIdentifier (t,p) = case treeUnderCursor p' t' of
        Just (FunctionTerm _ _ _) -> Prelude.last p == 0
        Just (AssignmentTerm _ _ _) -> Prelude.last p == 0
        _ -> False
      where (t',p') = goUp (t,p)
