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
validIdentifier t = (Identifier (findValidAssignmentId t))

findValidAssignmentId :: Token -> Int
findValidAssignmentId t = firstNumberNotInList (findAllIds t)

findAllIds :: Token -> [Int]
findAllIds (Identifier i) = [i]
findAllIds t = join (fmap findAllIds (children t))

-- Includes places where the identifier is still an UnknownToken
overIdentifier :: Token -> Path -> Bool
overIdentifier t p = case treeUnderCursor p' t of
        Just (Function _ _ _) -> Prelude.last p == 0
        Just (Assignment _ _ _) -> Prelude.last p == 0
        _ -> False
      where p' = goUp t p
