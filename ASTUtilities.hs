module ASTUtilities
        ( searchTree
        , replaceAtPoint
        ) where

import AST
import Utilities

import Control.Monad

searchTree :: (Tree a -> Bool) -> Tree a -> [Tree a]
searchTree f t@(Tree x ts) = (if f t then [t] else []) ++ join (fmap (searchTree f) ts) 

replaceAtPoint :: Tree a -> Path -> Tree a -> Tree a
replaceAtPoint t [] _ = t
replaceAtPoint t (p:ps) (Tree x ts) = Tree x (applyAtIndex p (replaceAtPoint t ps) ts)
