{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( Transformation
  , swapUp
  , swapDown
  , remove
  , insertBefore
  , insertAfter
  , replaceAtPoint'
  ) where

import AST
import Movements
import Utilities

type Transformation a = a -> Path -> Maybe (a, Path)

-- NOTE: THIS ONLY SWAPS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
swapDown :: Tree a => Transformation a
swapDown t (p:ps) = do swapped <- swapAdjacent p (children t)
                       newTree <- update t swapped
                       return (newTree, p+1:ps)

-- NOTE: THIS ONLY SWAPS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
swapUp :: Tree a => Transformation a
swapUp t (p:ps) = do swapped <- swapAdjacent (p-1) (children t)
                     newTree <- update t swapped
                     return (newTree, p-1:ps)

-- NOTE: THIS ONLY REMOVES AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
remove :: Tree a => Transformation a
remove t (p:ps) = do removed <- removeAtIndex p (children t)
                     newTree <- update t removed
                     return (newTree, [p-1])

-- NOTE: THIS ONLY INSERTS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
insertBefore :: Tree a => a -> Transformation a
insertBefore insertion tree (p:ps) = do newTree <- update tree (insertAt p insertion (children tree))
                                        return (newTree, p+1:ps)

-- NOTE: THIS ONLY INSERTS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
insertAfter :: Tree a => a -> Transformation a
insertAfter insertion tree (p:ps) = do newTree <- update tree (insertAt (p+1) insertion (children tree))
                                       return (newTree, p:ps)

replaceAtPoint' :: Tree a => a -> Transformation a
replaceAtPoint' replacement tree path = do newTree <- replaceAtPoint replacement path tree
                                           return (newTree, path)
