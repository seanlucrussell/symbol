{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( Transformation
  , replaceAtPoint'
  ) where

import AST
import Movements
import Utilities

type Transformation a = a -> Path -> Maybe (a, Path)

replaceAtPoint' :: Tree a => a -> Transformation a
replaceAtPoint' replacement tree path = do newTree <- replaceAtPoint replacement path tree
                                           return (newTree, path)
