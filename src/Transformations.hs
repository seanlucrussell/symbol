{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Transformations 
  ( Transformation
  , replaceAtPoint'
  , movementToTransformation
  ) where

import AST
import Movements

type Transformation a = a -> Path -> Maybe (a, Path)

movementToTransformation :: Movement a -> Transformation a
movementToTransformation m t p = fmap (t,) (m t p)

replaceAtPoint' :: Tree a => a -> Transformation a
replaceAtPoint' replacement tree path = do newTree <- replaceAtPoint replacement path tree
                                           return (newTree, path)
