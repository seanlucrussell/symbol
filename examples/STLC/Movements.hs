{-# LANGUAGE OverloadedStrings #-}
module STLC.Movements
  ( previousHole
  , nextHole
  , previousHole'
  , nextHole'
  ) where

import AST
import STLC.Data
import Utilities

-- overUnknown :: (Token, Path) -> Bool
-- overUnknown (t,p) = case treeUnderCursor p t of
--                 Just t' -> t' == Unknown
--                 Nothing -> False

nextHole' :: (Token, Path) -> Maybe ((Token, Path))
nextHole' = error "Unimplemented"
-- nextHole' z = nextHole'' overUnknown z

previousHole' :: (Token, Path) -> Maybe ((Token, Path))
previousHole' = error "Unimplemented"
-- previousHole' z = previousHole'' overUnknown z

previousHole :: (Token, Path) -> (Token, Path)
previousHole = try previousHole'

nextHole :: (Token, Path) -> (Token, Path)
nextHole = try nextHole'
