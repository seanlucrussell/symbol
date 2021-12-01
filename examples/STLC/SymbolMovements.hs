{-# LANGUAGE OverloadedStrings #-}
module STLC.SymbolMovements
  ( previousHole
  , nextHole
  , previousHole'
  , nextHole'
  ) where

import AST
import STLC.SymbolData
import Utilities
import Movements

import qualified Data.Text as T

import Data.List
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- movements: the underlying ast doesn't change, just the position in it

overUnknown :: (Token, Path) -> Bool
overUnknown (t,p) = case treeUnderCursor p t of
                Just t' -> t' == Unknown
                Nothing -> False

nextHole' :: (Token, Path) -> Maybe ((Token, Path))
nextHole' z = error "Unimplemented"
-- nextHole' z = nextHole'' overUnknown z

previousHole' :: (Token, Path) -> Maybe ((Token, Path))
previousHole' z = error "Unimplemented"
-- previousHole' z = previousHole'' overUnknown z

previousHole :: (Token, Path) -> (Token, Path)
previousHole = try previousHole'

nextHole :: (Token, Path) -> (Token, Path)
nextHole = try nextHole'
