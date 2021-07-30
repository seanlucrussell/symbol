{-# LANGUAGE OverloadedStrings #-}
module SymbolMovements
  ( previousHole
  , nextHole
  , previousHole'
  , nextHole'
  ) where

import AST
import SymbolData
import Utilities
import Movements

import qualified Data.Text as T

import Data.List
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- movements: the underlying ast doesn't change, just the position in it

overUnknownTerm :: Zipper Token -> Bool
overUnknownTerm = (== UnknownTerm) . tokenUnderCursor

nextHole' :: Zipper Token -> Maybe (Zipper Token)
nextHole' z = nextHole'' overUnknownTerm z

previousHole' :: Zipper Token -> Maybe (Zipper Token)
previousHole' z = previousHole'' overUnknownTerm z

previousHole :: Zipper Token -> Zipper Token
previousHole = try previousHole'

nextHole :: Zipper Token -> Zipper Token
nextHole = try nextHole'
