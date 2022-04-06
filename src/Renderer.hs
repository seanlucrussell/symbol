{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( Style (..)
  , Render (render)
  , Cell (..)
  , Rendering
  , layout
  , highlightAtPath
  ) where

import AST

import Prettyprinter
import Data.Map

data Cell = Cell { char :: Char
                 , style :: Style
                 , paths :: [Path]
                 } deriving (Show)

type Rendering = Map (Int, Int) Cell

data Style = Highlight | Yellow | White | Green | Blue | Magenta | Cyan | Red | Default deriving (Show)

class Render a where
  render :: Int -> a -> Rendering

layout :: Int -> Doc a -> SimpleDocStream a
layout n = layoutSmart (LayoutOptions (AvailablePerLine n 1))

highlightAtPath :: Path -> Rendering -> Rendering
highlightAtPath p = Data.Map.map highlight
        where highlight c = if elem p (paths c) then c {style = Highlight} else c
