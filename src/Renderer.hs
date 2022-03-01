{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( Style (..)
  , Render (render)
  , Cell (..)
  , Rendering
  , StackInstructions (Push, Pop, NewLine, StackLiteral)
  , layout
  , highlightAtPath
  , treeToStack
  ) where

import AST

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Data.Map

-- need new concept: renderer should just be a function that takes an int for
-- the screen width and returns a 2d map of chars+path info

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

-- termToPathMap :: (Tree a, Renderable a) => Int -> a -> PathMap
-- termToPathMap n t = generatePathMap [] (0,0) layoutPathsOnly
--         where removeNonPath (Location p) = Just (Location p)
--               removeNonPath _            = Nothing
--               layoutPathsOnly = alterAnnotationsS removeNonPath (layout n (renderTerm t))

-- highlightAtPath :: Path -> Doc Style -> Doc Style
-- highlightAtPath p = reAnnotate (\q -> case q of 
--                                         Location r -> if r == p then Highlight else q
--                                         _ -> q)

-- renderTerm'' :: (Tree a, Renderable a) => Path -> a -> Doc Style
-- renderTerm'' path c t = annotate (Location path) (renderTerm' (t,[]))

-- renderZipper :: (Tree a, Render a) => a -> Path -> Rendering
-- renderZipper t p = highlightAtPath p (renderTerm'' [] t)

-- below is generic plumbing to transform a Doc Style into a Widget for Brick
-- could maybe be replaced by SimpleDocStream? I literally think that is what it
-- is for
data StackInstructions a = StackLiteral String
                         | Push a
                         | Pop
                         | NewLine Int


treeToStack :: SimpleDocTree a -> [StackInstructions a]
treeToStack (STEmpty)           = []
treeToStack (STChar c)          = [StackLiteral [c]]
treeToStack (STText _ t)        = [StackLiteral (T.unpack t)]
treeToStack (STLine i)          = [NewLine i]
treeToStack (STAnn m content)   = [Push m] ++ (treeToStack content) ++ [Pop]
treeToStack (STConcat contents) = concat (fmap treeToStack contents)

