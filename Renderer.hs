{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( Marking (Highlight, Yellow, White, Green, Blue, Magenta, Cyan, Red, Location)
  , PathMap
  , Position
  , RenderContext (RenderContext, NoRenderContext)
  , Renderable (renderTerm')
  , StackInstructions (Push, Pop, NewLine, StackLiteral)
  , layout
  , renderTerm
  , renderZipper
  , termToPathMap
  , treeToStack
  ) where

import AST
import SymbolData
import Utilities

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Data.Map

-- contains info on parent of a given term
data RenderContext a = RenderContext a Int
                     | NoRenderContext

-- split this into internal and external annotations or something. i.e. the
-- renderer shouldn't have to worry about highlighting and stuff.
data Marking = Highlight | Yellow | White | Green | Blue | Magenta | Cyan | Red | Location Path deriving (Show)

class Renderable a where
  renderTerm' :: SymbolTable -> RenderContext a -> a -> [Doc Marking] -> Doc Marking

layout :: Int -> Doc a -> SimpleDocStream a
layout n = layoutSmart (LayoutOptions (AvailablePerLine n 1))


type Position = (Int, Int)
type PathMap = Map Position Path

-- push/pop requires that we ignore annotations that aren't markings. currently
-- fixed in termToPathMap but more robust solution would be preferred
generatePathMap :: [Path] -> Position -> SimpleDocStream Marking -> PathMap
generatePathMap _      (x,y) SFail                      = empty
generatePathMap _      (x,y) SEmpty                     = empty
generatePathMap (p:ps) (x,y) (SChar _ s)                = insert (x,y) p (generatePathMap (p:ps) (x+1,y) s)
generatePathMap (p:ps) (x,y) (SText x' _ s)             = union (fromList [((x+n,y),p) | n <- [0..x']])
                                                                (generatePathMap (p:ps) (x+x',y) s)
generatePathMap (p:ps) (_,y) (SLine x s)                = generatePathMap (p:ps) (x,y+1) s
generatePathMap p      (x,y) (SAnnPush (Location p') s) = generatePathMap (p':p) (x,y) s
generatePathMap (p:ps) (x,y) (SAnnPush _ s)             = generatePathMap (p:ps) (x,y) s
generatePathMap (p:ps) (x,y) (SAnnPop s)                = generatePathMap ps (x,y) s
generatePathMap []     x     n                          = error ("unmatched pattern in generatePathMap: "
                                                             ++ "empty path"
                                                             ++ ", "
                                                             ++ show x ++ show n)

termToPathMap :: (Tree a, Renderable a) => SymbolTable -> Int -> a -> PathMap
termToPathMap s n t = generatePathMap [] (0,0) layoutPathsOnly
        where removeNonPath (Location p) = Just (Location p)
              removeNonPath _            = Nothing
              layoutPathsOnly = alterAnnotationsS removeNonPath (layout n (renderTerm s NoRenderContext t))

highlightAtPath :: Path -> Doc Marking -> Doc Marking
highlightAtPath p = reAnnotate (\q -> case q of 
                                        Location r -> if r == p then Highlight else q
                                        _ -> q)

renderTerm'' :: (Tree a, Renderable a) => Path -> SymbolTable -> RenderContext a -> a -> Doc Marking
renderTerm'' path s c t = annotate (Location path) (renderTerm' s c t renderedChildren)
        where renderedChildren = [renderTerm'' (path ++ [p]) s (RenderContext t p) t' | (p,t') <- zip [0..] (children t)]

renderZipper :: (Tree a, Renderable a) => SymbolTable -> a -> Path -> Doc Marking
renderZipper s t p = highlightAtPath p (renderTerm'' [] s NoRenderContext t)

renderTerm :: (Tree a, Renderable a) => SymbolTable -> RenderContext a -> a -> Doc Marking
renderTerm = renderTerm'' []

-- below is generic plumbing to transform a Doc Marking into a Widget for Brick
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

