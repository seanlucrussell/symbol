{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( zipperToWidget
  , renderDoc
  , renderTerm
  , Renderable (renderTerm')
  , RenderContext (RenderContext, NoRenderContext)
  , Marking (Highlight, Yellow, White, Green, Blue, Magenta, Cyan, Red)
  ) where

import AST
import SymbolData
import Utilities

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr)
import Lens.Micro
import Control.Monad

-- generic stuff for rendering zippers

data RenderContext a = RenderContext a Int
                     | NoRenderContext

class Renderable a where
  renderTerm' :: SymbolTable -> RenderContext a -> a -> [Doc Marking] -> Doc Marking

renderZipper :: Renderable a => SymbolTable -> Zipper a -> Doc Marking
renderZipper s (t, p) = rz s NoRenderContext p t

rz :: Renderable a => SymbolTable -> RenderContext a -> Path -> Term a -> Doc Marking
rz s c [] t = annotate Highlight (renderTerm s c t)
rz s c (p:ps) (Term x ts) = renderTerm' s c x [if p' == p then rz s (RenderContext x p') ps t' else renderTerm s (RenderContext x p') t' | (t',p') <- zip ts [0..] ]

flatten :: Term a -> [a]
flatten (Term x xs) = x:join (fmap flatten xs)

renderTerm :: Renderable a => SymbolTable -> RenderContext a -> Term a -> Doc Marking
renderTerm s c (Term t ts) = renderTerm' s c t [renderTerm s (RenderContext t p) t' | (p,t') <- zip [0..] ts]

-- below is generic plumbing to transform a Doc Marking into a Widget for Brick

data StackInstructions = StackLiteral String
                       | Push Marking
                       | Pop
                       | NewLine Int

data Marking = Highlight | Yellow | White | Green | Blue | Magenta | Cyan | Red

renderStack :: [StackInstructions] -> [Widget ()]
renderStack lines = renderStack' lines []
 where
 renderStack' [] _                  = [str ""]
 renderStack' (Push m:ts) s         = renderStack' ts (m:s)
 renderStack' (Pop:ts) (_:ss)       = renderStack' ts ss
 renderStack' (Pop:_) []            = []
 renderStack' (NewLine n:ts) s      = str " ":str (replicate n ' ') Brick.<+> a:as
   where a:as = renderStack' ts s
 renderStack' (StackLiteral l:ts) s = modifyDefAttr (\x -> x <> attributes s) (str l) Brick.<+> a:as
   where a:as = renderStack' ts s
         attributes []             = currentAttr
         attributes (Highlight:ss) = attributes ss `withBackColor` brightBlack
         attributes (Yellow:ss)    = attributes ss `withForeColor` yellow
         attributes (Green:ss)    = attributes ss `withForeColor` green
         attributes (Red:ss)    = attributes ss `withForeColor` red
         attributes (Blue:ss)    = attributes ss `withForeColor` blue
         attributes (Magenta:ss)    = attributes ss `withForeColor` magenta
         attributes (Cyan:ss)    = attributes ss `withForeColor` cyan
         attributes (White:ss)     = attributes ss `withForeColor` white
-- more colors:
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes-Color.html    
-- more info on vty styling (bold, underline, etc):
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes.html

renderTree :: SimpleDocTree Marking -> [StackInstructions]
renderTree (STEmpty)           = []
renderTree (STChar c)          = [StackLiteral [c]]
renderTree (STText _ t)        = [StackLiteral (T.unpack t)]
renderTree (STLine i)          = [NewLine i]
renderTree (STAnn m content)   = [Push m] ++ (renderTree content) ++ [Pop]
renderTree (STConcat contents) = concat (fmap renderTree contents)

renderDoc :: Doc Marking -> Widget ()
renderDoc d = Brick.Widget Brick.Fixed Brick.Fixed
  (do ctx <- Brick.getContext
      Brick.render $ vBox $ renderStack $ renderTree $ treeForm $ layoutSmart
                    (LayoutOptions (AvailablePerLine (ctx^.Brick.availWidthL) 1.0)) d)

zipperToWidget :: Renderable a => SymbolTable -> Zipper a -> Widget ()
zipperToWidget s = renderDoc . renderZipper s

