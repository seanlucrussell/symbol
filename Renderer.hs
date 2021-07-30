{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( zipperToWidget
  , renderDoc
  , renderTerm
  , Renderable (renderTerm')
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

-- generic stuff for rendering zippers

class Renderable a where
  renderTerm' :: a -> [Doc Marking] -> Doc Marking

renderZipper :: Renderable a => Zipper a -> Doc Marking
renderZipper (t, p) = rz p t

rz :: Renderable a => [Int] -> Term a -> Doc Marking
rz [] t = annotate Highlight (renderTerm t)
rz (p:ps) (Term x ts) = renderTerm' x (changeAtIndex p (rz ps (ts!!p)) (fmap renderTerm ts))


renderTerm :: Renderable a => Term a -> Doc Marking
renderTerm (Term t ts) = renderTerm' t (fmap renderTerm ts)

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

zipperToWidget :: Renderable a => Zipper a -> Widget ()
zipperToWidget = renderDoc . renderZipper

