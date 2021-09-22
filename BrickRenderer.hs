{-# LANGUAGE OverloadedStrings #-}
module BrickRenderer
  ( zipperToWidget
  , renderDoc
  ) where

import AST
import SymbolData
import Utilities
import Renderer

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr)
import Lens.Micro
import Control.Monad

renderStack :: [StackInstructions Marking] -> [Widget ()]
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
         -- attributes (Highlight:ss) = defAttr `withForeColor` brightWhite `withBackColor` brightBlack
         attributes (Location _:ss) = attributes ss
         attributes (Highlight:ss) = defAttr `withForeColor` (rgbColor 240 240 240) `withBackColor` brightBlack
         attributes (Yellow:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` yellow
         attributes (Green:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` green
         attributes (Red:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` red
         attributes (Blue:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` blue
         attributes (Magenta:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` magenta
         attributes (Cyan:ss)    = if highlighted ss then attributes ss else attributes ss `withForeColor` cyan
         attributes (White:ss)     = if highlighted ss then attributes ss else attributes ss `withForeColor` white
         highlighted (Highlight:_)  = True
         highlighted []  = False
         highlighted (_:xs) = highlighted xs
-- more colors:
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes-Color.html    
-- more info on vty styling (bold, underline, etc):
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes.html

renderDoc :: Doc Marking -> Widget ()
renderDoc d = Brick.Widget Brick.Fixed Brick.Fixed
  (do ctx <- Brick.getContext
      Brick.render $ vBox $ renderStack $ treeToStack $ treeForm $ layout (ctx^.Brick.availWidthL) d)

zipperToWidget :: Renderable a => SymbolTable -> Zipper a -> Widget ()
zipperToWidget s = renderDoc . renderZipper s

