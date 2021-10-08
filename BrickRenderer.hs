{-# LANGUAGE OverloadedStrings #-}
module BrickRenderer
  ( zipperToWidget
  , renderDoc
  , Name (ZipperName, PopupName)
  , popup
  , drawUI
  ) where

import AST
import SymbolData
import Utilities
import Renderer
import Application
import SymbolRenderer

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr, hLimit, str, vBox, vLimit, padLeft, padTop)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec

import Lens.Micro
import Control.Monad

renderStack :: [StackInstructions Marking] -> [Widget Name]
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

renderDoc :: Doc Marking -> Widget Name
renderDoc d = Brick.Widget Brick.Fixed Brick.Fixed
  (do ctx <- Brick.getContext
      Brick.render $ vBox $ renderStack $ treeToStack $ treeForm $ layout (ctx^.Brick.availWidthL) d)

data Name = ZipperName | PopupName deriving (Eq, Show)

instance Ord Name where
  ZipperName <= PopupName = True
  PopupName <= ZipperName = False
  _ <= _ = True

zipperToWidget :: Renderable a => SymbolTable -> Zipper a -> Widget Name
zipperToWidget s = Brick.reportExtent ZipperName . renderDoc . renderZipper s

drawUI :: StateData -> [Widget Name]
drawUI (StateData (s, z, x, p) u _) = (case p of
     Just (l, n) -> [popup x s (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1))]
     _ -> []) ++ [zipperToWidget s z]

popup :: Position -> SymbolTable -> L.List Name (Term Token) -> Widget Name
-- popup (x,y) s l = C.centerLayer $ B.borderWithLabel label $ hLimit 50 $ vBox
popup (x,y) s l =  Brick.translateBy (Brick.Location (x-1,y+1)) $ B.border $ hLimit 50 $ box
    where
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement _ a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                (renderDoc (renderTerm s NoRenderContext a)) Brick.<+> Brick.fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l
