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

import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core (vBox, str, modifyDefAttr, hLimit, str, vBox, vLimit)
import qualified Data.Vector as Vec
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

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
drawUI (StateData s z u _ p) = (case p of
     Just (l, n) -> [popup s (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1))]
     _ -> []) ++ [zipperToWidget s z]

popup :: SymbolTable -> L.List Name (Term Token) -> Widget Name
popup s l = C.centerLayer $ B.borderWithLabel label $ hLimit 50 $ vBox
                              [ str " "
                              , C.hCenter box
                              , str " "
                              , C.hCenter (str "Use arrow keys to move up/down.")
                              , C.hCenter (str "Press p to exit.")
                              ]
    where
        label = str "Item " Brick.<+> str cur Brick.<+> str " of " Brick.<+> str total
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i  -> show (i + 1)
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement _ a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                str "    " Brick.<+> (renderDoc (renderTerm s NoRenderContext a)) Brick.<+> Brick.fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l
