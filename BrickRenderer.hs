{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BrickRenderer
  ( zipperToWidget
  , renderDoc
  , Name (MainWindowName, PopupName)
  , popup
  , drawUI
  , AppInput
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

type AppInput = (Key, Int) -- Int represents screen width, needed for renderer

instance SymbolAppInput AppInput where
  extractInput ((KChar '\t'),_) = Tab
  extractInput (KBackTab,_)     = BackTab
  extractInput ((KChar c),_) = Key c
  extractInput (KEnter,_)    = Enter
  extractInput (KBS,_)       = Del
  extractInput (KUp,_)       = UpArrow
  extractInput (KDown,_)     = DownArrow
  extractInput (KLeft,_)     = LeftArrow
  extractInput (KRight,_)     = RightArrow
  extractInput (KEsc,_)     = Esc
  extractInput _             = Other
  extractWidth (_,n) = n

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
         -- attributes (Highlight:ss) = defAttr `withForeColor` (rgbColor 240 240 240) `withBackColor` brightBlack
         attributes (Highlight:ss) = defAttr `withStyle` standout
         attributes (Yellow:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` yellow
         attributes (Green:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` green) `withStyle` dim
         attributes (Red:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` red) `withStyle` bold
         attributes (Blue:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` blue) `withStyle` underline
         attributes (Magenta:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` magenta) `withStyle` italic
         attributes (Cyan:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` cyan
         attributes (White:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` white
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

data Name = MainWindowName | PopupName deriving (Eq, Show)

instance Ord Name where
  MainWindowName <= PopupName = True
  PopupName <= MainWindowName = False
  _ <= _ = True

zipperToWidget :: (Tree a, Renderable a) => SymbolTable -> (a, Path) -> Widget Name
zipperToWidget s (t,p) = Brick.reportExtent MainWindowName (renderDoc (renderZipper s t p))

drawUI :: StateData AppInput -> [Widget Name]
drawUI (StateData (s, z, x, p) u _) = (case p of
     Just (l, n) -> [popup x s (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1))]
     _ -> []) ++ [zipperToWidget s z]

popup :: Position -> SymbolTable -> L.List Name Token -> Widget Name
-- popup (x,y) s l = C.centerLayer $ B.borderWithLabel label $ hLimit 50 $ vBox
popup (x,y) s l =  Brick.translateBy (Brick.Location (x-1,y+1)) $ B.border $ hLimit 50 $ box
    where
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement _ a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                (renderDoc (renderTerm s NoRenderContext a)) Brick.<+> Brick.fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l
