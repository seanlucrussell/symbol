{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module STLC.BrickRenderer
  ( drawMainWindow
  , Name (MainWindowName, PopupName)
  , drawUI
  ) where

import AST
import Utilities
import Renderer
import STLC.Application
import STLC.Data
import STLC.Renderer

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr, hLimit, str, vBox, vLimit, padLeft, padTop)
import Data.Map
import Data.Maybe
import Data.List
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

xMin :: Map (Int,Int) t -> Int
xMin m = minimum (fmap fst (keys m))

xMax :: Map (Int,Int) t -> Int
xMax m = maximum (fmap fst (keys m))

yMin :: Map (Int,Int) t -> Int
yMin m = minimum (fmap snd (keys m))

yMax :: Map (Int,Int) t -> Int
yMax m = maximum (fmap snd (keys m))

renderAsWidget :: Render a => a -> Widget Name
renderAsWidget = renderingToWidget . render 100

renderWithPathHighlighted :: Render a => Path -> Int -> a -> Widget Name
renderWithPathHighlighted p n = renderingToWidget . highlightAtPath p . render n

-- takes map over 2d coordinates and a default value and returns a 2d grid
grid :: Map (Int,Int) t -> t -> [[t]]
grid m d = [ [fromMaybe d (Data.Map.lookup (x,y) m) | y <- [yMin m..yMax m] ] | x <- [xMin m..xMax m]]

cellToWidget :: Cell -> Widget Name
cellToWidget (Cell c Highlight _) = modifyDefAttr (\x -> x <> defAttr `withStyle` standout) (str [c])
cellToWidget (Cell c Yellow _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` yellow) (str [c])
cellToWidget (Cell c White _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` white) (str [c])
cellToWidget (Cell c Green _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` green) (str [c])
cellToWidget (Cell c Blue _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` blue) (str [c])
cellToWidget (Cell c Magenta _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` magenta) (str [c])
cellToWidget (Cell c Cyan _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` cyan) (str [c])
cellToWidget (Cell c Red _) = modifyDefAttr (\x -> x <> defAttr `withForeColor` red) (str [c])
cellToWidget (Cell c Renderer.Default _) = str [c]

gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap = fmap . fmap

renderingToWidget :: Rendering -> Widget Name
renderingToWidget r = vBox (fmap (Prelude.foldr (Brick.<+>) Brick.emptyWidget) (gridMap cellToWidget (Data.List.transpose (grid r emptyCell))))
        where emptyCell = Cell ' ' Renderer.Default []

-- renderStack :: [StackInstructions Marking] -> [Widget Name]
-- renderStack lines = renderStack' lines []
--  where
--  renderStack' [] _                  = [str ""]
--  renderStack' (Push m:ts) s         = renderStack' ts (m:s)
--  renderStack' (Pop:ts) (_:ss)       = renderStack' ts ss
--  renderStack' (Pop:_) []            = []
--  renderStack' (NewLine n:ts) s      = str " ":str (replicate n ' ') Brick.<+> a:as
--    where a:as = renderStack' ts s
--  renderStack' (StackLiteral l:ts) s = modifyDefAttr (\x -> x <> attributes s) (str l) Brick.<+> a:as
--    where a:as = renderStack' ts s
--          attributes []             = currentAttr
--          -- attributes (Highlight:ss) = defAttr `withForeColor` brightWhite `withBackColor` brightBlack
--          attributes (Location _:ss) = attributes ss
--          -- attributes (Highlight:ss) = defAttr `withForeColor` (rgbColor 240 240 240) `withBackColor` brightBlack
--          attributes (Highlight:ss) = defAttr `withStyle` standout
--          attributes (Yellow:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` yellow
--          attributes (Green:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` green) `withStyle` dim
--          attributes (Red:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` red) `withStyle` bold
--          attributes (Blue:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` blue) `withStyle` underline
--          attributes (Magenta:ss) = (if highlighted ss then attributes ss else attributes ss `withForeColor` magenta) `withStyle` italic
--          attributes (Cyan:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` cyan
--          attributes (White:ss) = if highlighted ss then attributes ss else attributes ss `withForeColor` white
--          highlighted (Highlight:_)  = True
--          highlighted []  = False
--          highlighted (_:xs) = highlighted xs
-- more colors:
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes-Color.html    
-- more info on vty styling (bold, underline, etc):
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes.html

-- renderDoc :: Doc Marking -> Widget Name
-- renderDoc d = Brick.Widget Brick.Fixed Brick.Fixed
--   (do ctx <- Brick.getContext
--       Brick.render $ vBox $ renderStack $ treeToStack $ treeForm $ layout (ctx^.Brick.availWidthL) d)

data Name = MainWindowName | PopupName deriving (Eq, Show)

instance Ord Name where
  MainWindowName <= PopupName = True
  PopupName <= MainWindowName = False
  _ <= _ = True

drawUI :: App Token -> [Widget Name]
drawUI a@(App t p' x p u _) = (case popupData a of
     Just (l, n) -> [popup (renderContextAtPoint (path a) (tree a)) (position a) (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1))]
     _ -> []) ++ [drawMainWindow (path a) (tree a,[] :: [String])]

drawMainWindow :: Render a => Path -> a -> Widget Name
drawMainWindow p = Brick.reportExtent MainWindowName . renderWithPathHighlighted p 100

popup :: [String] -> (Int, Int) -> L.List Name Token -> Widget Name
popup renderContext (x,y) l =  Brick.translateBy (Brick.Location (x-1,y+1)) $ B.border $ hLimit 50 $ box
    where
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement :: Bool -> Token -> Widget Name
        listDrawElement selected a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                if selected
                                then (renderAsWidget (a,renderContext)) Brick.<+> (modifyDefAttr (const (defAttr `withStyle` standout)) (Brick.fill ' '))
                                else (renderAsWidget (a,renderContext)) Brick.<+> Brick.fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l
