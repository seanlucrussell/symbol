{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module STLC.BrickRenderer
  ( drawMainWindow
  , Name (MainWindowName, PopupName)
  , drawUI
  ) where

import AST
import Renderer
import STLC.Application
import STLC.Data
import STLC.Renderer

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr, hLimit, str, vBox, vLimit)
import Data.Map
import Data.Maybe
import Data.List
import Graphics.Vty
import qualified Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Lens.Micro

xMin :: Map (Int,Int) t -> Int
xMin = minimum . fmap fst . keys

xMax :: Map (Int,Int) t -> Int
xMax = maximum . fmap fst . keys

yMin :: Map (Int,Int) t -> Int
yMin = minimum . fmap snd . keys

yMax :: Map (Int,Int) t -> Int
yMax = maximum . fmap snd . keys

-- renderAsWidget :: Render a => a -> Widget Name
-- renderAsWidget = renderingToWidget . render 100

renderAsWidget :: Render a => a -> Widget Name
renderAsWidget t = Brick.Widget Brick.Fixed  Brick.Fixed
        (do ctx <- Brick.getContext
            Brick.render $ renderingToWidget (render (ctx^.Brick.availWidthL) t))

renderAsWidgetWithHighlight :: Render a => Path -> a -> Widget Name
renderAsWidgetWithHighlight p t = Brick.Widget Brick.Fixed  Brick.Fixed
        (do ctx <- Brick.getContext
            Brick.render $ renderingToWidget (highlightAtPath p (render (ctx^.Brick.availWidthL) t)))

renderForPopup :: Render a => a -> Widget Name
renderForPopup x = renderingToWidget (Data.Map.map (\n -> n {style = Highlight}) (render 100 x))

-- takes map over 2d coordinates and a default value and returns a 2d grid
grid :: Map (Int,Int) t -> t -> [[t]]
grid m d = [ [fromMaybe d (Data.Map.lookup (x,y) m) | y <- [yMin m..yMax m] ] | x <- [xMin m..xMax m]]

cellToWidget :: Cell -> Widget Name
cellToWidget (Cell c Highlight _) = modifyDefAttr (const $  defAttr `withStyle` standout) (str [c])
cellToWidget (Cell c Yellow _) = modifyDefAttr (const $  defAttr `withForeColor` yellow) (str [c])
cellToWidget (Cell c White _) = modifyDefAttr (const $  defAttr `withForeColor` white) (str [c])
cellToWidget (Cell c Green _) = modifyDefAttr (const $  defAttr `withForeColor` green) (str [c])
cellToWidget (Cell c Blue _) = modifyDefAttr (const $  defAttr `withForeColor` blue) (str [c])
cellToWidget (Cell c Magenta _) = modifyDefAttr (const $  defAttr `withForeColor` magenta) (str [c])
cellToWidget (Cell c Cyan _) = modifyDefAttr (const $  defAttr `withForeColor` cyan) (str [c])
cellToWidget (Cell c Red _) = modifyDefAttr (const $  defAttr `withForeColor` red) (str [c])
cellToWidget (Cell c Renderer.Default _) = str [c]

gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap = fmap . fmap

renderingToWidget :: Rendering -> Widget Name
renderingToWidget r = vBox (fmap (Prelude.foldr (Brick.<+>) Brick.emptyWidget) (gridMap cellToWidget (Data.List.transpose (grid r emptyCell))))
        where emptyCell = Cell ' ' Renderer.Default []

-- more colors:
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes-Color.html    
-- more info on vty styling (bold, underline, etc):
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes.html

data Name = MainWindowName | PopupName deriving (Eq, Show)

instance Ord Name where
  MainWindowName <= PopupName = True
  PopupName <= MainWindowName = False
  _ <= _ = True

drawUI :: App Token -> [Widget Name]
drawUI a = (case popupData a of
     Just (l, n) -> [popup (renderContextAtPoint (path a) (tree a)) (position a) (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1))]
     _ -> []) ++ [drawMainWindow (path a) (tree a,[] :: [String])]

drawMainWindow :: Render a => Path -> a -> Widget Name
drawMainWindow p = Brick.reportExtent MainWindowName . renderAsWidgetWithHighlight p

popup :: [String] -> (Int, Int) -> L.List Name Token -> Widget Name
popup renderContext (x,y) l =  Brick.translateBy (Brick.Location (x-1,y+1)) $ B.border $ hLimit 50 box
    where
        listDrawElement :: Bool -> Token -> Widget Name
        listDrawElement selected a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                if selected 
                                then renderForPopup (a,renderContext) Brick.<+> modifyDefAttr (const $  defAttr `withStyle` standout) (Brick.fill ' ')
                                else renderAsWidget (a,renderContext) Brick.<+> Brick.fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l
