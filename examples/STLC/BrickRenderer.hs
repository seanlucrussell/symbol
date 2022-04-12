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
drawUI a = [drawDropdown a, drawMainWindow a]

currentLine :: Int -> App Token -> Int
currentLine n = snd . getPosition n
lineCount :: Int -> App Token -> Int
lineCount n a = maximum (fmap snd (keys (renderApp n a)))

drawStatusBar :: App Token -> Widget Name
drawStatusBar a = modifyDefAttr (const $  defAttr `withStyle` standout) $ vLimit 1 $ Brick.hBox
                [ Brick.Widget Brick.Fixed Brick.Fixed lineNumWidget
                , Brick.fill ' '
                , str " Press 'h' for help "
                ]
            where lineNumWidget = do ctx <- Brick.getContext
                                     let w = Brick.availWidth ctx
                                     Brick.render $ Brick.str ("Line " ++ show (currentLine w a + 1) ++ " of " ++ show (lineCount w a + 1)) 

drawMainWindow :: App Token -> Widget Name
drawMainWindow a = Brick.reportExtent MainWindowName (renderAsWidgetWithHighlight (path a) (tree a, [] :: [String])) Brick.<=> Brick.fill ' ' Brick.<=> drawStatusBar a

drawDropdown :: App Token -> Widget Name
drawDropdown a = maybe Brick.emptyWidget d (popupData a)
         where d (l,n) = Brick.Widget Brick.Fixed  Brick.Fixed
                     (do ctx <- Brick.getContext
                         Brick.render $ popup (renderContextAtPoint (path a) (tree a)) (getPosition (ctx^.Brick.availWidthL) a) (L.listMoveBy n (L.list PopupName (Vec.fromList l) 1)))

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
