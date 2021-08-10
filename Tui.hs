{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where


import AST
import SymbolData
import SymbolRenderer
import Movements
import Renderer
import Transformations
import Application

import Data.Text
import Graphics.Vty
import Brick

import Text.Read

import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import qualified Control.Monad.State as S

appEvent :: AppStateData -> BrickEvent n e -> EventM () (Next AppStateData)
appEvent d (VtyEvent (EvKey e [] )) = case nextState of 
                     (_, _, Exiting) -> halt d
                     _ -> continue nextState
  where nextState = S.execState (stateHandler e) d
appEvent d _ = continue d

drawUI :: AppStateData -> [Widget ()]
drawUI (s, z, u) = (case u of
     SelectingTerm l n -> [popup s (L.listMoveBy n (L.list () (Vec.fromList l) 1))]
     _ -> []) ++ [zipperToWidget s z]

popup :: SymbolTable -> L.List () (Term Token) -> Widget ()
popup s l = C.centerLayer $ B.borderWithLabel label $ hLimit 50 $ vBox
                              [ str " "
                              , C.hCenter box
                              , str " "
                              , C.hCenter (str "Use arrow keys to move up/down.")
                              , C.hCenter (str "Press p to exit.")
                              ]
    where
        label = str "Item " <+> str cur <+> str " of " <+> str total
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i  -> show (i + 1)
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement _ a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                str "    " <+> (renderDoc (renderTerm s NoRenderContext a)) <+> fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap defAttr
    [ (L.listSelectedAttr, bg brightBlack)
    ]

theApp :: App AppStateData e ()
theApp =
      App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO AppStateData
main = defaultMain theApp initialState
