{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

-- need to fix bug with text insertion still

import Data.Text

import SymbolData
import Movements
import Renderer
import Transformations

import Graphics.Vty
import Brick

import Text.Read

import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

popup :: Renderer -> L.List () Term -> Widget ()
popup renderer l = C.centerLayer $ B.borderWithLabel label $ hLimit 50 $ vBox
                              [ str " "
                              , C.hCenter box
                              , str " "
                              , C.hCenter (str "Press j/k to go down/up.")
                              , C.hCenter (str "Press p to exit.")
                              ]
    where
        label = str "Item " <+> str cur <+> str " of " <+> str total
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i  -> show (i + 1)
        total = show (Vec.length (l^.(L.listElementsL)))
        listDrawElement _ a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                str "    " <+> (renderDoc (renderTerm renderer a)) <+> fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l

drawUI :: State -> [Widget ()]
drawUI (State renderer (SelectingTerm l) z) = [popup renderer l, zipperToWidget renderer z]
drawUI (State renderer _ z) = [zipperToWidget renderer z]

data ReaderState = AddingText String
                 | AddingName String
                 | AddingVar String
                 | AddingInt String
                 | SelectingTerm (L.List () Term)
                 | NotReading
data State = State Renderer ReaderState Zipper

events :: Key -> Zipper -> Zipper
events (KChar 'n')  z = nextHole z
events (KChar 'N')  z = previousHole z
events (KChar 'O')  z = insertBefore z
events (KChar 'o')  z = insertAfter z
events (KChar 'j')  z = selectFirst z
events (KChar 'l')  z = selectNext z
events (KChar 'h')  z = selectPrev z
events (KChar 'k')  z = goup z
events  _           z = z

exit :: Renderer -> Zipper -> State
exit renderer = State renderer NotReading . nextHole
delLast :: Renderer -> ([a] -> ReaderState) -> (t1 -> t2 -> Zipper) -> ([a] -> t1) -> [a] -> t2 -> State
delLast renderer f r c t z = State renderer (f (Prelude.init t)) (r (c (Prelude.init t)) z)
delWhenEmpty :: Renderer -> (String -> ReaderState) -> (Zipper -> Zipper) -> Zipper -> State
delWhenEmpty renderer f r z = State renderer (f "") (r z)
addChar :: Renderer -> (String -> ReaderState) -> (Text -> t -> Zipper) -> String -> t -> Char -> State
addChar renderer f r t z c = State renderer (f (t ++ [c])) (r (pack (t ++ [c])) z)

readingAssistant :: Renderer -> (String -> ReaderState) -> (Text -> Zipper -> Zipper) -> Text -> String -> Zipper -> Key -> State
readingAssistant renderer f r _ t   z (KChar c) = addChar renderer f r t z c
readingAssistant renderer f r d []  z KBS       = delWhenEmpty renderer f (r d) z
readingAssistant renderer f r d [_] z KBS       = delWhenEmpty renderer f (r d) z
readingAssistant renderer f r _ t   z KBS       = delLast renderer f r pack t z
readingAssistant renderer _ _ _ _   z KEnter    = exit renderer z
readingAssistant renderer _ _ _ _   z _         = exit renderer z

-- handleInt :: Renderer -> String -> Zipper -> Key -> State
-- handleInt renderer t z (KChar c) = case maybeNum of 
--                                Just n -> State renderer (AddingInt (t ++ [c])) (replaceWithInt n z)
--                                Nothing -> State renderer (AddingInt t) z
--   where
--     maybeNum = readMaybe (t ++ [c]) :: Maybe Integer
-- handleInt renderer [] z KBS = delWhenEmpty renderer AddingInt (replaceWithInt 0) z
-- handleInt renderer [_] z KBS = delWhenEmpty renderer AddingInt (replaceWithInt 0) z
-- handleInt renderer t z KBS = delLast renderer AddingInt replaceWithInt (read :: String -> Integer) t z
-- handleInt renderer _ z KEnter = exit renderer z
-- handleInt renderer _ z _ = exit renderer z

handleName :: Renderer -> String -> Zipper -> Key -> State
handleName r = readingAssistant r AddingName (\x -> replaceWithTerm (IdentifierTerm x)) " "

-- handleText :: Renderer -> String -> Zipper -> Key -> State
-- handleText r = readingAssistant r AddingText replaceWithString ""
-- 
-- handleVar :: Renderer -> String -> Zipper -> Key -> State
-- handleVar r = readingAssistant r AddingVar replaceWithVariable " "

appEvent :: State -> BrickEvent n e -> EventM () (Next State)
-- -- appEvent (State _ a z) (VtyEvent (EvKey (KChar '[') [])) = continue $ State LispRenderer a z
-- appEvent (State _ a z) (VtyEvent (EvKey (KChar ']') [])) = continue $ State SymbolRenderer a z
-- -- appEvent (State _ a z) (VtyEvent (EvKey (KChar '\\') [])) = continue $ State PythonRenderer a z
appEvent (State renderer NotReading z) (VtyEvent (EvKey (KChar 't') [])) = continue (State renderer (AddingName "") (replaceWithTerm (IdentifierTerm " ") z))
-- appEvent r@(State renderer NotReading z) (VtyEvent (EvKey (KChar 't') [])) = if valueTypeChecks z (StringLiteral "") then continue (State renderer (AddingText "") (replaceWithString "" z)) else continue r
-- appEvent r@(State renderer NotReading z) (VtyEvent (EvKey (KChar 'i') [])) = if valueTypeChecks z (IntLiteral 0) then continue (State renderer (AddingInt "") (replaceWithInt 0 z)) else continue r
appEvent (State renderer (AddingName t) z) (VtyEvent (EvKey e [])) = continue (handleName renderer t z e)
-- appEvent (State renderer (AddingText t) z) (VtyEvent (EvKey e [])) = continue (handleText renderer t z e)
-- appEvent (State renderer (AddingVar t) z) (VtyEvent (EvKey e [])) = continue (handleVar renderer t z e)
-- appEvent (State renderer (AddingInt t) z) (VtyEvent (EvKey e [])) = continue (handleInt renderer t z e)
appEvent s (VtyEvent (EvKey KEsc [])) = halt s
-- 

appEvent (State renderer NotReading z) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer (SelectingTerm (L.list () (Vec.fromList (possibleTerms z)) 1)) z)
appEvent (State renderer (SelectingTerm _) z) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer NotReading z)
appEvent (State renderer (SelectingTerm l) z) (VtyEvent (EvKey KEnter [])) = continue (State renderer NotReading z')
  where z' = case L.listSelectedElement l of
                  Just (_,v) -> nextHole $ replaceWithTerm v z
                  Nothing -> z
appEvent (State renderer (SelectingTerm l) z) (VtyEvent ev) =
    do l' <- (L.handleListEventVi L.handleListEvent) ev l
       continue (State renderer (SelectingTerm l') z)

appEvent (State renderer NotReading z) (VtyEvent (EvKey k [])) = continue (State renderer NotReading (events k z))
appEvent s _                               = continue s


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedAttr,    bg V.brightBlack)
    ]
--     [ (L.listAttr,            V.white `on` V.blue)
--     , (L.listSelectedAttr,    V.blue `on` V.white)
--     , (customAttr,            fg V.cyan)
--     ]


initialState :: State
-- initialState = blankZipper
initialState = State basicRenderer NotReading z

theApp :: App State e ()
theApp =
      App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO State
main = defaultMain theApp initialState
