{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Data.Text

import SymbolData
import Renderer
import Transformations

import Graphics.Vty
import Brick

import Text.Read

import Data.Monoid
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

-- todo:
--   - [Done] get highlighting right
--   - [Done] handle multi-character input (for handling numbers / text)
--    - [Done] show input while typing
--   - [Done] pretty-print to width of screen
--   - [Done] syntax highlighting : different colors for string literals, numeric
--     literals, types, builtins, variables, holes
--   - swap between renderers
--   - arrow key navigation (might be dependent on prettyprint/screen width, so
--     this could be a bit complex)
--   - undo/redo
--   - cut/paste
--   - save to/load from file
--   - [Done] deal with context dependent editing: e.g. different keybindings when
--     cursor is on a value than when cursor is on a statement


popup :: (Renderer r, Prettify a) => r -> L.List () a -> Widget ()
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
        listDrawElement sel a = C.hCenter $ hLimit 35 $ vLimit 1 $
                                str "    " <+> (renderDoc (prettify renderer a)) <+> fill ' '
        box = hLimit 35 $
              vLimit 15 $
              L.renderList listDrawElement True l

drawUI :: State -> [Widget ()]
drawUI (State renderer (SelectingValue l) z) = [popup renderer l, zipperToWidget renderer z]
drawUI (State renderer (SelectingStatement l) z) = [popup renderer l, zipperToWidget renderer z]
drawUI (State renderer (SelectingType l) z) = [popup renderer l, zipperToWidget renderer z]
drawUI (State renderer _ z) = [zipperToWidget renderer z]

data ReaderState = AddingText String
                 | AddingName String
                 | AddingVar String
                 | AddingInt String
                 | SelectingValue (L.List () Value)
                 | SelectingStatement (L.List () Statement)
                 | SelectingType (L.List () Type)
                 | NotReading
data State = forall r. Renderer r => State r ReaderState Zipper

events :: Key -> Zipper -> Zipper
-- editing statements
events (KChar 'r')  z@(ZipperSt _ _) = replaceWithReturn z
events (KChar 'i')  z@(ZipperSt _ _) = replaceWithIf z
events (KChar 'b')  z@(ZipperSt _ _) = replaceWithBlock z
events (KChar 'a')  z@(ZipperSt _ _) = replaceWithAssignment z
-- editing types
events (KChar 'I')  z@(ZipperTyp _ _) = replaceWithIntType z
events (KChar 'F')  z@(ZipperTyp _ _) = replaceWithFnType z
events (KChar 'S')  z@(ZipperTyp _ _) = replaceWithStrType z
events (KChar 'B')  z@(ZipperTyp _ _) = replaceWithBoolType z
-- editing values
events (KChar 'T')  z@(ZipperVal _ _) = replaceWithTrue z
events (KChar 'F')  z@(ZipperVal _ _) = replaceWithFalse z
events (KChar 'i')  z@(ZipperVal _ _) = replaceWithInt 7 z
events (KChar 'c')  z@(ZipperVal _ _) = replaceWithCall z
events (KChar 'f')  z@(ZipperVal _ _) = replaceWithFunction z
events (KChar '=')  z@(ZipperVal _ _) = replaceWithOp Equal z
events (KChar '<')  z@(ZipperVal _ _) = replaceWithOp LessThan z
events (KChar '>')  z@(ZipperVal _ _) = replaceWithOp GreaterThan z
events (KChar '+')  z@(ZipperVal _ _) = replaceWithOp Add z
events (KChar '*')  z@(ZipperVal _ _) = replaceWithOp Multiply z
events (KChar '%')  z@(ZipperVal _ _) = replaceWithOp Mod z
events (KChar '&')  z@(ZipperVal _ _) = replaceWithOp And z
events (KChar '|')  z@(ZipperVal _ _) = replaceWithOp Or z
-- context independent commands
events (KChar 'x')  z = deleteAtCursor z
events (KChar 'n')  z = nextHole z
events (KChar 'N')  z = previousHole z
events (KChar 'O')  z = insertBefore z
events (KChar 'o')  z = insertAfter z
events (KChar ' ')  z = goup z
events (KChar '?')  z = blank z
events (KChar '\t') z = selectNext z
events KBackTab     z = selectPrev z
events KEnter       z = selectFirst z
events  _           z = z

exit renderer = State renderer NotReading . selectNext
delLast renderer f r c t z = State renderer (f (Prelude.init t)) (r (c (Prelude.init t)) z)
delWhenEmpty renderer f r z = State renderer (f "") (r z)
addChar renderer f r t z c = State renderer (f (t ++ [c])) (r (pack (t ++ [c])) z)

readingAssistant renderer f r _ t   z (KChar c) = addChar renderer f r t z c
readingAssistant renderer f r d []  z KBS       = delWhenEmpty renderer f (r d) z
readingAssistant renderer f r d [_] z KBS       = delWhenEmpty renderer f (r d) z
readingAssistant renderer f r _ t   z KBS       = delLast renderer f r pack t z
readingAssistant renderer _ _ _ _   z KEnter    = exit renderer z
readingAssistant renderer _ _ _ _   z _         = exit renderer z

handleInt :: Renderer r => r -> String -> Zipper -> Key -> State
handleInt renderer t z (KChar c) = case maybeNum of 
                               Just n -> State renderer (AddingInt (t ++ [c])) (replaceWithInt n z)
                               Nothing -> State renderer (AddingInt t) z
  where
    maybeNum = readMaybe (t ++ [c]) :: Maybe Integer
handleInt renderer [] z KBS = delWhenEmpty renderer AddingInt (replaceWithInt 0) z
handleInt renderer [_] z KBS = delWhenEmpty renderer AddingInt (replaceWithInt 0) z
handleInt renderer t z KBS = delLast renderer AddingInt replaceWithInt (read :: String -> Integer) t z
handleInt renderer _ z KEnter = exit renderer z
handleInt renderer _ z _ = exit renderer z

handleName :: Renderer r => r -> String -> Zipper -> Key -> State
handleName r = readingAssistant r AddingName replaceWithName " "

handleText :: Renderer r => r -> String -> Zipper -> Key -> State
handleText r = readingAssistant r AddingText replaceWithString ""

handleVar :: Renderer r => r -> String -> Zipper -> Key -> State
handleVar r = readingAssistant r AddingVar replaceWithVariable " "

appEvent :: State -> BrickEvent n e -> EventM () (Next State)
appEvent (State _ a z) (VtyEvent (EvKey (KChar '[') [])) = continue $ State LispRenderer a z
appEvent (State _ a z) (VtyEvent (EvKey (KChar ']') [])) = continue $ State SymbolRenderer a z
appEvent (State _ a z) (VtyEvent (EvKey (KChar '\\') [])) = continue $ State PythonRenderer a z
appEvent (State renderer NotReading z@(ZipperNam _ _)) (VtyEvent (EvKey (KChar 't') [])) = continue (State renderer (AddingName "") (replaceWithName " " z))
appEvent r@(State renderer NotReading z@(ZipperVal _ _)) (VtyEvent (EvKey (KChar 't') [])) = if valueTypeChecks z (StringLiteral "") then continue (State renderer (AddingText "") (replaceWithString "" z)) else continue r
-- appEvent (State renderer NotReading z@(ZipperVal _ _)) (VtyEvent (EvKey (KChar 'v') [])) = continue (State renderer (AddingVar "") (replaceWithVariable " " z))
appEvent r@(State renderer NotReading z@(ZipperVal _ _)) (VtyEvent (EvKey (KChar 'i') [])) = if valueTypeChecks z (IntLiteral 0) then continue (State renderer (AddingInt "") (replaceWithInt 0 z)) else continue r
appEvent (State renderer (AddingName t) z) (VtyEvent (EvKey e [])) = continue (handleName renderer t z e)
appEvent (State renderer (AddingText t) z) (VtyEvent (EvKey e [])) = continue (handleText renderer t z e)
appEvent (State renderer (AddingVar t) z) (VtyEvent (EvKey e [])) = continue (handleVar renderer t z e)
appEvent (State renderer (AddingInt t) z) (VtyEvent (EvKey e [])) = continue (handleInt renderer t z e)
appEvent s (VtyEvent (EvKey KEsc [])) = halt s

appEvent (State renderer NotReading z@(ZipperTyp _ _)) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer (SelectingType (L.list () (Vec.fromList (possibleTypes z)) 1)) z)
appEvent (State renderer (SelectingType l) z) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer NotReading z)
appEvent (State renderer (SelectingType l) z) (VtyEvent (EvKey KEnter [])) = continue (State renderer NotReading z')
  where z' = case L.listSelectedElement l of
                  Just (_,v) -> replaceWithType v z
                  Nothing -> z
appEvent (State renderer (SelectingType l) z) (VtyEvent ev) =
    do l' <- (L.handleListEventVi L.handleListEvent) ev l
       continue (State renderer (SelectingType l') z)

appEvent (State renderer NotReading z@(ZipperSt _ _)) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer (SelectingStatement (L.list () (Vec.fromList (possibleStatements z)) 1)) z)
appEvent (State renderer (SelectingStatement l) z) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer NotReading z)
appEvent (State renderer (SelectingStatement l) z) (VtyEvent (EvKey KEnter [])) = continue (State renderer NotReading z')
  where z' = case L.listSelectedElement l of
                  Just (_,v) -> replaceWithStatement v z
                  Nothing -> z
appEvent (State renderer (SelectingStatement l) z) (VtyEvent ev) =
    do l' <- (L.handleListEventVi L.handleListEvent) ev l
       continue (State renderer (SelectingStatement l') z)

appEvent (State renderer NotReading z@(ZipperVal _ _)) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer (SelectingValue (L.list () (Vec.fromList (possibleValues z)) 1)) z)
appEvent (State renderer (SelectingValue l) z@(ZipperVal _ _)) (VtyEvent (EvKey (KChar 'p') [])) = continue (State renderer NotReading z)
appEvent (State renderer (SelectingValue l) z@(ZipperVal _ _)) (VtyEvent (EvKey KEnter [])) = continue (State renderer NotReading z')
  where z' = case L.listSelectedElement l of
                  Just (_,v) -> replaceWithValue v z
                  Nothing -> z
appEvent (State renderer (SelectingValue l) z@(ZipperVal _ _)) (VtyEvent ev) =
    do l' <- (L.handleListEventVi L.handleListEvent) ev l
       continue (State renderer (SelectingValue l') z)

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
initialState = State LispRenderer NotReading c

theApp :: App State e ()
theApp =
      App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main = defaultMain theApp initialState
