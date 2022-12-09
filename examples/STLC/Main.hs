{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Brick (continueWithoutRedraw)
import qualified Brick as B
import qualified Brick.AttrMap as A
import qualified Brick.Main
import qualified Brick.Widgets.List as L
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (void, when)
import qualified Control.Monad.State as S
import qualified Graphics.Vty as V
import STLC.Application
  ( App (App, output, tree),
    ApplicationInput (..),
    ApplicationOutput (Continue, Save, Terminate),
    homeHandler,
    stateHandler,
  )
import STLC.BrickRenderer (Name (MainWindowName), drawUI)
import STLC.Data (Token (Assignment, EndOfProgram, Name, Unknown))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

extractInput :: V.Key -> ApplicationInput
extractInput (V.KChar '\t') = Tab
extractInput V.KBackTab = BackTab
extractInput (V.KChar c) = Key c
extractInput V.KEnter = Enter
extractInput V.KBS = Del
extractInput V.KUp = UpArrow
extractInput V.KDown = DownArrow
extractInput V.KLeft = LeftArrow
extractInput V.KRight = RightArrow
extractInput V.KEsc = Esc
extractInput _ = Other

appEvent :: String -> App Token -> B.BrickEvent Name e -> B.EventM Name (App Token) ()
appEvent file d (B.VtyEvent (V.EvKey e [])) =
  do
    mExtent <- Brick.Main.lookupExtent MainWindowName
    case mExtent of
      Nothing -> error "Couldn't find main window display widget!"
      Just extent ->
        let nextState = stateHandler (extractInput e, fst (B.extentSize extent)) d
         in case output nextState of
              Terminate -> Brick.Main.halt
              Save -> S.liftIO (serializeToFile file nextState) >> S.put nextState
              Continue -> S.put nextState
appEvent _ _ _ = continueWithoutRedraw

stateDataFromString :: String -> Maybe (App Token)
stateDataFromString s = do
  program <- readMaybe s
  let state = App program [0] Nothing Continue homeHandler state
   in return state

serializeToFile :: String -> App Token -> IO ()
serializeToFile f a = writeFile f (show (tree a))

emptyState :: App Token
emptyState =
  let state =
        App
          (Assignment (Name Nothing) Unknown Unknown EndOfProgram)
          [0]
          Nothing
          Continue
          homeHandler
          state
   in state

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, B.bg V.brightBlack)
    ]

theApp :: String -> App Token -> Brick.Main.App (App Token) e Name
theApp file initialApp =
  Brick.Main.App
    { B.appDraw = drawUI,
      B.appChooseCursor = Brick.Main.showFirstCursor,
      B.appHandleEvent = appEvent file initialApp,
      B.appStartEvent = return (),
      B.appAttrMap = const theMap
    }

main :: IO ()
main = do
  args <- getArgs
  when (Prelude.length args /= 1) (putStrLn "Please supply 1 file name" >> exitFailure)
  let fileName = head args
  initialState <-
    ( do
        contents <- readFile fileName
        maybe (putStrLn "Could not parse file" >> exitFailure) return (stateDataFromString contents)
      )
      <|> return emptyState
  void (Brick.Main.defaultMain (theApp fileName initialState) initialState)
