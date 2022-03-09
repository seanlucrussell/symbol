{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import STLC.Data
import STLC.Application
import STLC.BrickRenderer
import STLC.Serialize

import Graphics.Vty
import Brick

import System.Directory
import System.Environment
import System.Exit
import qualified Brick.AttrMap as A
import qualified Brick.Main
import qualified Brick.Widgets.List as L
import qualified Control.Monad.State as S
import Control.Monad

extractInput :: Key -> ApplicationInput
extractInput (KChar '\t') = Tab
extractInput KBackTab     = BackTab
extractInput (KChar c)    = Key c
extractInput KEnter       = Enter
extractInput KBS          = Del
extractInput KUp          = UpArrow
extractInput KDown        = DownArrow
extractInput KLeft        = LeftArrow
extractInput KRight       = RightArrow
extractInput KEsc         = Esc
extractInput _            = Other

appEvent :: String -> STLC.Application.App Token -> BrickEvent n e -> EventM Name (Next (STLC.Application.App Token))
appEvent file d (VtyEvent (EvKey e [] )) =
             do mExtent <- Brick.Main.lookupExtent MainWindowName
                case mExtent of
                  Nothing -> error "Couldn't find main window display widget!"
                  Just extent ->
                        let nextState = stateHandler (extractInput e, fst (extentSize extent)) d in 
                        case output nextState of 
                           Terminate -> halt nextState
                           Save      -> S.liftIO (serializeToFile file nextState) >> continue nextState
                           Continue  -> continue nextState
appEvent _ d _ = continue d

stateDataFromString :: String -> Maybe (STLC.Application.App Token)
stateDataFromString s = do (program, p) <- deserialize s
                           let state = STLC.Application.App program p (0,0) Nothing Continue homeHandler state in
                               return state

serializeToFile :: String -> STLC.Application.App Token -> IO ()
serializeToFile f a = writeFile f (serialize (tree a, path a))

emptyState :: STLC.Application.App Token
emptyState = let state = STLC.Application.App 
                             (Assignment (Name Nothing) Unknown Unknown EndOfProgram)
                             [0]
                             (0,0)
                             Nothing
                             Continue
                             homeHandler
                             state in state

theMap :: A.AttrMap
theMap = A.attrMap defAttr [ (L.listSelectedAttr, bg brightBlack) ]

theApp :: String -> Brick.Main.App (STLC.Application.App Token) e Name
theApp file = Brick.Main.App
          { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent file
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do args <- getArgs
          when (Prelude.length args /= 1) (putStrLn "Please supply 1 file name" >> exitFailure)
          let fileName = args !! 0
          fileExists <- doesFileExist fileName
          initialState <- if fileExists
                          then do contents <- readFile fileName
                                  case stateDataFromString contents of
                                       Just state -> return state
                                       Nothing -> putStrLn "Could not parse file" >> exitFailure
                          else return emptyState
          defaultMain (theApp fileName) initialState >> return ()
