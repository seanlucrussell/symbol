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

import System.Environment
import qualified Brick.AttrMap as A
import qualified Brick.Main
import qualified Brick.Widgets.List as L
import qualified Control.Monad.State as S

-- how we gonna do cool positional stuff:
-- have a function
-- Tree -> Map (Int,Int) Path 
-- (which would have to be renderer dependent) that produces a map mapping xy
-- coordinates on the screen to the path to the term at those coordinates

extractInput :: Key -> ApplicationInput
extractInput (KChar '\t') = Tab
extractInput KBackTab     = BackTab
extractInput (KChar c) = Key c
extractInput KEnter    = Enter
extractInput KBS       = Del
extractInput KUp       = UpArrow
extractInput KDown     = DownArrow
extractInput KLeft     = LeftArrow
extractInput KRight     = RightArrow
extractInput KEsc     = Esc
extractInput _             = Other

serializeToFile :: String -> STLC.Application.App Token -> IO ()
serializeToFile f a = writeFile f (serialize (tree a, path a))

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

-- use this when file doesn't exist already
-- emptyState :: (STLC.Application.App Token)
-- emptyState = StateData (initialSymbolTable, initialZipper, (0,0), Nothing) (Just homeHandler) state

theMap :: A.AttrMap
theMap = A.attrMap defAttr
    [ (L.listSelectedAttr, bg brightBlack)
    ]

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
          if Prelude.length args /= 1
          then putStrLn "Please supply 1 file name"
          else do contents <- readFile (args !! 0)
                  case stateDataFromString contents of
                       Just state -> defaultMain (theApp (args !! 0)) state >> return ()
                       Nothing -> putStrLn "Could not parse file"
