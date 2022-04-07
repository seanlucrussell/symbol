{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import STLC.Data
import STLC.Application
import STLC.BrickRenderer
import STLC.Serialize

import qualified Graphics.Vty as V
import qualified Brick as B

import System.Environment
import System.Exit
import Control.Applicative
import qualified Brick.AttrMap as A
import qualified Brick.Main
import qualified Brick.Widgets.List as L
import qualified Control.Monad.State as S
import Control.Monad

extractInput :: V.Key -> ApplicationInput
extractInput (V.KChar '\t') = Tab
extractInput V.KBackTab     = BackTab
extractInput (V.KChar c)    = Key c
extractInput V.KEnter       = Enter
extractInput V.KBS          = Del
extractInput V.KUp          = UpArrow
extractInput V.KDown        = DownArrow
extractInput V.KLeft        = LeftArrow
extractInput V.KRight       = RightArrow
extractInput V.KEsc         = Esc
extractInput _              = Other

appEvent :: String -> App Token -> B.BrickEvent n e -> B.EventM Name (B.Next (App Token))
appEvent file d (B.VtyEvent (V.EvKey e [] )) =
             do mExtent <- Brick.Main.lookupExtent MainWindowName
                case mExtent of
                  Nothing -> error "Couldn't find main window display widget!"
                  Just extent ->
                        let nextState = stateHandler (extractInput e, fst (B.extentSize extent)) d in
                        case output nextState of
                           Terminate -> Brick.Main.halt nextState
                           Save      -> S.liftIO (serializeToFile file nextState) >> Brick.Main.continue nextState
                           Continue  -> Brick.Main.continue nextState
appEvent _ d _ = Brick.Main.continue d

stateDataFromString :: String -> Maybe (App Token)
stateDataFromString s = do (program, p) <- deserialize s
                           let state = App program p Nothing Continue homeHandler state in
                               return state

serializeToFile :: String -> App Token -> IO ()
serializeToFile f a = writeFile f (serialize (tree a, path a))

emptyState :: App Token
emptyState = let state = App (Assignment (Name Nothing) Unknown Unknown EndOfProgram)
                             [0]
                             Nothing
                             Continue
                             homeHandler
                             state in state

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [ (L.listSelectedAttr, B.bg V.brightBlack) ]

theApp :: String -> Brick.Main.App (App Token) e Name
theApp file = Brick.Main.App
          { B.appDraw = drawUI
          , B.appChooseCursor = Brick.Main.showFirstCursor
          , B.appHandleEvent = appEvent file
          , B.appStartEvent = return
          , B.appAttrMap = const theMap
          }

main :: IO ()
main = do args <- getArgs
          when (Prelude.length args /= 1) (putStrLn "Please supply 1 file name" >> exitFailure)
          let fileName = head args
          initialState <- (do contents <- readFile fileName
                              maybe (putStrLn "Could not parse file" >> exitFailure) return (stateDataFromString contents))
                           <|> return emptyState
          void (Brick.Main.defaultMain (theApp fileName) initialState)
