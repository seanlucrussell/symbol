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
import BrickRenderer
import SymbolSerialize

import Data.Text
import Graphics.Vty
import Brick
import Text.Read

import System.Environment
import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Main
import qualified Brick.Widgets.List as L
import qualified Control.Monad.State as S

-- how we gonna do cool positional stuff:
-- have a function
-- Tree -> Map (Int,Int) Path 
-- (which would have to be renderer dependent) that produces a map mapping xy
-- coordinates on the screen to the path to the term at those coordinates

appEvent :: StateData -> BrickEvent n e -> EventM Name (Next StateData)
appEvent d (VtyEvent (EvKey e [] )) =
             do mExtent <- Brick.Main.lookupExtent ZipperName
                case mExtent of
                  Nothing -> error "Couldn't find main Zipper display widget!"
                  Just (Extent _ _ (width, _) _) ->
                        case nextState width of 
                                (StateData (_, _, _, _) Nothing _) -> halt d
                                _ -> continue (nextState width)
  where nextState n = S.execState (stateHandler (e,n)) d -- this needs to extract the screen width
appEvent d _ = continue d

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

stateDataFromString :: String -> Maybe StateData
stateDataFromString s = do (symbolTable, program, path) <- deserialize s
                           let state = StateData (symbolTable, (program, path), (0,0), Nothing) (Just homeHandler) state in
                               return state
        

theMap :: A.AttrMap
theMap = A.attrMap defAttr
    [ (L.listSelectedAttr, bg brightBlack)
    ]

theApp :: App StateData e Name
theApp =
      App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }


-- main :: IO StateData
main = do args <- getArgs
          if Prelude.length args /= 1
          then putStrLn "Please supply 1 file name"
          else do contents <- readFile (args !! 0)
                  case stateDataFromString contents of
                       Just state -> defaultMain theApp state >> putStrLn ""
                       Nothing -> putStrLn "Could not parse file"
          --         -- let newContents = map toUpper contents
          --         -- putStr newContents
          --         -- writeFile (args !! 0) newContents
          --         defaultMain theApp initialState
