{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module STLC.Application
  ( stateHandler
  , homeHandler
  , App (..)
  , SymbolAppInput
  , ApplicationInput (..)
  ) where

import AST
import Movements
import Renderer
import Transformations
import Utilities

import STLC.Data
import STLC.Movements
import STLC.Renderer
import STLC.Transformations
import STLC.TypeChecker

import Control.Monad.State
import Data.Map
import Data.Text
import Data.Char

type SymbolAppInput = (ApplicationInput, Int)

data ApplicationInput = Key Char | Enter | Del | UpArrow | DownArrow | LeftArrow | RightArrow | Esc | Tab | BackTab |  Other

type PopupData a = ([a], Int)

data App a = App
                { tree :: a
                , path :: Path
                , position:: (Int, Int)
                , popupData :: Maybe (PopupData a)
                , next :: Maybe FoldMachine
                , prev :: App a
                }

type FoldMachine = SymbolAppInput -> State (App Token) ()

-- try to apply a movement
applyMovement :: Movement Token -> State (App Token) ()
applyMovement m = do term <- gets tree
                     applyToPath (try (m term))

-- try to apply a transformation, commiting if the transformation leaves the
-- data structures in a valid state
applyTransformation :: Transformation Token -> State (App Token) ()
applyTransformation t = do term <- gets tree
                           path <- gets path
                           case t term path of
                               Just (term', path') -> if validateProgram term' && validatePath term' path'
                                                      then do setTerm term'
                                                              setPath path'
                                                              commit
                                                      else return ()
                               Nothing -> return ()

-- like applyTransformation, but doesn't do checking, doesn't commit change.
-- good for intermediate transitions where the underlying datastructures aren't
-- sound
applyTransformationPartial :: Transformation Token -> State (App Token) ()
applyTransformationPartial t = do term <- gets tree
                                  path <- gets path
                                  case t term path of
                                      Just (term', path') -> if validatePath term' path'
                                                             then do setTerm term'
                                                                     setPath path'
                                                             else return ()
                                      Nothing -> return ()

-- cool idea: certain input events don't fully specify a new, valid state. e.g.
-- naming a variable. you could have invalid variable names while editing. so
-- what to do? take a page from the book on databases: transactions!
--
-- most state changes will be like this: given some input and a state, calculate
-- the next state. when the next state is a valid state, then great! but what do
-- we do in the case we don't have a valid state?
--
-- first, keep a complete copy of the current state that we can fall back into.
-- then, do your changes without verifying the correctness of the output.
-- finally, commit your changes. if the changes worked, then great! we exit the
-- transaction. if not, oh well. we revert to the previous state
--
-- so what do we need? some functions:
--
-- validate :: (App Token) -> Boolean
-- startTransaction :: State (App Token) ()
-- commit :: State (App Token) ()
--
-- and an extra element to the UIState data type
-- Transacting :: (App Token) -> UIState

-- ok so state data should maybe accept a Rendering callback or something like
-- that? something like
--   render :: (App Token) -> ExtraInfo -> a
-- ? or maybe we need to be clearer about what info there is that could be
-- rendered. e.g. have a diff data structure for it:
--   S = S SymbolTable ((Token, Path)) (Int, Int) (Maybe ([Token], Int))
-- or even
--   MainWindowData = MainWindowData SymbolTable ((Token, Path)) (Int, Int)
--   SelectionPopupData = SelectionPopupData [Token] Int
--   AppData = MainWindow MainWindowData | Popup SelectionPopupData MainWindowData
-- so then what am I really saving by having this new model for managing app
-- transformations? It does seem slightly simpler, but I end up replicating bits
-- of it anyhow to communicate w/ the renderer

setName :: String -> State (App Token) ()
setName s = do changeState (addingNameHandler s)
               t <- gets tree
               p <- gets path
               if treeUnderCursor p t == (Just Unknown)
               then applyTransformationPartial (replaceAtPoint' undefined)
               else return ()
               t' <- gets tree
               p' <- gets path
               -- applyToSymbolTable (try (updateSymbolTable t' p' (pack s)))
               -- error "Need to update relevant Name"
               applyTransformationPartial (replaceAtPoint' (Name (Just s)))

overIdentifier :: Token -> Path -> Bool
overIdentifier t p = case treeUnderCursor p t of
                        Just (Name _) -> True
                        _ -> False

whenOverIdentifier :: State (App Token) a -> State (App Token) a -> State (App Token) a
whenOverIdentifier yes no = do t <- gets tree
                               p <- gets path
                               if overIdentifier t p then yes else no

setPath :: Tree a => Path -> State (App a) ()
setPath = applyToPath . const

setTerm :: Tree a => a -> State (App a) ()
setTerm = applyToTerm . const

applyToTerm :: Tree a => (a -> a) -> State (App a) ()
applyToTerm f = modify (\s -> s {tree = f (tree s)})

applyToPath :: Tree a => (Path -> Path) -> State (App a) ()
applyToPath f = modify (\s -> s {path = f (path s)})

applyToPosition :: Tree a => ((Int, Int) -> (Int, Int)) -> State (App a) ()
applyToPosition f = modify (\s -> s {position = f (position s)})

setPopup :: Tree a => Maybe (PopupData a) -> State (App a) ()
setPopup x = modify (\s -> s {popupData = x})

-- needs more thought, but heres some improvements:
--  1. should always result either in a new path or no change whatsoever
--  2. left/right movement should stay on the same level or something. idk
--  3. up/down movement should keep cursor in same location until we do
--     right/left movement, except maybe see next point
--  4. if next/previous line is shorter and at the end of a line, up/down
--     movement should move to end of line (tho cursor should be in same
--     location. in other words, select the nearest path on that line). same
--     goes for short lines
--  might need a bounding box to accomadate some of these things. e.g. min/max
--  search space so when we search down/up we know when to stop
selectRight :: (Int, Int) -> (Int, Int)
selectRight (x,y) = (x+1,y)
selectLeft :: (Int, Int) -> (Int, Int)
selectLeft (x,y) = (x-1,y)
selectDown :: (Int, Int) -> (Int, Int)
selectDown (x,y) = (x,y+1)
selectUp :: (Int, Int) -> (Int, Int)
selectUp (x,y) = (x,y-1)

getRendering :: Int -> State (App Token) Rendering
getRendering n = do t <- gets tree
                    return (render n (t,[] :: [String]))

positionFromPath :: Rendering -> Path -> (Int, Int)
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in Rendering. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,Cell {paths = p}) <- toList pathMap, path `elem` p ]

updatePosition :: Int -> State (App Token) ()
updatePosition n = do pathMap <- getRendering n
                      path <- gets path
                      applyToPosition (const (positionFromPath pathMap path))

exitPopup :: State (App Token) ()
exitPopup = setPopup Nothing >> changeState homeHandler

commit :: Tree a => State (App a) ()
commit = modify (\s -> s {prev = s})

revert :: Tree a => State (App a) ()
revert = modify (\s -> prev s)

changeState :: FoldMachine -> State (App Token) ()
changeState u = modify (\s -> s {next = Just u})

terminate :: State (App Token) ()
terminate = modify (\s -> s {next = Nothing})

updatePath :: Int -> State (App Token) ()
updatePath n = do pMap <- pathFromPosition n
                  case pMap of
                         Just p -> applyToPath (const p)
                         Nothing -> return ()

pathFromPosition :: Int -> State (App Token) (Maybe Path)
pathFromPosition n = do pathMap <- getRendering n
                        position <- gets position
                        return (fmap Prelude.head (case Data.Map.lookup position pathMap of 
                                                           Just c -> Just (Renderer.paths c)
                                                           Nothing -> Nothing))

selectTerm :: [Token] -> Int -> State (App Token) ()
selectTerm l n = setPopup (Just (l,n')) >> changeState (selectingTermHandler l n')
        where n' = mod n (Prelude.length l)

addingNameHandler :: String -> FoldMachine
addingNameHandler s ((Key k),_) = if isAlphaNum k then setName (s ++ [k]) else return ()
addingNameHandler s (Enter  ,_) = if s /= "" then changeState homeHandler else return ()
addingNameHandler s (Del    ,_) = if s /= "" then setName (Prelude.init s) else return ()
addingNameHandler _ ( _     ,_) = return ()

selectingTermHandler :: [Token] -> Int -> FoldMachine
selectingTermHandler _ _ ((Key 'p'),_) = exitPopup
selectingTermHandler l n (Enter    ,_) = exitPopup >> applyTransformation (replaceAtPoint' (l!!n))
selectingTermHandler l n ((Key 'k'),_) = selectTerm l (n-1)
selectingTermHandler l n ((Key 'j'),_) = selectTerm l (n+1)
selectingTermHandler l n (UpArrow  ,_) = selectTerm l (n-1)
selectingTermHandler l n (DownArrow,_) = selectTerm l (n+1)
selectingTermHandler _ _ (_        ,_) = return ()

homeHandler :: FoldMachine
-- homeHandler ((Key 'n') ,n) = applyMovement nextHole >> updatePosition n
-- homeHandler ((Key 'N') ,n) = applyMovement previousHole >> updatePosition n
-- homeHandler (RightArrow,n) = applyMovement nextLeaf >> updatePosition n
-- homeHandler (LeftArrow ,n) = applyMovement prevLeaf >> updatePosition n
-- homeHandler (LeftArrow ,n) = applyToPosition selectLeft >> updatePath n
-- homeHandler (RightArrow,n) = applyToPosition selectRight >> updatePath n
homeHandler (Tab       ,n) = applyMovement nextLeaf >> updatePosition n
homeHandler (BackTab   ,n) = applyMovement prevLeaf >> updatePosition n
homeHandler ((Key 'j') ,n) = applyMovement selectFirst >> updatePosition n
homeHandler ((Key 'l') ,n) = applyMovement selectNext >> updatePosition n
homeHandler ((Key 'h') ,n) = applyMovement selectPrev >> updatePosition n
homeHandler ((Key 'k') ,n) = applyMovement goUp >> updatePosition n
homeHandler ((Key 's') ,n) = applyTransformation swapAssignmentUp >> updatePosition n
homeHandler ((Key 'S') ,n) = applyTransformation swapAssignmentDown >> updatePosition n
homeHandler ((Key 'x') ,n) = applyTransformation remove >> updatePosition n
homeHandler ((Key 'c') ,_) = commit
homeHandler ((Key 'u') ,_) = revert
homeHandler (Esc       ,_) = terminate
homeHandler (UpArrow   ,n) = applyToPosition selectUp >> updatePath n
homeHandler (DownArrow ,n) = applyToPosition selectDown >> updatePath n
homeHandler ((Key 'r') ,_) = whenOverIdentifier (setName "") (return ())
homeHandler ((Key 'p') ,n) = whenOverIdentifier (return ()) (do t <- gets tree
                                                                p <- gets path
                                                                selectTerm (possibleTerms t p) 0)
homeHandler ((Key 'O') ,_) = applyTransformation (insertBefore (Assignment (Name Nothing) Unknown Unknown))
homeHandler ((Key 'o') ,_) = applyTransformation (insertAfter (Assignment (Name Nothing) Unknown Unknown))
homeHandler ((Key '?') ,_) = applyTransformation (replaceAtPoint' Unknown)
homeHandler ((Key 't') ,_) = applyTransformation (replaceAtPoint' TrueTerm)
homeHandler ((Key 'f') ,_) = applyTransformation (replaceAtPoint' FalseTerm)
homeHandler ((Key 'b') ,_) = applyTransformation (replaceAtPoint' BoolType)
homeHandler ((Key '>') ,_) = applyTransformation (replaceAtPoint' (FunctionType Unknown Unknown))
homeHandler ((Key '\\'),_) = applyTransformation (replaceAtPoint' (Function Unknown Unknown Unknown))
homeHandler (_         ,_) = return ()

stateHandler :: FoldMachine
stateHandler k = do u <- gets next
                    case u of
                        Just u' -> u' k
                        Nothing -> return ()
