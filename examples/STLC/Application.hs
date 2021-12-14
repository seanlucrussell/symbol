{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module STLC.Application
  ( stateHandler
  , homeHandler
  , StateData (StateData)
  , SymbolAppInput (..)
  , ApplicationInput (..)
  , SymbolState (..)
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
import Data.Maybe
import Data.Text
import Data.Char
import qualified Data.Set as S

type SymbolAppInput = (ApplicationInput, Int)

data ApplicationInput = Key Char | Enter | Del | UpArrow | DownArrow | LeftArrow | RightArrow | Esc | Tab | BackTab |  Other

type PopupData = ([Token], Int)

data SymbolState a = SymbolState
                { symbolTable :: SymbolTable
                , tree :: a
                , path :: Path
                , position:: Position
                , popupData :: Maybe PopupData
                }

-- so this datatype is getting out of hand. BUT! it works pretty dang well. the
-- recursive bit at the end means that this thing keeps track of history, so we
-- can commit and revert changes as batches.
data StateData = StateData (SymbolState Token) (Maybe FoldMachine) StateData

type FoldMachine = SymbolAppInput -> (State StateData) ()

-- class Movable a where
--   move :: Tree b => a -> Movement b -> Maybe a
-- 
-- class Transformable a where
--   transform :: Tree b => a -> Transformation b -> Maybe a
-- 
-- class Application a b where
--   advance :: a -> b -> Maybe a
-- 
-- class Transaction a where
--   commit :: a -> a
--   revert :: a -> a

-- try to apply a movement
applyMovement :: Movement Token -> (State StateData) ()
applyMovement m = applyToZipper (try f)
        where f :: (Token, Path) -> Maybe (Token, Path)
              f (tree, path) = do path' <- m tree path
                                  return (tree, path')

-- try to apply a transformation, commiting if the transformation leaves the
-- data structures in a valid state
applyTransformation :: Transformation Token -> (State StateData) ()
applyTransformation t = applyToZipper (try f) >> commit
        where f :: (Token, Path) -> Maybe (Token, Path)
              f (tree, path) = do (tree', path') <- t tree path
                                  if validateProgram tree' && validatePath tree' path'
                                  then return (tree', path')
                                  else Nothing

-- like applyTransformation, but doesn't do checking, doesn't commit change.
-- good for intermediate transitions where the underlying datastructures aren't
-- sound
applyTransformationPartial :: Transformation Token -> (State StateData) ()
applyTransformationPartial t = applyToZipper (try f)
        where f :: (Token, Path) -> Maybe (Token, Path)
              f (tree, path) = do (tree', path') <- t tree path
                                  if validatePath tree' path'
                                  then return (tree', path')
                                  else Nothing

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
-- validate :: StateData -> Boolean
-- startTransaction :: (State StateData) ()
-- commit :: (State StateData) ()
--
-- and an extra element to the UIState data type
-- Transacting :: StateData -> UIState

-- ok so state data should maybe accept a Rendering callback or something like
-- that? something like
--   render :: StateData -> ExtraInfo -> a
-- ? or maybe we need to be clearer about what info there is that could be
-- rendered. e.g. have a diff data structure for it:
--   S = S SymbolTable ((Token, Path)) Position (Maybe ([Token], Int))
-- or even
--   MainWindowData = MainWindowData SymbolTable ((Token, Path)) Position
--   SelectionPopupData = SelectionPopupData [Token] Int
--   AppData = MainWindow MainWindowData | Popup SelectionPopupData MainWindowData
-- so then what am I really saving by having this new model for managing app
-- transformations? It does seem slightly simpler, but I end up replicating bits
-- of it anyhow to communicate w/ the renderer

setName :: String -> (State StateData) ()
setName s = do changeState (addingNameHandler s)
               t <- getTerm
               p <- getPath
               if treeUnderCursor p t == (Just Unknown)
               then applyTransformationPartial (replaceAtPoint' (validIdentifier t))
               else return ()
               t' <- getTerm
               p' <- getPath
               applyToSymbolTable (try (updateSymbolTable t' p' (pack s)))

whenOverIdentifier :: (State StateData) a -> (State StateData) a -> (State StateData) a
whenOverIdentifier yes no = do t <- getTerm
                               p <- getPath
                               if overIdentifier t p then yes else no

applyToSymbolTable :: (SymbolTable -> SymbolTable) -> (State StateData) ()
applyToSymbolTable f = applyToSymbolState (\(SymbolState s t p' p x) -> SymbolState (f s) t p' p x)

applyToZipper :: ((Token, Path) -> (Token, Path)) -> (State StateData) ()
applyToZipper f = applyToSymbolState (\(SymbolState s t p' p x) -> let (t',p'') = f (t,p') in SymbolState s t' p'' p x)

applyToPosition :: (Position -> Position) -> (State StateData) ()
applyToPosition f = applyToSymbolState (\(SymbolState s t p' p x) -> SymbolState s t p' (f p) x)

setPopup :: Maybe ([Token], Int) -> (State StateData) ()
setPopup x = applyToSymbolState (\(SymbolState s t p' p _) -> SymbolState s t p' p x)

applyToSymbolState :: ((SymbolState Token) -> (SymbolState Token)) -> (State StateData) ()
applyToSymbolState f = do StateData s u h <- get
                          put (StateData (f s) u h)

getSymbolState :: (State StateData) (SymbolState Token)
getSymbolState = do StateData s _ _ <- get
                    return s

getSymbolTable :: (State StateData) SymbolTable
getSymbolTable = do SymbolState s _ _ _ _ <- getSymbolState
                    return s

getTerm :: (State StateData) (Token)
getTerm = do SymbolState _ t _ _ _ <- getSymbolState
             return t

getPath :: (State StateData) Path
getPath = do SymbolState _ _ p _ _ <- getSymbolState
             return p
getPosition :: (State StateData) Position
getPosition = do SymbolState _ _ _ p _ <- getSymbolState
                 return p

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
selectRight :: Position -> Position
selectRight (x,y) = (x+1,y)
selectLeft :: Position -> Position
selectLeft (x,y) = (x-1,y)
selectDown :: Position -> Position
selectDown (x,y) = (x,y+1)
selectUp :: Position -> Position
selectUp (x,y) = (x,y-1)

getPathMap :: Int -> (State StateData) PathMap
getPathMap n = do s <- getSymbolTable
                  t <- getTerm
                  return (termToPathMap s n t)

positionFromPath :: PathMap -> Path -> Position
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in PathMap. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,p) <- toList pathMap, path `elem` p ]

updatePosition :: Int -> (State StateData) ()
updatePosition n = do pathMap <- getPathMap n
                      path <- getPath
                      applyToPosition (const (positionFromPath pathMap path))

exitPopup :: (State StateData) ()
exitPopup = setPopup Nothing >> changeState homeHandler

commit :: (State StateData) ()
commit = do sd@(StateData s u _) <- get
            put (StateData s u sd)

revert :: (State StateData) ()
revert = do StateData _ _ h <- get
            put h

updatePath :: Int -> (State StateData) ()
updatePath n = do StateData (SymbolState s t _ p x) u h <- get
                  pMap <- pathFromPosition n
                  case pMap of
                         Just p' -> put (StateData (SymbolState s t p' p x) u h)
                         Nothing -> return ()

pathFromPosition :: Int -> (State StateData) (Maybe Path)
pathFromPosition n = do pathMap <- getPathMap n
                        position <- getPosition
                        return (fmap Prelude.head (Data.Map.lookup position pathMap))

selectTerm :: [Token] -> Int -> (State StateData) ()
selectTerm l n = setPopup (Just (l,n')) >> changeState (selectingTermHandler l n')
        where n' = mod n (Prelude.length l)

-- what is going on here? there are a few key pieces of data.
--   1. how should the empty string get rendered?
--   2. what acceptance criteria are there before commiting a change?
--   3. what are valid inputs?
-- For 3, that means we need a function Char -> Bool. For 2, that is just the
-- basic validation stuff. 1 arguably shouldn't be under the purview of the name
-- handling code.

addingNameHandler :: String -> FoldMachine
addingNameHandler s ((Key k),_) = if isAlphaNum k then setName (s ++ [k]) else return ()
addingNameHandler s (Enter  ,_) = if s /= "" then changeState homeHandler else return ()
addingNameHandler s (Del    ,_) = if s /= "" then setName (Prelude.init s) else return ()
addingNameHandler s ( _     ,_) = return ()

selectingTermHandler :: [Token] -> Int -> FoldMachine
selectingTermHandler l n ((Key 'p'),_) = exitPopup
selectingTermHandler l n (Enter    ,_) = exitPopup >> applyTransformation (replaceAtPoint' (l!!n))
selectingTermHandler l n ((Key 'k'),_) = selectTerm l (n-1)
selectingTermHandler l n ((Key 'j'),_) = selectTerm l (n+1)
selectingTermHandler l n (UpArrow  ,_) = selectTerm l (n-1)
selectingTermHandler l n (DownArrow,_) = selectTerm l (n+1)
selectingTermHandler l n (_        ,_) = return ()

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
homeHandler ((Key 's') ,n) = applyTransformation swapUp
homeHandler ((Key 'S') ,n) = applyTransformation swapDown
homeHandler ((Key 'x') ,n) = applyTransformation remove
homeHandler ((Key 'c') ,_) = commit
homeHandler ((Key 'u') ,_) = revert
homeHandler (Esc       ,_) = terminate
homeHandler (UpArrow   ,n) = applyToPosition selectUp >> updatePath n
homeHandler (DownArrow ,n) = applyToPosition selectDown >> updatePath n
homeHandler ((Key 'r') ,_) = whenOverIdentifier (setName "") (return ())
homeHandler ((Key 'p') ,n) = whenOverIdentifier (return ()) (do updatePosition n
                                                                t <- getTerm
                                                                p <- getPath
                                                                selectTerm (possibleTerms t p) 0)
homeHandler ((Key 'O') ,_) = applyTransformation (insertBefore (Assignment Unknown Unknown Unknown))
homeHandler ((Key 'o') ,_) = applyTransformation (insertAfter (Assignment Unknown Unknown Unknown))
homeHandler ((Key '?') ,_) = applyTransformation (replaceAtPoint' Unknown)
homeHandler ((Key 't') ,_) = applyTransformation (replaceAtPoint' TrueTerm)
homeHandler ((Key 'f') ,_) = applyTransformation (replaceAtPoint' FalseTerm)
homeHandler ((Key 'b') ,_) = applyTransformation (replaceAtPoint' BoolType)
homeHandler ((Key '>') ,_) = applyTransformation (replaceAtPoint' (FunctionType Unknown Unknown))
homeHandler ((Key '\\'),_) = applyTransformation (replaceAtPoint' (Function Unknown Unknown Unknown))
homeHandler (_         ,_) = return ()

changeState :: FoldMachine -> (State StateData) ()
changeState u = do StateData s _ h <- get
                   put (StateData s (Just u) h)

terminate :: (State StateData) ()
terminate = do StateData s _ h <- get
               put (StateData s Nothing h)

getUIState :: (State StateData) (Maybe (FoldMachine))
getUIState = do StateData _ u _ <- get
                return u

appHasTerminated :: Maybe (FoldMachine) -> Bool
appHasTerminated = isNothing

stateHandler :: FoldMachine
stateHandler k = do u <- getUIState
                    case u of
                        Just u' -> u' k
                        Nothing -> return ()
