{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module STLC.Application
  ( stateHandler
  , homeHandler
  , SymbolState (..)
  , SymbolAppInput (..)
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
import Control.Monad
import Data.Map
import Data.Maybe
import Data.Text
import Data.Char
import qualified Data.Set as S

type SymbolAppInput = (ApplicationInput, Int)

data ApplicationInput = Key Char | Enter | Del | UpArrow | DownArrow | LeftArrow | RightArrow | Esc | Tab | BackTab |  Other

type PopupData a = ([a], Int)

data SymbolState a = SymbolState
                { symbolTable :: SymbolTable
                , tree :: a
                , path :: Path
                , position:: Position
                , popupData :: Maybe (PopupData a)
                , next :: Maybe FoldMachine
                , prev :: SymbolState a
                }

type FoldMachine = SymbolAppInput -> State (SymbolState Token) ()

-- try to apply a movement
applyMovement :: Movement Token -> State (SymbolState Token) ()
applyMovement m = do term <- gets tree
                     applyToPath (try (m term))

-- ok so what we need is some sort of IndexedTree class, with methods for
-- extracting and updating terms and paths. or something. That then gets wrapped
-- up in a state monad, and the state monad adds the temporal element. getTree,
-- getPath, setTree, setPath are then defined over the IndexedTree class, e.g.
--   getPath :: IndexedTree a => State a Path
--   getTree :: IndexedTree a => State a (Tree b)
--   setPath :: IndexedTree a => Path -> State a ()
--   setTree :: IndexedTree a => Tree b -> State a ()
-- this clearly means IndexedTree should be parameterized by the type of the
-- tree in addition to the type of the general datastructure


-- try to apply a transformation, commiting if the transformation leaves the
-- data structures in a valid state
applyTransformation :: Transformation Token -> State (SymbolState Token) ()
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
applyTransformationPartial :: Transformation Token -> State (SymbolState Token) ()
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
-- validate :: (SymbolState Token) -> Boolean
-- startTransaction :: State (SymbolState Token) ()
-- commit :: State (SymbolState Token) ()
--
-- and an extra element to the UIState data type
-- Transacting :: (SymbolState Token) -> UIState

-- ok so state data should maybe accept a Rendering callback or something like
-- that? something like
--   render :: (SymbolState Token) -> ExtraInfo -> a
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

setName :: String -> State (SymbolState Token) ()
setName s = do changeState (addingNameHandler s)
               t <- gets tree
               p <- gets path
               if treeUnderCursor p t == (Just Unknown)
               then applyTransformationPartial (replaceAtPoint' (validIdentifier t))
               else return ()
               t' <- gets tree
               p' <- gets path
               applyToSymbolTable (try (updateSymbolTable t' p' (pack s)))

whenOverIdentifier :: State (SymbolState Token) a -> State (SymbolState Token) a -> State (SymbolState Token) a
whenOverIdentifier yes no = do t <- gets tree
                               p <- gets path
                               if overIdentifier t p then yes else no

applyToSymbolTable :: (SymbolTable -> SymbolTable) -> State (SymbolState Token) ()
applyToSymbolTable f = modify (\s -> s {symbolTable = f (symbolTable s)})

setPath :: Path -> State (SymbolState Token) ()
setPath = applyToPath . const

setTerm :: Token -> State (SymbolState Token) ()
setTerm = applyToTerm . const

applyToTerm :: (Token -> Token) -> State (SymbolState Token) ()
applyToTerm f = modify (\s -> s {tree = f (tree s)})

applyToPath :: (Path -> Path) -> State (SymbolState Token) ()
applyToPath f = modify (\s -> s {path = f (path s)})

applyToPosition :: (Position -> Position) -> State (SymbolState Token) ()
applyToPosition f = modify (\s -> s {position = f (position s)})

setPopup :: Maybe ([Token], Int) -> State (SymbolState Token) ()
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
selectRight :: Position -> Position
selectRight (x,y) = (x+1,y)
selectLeft :: Position -> Position
selectLeft (x,y) = (x-1,y)
selectDown :: Position -> Position
selectDown (x,y) = (x,y+1)
selectUp :: Position -> Position
selectUp (x,y) = (x,y-1)

getPathMap :: Int -> State (SymbolState Token) PathMap
getPathMap n = do s <- gets symbolTable
                  t <- gets tree
                  return (termToPathMap s n t)

positionFromPath :: PathMap -> Path -> Position
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in PathMap. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,p) <- toList pathMap, path `elem` p ]

updatePosition :: Int -> State (SymbolState Token) ()
updatePosition n = do pathMap <- getPathMap n
                      path <- gets path
                      applyToPosition (const (positionFromPath pathMap path))

exitPopup :: State (SymbolState Token) ()
exitPopup = setPopup Nothing >> changeState homeHandler

commit :: State (SymbolState Token) ()
commit = modify (\s -> s {prev = s})

revert :: State (SymbolState Token) ()
revert = modify (\s -> prev s)

changeState :: FoldMachine -> State (SymbolState Token) ()
changeState u = modify (\s -> s {next = Just u})

terminate :: State (SymbolState Token) ()
terminate = modify (\s -> s {next = Nothing})

updatePath :: Int -> State (SymbolState Token) ()
updatePath n = do SymbolState s t _ p x u h <- get
                  pMap <- pathFromPosition n
                  case pMap of
                         Just p' -> put (SymbolState s t p' p x u h)
                         Nothing -> return ()

pathFromPosition :: Int -> State (SymbolState Token) (Maybe Path)
pathFromPosition n = do pathMap <- getPathMap n
                        position <- gets position
                        return (fmap Prelude.head (Data.Map.lookup position pathMap))

selectTerm :: [Token] -> Int -> State (SymbolState Token) ()
selectTerm l n = setPopup (Just (l,n')) >> changeState (selectingTermHandler l n')
        where n' = mod n (Prelude.length l)

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
                                                                t <- gets tree
                                                                p <- gets path
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

appHasTerminated :: Maybe (FoldMachine) -> Bool
appHasTerminated = isNothing

stateHandler :: FoldMachine
stateHandler k = do u <- gets next
                    case u of
                        Just u' -> u' k
                        Nothing -> return ()
