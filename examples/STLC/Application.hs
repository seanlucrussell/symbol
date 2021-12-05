{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module STLC.Application
  ( stateHandler
  , homeHandler
  , StateData (StateData)
  , SymbolAppInput (..)
  , ApplicationInput (..)
  ) where

import AST
import Movements
import Renderer
import Transformations
import Utilities

import STLC.SymbolData
import STLC.SymbolMovements
import STLC.SymbolRenderer
import STLC.Transformations
import STLC.TypeChecker

import Control.Monad.State
import Data.Map
import Data.Maybe
import Data.Text
import qualified Data.Set as S

-- try to apply a movement
applyMovement :: SymbolAppInput a => Movement Token -> (State (StateData a)) ()
applyMovement m = applyToZipper (try f)
        where f :: (Token, Path) -> Maybe (Token, Path)
              f (tree, path) = do path' <- m tree path
                                  return (tree, path')

-- try to apply a transformation, commiting if the transformation leaves the
-- data structures in a valid state
applyTransformation :: SymbolAppInput a => Transformation Token -> (State (StateData a)) ()
applyTransformation t = applyToZipper (try f) >> commit
        where f :: (Token, Path) -> Maybe (Token, Path)
              f (tree, path) = do (tree', path') <- t tree path
                                  if validateProgram tree' && validatePath tree' path'
                                  then return (tree', path')
                                  else Nothing

-- like applyTransformation, but doesn't do checking, doesn't commit change.
-- good for intermediate transitions where the underlying datastructures aren't
-- sound
applyTransformationPartial :: SymbolAppInput a => Transformation Token -> (State (StateData a)) ()
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
-- validate :: (StateData b) -> Boolean
-- startTransaction :: (State StateData) ()
-- commit :: (State StateData) ()
--
-- and an extra element to the UIState data type
-- Transacting :: (StateData b) -> UIState

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

-- so this datatype is getting out of hand. BUT! it works pretty dang well. the
-- recursive bit at the end means that this thing keeps track of history, so we
-- can commit and revert changes as batches.

-- symbol specific stuff
type SymbolState = (SymbolTable, (Token, Path), Position, Maybe PopupData)
type PopupData = ([Token], Int)

data StateData a = SymbolAppInput a => StateData SymbolState (Maybe (a -> (State (StateData a)) ())) (StateData a)

-- semantics for string reader:
--   some sort of parser to validate string (i.e. int parser, id parser,
--   whatever-else parser).
--   intermediate rendering. some strings may not parse alone, but do parse when
--   all put together. e.g. the string '-' is not a valid integer, but '-1' is.
--   revert changes when you hit enter if the parsed string ain't valid
-- so maybe what we do is have some sort of different thing where we don't
-- validate changes to the zipper until we commit all at once

-- so we have a problem, where the data structure isn't getting validated at
-- each step. this is instead happening in the transformations part of the
-- codebase, which is probably the wrong way to do it? or maybe not? i'm not
-- sure actually.

transitionHome :: SymbolAppInput a => ((Token, Path) -> (Token, Path)) -> (State (StateData a)) ()
transitionHome f = do changeUIState homeHandler
                      applyToZipper f

setName :: SymbolAppInput a => String -> (State (StateData a)) ()
setName s = do changeUIState (addingNameHandler s)
               (t,p) <- getZipper
               if treeUnderCursor p t == (Just Unknown)
               then applyTransformationPartial (replaceAtPoint' (validIdentifier t))
               else return ()
               z' <- getZipper
               applyToSymbolTable (try (updateSymbolTable z' (pack s)))

whenOverIdentifier :: SymbolAppInput b => (State (StateData b)) a -> (State (StateData b)) a -> (State (StateData b)) a
whenOverIdentifier yes no = do (t,p) <- getZipper
                               if overIdentifier t p then yes else no

applyToSymbolTable :: SymbolAppInput b => (SymbolTable -> SymbolTable) -> (State (StateData b)) ()
applyToSymbolTable f = applyToSymbolState (\(s, z, p, x) -> ((f s), z, p, x))

applyToZipper :: SymbolAppInput b => ((Token, Path) -> (Token, Path)) -> (State (StateData b)) ()
applyToZipper f = applyToSymbolState (\(s, z, p, x) -> (s, (f z), p, x))

applyToPosition :: SymbolAppInput b => (Position -> Position) -> (State (StateData b)) ()
applyToPosition f = applyToSymbolState (\(s, z, p, x) -> (s, z, (f p), x))

setPopup :: SymbolAppInput b => Maybe ([Token], Int) -> (State (StateData b)) ()
setPopup x = applyToSymbolState (\(s, z, p, _) -> (s, z, p, x))


applyToSymbolState :: SymbolAppInput b => (SymbolState -> SymbolState) -> (State (StateData b)) ()
applyToSymbolState f = do StateData s u h <- get
                          put (StateData (f s) u h)

getSymbolState :: SymbolAppInput b => (State (StateData b)) SymbolState
getSymbolState = do StateData s _ _ <- get
                    return s

getSymbolTable :: SymbolAppInput b => (State (StateData b)) SymbolTable
getSymbolTable = do (s, _, _, _) <- getSymbolState
                    return s

getTerm :: SymbolAppInput b => (State (StateData b)) (Token)
getTerm = do (t, _) <- getZipper
             return t

getPath :: SymbolAppInput b => (State (StateData b)) Path
getPath = do (_, p) <- getZipper
             return p

getZipper :: SymbolAppInput b => (State (StateData b)) ((Token, Path))
getZipper = do (_, z, _, _) <- getSymbolState
               return z

getPosition :: SymbolAppInput b => (State (StateData b)) Position
getPosition = do (_, _, p, _) <- getSymbolState
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

getPathMap :: SymbolAppInput b => Int -> (State (StateData b)) PathMap
getPathMap n = do s <- getSymbolTable
                  t <- getTerm
                  return (termToPathMap s n t)

positionFromPath :: PathMap -> Path -> Position
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in PathMap. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,p) <- toList pathMap, p == path]

updatePosition :: SymbolAppInput b => Int -> (State (StateData b)) ()
updatePosition n = do pathMap <- getPathMap n
                      path <- getPath
                      applyToPosition (const (positionFromPath pathMap path))


updatePath :: SymbolAppInput b => Int -> (State (StateData b)) ()
updatePath n = do StateData (s, (t, _), p, x) u h <- get
                  pMap <- pathFromPosition n
                  case pMap of
                         Just p' -> put (StateData (s, (t, p'), p, x) u h)
                         Nothing -> return ()

pathFromPosition :: SymbolAppInput b => Int -> (State (StateData b)) (Maybe Path)
pathFromPosition n = do pathMap <- getPathMap n
                        position <- getPosition
                        return (Data.Map.lookup position pathMap)

selectTerm :: SymbolAppInput b => [Token] -> Int -> (State (StateData b)) ()
selectTerm l n = setPopup (Just (l,n')) >> changeUIState (selectingTermHandler l n')
        where n' = mod n (Prelude.length l)

exitPopup :: SymbolAppInput b => (State (StateData b)) ()
exitPopup = setPopup Nothing >> changeUIState homeHandler

class SymbolAppInput a where
  extractInput :: a -> ApplicationInput
  extractWidth :: a -> Int

data ApplicationInput = Key Char | Enter | Del | UpArrow | DownArrow | LeftArrow | RightArrow | Esc | Tab | BackTab |  Other

addingNameHandler :: SymbolAppInput a => String -> (a -> (State (StateData a)) ())
addingNameHandler s i = f s (extractInput i)
        where f " " (Key ' ') = return ()
              f " " (Key k)   = setName [k]
              f s   (Key k)   = setName (s ++ [k])
              f " " Enter     = return ()
              f s   Enter     = transitionHome id
              f []  Del       = return ()
              f [_] Del       = setName " "
              f s   Del       = setName (Prelude.init s)
              f _   _         = return ()

selectingTermHandler :: SymbolAppInput a => [Token] -> Int -> (a -> (State (StateData a)) ())
selectingTermHandler l n i = f (extractInput i)
        where f (Key 'p')  = exitPopup
              -- f Enter      = transitionHome (id . replaceWithTerm (l!!n)) >> exitPopup
              f Enter      = changeUIState homeHandler >> applyTransformation (replaceAtPoint' (l!!n)) >> exitPopup
              f (Key 'k')  = selectTerm l (n-1)
              f (Key 'j')  = selectTerm l (n+1)
              f UpArrow    = selectTerm l (n-1)
              f DownArrow  = selectTerm l (n+1)
              f _          = return ()

languageModifier :: SymbolAppInput a => (a -> (State (StateData a)) ())
languageModifier i = f (extractInput i) (extractWidth i)
        where f (Key 'r') _ = whenOverIdentifier (setName " ") (return ())
              f (Key 'p') n = whenOverIdentifier (return ()) (do updatePosition n
                                                                 z <- getZipper
                                                                 selectTerm (possibleTerms z) 0)
              f (Key 'O') _ = applyTransformation (insertBefore (Assignment Unknown Unknown Unknown))
              f (Key 'o') _ = applyTransformation (insertAfter (Assignment Unknown Unknown Unknown))
              f _         _ = return ()

homeHandler :: SymbolAppInput a => (a -> (State (StateData a)) ())
homeHandler i = f (extractInput i) (extractWidth i)
        where -- f (Key 'n')  n = applyMovement nextHole >> updatePosition n
              -- f (Key 'N')  n = applyMovement previousHole >> updatePosition n
              f Tab        n = applyMovement nextLeaf >> updatePosition n
              f BackTab    n = applyMovement prevLeaf >> updatePosition n
              f (Key 'j')  n = applyMovement selectFirst >> updatePosition n
              f (Key 'l')  n = applyMovement selectNext >> updatePosition n
              f (Key 'h')  n = applyMovement selectPrev >> updatePosition n
              f (Key 'k')  n = applyMovement goUp >> updatePosition n
              f (Key 's')  n = applyTransformation swapUp
              f (Key 'S')  n = applyTransformation swapDown
              f (Key 'x')  n = applyTransformation remove
              f (Key 'c')  _ = commit
              f (Key 'u')  _ = revert
              f Esc        _ = terminate
              f UpArrow    n = applyToPosition selectUp >> updatePath n
              f DownArrow  n = applyToPosition selectDown >> updatePath n
              -- f RightArrow n = applyMovement nextLeaf >> updatePosition n
              -- f LeftArrow  n = applyMovement prevLeaf >> updatePosition n
              f k          n = languageModifier i
              -- f LeftArrow n = applyToPosition selectLeft >> updatePath n
              -- f RightArrow n = applyToPosition selectRight >> updatePath n

commit :: SymbolAppInput b => (State (StateData b)) ()
commit = do sd@(StateData s u _) <- get
            put (StateData s u sd)

revert :: SymbolAppInput b => (State (StateData b)) ()
revert = do StateData _ _ h <- get
            put h

changeUIState :: SymbolAppInput b => (b -> (State (StateData b)) ()) -> (State (StateData b)) ()
changeUIState u = do StateData s _ h <- get
                     put (StateData s (Just u) h)

terminate :: SymbolAppInput b => (State (StateData b)) ()
terminate = do StateData s _ h <- get
               put (StateData s Nothing h)

getUIState :: SymbolAppInput b => (State (StateData b)) (Maybe (b -> (State (StateData b)) ()))
getUIState = do StateData _ u _ <- get
                return u

appHasTerminated :: SymbolAppInput b => Maybe (b -> (State (StateData b)) ()) -> Bool
appHasTerminated = isNothing

stateHandler :: SymbolAppInput b => (b -> (State (StateData b)) ())
stateHandler k = do u <- getUIState
                    case u of
                        Just u' -> u' k
                        Nothing -> return ()
