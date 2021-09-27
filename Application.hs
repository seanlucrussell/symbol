{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Application
  ( stateHandler
  , AppStateData
  , initialState
  , UIState
    ( AddingName
    , SelectingTerm
    , Home
    , Exiting)) where

import AST
import Movements
import Transformations
import Utilities
import Renderer
import SymbolRenderer

import SymbolData
import SymbolMovements

import qualified Data.Set as S
import Data.Text
import Data.Map
import Graphics.Vty
import Control.Monad.State

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
-- validate :: AppStateData -> Boolean
-- startTransaction :: AppState ()
-- commit :: AppState ()
--
-- and an extra element to the UIState data type
-- Transacting :: AppStateData -> UIState

data UIState = AddingName String
             | SelectingTerm [Term Token] Int
             | Home
             | Exiting
             -- random idea: UIState is data type that really is just 
             --   Key -> AppState ()
             -- may be good for extensibility. don't have to change existing
             -- functions when adding new UIState thing

type AppStateData = (SymbolTable, Zipper Token, UIState, Position)
type AppState = State AppStateData
type StateHandler = Key -> AppState ()

-- language specific transformations

-- semantics for string reader:
--   some sort of parser to validate string (i.e. int parser, id parser,
--   whatever-else parser).
--   intermediate rendering. some strings may not parse alone, but do parse when
--   all put together. e.g. the string '-' is not a valid integer, but '-1' is.
--   revert changes when you hit enter if the parsed string ain't valid
-- so maybe what we do is have some sort of different thing where we don't
-- validate changes to the zipper until we commit all at once
--
-- should be easy to implement too. split AppStateData into 
--   (SymbolTable, Zipper Token, Position)
-- and
--   UIState
-- then add a new thing that is [AppStateData] for all the commited changes.
-- Then add new functions
--   commit :: AppState ()
--   revert :: AppState ()
-- where you respectively push and pop the current app data to the change stack

validIdentifier :: Term Token -> Term Token
validIdentifier t = Term (IdentifierTerm (findValidAssignmentId t)) []

findValidAssignmentId :: Term Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Term Token -> [Int]
findAllIds (Term (IdentifierTerm i) ts) = i:join (fmap findAllIds ts)
findAllIds (Term _ ts) = join (fmap findAllIds ts)

firstNumberNotInList :: [Int] -> Int
firstNumberNotInList l = f 0
     where s = S.fromList l
           f n = if S.member n s then f (n+1) else n

transitionHome :: (Zipper Token -> Zipper Token) -> AppState ()
transitionHome f = do changeUIState Home
                      applyToZipper f

setName :: String -> AppState ()
setName s = do changeUIState (AddingName s)
               z <- getZipper
               if tokenUnderCursor z == UnknownTerm
               then applyToZipper (replaceWithTerm (validIdentifier (zipperToTerm z)))
               else return ()
               z' <- getZipper
               applyToSymbolTable (try (updateSymbolTable z' (pack s)))

addingNameHandler :: String -> StateHandler
addingNameHandler " " (KChar ' ') = return ()
addingNameHandler " " (KChar k)   = setName [k]
addingNameHandler s   (KChar k)   = setName (s ++ [k])
addingNameHandler " " KEnter      = return ()
addingNameHandler s   KEnter      = transitionHome nextHole
addingNameHandler []  KBS         = return ()
addingNameHandler [_] KBS         = setName " "
addingNameHandler s   KBS         = setName (Prelude.init s)
addingNameHandler _   _           = return ()

overIdentifier :: Zipper Token -> Bool
overIdentifier (t,p) = case tokenUnderCursor (goUp (t,p)) of
        FunctionTerm -> Prelude.last p == 0
        AssignmentTerm -> Prelude.last p == 0
        _ -> False

whenOverIdentifier :: AppState a -> AppState a -> AppState a
whenOverIdentifier yes no = do z <- getZipper
                               if overIdentifier z then yes else no

languageModifier :: StateHandler
languageModifier (KChar 'r') = whenOverIdentifier (setName " ") (return ())
languageModifier (KChar 'p') = whenOverIdentifier (return ()) (do {z <- getZipper; selectTerm (possibleTerms z) 0})
languageModifier (KChar 'O') = applyToZipper insertBefore
languageModifier (KChar 'o') = applyToZipper insertAfter
languageModifier _           = return ()

-- language agnostic transformations

changeUIState :: UIState -> AppState ()
changeUIState u = do (s, z, _, p) <- get
                     put (s, z, u, p)

applyToSymbolTable :: (SymbolTable -> SymbolTable) -> AppState ()
applyToSymbolTable f = do (s, z, u, p) <- get
                          put (f s, z, u, p)

applyToZipper :: (Zipper Token -> Zipper Token) -> AppState ()
applyToZipper f = do (s, z, u, p) <- get
                     put (s, f z, u, p)
                     updatePosition

applyToPosition :: (Position -> Position) -> AppState ()
applyToPosition f = do (s, z, u, p) <- get
                       put (s, z, u, f p)
                       updatePath

getUIState :: AppState UIState
getUIState = do (_, _, u, _) <- get
                return u

getSymbolTable :: AppState SymbolTable
getSymbolTable = do (s, _, _, _) <- get
                    return s

getZipper :: AppState (Zipper Token)
getZipper = do (_, z, _, _) <- get
               return z

getPosition :: AppState Position
getPosition = do (_, _, _, p) <- get
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

getPathMap :: AppState PathMap
getPathMap = do s <- getSymbolTable
                (t, _) <- getZipper
                -- AAHHH WHAT IS THIS??? pretending like the screen is always
                -- 200 characters wide is dumb and wrong
                return (termToPathMap s 200 t)

positionFromPath :: PathMap -> Path -> Position
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in PathMap. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,p) <- toList pathMap, p == path]

-- convert path map to list of tuples
-- filter list to things that match path
-- sort so lowest y values are first, lowest x values second
-- take first thing in that list!

updatePosition :: AppState ()
updatePosition = do pathMap <- getPathMap
                    (_, path) <- getZipper
                    applyToPosition (const (positionFromPath pathMap path))


updatePath :: AppState ()
updatePath = do (s, (t, _), u, p) <- get
                pMap <- pathFromPosition
                case pMap of
                       Just p' -> put (s, (t, p'), u, p)
                       Nothing -> return ()

pathFromPosition :: AppState (Maybe Path)
pathFromPosition = do pathMap <- getPathMap
                      position <- getPosition
                      return (Data.Map.lookup position pathMap)

selectTerm :: [Term Token] -> Int -> AppState ()
selectTerm l n = changeUIState (SelectingTerm l (mod n (Prelude.length l)))

selectingTermHandler :: [Term Token] -> Int -> StateHandler
selectingTermHandler _ _ (KChar 'p') = changeUIState Home
selectingTermHandler l n KEnter      = transitionHome (nextHole . replaceWithTerm (l!!n))
selectingTermHandler l n (KChar 'k') = selectTerm l (n-1)
selectingTermHandler l n (KChar 'j') = selectTerm l (n+1)
selectingTermHandler l n KUp         = selectTerm l (n-1)
selectingTermHandler l n KDown       = selectTerm l (n+1)
selectingTermHandler _ _ _           = return ()

homeHandler :: StateHandler
homeHandler (KChar 'n')  = applyToZipper nextHole
homeHandler (KChar 'N')  = applyToZipper previousHole
homeHandler (KChar '\t') = applyToZipper nextLeaf
homeHandler KBackTab     = applyToZipper prevLeaf
homeHandler (KChar 'j')  = applyToZipper selectFirst
homeHandler (KChar 'l')  = applyToZipper selectNext
homeHandler (KChar 'h')  = applyToZipper selectPrev
homeHandler (KChar 'k')  = applyToZipper goUp
homeHandler KEsc         = changeUIState Exiting
homeHandler KUp          = applyToPosition selectUp
homeHandler KDown        = applyToPosition selectDown
homeHandler KLeft        = applyToPosition selectLeft
homeHandler KRight       = applyToPosition selectRight
homeHandler k            = languageModifier k

initialState :: AppStateData
initialState = (initialSymbolTable, initialZipper, Home, (0,0))


stateHandler :: StateHandler
stateHandler k = do u <- getUIState
                    case u of AddingName    s   -> addingNameHandler s k
                              SelectingTerm l n -> selectingTermHandler l n k
                              Home              -> homeHandler k
                              Exiting           -> return ()

