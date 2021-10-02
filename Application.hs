{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Application
  ( stateHandler
  , StateData (StateData)
  , initialState
  ) where

import AST
import Movements
import Transformations
import Utilities
import Renderer
import SymbolRenderer

import SymbolData
import SymbolMovements

import Data.Maybe
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
-- validate :: StateData -> Boolean
-- startTransaction :: AppState ()
-- commit :: AppState ()
--
-- and an extra element to the UIState data type
-- Transacting :: StateData -> UIState

-- ok so state data should maybe accept a Rendering callback or something like
-- that? something like
--   render :: StateData -> ExtraInfo -> a
-- ? or maybe we need to be clearer about what info there is that could be
-- rendered. e.g. have a diff data structure for it:
--   S = S SymbolTable (Zipper Token) Position (Maybe ([Term Token], Int))
-- or even
--   MainWindowData = MainWindowData SymbolTable (Zipper Token) Position
--   SelectionPopupData = SelectionPopupData [Term Token] Int
--   AppData = MainWindow MainWindowData | Popup SelectionPopupData MainWindowData
-- so then what am I really saving by having this new model for managing app
-- transformations? It does seem slightly simpler, but I end up replicating bits
-- of it anyhow to communicate w/ the renderer
data StateData = StateData SymbolTable (Zipper Token) (Maybe StateHandler) Position (Maybe ([Term Token], Int))
type AppState = State StateData
type AppInput = (Key, Int) -- Int represents screen width, needed for renderer
type StateHandler = AppInput -> AppState ()

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
-- should be easy to implement too. split StateData into 
--   (SymbolTable, Zipper Token, Position)
-- and
--   UIState
-- then add a new thing that is [StateData] for all the commited changes.
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
transitionHome f = do changeUIState homeHandler
                      applyToZipper f

setName :: String -> AppState ()
setName s = do changeUIState (addingNameHandler s)
               z <- getZipper
               if tokenUnderCursor z == UnknownTerm
               then applyToZipper (replaceWithTerm (validIdentifier (zipperToTerm z)))
               else return ()
               z' <- getZipper
               applyToSymbolTable (try (updateSymbolTable z' (pack s)))

addingNameHandler :: String -> StateHandler
addingNameHandler " " ((KChar ' '), _) = return ()
addingNameHandler " " ((KChar k)  , _) = setName [k]
addingNameHandler s   ((KChar k)  , _) = setName (s ++ [k])
addingNameHandler " " (KEnter     , _) = return ()
addingNameHandler s   (KEnter     , _) = transitionHome nextHole
addingNameHandler []  (KBS        , _) = return ()
addingNameHandler [_] (KBS        , _) = setName " "
addingNameHandler s   (KBS        , _) = setName (Prelude.init s)
addingNameHandler _   (_          , _) = return ()

overIdentifier :: Zipper Token -> Bool
overIdentifier (t,p) = case tokenUnderCursor (goUp (t,p)) of
        FunctionTerm -> Prelude.last p == 0
        AssignmentTerm -> Prelude.last p == 0
        _ -> False

whenOverIdentifier :: AppState a -> AppState a -> AppState a
whenOverIdentifier yes no = do z <- getZipper
                               if overIdentifier z then yes else no

languageModifier :: StateHandler
languageModifier ((KChar 'r'), _) = whenOverIdentifier (setName " ") (return ())
languageModifier ((KChar 'p'), _) = whenOverIdentifier (return ()) (do z <- getZipper
                                                                       selectTerm (possibleTerms z) 0)
languageModifier ((KChar 'O'), _) = applyToZipper insertBefore
languageModifier ((KChar 'o'), _) = applyToZipper insertAfter
languageModifier (_          , _) = return ()

-- language agnostic transformations

changeUIState :: StateHandler -> AppState ()
changeUIState u = do StateData s z _ p x <- get
                     put (StateData s z (Just u) p x)

terminate :: AppState ()
terminate = do StateData s z _ p x <- get
               put (StateData s z Nothing p x)

applyToSymbolTable :: (SymbolTable -> SymbolTable) -> AppState ()
applyToSymbolTable f = do StateData s z u p x <- get
                          put (StateData (f s) z u p x)

applyToZipper :: (Zipper Token -> Zipper Token) -> AppState ()
applyToZipper f = do StateData s z u p x <- get
                     put (StateData s (f z) u p x)
                     -- updatePosition
                     -- ^^^^^^^^^^^^^^ this is causing errors right now. due to
                     -- some bits of the program that make incremental changes
                     -- to the zipper, where intermediate states are invalid
                     -- (e.g. not specifying things in the symbol table) but the
                     -- transformation as a whole is kosher. need a better model
                     -- to deal w/ that

applyToPosition :: (Position -> Position) -> AppState ()
applyToPosition f = do StateData s z u p x <- get
                       put (StateData s z u (f p) x)
                       -- updatePath
                       -- ^^^^^^^^^^ this is causing errors right now. due to
                       -- some bits of the program that make incremental changes
                       -- to the zipper, where intermediate states are invalid
                       -- (e.g. not specifying things in the symbol table) but
                       -- the transformation as a whole is kosher. need a better
                       -- model to deal w/ that

setPopup :: Maybe ([Term Token], Int) -> AppState ()
setPopup x = do StateData s z u p _ <- get
                put (StateData s z u p x)

getUIState :: AppState (Maybe StateHandler)
getUIState = do StateData _ _ u _ _ <- get
                return u

getSymbolTable :: AppState SymbolTable
getSymbolTable = do StateData s _ _ _ _ <- get
                    return s

getZipper :: AppState (Zipper Token)
getZipper = do StateData _ z _ _ _ <- get
               return z

getPosition :: AppState Position
getPosition = do StateData _ _ _ p _ <- get
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

getPathMap :: Int -> AppState PathMap
getPathMap n = do s <- getSymbolTable
                  (t, _) <- getZipper
                  -- AAHHH WHAT IS THIS??? pretending like the screen is always
                  -- 200 characters wide is dumb and wrong
                  return (termToPathMap s n t)

positionFromPath :: PathMap -> Path -> Position
positionFromPath pathMap path = leastInList positions
        where lowest (x,y) (x',y') = if y < y' then (x,y) else if x < x' then (x,y) else (x',y')
              leastInList [] = error "Path not in PathMap. How did that happen?"
              leastInList (l:[]) = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = [position | (position,p) <- toList pathMap, p == path]

updatePosition :: Int -> AppState ()
updatePosition n = do pathMap <- getPathMap n
                      (_, path) <- getZipper
                      applyToPosition (const (positionFromPath pathMap path))


updatePath :: Int -> AppState ()
updatePath n = do StateData s (t, _) u p x <- get
                  pMap <- pathFromPosition n
                  case pMap of
                         Just p' -> put (StateData s (t, p') u p x)
                         Nothing -> return ()

pathFromPosition :: Int -> AppState (Maybe Path)
pathFromPosition n = do pathMap <- getPathMap n
                        position <- getPosition
                        return (Data.Map.lookup position pathMap)

selectTerm :: [Term Token] -> Int -> AppState ()
selectTerm l n = setPopup (Just (l,n')) >> changeUIState (selectingTermHandler l n')
        where n' = mod n (Prelude.length l)

exitPopup :: AppState ()
exitPopup = setPopup Nothing >> changeUIState homeHandler

selectingTermHandler :: [Term Token] -> Int -> StateHandler
selectingTermHandler _ _ ((KChar 'p'), _) = exitPopup
selectingTermHandler l n (KEnter     , _) = transitionHome (nextHole . replaceWithTerm (l!!n)) >> exitPopup
selectingTermHandler l n ((KChar 'k'), _) = selectTerm l (n-1)
selectingTermHandler l n ((KChar 'j'), _) = selectTerm l (n+1)
selectingTermHandler l n (KUp        , _) = selectTerm l (n-1)
selectingTermHandler l n (KDown      , _) = selectTerm l (n+1)
selectingTermHandler _ _ (_          , _) = return ()

homeHandler :: StateHandler
homeHandler ((KChar 'n') , _) = applyToZipper nextHole
homeHandler ((KChar 'N') , _) = applyToZipper previousHole
homeHandler ((KChar '\t'), _) = applyToZipper nextLeaf
homeHandler (KBackTab    , _) = applyToZipper prevLeaf
homeHandler ((KChar 'j') , _) = applyToZipper selectFirst
homeHandler ((KChar 'l') , _) = applyToZipper selectNext
homeHandler ((KChar 'h') , _) = applyToZipper selectPrev
homeHandler ((KChar 'k') , _) = applyToZipper goUp
homeHandler (KEsc        , _) = terminate
homeHandler (KUp         , _) = applyToPosition selectUp
homeHandler (KDown       , _) = applyToPosition selectDown
homeHandler (KLeft       , _) = applyToPosition selectLeft
homeHandler (KRight      , _) = applyToPosition selectRight
homeHandler (k           , n) = languageModifier (k, n)

initialState :: StateData
initialState = StateData initialSymbolTable initialZipper (Just homeHandler) (0,0) Nothing

appHasTerminated :: Maybe StateHandler -> Bool
appHasTerminated = isNothing

stateHandler :: StateHandler
stateHandler k = do u <- getUIState
                    case u of
                        Just u' -> u' k
                        Nothing -> return ()
