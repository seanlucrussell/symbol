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
    , NotReading
    , Exiting)) where

import AST
import Movements
import Transformations
import Utilities

import SymbolData
import SymbolMovements

import qualified Data.Set as S
import Data.Text
import Data.Map
import Graphics.Vty
import Control.Monad.State

data UIState = AddingName String
             | SelectingTerm [Term Token] Int
             | NotReading
             | Exiting

type AppStateData = (SymbolTable, Zipper Token, UIState)
type AppState = State AppStateData

-- language specific transformations


findValidAssignmentId :: Term Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

findAllIds :: Term Token -> [Int]
findAllIds (Term (IdentifierTerm i) ts) = i:join (fmap findAllIds ts)
findAllIds (Term _ ts) = join (fmap findAllIds ts)

firstNumberNotInList :: [Int] -> Int
firstNumberNotInList l = f 0
     where s = S.fromList l
           f n = if S.member n s then f (n+1) else n

exitReader :: (Zipper Token -> Zipper Token) -> AppState ()
exitReader f = do changeUIState NotReading
                  applyToZipper f

setName :: String -> AppState ()
setName s = do changeUIState (AddingName s)
               z <- getZipper
               if tokenUnderCursor z == UnknownTerm
               then applyToZipper (replaceWithTerm (Term (IdentifierTerm (findValidAssignmentId (zipperToTerm z))) [])) 
               else return ()
               applyToSymbolTable (try (updateSymbolTable z (pack s)))

addingNameHandler :: Key -> String -> AppState ()
addingNameHandler (KChar ' ') " " = return ()
addingNameHandler (KChar k) s = setName (case s of " " -> [k]
                                                   _   -> s ++ [k])
addingNameHandler KEnter s = exitReader (case s of " " -> replaceWithTerm blankUnknown
                                                   _   -> nextHole)
addingNameHandler KBS s = case s of []  -> return ()
                                    [_] -> setName " "
                                    s'  -> setName (Prelude.init s')
addingNameHandler _ _ = return ()

overIdentifier :: Zipper Token -> Bool
overIdentifier z@(_, p) = case tokenUnderCursor (goUp z) of
        FunctionTerm -> Prelude.last p == 0
        AssignmentTerm -> Prelude.last p == 0
        _ -> False

whenOverIdentifier :: AppState a -> AppState a -> AppState a
whenOverIdentifier yes no = do z <- getZipper
                               if overIdentifier z then yes else no

languageModifier :: Key -> AppState ()
languageModifier (KChar 'r') = whenOverIdentifier (setName " ") (return ())
languageModifier (KChar 'p') = whenOverIdentifier (return ()) (do {z <- getZipper; selectTerm (possibleTerms z) 0})
languageModifier (KChar 'O') = applyToZipper insertBefore
languageModifier (KChar 'o') = applyToZipper insertAfter
languageModifier _           = return ()

-- language agnostic transformations

changeUIState :: UIState -> AppState ()
changeUIState u = do (s, z, _) <- get
                     put (s, z, u)

changeZipper :: Zipper Token -> AppState ()
changeZipper = applyToZipper . const

applyToSymbolTable :: (SymbolTable -> SymbolTable) -> AppState ()
applyToSymbolTable f = do (s, z, u) <- get
                          put (f s, z, u)

applyToZipper :: (Zipper Token -> Zipper Token) -> AppState ()
applyToZipper f = do (s, z, u) <- get
                     put (s, f z, u)

getUIState :: AppState UIState
getUIState = do (_, _, u) <- get
                return u

getSymbolTable :: AppState SymbolTable
getSymbolTable = do (s, _, _) <- get
                    return s

getZipper :: AppState (Zipper Token)
getZipper = do (_, z, _) <- get
               return z

selectTerm :: [Term Token] -> Int -> AppState ()
selectTerm l n = changeUIState (SelectingTerm l (mod n (Prelude.length l)))

selectingTermHandler :: Key -> [Term Token] -> Int -> AppState ()
selectingTermHandler (KChar 'p') _ _ = changeUIState NotReading
selectingTermHandler KEnter l n      = exitReader (nextHole . replaceWithTerm (l!!n))
selectingTermHandler (KChar 'k') l n = selectTerm l (n-1)
selectingTermHandler (KChar 'j') l n = selectTerm l (n+1)
selectingTermHandler KUp l n         = selectTerm l (n-1)
selectingTermHandler KDown l n       = selectTerm l (n+1)
selectingTermHandler _ _ _           = return ()

notReadingHandler :: Key -> AppState ()
notReadingHandler (KChar 'n')  = applyToZipper nextHole
notReadingHandler (KChar 'N')  = applyToZipper previousHole
notReadingHandler (KChar '\t') = applyToZipper nextLeaf
notReadingHandler KBackTab     = applyToZipper prevLeaf
notReadingHandler (KChar 'j')  = applyToZipper selectFirst
notReadingHandler (KChar 'l')  = applyToZipper selectNext
notReadingHandler (KChar 'h')  = applyToZipper selectPrev
notReadingHandler (KChar 'k')  = applyToZipper goUp
notReadingHandler KEsc         = changeUIState Exiting
notReadingHandler k            = languageModifier k

initialState :: AppStateData
initialState = (initialSymbolTable, initialZipper, NotReading)

stateHandler :: Key -> AppState ()
stateHandler e = do u <- getUIState
                    case u of AddingName    s   -> addingNameHandler e s
                              SelectingTerm l n -> selectingTermHandler e l n
                              NotReading        -> notReadingHandler e
                              Exiting           -> return ()

