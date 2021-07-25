{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Application
  ( stateHandler
  , AppStateData
  , UIState
    ( AddingName
    , SelectingTerm
    , NotReading
    , Exiting)) where

import Data.Text

import SymbolData
import Movements
import Transformations

import Graphics.Vty
import Control.Monad.State

safeListIndex :: Int -> [a] -> Maybe a
safeListIndex 0 (x:xs) = Just x
safeListIndex n [] = Nothing
safeListIndex n (x:xs) = if n < 0 then Nothing else safeListIndex (n-1) xs

data UIState = AddingName String
             | SelectingTerm [Term] Int
             | NotReading
             | Exiting

type AppStateData = (Zipper, (), UIState)
type AppState = State AppStateData

changeUIState :: UIState -> AppState ()
changeUIState nextUIState = do (z, r, _) <- get
                               put (z, r, nextUIState)

changeZipper :: Zipper -> AppState ()
changeZipper zipper = do (_, r, u) <- get
                         put (zipper, r, u)

applyToZipper :: (Zipper -> Zipper) -> AppState ()
applyToZipper f = do (z, r, u) <- get
                     put (f z, r, u)

getUIState :: AppState UIState
getUIState = do (_, _, u) <- get
                return u

getZipper :: AppState Zipper
getZipper = do (z, _, _) <- get
               return z

addingNameHandler :: Key -> String -> AppState ()
addingNameHandler (KChar ' ') " " = return ()
addingNameHandler (KChar k) s = do changeUIState (AddingName newName)
                                   applyToZipper (replaceWithTerm (Term (IdentifierTerm (pack newName)) []))
                       where newName = case s of
                                 " " -> [k]
                                 _ -> s ++ [k]
addingNameHandler KEnter s = case s of
                " " -> do changeUIState NotReading
                          applyToZipper (replaceWithTerm (Term UnknownTerm []))
                s' -> do changeUIState NotReading
                         applyToZipper nextHole
addingNameHandler KBS s = case s of
                [] -> return ()
                [_] -> do changeUIState (AddingName " ")
                          applyToZipper (replaceWithTerm (Term (IdentifierTerm (pack " ")) []))
                s' -> do changeUIState (AddingName (Prelude.init s'))
                         applyToZipper (replaceWithTerm (Term (IdentifierTerm (pack (Prelude.init s'))) []))
addingNameHandler _ _ = return ()

overIdentifier :: Zipper -> Bool
overIdentifier z@(Zipper _ p) = case termUnderCursor (goup z) of
        Term FunctionTerm _ -> Prelude.last p == 0
        Term AssignmentTerm _ -> Prelude.last p == 0
        _ -> False

notReadingHandler :: Key -> AppState ()
notReadingHandler (KChar 'r') = do z <- getZipper
                                   if overIdentifier z
                                   then do applyToZipper (replaceWithTerm (Term (IdentifierTerm " ") []))
                                           changeUIState (AddingName " ")
                                   else return ()
notReadingHandler (KChar 'p') = do z <- getZipper
                                   if overIdentifier z
                                   then return ()
                                   else changeUIState (SelectingTerm (possibleTerms z) 0)
notReadingHandler (KChar 'n') = applyToZipper nextHole
notReadingHandler (KChar 'N') = applyToZipper previousHole
notReadingHandler (KChar 'O') = applyToZipper insertBefore
notReadingHandler (KChar 'o') = applyToZipper insertAfter
notReadingHandler (KChar 'j') = applyToZipper selectFirst
notReadingHandler (KChar 'l') = applyToZipper selectNext
notReadingHandler (KChar 'h') = applyToZipper selectPrev
notReadingHandler (KChar 'k') = applyToZipper goup
notReadingHandler KEsc = changeUIState Exiting
notReadingHandler _  = return ()

selectingTermHandler :: Key -> [Term] -> Int -> AppState ()
selectingTermHandler (KChar 'p') _ _ = changeUIState NotReading
selectingTermHandler KEnter l n = do changeUIState NotReading
                                     applyToZipper z'
  where z' = case safeListIndex n l of
                  Just a -> nextHole . replaceWithTerm a
                  Nothing -> id
selectingTermHandler (KChar 'k') l n = changeUIState (SelectingTerm l (if n == 0 then Prelude.length l - 1 else n - 1))
selectingTermHandler (KChar 'j') l n = changeUIState (SelectingTerm l (if n == Prelude.length l - 1 then 0 else n + 1))
selectingTermHandler KUp l n = changeUIState (SelectingTerm l (if n == 0 then Prelude.length l - 1 else n - 1))
selectingTermHandler KDown l n = changeUIState (SelectingTerm l (if n == Prelude.length l - 1 then 0 else n + 1))
selectingTermHandler _ _ _ = return ()


stateHandler :: Key -> AppState ()
stateHandler e = do (z, r, u) <- get
                    case u of
                         AddingName s -> addingNameHandler e s
                         SelectingTerm l n -> selectingTermHandler e l n
                         NotReading -> notReadingHandler e
                         Exiting -> return ()

