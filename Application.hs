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
import Renderer
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

type AppStateData = (Zipper, Renderer, UIState)
type AppState = State AppStateData

changeUIState :: UIState -> AppState ()
changeUIState nextUIState = do (z, r, _) <- get
                               put (z, r, nextUIState)

changeRenderer :: Renderer -> AppState ()
changeRenderer renderer = do (z, _, u) <- get
                             put (z, renderer, u)

changeZipper :: Zipper -> AppState ()
changeZipper zipper = do (_, r, u) <- get
                         put (zipper, r, u)

applyToZipper :: (Zipper -> Zipper) -> AppState ()
applyToZipper f = do (z, r, u) <- get
                     put (f z, r, u)

getUIState :: AppState UIState
getUIState = do (_, _, u) <- get
                return u

getRenderer :: AppState Renderer
getRenderer = do (_, r, _) <- get
                 return r

getZipper :: AppState Zipper
getZipper = do (z, _, _) <- get
               return z

addingNameHandler :: Key -> String -> AppState ()
addingNameHandler (KChar ' ') " " = return ()
addingNameHandler (KChar k) s = do changeUIState (AddingName newName)
                                   applyToZipper (replaceWithTerm (IdentifierTerm (pack newName)))
                       where newName = case s of
                                 " " -> [k]
                                 _ -> s ++ [k]
addingNameHandler KEnter s = case s of
                " " -> do changeUIState NotReading
                          applyToZipper (replaceWithTerm UnknownTerm)
                s' -> do changeUIState NotReading
                         applyToZipper nextHole
addingNameHandler KBS s = case s of
                [] -> return ()
                [_] -> do changeUIState (AddingName " ")
                          applyToZipper (replaceWithTerm (IdentifierTerm (pack " ")))
                s' -> do changeUIState (AddingName (Prelude.init s'))
                         applyToZipper (replaceWithTerm (IdentifierTerm (pack (Prelude.init s'))))
addingNameHandler _ _ = return ()

notReadingHandler :: Key -> AppState ()
notReadingHandler (KChar 't') = do
    (z, r, u) <- get
    case z of 
         Zipper _ (FunctionArg _ _ _) -> do changeZipper (replaceWithTerm (IdentifierTerm " ") z)
                                            changeUIState (AddingName " ")
         Zipper _ (AssignmentId _ _) -> do changeZipper (replaceWithTerm (IdentifierTerm " ") z)
                                           changeUIState (AddingName " ")
         _ -> return ()
notReadingHandler (KChar 'p') = do z <- getZipper
                                   changeUIState (SelectingTerm (possibleTerms z) 1)
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

