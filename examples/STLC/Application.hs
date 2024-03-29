{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module STLC.Application
  ( stateHandler
  , homeHandler
  , App (..)
  , SymbolAppInput
  , ApplicationInput (..)
  , ApplicationOutput (..)
  , renderApp
  , getPosition
  ) where

import AST
import Renderer
import Transformations

import STLC.Data
import STLC.Renderer ()
import STLC.Transformations
import STLC.TypeChecker

import Data.Map
import Data.Char

position :: Rendering -> Path -> (Int, Int)
position pathMap pa = leastInList positions
        where lowest (x,y) (x',y')
                | y < y' = (x,y)
                | x < x' = (x,y)
                | otherwise = (x',y')
              leastInList [] = error "Path not in Rendering. How did that happen?"
              leastInList [l] = l
              leastInList (l:ls) = lowest l (leastInList ls)
              positions = fmap fst (Prelude.filter (elem pa . paths . snd) (toList pathMap))

type SymbolAppInput = (ApplicationInput, Int)
data ApplicationInput = Key Char | Enter | Del | UpArrow | DownArrow | LeftArrow | RightArrow | Esc | Tab | BackTab |  Other
data ApplicationOutput = Continue | Terminate | Save
type PopupData a = ([a], Int)
data App a = App
                { tree :: a
                , path :: Path
                , popupData :: Maybe (PopupData a)
                , output :: ApplicationOutput
                , next :: Continuation
                , prev :: App a
                }
                
type Continuation = SymbolAppInput -> App Token -> App Token

closePopup :: App Token -> App Token
closePopup a = a {popupData = Nothing, next = homeHandler}

renderApp :: Int -> App Token -> Rendering
renderApp windowWidth a = render windowWidth (tree a,[] :: [String])

updateName :: String -> App Token -> App Token
updateName s a = partialTransform (transformAtPoint (Name (Just s))) (a {next = addingNameHandler s})

setPopupSelection :: [Token] -> Int -> App Token -> App Token
setPopupSelection l n a = a {popupData = Just (l,mod n (Prelude.length l)), next = selectingTermHandler l n}

getPosition :: Int -> App Token -> (Int,Int)
getPosition n a = position (renderApp n a) (path a)

-- try to apply a transformation, commiting if the transformation leaves the
-- data structures in a valid state
transform :: Transformation Token -> App Token -> App Token
transform t a = case t (tree a) (path a) of
                     Just (newTerm, newPath) -> if validateProgram newTerm newPath
                                                then a {tree = newTerm, path = newPath, prev = a}
                                                else a
                     Nothing -> a

-- like transform, but doesn't do checking, doesn't commit change.  good for
-- intermediate transitions where the underlying datastructures aren't sound
partialTransform :: Transformation Token -> App Token -> App Token
partialTransform t a = case t (tree a) (path a) of
                     Just (newTerm, newPath) -> if validateSTLCPath newTerm newPath
                                                then a {tree = newTerm, path = newPath}
                                                else a
                     Nothing -> a

-- in the future, all navigation should be done based on the view. i.e. select next, select prev, up, down. whatever. it should take into account the view.

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

overIdentifier :: App Token -> Bool
overIdentifier a = case treeUnderCursor (path a) (tree a) of
                        Just (Name _) -> True
                        _ -> False

addingNameHandler :: String -> Continuation
addingNameHandler s (Key k,_) a = if isAlphaNum k then updateName (s ++ [k]) a else a
addingNameHandler s (Enter  ,_) a = if s /= "" then a {next = homeHandler} else a
addingNameHandler s (Del    ,_) a = if s /= "" then updateName (Prelude.init s) a else a
addingNameHandler _ ( _     ,_) a = a

selectingTermHandler :: [Token] -> Int -> Continuation
selectingTermHandler _ _ (Key 'p',_) a = closePopup a
selectingTermHandler l n (Enter    ,_) a = transform (transformAtPoint (l!!n)) (closePopup a)
selectingTermHandler l n (Key 'k',_) a = setPopupSelection l (n-1) a
selectingTermHandler l n (Key 'j',_) a = setPopupSelection l (n+1) a
selectingTermHandler l n (UpArrow  ,_) a = setPopupSelection l (n-1) a
selectingTermHandler l n (DownArrow,_) a = setPopupSelection l (n+1) a
selectingTermHandler _ _ (_        ,_) a = a

saveHandler :: Continuation -> Continuation
saveHandler f i a = f i (a {output = Continue})

homeHandler :: Continuation
homeHandler (Tab       ,_) a = partialTransform nextLeaf a
homeHandler (BackTab   ,_) a = partialTransform prevLeaf a
homeHandler (Key 'j' ,_) a = partialTransform selectFirst a
homeHandler (Key 'l' ,_) a = partialTransform selectNext a
homeHandler (Key 'h' ,_) a = partialTransform selectPrev a
homeHandler (Key 'k' ,_) a = partialTransform goUp a
homeHandler (Key 's' ,_) a = transform swapAssignmentUp a
homeHandler (Key 'S' ,_) a = transform swapAssignmentDown a
homeHandler (Key 'x' ,_) a = transform removeAssignment a
homeHandler (Key 'O' ,_) a = transform insertAssignmentBefore a
homeHandler (Key 'o' ,_) a = transform insertAssignmentAfter a
homeHandler (Key 'w' ,_) a = a {output = Save, next = saveHandler homeHandler}
homeHandler (Key 'c' ,_) a = a {prev = a}
homeHandler (Key 'u' ,_) a = prev a
homeHandler (Esc       ,_) a = a {output = Terminate}
-- homeHandler (UpArrow   ,n) a = setPath n (changePosition selectUp a)
-- homeHandler (DownArrow ,n) a = setPath n (changePosition selectDown a)
homeHandler (Key 'r' ,_) a = if overIdentifier a then updateName "" a else a
homeHandler (Key 'p' ,_) a = if overIdentifier a then a else setPopupSelection (possibleTerms (tree a) (path a)) 0 a
homeHandler (Key '?' ,_) a = transform (transformAtPoint Unknown) a
homeHandler (Key 't' ,_) a = transform (transformAtPoint TrueTerm) a
homeHandler (Key 'f' ,_) a = transform (transformAtPoint FalseTerm) a
homeHandler (Key 'b' ,_) a = transform (transformAtPoint BoolType) a
homeHandler (Key '>' ,_) a = transform (transformAtPoint (FunctionType Unknown Unknown)) a
homeHandler (Key '\\',_) a = transform (transformAtPoint (Function Unknown Unknown Unknown)) a
homeHandler (_         ,_) a = a

stateHandler :: Continuation
stateHandler k a = next a k a
