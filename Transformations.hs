{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , replaceWithTerm) where

import SymbolData
import Movements

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

insertBefore :: Zipper -> Zipper
insertBefore (Zipper s (TopLevel a b)) = Zipper s (TopLevel (Assignment UnknownTerm  UnknownTerm:a) b) .- selectPrev
insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter (Zipper s (TopLevel a b)) = Zipper s (TopLevel a (Assignment UnknownTerm UnknownTerm:b)) .- selectNext
insertAfter z = z .- goup .- insertAfter

replaceWithTerm :: Term -> Zipper -> Zipper
replaceWithTerm t z@(Zipper _ c) = let z' = Zipper t c in if validateZipper z' then z' else z

searchForNamedVariables :: Zipper -> [Term]
searchForNamedVariables z@(Zipper _ (FunctionBody v _ _)) = v:searchForNamedVariables (goup z)
searchForNamedVariables z@(Zipper _ (TopLevel (Assignment v _:_) _)) = v:searchForNamedVariables (selectPrev z)
searchForNamedVariables z@(Zipper _ (TopLevel [] _)) = []
searchForNamedVariables z = searchForNamedVariables (goup z)

functionCalls :: Zipper -> [Term]
-- this is a dumb way of doing things!!! do it right!!! when you have more
-- time!!!
functionCalls z = concat [ [(ApplicationTerm x UnknownTerm)
                  , (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm)
                  , (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm)
                  , (ApplicationTerm (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm) UnknownTerm)] | x <- searchForNamedVariables z]


standardTerms :: [Term]
standardTerms = [ BooleanLiteralTerm True
                , BooleanLiteralTerm False
                , FunctionTerm UnknownTerm UnknownTerm UnknownTerm
                , ApplicationTerm UnknownTerm UnknownTerm
                , ConditionalTerm UnknownTerm UnknownTerm UnknownTerm
                , UnknownTerm
                , FnTypeTerm UnknownTerm UnknownTerm
                , BoolTypeTerm
                , Assignment UnknownTerm UnknownTerm ]

allPossibleTerms :: Zipper -> [Term]
allPossibleTerms z = standardTerms ++ (searchForNamedVariables z) ++ (functionCalls z)

termTypeChecks :: Zipper -> Term -> Bool
termTypeChecks (Zipper _ c) t = validateZipper (Zipper t c)

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
