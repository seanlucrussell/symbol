{-# LANGUAGE OverloadedStrings #-}
module Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , replaceWithTermAndSelectNext
  , replaceWithTerm) where

import SymbolData
import Movements
import TypeChecker

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- stolen from https://hackage.haskell.org/package/tomland-1.3.3.0/docs/src/Toml.Codec.Types.html#%3C%21%3E
infixl 3 <!>
(<!>) :: Alternative f => (a -> f x) -> (a -> f x) -> (a -> f x)
f <!> g = \a -> f a <|> g a
{-# INLINE (<!>) #-}

try :: (a -> Maybe a) -> a -> a
try f x = case f x of
        Just y -> y
        Nothing -> x

-- assumes we are in bounds of array
applyAtIndex :: Int -> (a -> a) -> [a] -> [a]
applyAtIndex 0 f (y:ys) = f y:ys
applyAtIndex n f (y:ys) = y:(applyAtIndex (n-1) f ys)

insertBefore :: Zipper -> Zipper
insertBefore = id
-- insertBefore (Zipper s (TopLevel a b)) = Zipper s (TopLevel (Assignment UnknownTerm UnknownTerm UnknownTerm:a) b) .- selectPrev
-- insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter = id
-- insertAfter (Zipper s (TopLevel a b)) = Zipper s (TopLevel a (Assignment UnknownTerm UnknownTerm UnknownTerm:b)) .- selectNext
-- insertAfter z = z .- goup .- insertAfter

replaceWithTerm :: Term -> Zipper -> Zipper
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Term -> Zipper -> Zipper
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))


replaceWithTerm' :: Term -> Zipper -> Maybe Zipper
replaceWithTerm' t (Zipper x p) = if validateZipper replaced then Just replaced else Nothing
        where replaced = Zipper (replaceWithTerm'' t p x) p

replaceWithTerm'' :: Term -> [Int] -> Term -> Term
replaceWithTerm'' t [] _ = t
replaceWithTerm'' t (p:ps) (Term x ts) = Term x (applyAtIndex p (replaceWithTerm'' t ps) ts)

searchForNamedVariables :: Zipper -> [Term]
searchForNamedVariables _ = []
-- searchForNamedVariables z@(Zipper _ (FunctionBody v _ _)) = v:searchForNamedVariables (goup z)
-- searchForNamedVariables z@(Zipper _ (TopLevel (Assignment v _ _:_) _)) = v:searchForNamedVariables (selectPrev z)
-- searchForNamedVariables z@(Zipper _ (TopLevel [] _)) = []
-- searchForNamedVariables z = searchForNamedVariables (goup z)

functionCalls :: Zipper -> [Term]
functionCalls _ = []
-- this is a dumb way of doing things!!! do it right!!! when you have more
-- time!!!
-- functionCalls z = concat [ [(ApplicationTerm x UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm)
--                   , (ApplicationTerm (ApplicationTerm (ApplicationTerm (ApplicationTerm x UnknownTerm) UnknownTerm) UnknownTerm) UnknownTerm)] | x <- searchForNamedVariables z]


standardTerms :: [Term]
standardTerms = [ Term TrueTerm []
                , Term FalseTerm []
                , Term FunctionTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []]
                , Term ConditionalTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []]
                , Term FunctionTypeTerm [Term UnknownTerm [], Term UnknownTerm []]
                , Term BoolTypeTerm []
                , Term AssignmentTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []]
                , Term UnknownTerm [] ]

allPossibleTerms :: Zipper -> [Term]
allPossibleTerms z = (reverse (searchForNamedVariables z)) ++ (functionCalls z) ++ standardTerms

termTypeChecks :: Zipper -> Term -> Bool
termTypeChecks (Zipper _ c) t = validateZipper (Zipper t c)

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
