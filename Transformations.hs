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

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x ys = x:ys
insertAt n x (y:ys) = y:insertAt (n-1) x ys

insertBefore :: Zipper -> Zipper
insertBefore (Zipper (Term Program t) (p:ps)) = Zipper (Term Program (insertAt p (Term AssignmentTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []]) t)) (p:ps)

insertAfter :: Zipper -> Zipper
insertAfter (Zipper (Term Program t) (p:ps)) = Zipper (Term Program (insertAt (p+1) (Term AssignmentTerm [Term UnknownTerm [], Term UnknownTerm [], Term UnknownTerm []]) t)) (p:ps)

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
searchForNamedVariables z = filter (/= Term UnknownTerm []) (searchAbove (goup z) ++ prev)
        where prev = case gototop z of
                          Zipper _ [0] -> []
                          _ -> searchBefore (selectPrev (gototop z))
              searchAbove z = case termUnderCursor z of
                                   Term FunctionTerm [a, _, _] -> a:searchAbove (goup z)
                                   Term AssignmentTerm [_, _, _] -> []
                                   _ -> searchForNamedVariables (goup z)
              searchBefore z@(Zipper _ [0]) = case termUnderCursor z of
                                                   Term AssignmentTerm [a, _, _] -> [a]
              searchBefore z@(Zipper _ _) = case termUnderCursor z of
                                                 Term AssignmentTerm [a, _, _] -> a:searchBefore (selectPrev z)

searchForNamedVariables' :: Zipper -> [Term]
searchForNamedVariables' z@(Zipper _ [0]) = case termUnderCursor z of
        Term AssignmentTerm [a, _, _] -> [a]
searchForNamedVariables' z@(Zipper _ p) = case termUnderCursor z of
        Term FunctionTerm [a, _, _] -> a:searchForNamedVariables (goup z)
        Term AssignmentTerm [a, _, _] -> a:searchForNamedVariables (selectPrev z)
        _ -> searchForNamedVariables (goup z)
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
termTypeChecks z t = case replaceWithTerm' t z of
                Just z' -> validateZipper z'
                Nothing -> False
                        

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
