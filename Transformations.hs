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

insertBefore :: Zipper -> Zipper
insertBefore (Zipper s (TopLevel a b)) = Zipper s (TopLevel (Assignment UnknownTerm UnknownTerm UnknownTerm:a) b) .- selectPrev
insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter (Zipper s (TopLevel a b)) = Zipper s (TopLevel a (Assignment UnknownTerm UnknownTerm UnknownTerm:b)) .- selectNext
insertAfter z = z .- goup .- insertAfter

replaceWithTerm :: Term -> Zipper -> Zipper
replaceWithTerm t = try (replaceWithTerm' t)

replaceWithTermAndSelectNext :: Term -> Zipper -> Zipper
replaceWithTermAndSelectNext t = try (replaceWithTerm' t >=> (nextHole' <!> return))

replaceWithTerm' :: Term -> Zipper -> Maybe Zipper
replaceWithTerm' t z@(Zipper _ c) = let z' = Zipper t c in if validateZipper z' then Just z' else Nothing

searchForNamedVariables :: Zipper -> [Term]
searchForNamedVariables z@(Zipper _ (FunctionBody v _ _)) = v:searchForNamedVariables (goup z)
searchForNamedVariables z@(Zipper _ (TopLevel (Assignment v _ _:_) _)) = v:searchForNamedVariables (selectPrev z)
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
                , ConditionalTerm UnknownTerm UnknownTerm UnknownTerm
                , FnTypeTerm UnknownTerm UnknownTerm
                , BoolTypeTerm
                , Assignment UnknownTerm UnknownTerm UnknownTerm
                , UnknownTerm ]

allPossibleTerms :: Zipper -> [Term]
allPossibleTerms z = (reverse (searchForNamedVariables z)) ++ (functionCalls z) ++ standardTerms

termTypeChecks :: Zipper -> Term -> Bool
termTypeChecks (Zipper _ c) t = validateZipper (Zipper t c)

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
