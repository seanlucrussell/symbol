{-# LANGUAGE OverloadedStrings #-}
module Movements
  ( (.-)
  , previousHole
  , nextHole
  , selectFirst
  , selectNext
  , selectPrev
  , selectLast
  , goup) where

import SymbolData

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

-- move to Maybe based thing, so that we can compose successful operations

-- concatenate operations with this operator
(.-) :: a -> (a -> b) -> b
(.-) x f = f x

-- try to apply a function that might fail, fall back to current value if it
-- does
try :: (a -> Maybe a) -> a -> a
try f x = fromMaybe x (f x)

-- keep applying a function until the function fails
untilFailure :: (a -> Maybe a) -> a -> Maybe a
untilFailure f x = case f x of
        Just y  -> untilFailure f y
        Nothing -> Just x

-- movements: the underlying ast doesn't change, just the position in it

previousHole :: Zipper -> Zipper
previousHole = try previousHole'

nextHole :: Zipper -> Zipper
nextHole = try nextHole'

selectFirst :: Zipper -> Zipper
selectFirst = try selectFirst'

selectNext :: Zipper -> Zipper
selectNext = try selectNext'

selectPrev :: Zipper -> Zipper
selectPrev = try selectPrev'

selectLast :: Zipper -> Zipper
selectLast = try selectLast'

goup :: Zipper -> Zipper
goup = try goup'

selectLast' :: Zipper -> Maybe Zipper
selectLast' z = selectFirst' z >>= (untilFailure selectNext')

zipperOnHole :: Zipper -> Bool
zipperOnHole (Zipper UnknownTerm _) = True
zipperOnHole _ = False

nextHole' :: Zipper -> Maybe Zipper
nextHole' w = (searchStart w >>= searchDownRight) <|> searchUpRight w
   where searchStart = if zipperOnHole w then selectNext' else Just

searchDownRight :: Zipper -> Maybe Zipper
searchDownRight z = (searchChildrenRight z) <|> (selectNext' z >>= searchDownRight)
searchChildrenRight :: Zipper -> Maybe Zipper
searchChildrenRight z = if zipperOnHole z then Just z else selectFirst' z >>= searchDownRight
searchUpRight :: Zipper -> Maybe Zipper
searchUpRight z = (parent >>= selectNext' >>= searchDownRight) <|> (parent >>= searchUpRight)
  where parent = goup' z

previousHole' :: Zipper -> Maybe Zipper
previousHole' w = (selectPrev' w >>= searchDownLeft) <|> searchUpLeft w

searchDownLeft :: Zipper -> Maybe Zipper
searchDownLeft z = searchChildrenLeft z <|> (selectPrev' z >>= searchDownLeft)
searchChildrenLeft :: Zipper -> Maybe Zipper
searchChildrenLeft z = if zipperOnHole z then Just z else selectLast' z >>= searchDownLeft
searchUpLeft :: Zipper -> Maybe Zipper
searchUpLeft z = (parent >>= selectPrev' >>= searchDownLeft) <|> (parent >>= searchUpLeft)
   where parent = goup' z

-- goToEnclosingStatement :: Zipper -> Zipper
-- goToEnclosingStatement z@(Zipper _ (TopLevel _ _))   = z
-- goToEnclosingStatement z = case goup' z of
--         Just z' -> goToEnclosingStatement z'
--         Nothing -> z
-- 
-- goToEnclosingFunction :: Zipper -> Maybe Zipper
-- goToEnclosingFunction z@(Zipper (Function _ _) _) = Just z
-- goToEnclosingFunction z = goup' z >>= goToEnclosingFunction

-- This stuff needs to change when language features are added

selectFirst' :: Zipper -> Maybe Zipper
selectFirst' (Zipper t c) = case t of
    IdentifierTerm _ -> Nothing
    FunctionTerm x y z -> Just (Zipper x (FunctionArg y z c))
    ApplicationTerm x y -> Just (Zipper x (ApplicationFn y c))
    BooleanLiteralTerm _ -> Nothing
    ConditionalTerm x y z -> Just (Zipper x (ConditionalCond y z c))
    UnknownTerm -> Nothing
    FnTypeTerm x y -> Just (Zipper x (FnTypeArg y c))
    BoolTypeTerm -> Nothing
    Assignment x y z -> Just (Zipper x (AssignmentId y z c))
    Program [] -> Nothing
    Program (x:xs) -> Just (Zipper x (TopLevel [] xs))

selectPrev' :: Zipper -> Maybe Zipper
selectPrev' (Zipper t z) = case z of
    TopLevel [] bs -> Nothing
    TopLevel (a:as) bs -> Just (Zipper a (TopLevel as (t:bs)))
    FunctionArg a b c' -> Nothing
    FunctionArgType a b c' -> Just (Zipper a (FunctionArg t b c'))
    FunctionBody a b c' -> Just (Zipper b (FunctionArgType a t c'))
    ApplicationFn a c' -> Nothing
    ApplicationArg a c' -> Just (Zipper a (ApplicationFn t c'))
    ConditionalCond a b c' -> Nothing
    ConditionalOptOne a b c' -> Just (Zipper a (ConditionalCond t b c'))
    ConditionalOptTwo a b c' -> Just (Zipper b (ConditionalCond a t c'))
    AssignmentId a b c' -> Nothing
    AssignmentType a b c' -> Just (Zipper a (AssignmentId t b c'))
    AssignmentVal a b c' -> Just (Zipper b (AssignmentType a t c'))
    FnTypeArg a c' -> Nothing
    FnTypeRet a c' -> Just (Zipper a (FnTypeArg t c'))

selectNext' :: Zipper -> Maybe Zipper
selectNext' (Zipper t z) = case z of
    TopLevel as [] -> Nothing
    TopLevel as (b:bs) -> Just (Zipper b (TopLevel (t:as) bs))
    FunctionArg a b c' -> Just (Zipper a (FunctionArgType t b c'))
    FunctionArgType a b c' -> Just (Zipper b (FunctionBody a t c'))
    FunctionBody a b c' -> Nothing
    ApplicationFn a c' -> Just (Zipper a (ApplicationArg t c'))
    ApplicationArg a c' -> Nothing
    ConditionalCond a b c' -> Just (Zipper a (ConditionalOptOne t b c'))
    ConditionalOptOne a b c' -> Just (Zipper b (ConditionalOptTwo a t c'))
    ConditionalOptTwo a b c' -> Nothing
    AssignmentId a b c' -> Just (Zipper a (AssignmentType t b c'))
    AssignmentType a b c' -> Just (Zipper b (AssignmentVal a t c'))
    AssignmentVal a b c' -> Nothing
    FnTypeArg a c' -> Just (Zipper a (FnTypeRet t c'))
    FnTypeRet a c' -> Nothing

goup' :: Zipper -> Maybe Zipper
goup' (Zipper t z) = case z of
    TopLevel _ _ -> Nothing
    FunctionArg a b c' -> Just (Zipper (FunctionTerm t a b) c')
    FunctionArgType a b c' -> Just (Zipper (FunctionTerm a t b) c')
    FunctionBody a b c' -> Just (Zipper (FunctionTerm a b t) c')
    ApplicationFn a c' -> Just (Zipper (ApplicationTerm t a) c')
    ApplicationArg a c' -> Just (Zipper (ApplicationTerm a t) c')
    ConditionalCond a b c' -> Just (Zipper (ConditionalTerm t a b) c')
    ConditionalOptOne a b c' -> Just (Zipper (ConditionalTerm a t b) c')
    ConditionalOptTwo a b c' -> Just (Zipper (ConditionalTerm a b t) c')
    AssignmentId a b c' -> Just (Zipper (Assignment t a b) c')
    AssignmentType a b c' -> Just (Zipper (Assignment a t b) c')
    AssignmentVal a b c' -> Just (Zipper (Assignment a b t) c')
    FnTypeArg a c' -> Just (Zipper (FnTypeTerm t a) c')
    FnTypeRet a c' -> Just (Zipper (FnTypeTerm a t) c')

