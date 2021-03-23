{-# LANGUAGE OverloadedStrings #-}
module Movements where

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

selectFirst' :: Zipper -> Maybe Zipper
selectFirst' (ZipperDec (Declare n t) ts) = Just $ ZipperNam n (DeclareName t ts)
selectFirst' (ZipperVal value ts) = case value of
    Function d v -> Just $ ZipperDec d (FnArgs v ts)
    Call f a                    -> Just $ ZipperVal f (CallName a ts)
    BinaryOperator a op b       -> Just $ ZipperVal a (OpFirst op b ts)
    StringLiteral _             -> Nothing
    IntLiteral _                -> Nothing
    BooleanLiteral _            -> Nothing
    Variable _                  -> Nothing
    UnknownValue                -> Nothing
selectFirst' (ZipperAs (Assign d v) ts) = Just $ ZipperDec d (AssignDecl v ts)
selectFirst' (ZipperNam _ _)     = Nothing
selectFirst' (ZipperTyp t ts) = case t of
    FunctionType a r       -> Just $ ZipperTyp a (FnTypeArgs r ts)
    StringType                  -> Nothing
    BooleanType                 -> Nothing
    IntegerType                 -> Nothing
    UnknownType                 -> Nothing

selectPrev' :: Zipper -> Maybe Zipper
selectPrev' (ZipperNam _ _) = Nothing
selectPrev' (ZipperTyp t parent) = case parent of
    DeclareType n ts -> Just $ ZipperNam n (DeclareName t ts)
    FnTypeArgs _ _ -> Nothing
    FnTypeRet u ts -> Just $ ZipperTyp u (FnTypeArgs t ts)
selectPrev' (ZipperDec _ _) = Nothing
selectPrev' (ZipperAs s parent) = case parent of
    TopLevel (a:as) b          -> Just $ ZipperAs a (TopLevel as (s:b))
    TopLevel [] _              -> Nothing
selectPrev' (ZipperVal v parent) = case parent of
    AssignVal d ts -> Just $ ZipperDec d (AssignDecl v ts)
    -- AssignVal (Declare n t) ts -> Just $ ZipperTyp t (DeclareType n (AssignDecl v ts))
    CallArgs f ts        -> Just $ ZipperVal f (CallName v ts)
    FnBody d ts -> Just $ ZipperDec d (FnArgs v ts)
    OpSecond a op ts           -> Just $ ZipperVal a (OpFirst op v ts)
    CallName _ _               -> Nothing
    OpFirst _ _ _              -> Nothing

selectNext' :: Zipper -> Maybe Zipper
selectNext' (ZipperNam n (DeclareName t ts)) = Just $ ZipperTyp t (DeclareType n ts)
selectNext' (ZipperDec d parent) = case parent of
    AssignDecl v ts                  -> Just $ ZipperVal v (AssignVal d ts)
    FnArgs v ts -> Just $ ZipperVal v (FnBody d ts)
selectNext' (ZipperTyp t parent) = case parent of
--    DeclareType n (AssignDecl v ts)  -> Just $ ZipperVal v (AssignVal (Declare n t) ts)
--    DeclareType n (FnArgs v ts) -> Just $ ZipperVal v (FnBody (Declare n t) ts)
    DeclareType _ _ -> Nothing
    FnTypeArgs u ts -> Just $ ZipperTyp u (FnTypeRet t ts)
    FnTypeRet _ _ -> Nothing
selectNext' (ZipperAs s parent) = case parent of
    TopLevel a (b:bs)                -> Just $ ZipperAs b (TopLevel (s:a) bs)
    TopLevel _ []                    -> Nothing
selectNext' (ZipperVal v parent) = case parent of
    OpFirst op b ts                  -> Just $ ZipperVal b (OpSecond v op ts)
    CallName a ts                    -> Just $ ZipperVal a (CallArgs v ts)
    FnBody _ _                    -> Nothing
    CallArgs _ _                -> Nothing
    AssignVal _ _                    -> Nothing
    OpSecond _ _ _                   -> Nothing

zipperOnHole :: Zipper -> Bool
zipperOnHole (ZipperVal UnknownValue _) = True
zipperOnHole (ZipperTyp UnknownType _) = True
zipperOnHole (ZipperNam UnknownName _) = True
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

goup' :: Zipper -> Maybe Zipper
goup' (ZipperNam n parent) = case parent of
    DeclareName t ts -> Just $ ZipperDec (Declare n t) ts
    -- DeclareName t (AssignDecl v ts) -> Just $ ZipperAs (Assign (Declare n t) v) ts
    -- DeclareName t (FnArgs a ts) -> Just $ ZipperVal (Function (Declare n t) a) ts
goup' (ZipperTyp t parent) = case parent of
    DeclareType n ts -> Just $ ZipperDec (Declare n t) ts
    -- DeclareType n (AssignDecl v ts) -> Just $ ZipperAs (Assign (Declare n t) v) ts
    -- DeclareType n (FnArgs a ts) -> Just $ ZipperVal (Function (Declare n t) a) ts
    FnTypeArgs a ts -> Just $ ZipperTyp (FunctionType a t) ts
    FnTypeRet a ts -> Just $ ZipperTyp (FunctionType a t) ts
goup' (ZipperDec d parent) = case parent of
    FnArgs b ts -> Just $ ZipperVal (Function d b) ts
    AssignDecl v ts -> Just $ ZipperAs (Assign d v) ts
goup' (ZipperVal v parent) = case parent of
    CallName a ts -> Just $ ZipperVal (Call v a) ts
    CallArgs f ts -> Just $ ZipperVal (Call f v) ts
    FnBody d ts -> Just $ ZipperVal (Function d v) ts
    AssignVal d ts -> Just $ ZipperAs (Assign d v) ts
    OpFirst op b ts -> Just $ ZipperVal (BinaryOperator v op b) ts
    OpSecond a op ts -> Just $ ZipperVal (BinaryOperator a op v) ts
goup' (ZipperAs _ _) = Nothing

goToEnclosingStatement :: Zipper -> Zipper
goToEnclosingStatement z@(ZipperAs _ (TopLevel _ _))   = z
goToEnclosingStatement z = case goup' z of
        Just z' -> goToEnclosingStatement z'
        Nothing -> z

goToEnclosingFunction :: Zipper -> Maybe Zipper
goToEnclosingFunction z@(ZipperVal (Function _ _) _) = Just z
goToEnclosingFunction z = goup' z >>= goToEnclosingFunction
