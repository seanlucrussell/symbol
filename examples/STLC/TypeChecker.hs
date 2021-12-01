{-# LANGUAGE OverloadedStrings #-}
module STLC.TypeChecker
  ( validateProgram ) where

import STLC.SymbolData
import AST
import Utilities

import Control.Monad
import qualified Data.Map as M

data Type = BoolT
          | FunctionT Type Type
          | UnknownT
          deriving (Eq,Show)
data Trm = Ref Int
         | T
         | F
         | Fn Id Type Trm
         | App Trm Trm
         | Cond Trm Trm Trm
         | UnknownTrm
          deriving (Eq,Show)
data Assign = Assign Id Type Trm
          deriving (Eq,Show)
data Id = Id Int | UnknownId
          deriving (Eq,Show)
data Prog = Prog [Assign]
          deriving (Eq,Show)

chooseMoreSpecificType :: Type -> Type -> Type
chooseMoreSpecificType UnknownT t = t
chooseMoreSpecificType t        _ = t

valId :: Context -> Token -> Maybe Id
valId _ (Identifier t) = Just (Id t)
valId _ Unknown        = Just UnknownId
valId _ _              = Nothing

valType :: Context -> Token -> Maybe Type
valType _ BoolType = Just BoolT
valType _ Unknown = Just UnknownT
valType c (FunctionType a b) = do a' <- valType c a
                                  b' <- valType c b
                                  return (FunctionT a' b')
valType _ _ = Nothing

valTrm :: Context -> Token -> Maybe Trm
valTrm c (Identifier t) = M.lookup t c >> return (Ref t)
valTrm c (Unknown) = Just UnknownTrm
valTrm c (TrueTerm) = Just T
valTrm c (FalseTerm) = Just F
valTrm c (Application x y) = do x' <- valTrm c x
                                y' <- valTrm c y
                                xt <- typeOf c x
                                yt <- typeOf c y
                                case xt of
                                     FunctionT a b -> if typeEquality a yt then return (App x' y') else Nothing
                                     UnknownT -> return (App x' y')
                                     _ -> Nothing
valTrm c (Conditional x y z) = do x' <- valTrm c x
                                  y' <- valTrm c y
                                  z' <- valTrm c z
                                  xt <- typeOf c x
                                  yt <- typeOf c y
                                  zt <- typeOf c z
                                  guard (xt == BoolT || xt == UnknownT)
                                  guard (typeEquality yt zt)
                                  return (Cond x' y' z')
valTrm c (Function x y z) = do x' <- valId c x
                               y' <- valType c y
                               newContext <- updateContext x' y' c
                               z' <- valTrm newContext z
                               return (Fn x' y' z')
valTrm _ _ = Nothing

valAssign :: Context -> Token -> Maybe Assign
valAssign c (Assignment x y z) = do x' <- valId c x
                                    y' <- valType c y
                                    z' <- valTrm c z
                                    zt <- typeOf c z
                                    guard (typeEquality y' zt)
                                    return (Assign x' y' z')
valAssign _ _ = Nothing

valAssigns :: Context -> [Token] -> Maybe [Assign]
valAssigns _ [] = Just []
valAssigns c (a:as) = do a'@(Assign i t b) <- valAssign c a
                         c' <- updateContext i t c
                         as' <- valAssigns c' as
                         return (a':as')
                         
valProg :: Context -> Token -> Maybe Prog
valProg c (Program as) = fmap Prog (valAssigns c as)
valProg _ _ = Nothing

validateProgram :: Token -> Bool
validateProgram = maybeToBool . valProg emptyContext

typeEquality :: Type -> Type -> Bool
typeEquality UnknownT _ = True
typeEquality _ UnknownT = True
typeEquality BoolT BoolT = True
typeEquality (FunctionT a b) (FunctionT c d) = typeEquality a c && typeEquality b d
typeEquality _ _ = False

-- returns Nothing if the type checking is invalid
typeOf :: Context -> Token -> Maybe Type
typeOf c (Identifier i) = M.lookup i c
typeOf c (Function i t b) = do i' <- valId c i
                               argType <- valType c t
                               newContext <- updateContext i' argType c
                               bodyType <- typeOf newContext b
                               return (FunctionT argType bodyType)
typeOf c (Application f x) = do fType <- typeOf c f
                                xType <- typeOf c x
                                case fType of
                                  FunctionT a b -> if typeEquality xType a then return b else Nothing
                                  UnknownT -> Just UnknownT
                                  _ -> Nothing
typeOf _ TrueTerm = Just BoolT
typeOf _ FalseTerm = Just BoolT
typeOf c (Conditional b x y) = do bType <- typeOf c b
                                  xType <- typeOf c x
                                  yType <- typeOf c y
                                  guard (typeEquality bType BoolT)
                                  guard (typeEquality xType yType)
                                  typeOf c x
typeOf _ Unknown = Just UnknownT
typeOf _ _ = Nothing

type Context = M.Map Int Type

emptyContext :: Context
emptyContext = M.empty

updateContext :: Id -> Type -> Context -> Maybe Context
updateContext (Id i)    t = Just . M.insert i t
updateContext UnknownId _ = Just
