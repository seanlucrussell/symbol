{-# LANGUAGE OverloadedStrings #-}
module STLC.TypeChecker
  ( validateProgram ) where

import STLC.Data
import AST
import Utilities

import Control.Monad

data Type = BoolT
          | FunctionT Type Type
          | UnknownT
          deriving (Eq,Show)
data Trm = Ref Int
         | T
         | F
         | Fn Type Trm
         | App Trm Trm
         | Cond Trm Trm Trm
         | UnknownTrm
          deriving (Eq,Show)
data Assign = Assign Type Trm Assign | EOP
          deriving (Eq,Show)

chooseMoreSpecificType :: Type -> Type -> Type
chooseMoreSpecificType UnknownT t = t
chooseMoreSpecificType t        _ = t

valType :: Context -> Token -> Maybe Type
valType _ BoolType = Just BoolT
valType _ Unknown = Just UnknownT
valType c (FunctionType a b) = do a' <- valType c a
                                  b' <- valType c b
                                  return (FunctionT a' b')
valType _ _ = Nothing

valTrm :: Context -> Token -> Maybe Trm
valTrm c (Identifier t) = safeListIndex t c >> return (Ref t)
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
valTrm c (Function (Name _) y z) = do y' <- valType c y
                                      z' <- valTrm (updateContext y' c) z
                                      return (Fn y' z')
valTrm _ _ = Nothing

valAssign :: Context -> Token -> Maybe Assign
valAssign _ EndOfProgram = Just EOP
valAssign c (Assignment (Name _) y z w) = do y' <- valType c y
                                             z' <- valTrm c z
                                             zt <- typeOf c z
                                             next <- valAssign (updateContext y' c) w
                                             guard (typeEquality y' zt)
                                             return (Assign y' z' next)
valAssign _ _ = Nothing

validateProgram :: Token -> Bool
validateProgram = maybeToBool . valAssign emptyContext

typeEquality :: Type -> Type -> Bool
typeEquality UnknownT _ = True
typeEquality _ UnknownT = True
typeEquality BoolT BoolT = True
typeEquality (FunctionT a b) (FunctionT c d) = typeEquality a c && typeEquality b d
typeEquality _ _ = False

-- returns Nothing if the type checking is invalid
typeOf :: Context -> Token -> Maybe Type
typeOf c (Identifier i) = safeListIndex i c
typeOf c (Function (Name _) t b) = do argType <- valType c t
                                      bodyType <- typeOf (updateContext argType c) b
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

type Context = [Type]

emptyContext :: Context
emptyContext = []

updateContext :: Type -> Context -> Context
updateContext = (:)
