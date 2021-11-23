{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( validateProgram ) where

import SymbolData
import AST
import Utilities

import qualified Data.Map as M

{-# LANGUAGE XOverloadedStrings #-}

chooseMoreSpecificType :: Type -> Type -> Type
chooseMoreSpecificType UnknownType u = u
chooseMoreSpecificType t           _ = t

data Type = BooleanType
          | FunctionType Type Type
          | UnknownType
          deriving (Eq,Show)
data Trm = Ref Int
         | T
         | F
         | Fn Id Type Trm
         | App Trm Trm
         | Cond Trm Trm Trm
         | Unknown
          deriving (Eq,Show)
data Assign = Assign Id Type Trm
          deriving (Eq,Show)
data Id = Id Int | UnknownId
          deriving (Eq,Show)
data Prog = Prog [Assign]
          deriving (Eq,Show)

valId :: Context -> Token -> Maybe Id
valId _ (IdentifierTerm t) = Just (Id t)
valId _ (UnknownTerm)        = Just UnknownId
valId _ _                            = Nothing

valType :: Context -> Token -> Maybe Type
valType _ (BoolTypeTerm) = Just BooleanType
valType _ (UnknownTerm) = Just UnknownType
valType c (FunctionTypeTerm a b) = do a' <- valType c a
                                      b' <- valType c b
                                      return (FunctionType a' b')
valType _ _ = Nothing

valTrm :: Context -> Token -> Maybe Trm
valTrm c (IdentifierTerm t) = do _ <- M.lookup t c
                                 return (Ref t)
valTrm c (UnknownTerm) = Just Unknown
valTrm c (TrueTerm) = Just T
valTrm c (FalseTerm) = Just F
valTrm c (ApplicationTerm x y) = do x' <- valTrm c x
                                    y' <- valTrm c y
                                    xt <- typeOf c x
                                    yt <- typeOf c y
                                    case xt of
                                         FunctionType a b -> if typeEquality a yt then return (App x' y') else Nothing
                                         UnknownType -> return (App x' y')
                                         _ -> Nothing
valTrm c (ConditionalTerm x y z) = do x' <- valTrm c x
                                      y' <- valTrm c y
                                      z' <- valTrm c z
                                      xt <- typeOf c x
                                      yt <- typeOf c y
                                      zt <- typeOf c z
                                      case xt of
                                         BooleanType -> if typeEquality yt zt then return (Cond x' y' z') else Nothing
                                         UnknownType -> if typeEquality yt zt then return (Cond x' y' z') else Nothing
                                         _ -> Nothing
valTrm c (FunctionTerm x y z) = do x' <- valId c x
                                   y' <- valType c y
                                   newContext <- updateContext x' y' c
                                   z' <- valTrm newContext z
                                   return (Fn x' y' z')
valTrm _ _ = Nothing

valAssign :: Context -> Token -> Maybe Assign
valAssign c (AssignmentTerm x y z) = do x' <- valId c x
                                        y' <- valType c y
                                        z' <- valTrm c z
                                        zt <- typeOf c z
                                        if typeEquality y' zt then return (Assign x' y' z') else Nothing
valAssign _ _ = Nothing

valAssigns :: Context -> [Token] -> Maybe [Assign]
valAssigns _ [] = Just []
valAssigns c (a:as) = do a'@(Assign i t b) <- valAssign c a
                         c' <- updateContext i t c
                         as' <- valAssigns c' as
                         return (a':as')
                         

valProg :: Context -> Token -> Maybe Prog
valProg c (Program as) = do as' <- valAssigns c as
                            return (Prog as')
valProg _ _ = Nothing

validateProgram :: Token -> Bool
validateProgram = maybeToBool . valProg emptyContext

typeEquality :: Type -> Type -> Bool
typeEquality UnknownType _ = True
typeEquality _ UnknownType = True
typeEquality BooleanType BooleanType = True
typeEquality (FunctionType a b) (FunctionType c d) = typeEquality a c && typeEquality b d
typeEquality _ _ = False



-- returns Nothing if the type checking is invalid
typeOf :: Context -> Token -> Maybe Type
typeOf c ((IdentifierTerm i)) = M.lookup i c
--typeOf c (FunctionTerm [UnknownTerm, t, b]) = do argType <- valType c t
--                                                              bodyType <- typeOf c b
--                                                              return (FunctionType argType bodyType)
typeOf c (FunctionTerm i t b) = do i' <- valId c i
                                   argType <- valType c t
                                   newContext <- updateContext i' argType c
                                   bodyType <- typeOf newContext b
                                   return (FunctionType argType bodyType)
typeOf c (ApplicationTerm f x) = do fType <- typeOf c f
                                    xType <- typeOf c x
                                    case fType of   
                                      FunctionType a b -> if typeEquality xType a then return b else Nothing
                                      UnknownType -> Just UnknownType
                                      _ -> Nothing
typeOf _ TrueTerm = Just BooleanType
typeOf _ FalseTerm = Just BooleanType
typeOf c (ConditionalTerm b x y) = do bType <- typeOf c b
                                      xType <- typeOf c x
                                      yType <- typeOf c y
                                      if typeEquality bType BooleanType && typeEquality xType yType
                                         then typeOf c x -- what if x is UnknownType but y isn't?
                                         else Nothing
typeOf _ UnknownTerm = Just UnknownType
typeOf _ _ = Nothing

type Context = M.Map Int Type

emptyContext :: Context
emptyContext = M.empty

updateContext :: Id -> Type -> Context -> Maybe Context
updateContext (Id i)    t = Just . M.insert i t
updateContext UnknownId _ = Just
