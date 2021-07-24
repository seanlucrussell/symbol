{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( validateZipper
  , zipperToTerm
  , z) where

import SymbolData

import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Type = BooleanType
          | FunctionType Type Type
          | UnknownType
          deriving (Eq,Show)

-- checks that a whole program is specified correctly
validateProgram :: Term -> Bool
validateProgram (Program t) = validateProgram' baseContext t
        where validateProgram' c (Assignment (IdentifierTerm a) t' b:ts) = case typeOf c b of
                Just t -> case termToType t' of
                        Just t'' -> if typeEquality t t''
                                    then if t == UnknownType
                                         then validateProgram' (updateContext c a t'') ts
                                         else validateProgram' (updateContext c a t) ts
                                    else False
                        Nothing -> False
                Nothing -> False
              validateProgram' c (Assignment UnknownTerm t' b:ts) = case typeOf c b of
                Just t -> case termToType t' of
                        Just t'' -> if typeEquality t t'' then validateProgram' c ts else False
                        Nothing -> False
                Nothing -> False
              validateProgram' _ [] = True
              validateProgram' _ _ = False
validateProgram _ = False

type Context = T.Text -> Type

updateContext :: Context -> T.Text -> Type -> Context
updateContext c i t = \n -> if n == i then t else c n

baseContext :: Context
baseContext = \x -> UnknownType

typeEquality :: Type -> Type -> Bool
typeEquality UnknownType _ = True
typeEquality _ UnknownType = True
typeEquality BooleanType BooleanType = True
typeEquality (FunctionType a b) (FunctionType c d) = typeEquality a c && typeEquality b d
typeEquality _ _ = False

-- maps syntactic terms like FnTermType into the corresponding type representation
termToType :: Term -> Maybe Type
termToType (FnTypeTerm a b) = do a' <- termToType a
                                 b' <- termToType b
                                 return (FunctionType a' b')
termToType BoolTypeTerm = Just BooleanType
termToType UnknownTerm = Just UnknownType
termToType _ = Nothing

-- returns Nothing if the type checking is invalid
typeOf :: Context -> Term -> Maybe Type
typeOf c (IdentifierTerm i) = Just (c i)
typeOf c (FunctionTerm UnknownTerm t b) = do argType <- termToType t
                                             bodyType <- typeOf c b
                                             return (FunctionType argType bodyType)
typeOf c (FunctionTerm (IdentifierTerm i) t b) = do argType <- termToType t
                                                    bodyType <- typeOf (updateContext c i argType) b
                                                    return (FunctionType argType bodyType)
typeOf c (ApplicationTerm f x) = do fType <- typeOf c f
                                    xType <- typeOf c x
                                    case fType of   
                                      FunctionType a b -> if typeEquality xType a then return b else Nothing
                                      UnknownType -> Just UnknownType
                                      _ -> Nothing
typeOf _ (BooleanLiteralTerm _) = Just BooleanType
typeOf c (ConditionalTerm b x y) = do bType <- typeOf c b
                                      xType <- typeOf c x
                                      yType <- typeOf c y
                                      if typeEquality bType BooleanType && typeEquality xType yType
                                         then typeOf c x -- what if x is UnknownType but y isn't?
                                         else Nothing
typeOf _ UnknownTerm = Just UnknownType
typeOf _ _ = Nothing

validateZipper :: Zipper -> Bool
validateZipper = validateProgram . zipperToTerm
