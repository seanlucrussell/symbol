{-# LANGUAGE OverloadedStrings #-}
module Transformations where

import SymbolData
import Movements

import qualified Data.Text as T

import Data.List
import Data.Maybe
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

insertBefore :: Zipper -> Zipper
insertBefore (ZipperAs s (TopLevel a b)) = ZipperAs s (TopLevel (Assign (Declare UnknownName UnknownType) UnknownValue:a) b) .- selectPrev
insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter (ZipperAs s (TopLevel a b)) = ZipperAs s (TopLevel a (Assign (Declare UnknownName UnknownType) UnknownValue:b)) .- selectNext
insertAfter z = z .- goup .- insertAfter

replaceWithType :: Type -> Zipper -> Zipper
replaceWithType t (ZipperTyp _ xs) = ZipperTyp t xs
replaceWithType _ z = z

replaceWithName :: T.Text -> Zipper -> Zipper
replaceWithName t (ZipperNam _ xs) = ZipperNam (Name t) xs
replaceWithName _ z = z

replaceWithInt :: Integer -> Zipper -> Zipper
replaceWithInt i = replaceWithValue (IntLiteral i)

replaceWithString :: T.Text -> Zipper -> Zipper
replaceWithString t = replaceWithValue (StringLiteral t)

replaceWithVariable :: T.Text -> Zipper -> Zipper
replaceWithVariable v = replaceWithValue (Variable v)

replaceWithValue :: Value -> Zipper -> Zipper
replaceWithValue v z@(ZipperVal _ xs) = if valueTypeChecks z v
                                        then ZipperVal v xs
                                        else z
replaceWithValue _ z = z

typeTypeChecks :: Zipper -> Type -> Bool
typeTypeChecks z t = typesAreEqual (expectedType z) t

allPossibleTypes :: Zipper -> [Type]
allPossibleTypes _ = [ FunctionType UnknownType UnknownType
                     , IntegerType
                     , BooleanType
                     , StringType
                     ]

possibleTypes :: Zipper -> [Type]
possibleTypes z = filter (typeTypeChecks z) (allPossibleTypes z)

searchForNamedVariables :: Zipper -> [T.Text]
-- go up to enclosing Block or Top
-- for each previous item in that enclosing Block or Top, check if the thing is
-- an assignment
-- if it is an assignment, check if it is already in the list
-- if not, add it to the list
-- continue searching
-- NEED TO INCLUDE FUNCTION DEFINITIONS!!!
-- ALSO NEED TO REMOVE DUPLICATE WORDS!!!
-- supper inefficient and poorly written, but it works for now
searchForNamedVariables z = sort (nub (searchAbove ++ searchBefore ++ current ++ args))
  where
    extractNameFromDeclList (Declare (Name n) _) = [n]
    extractNameFromDeclList (Declare UnknownName _) = []
    args = case goToEnclosingFunction z of
        Just (ZipperVal (Function l _) _) -> extractNameFromDeclList l
        _ -> []
    extractNameFromZipper w = case w of
        ZipperAs s _ -> extractName s
        _ -> Nothing
    extractName s = case s of
        Assign (Declare (Name n) _) _ -> Just n
        _ -> Nothing
    enclosingStatement = goToEnclosingStatement z
    previousStatements = case enclosingStatement of
        ZipperAs _ (TopLevel p _) -> p
        _ -> []
    current = case extractNameFromZipper enclosingStatement of
        Just n -> [n]
        Nothing -> []
    searchBefore = catMaybes (fmap extractName previousStatements)
    searchAbove = case goToEnclosingFunction z >>= goup' of
        Just z' -> searchForNamedVariables z'
        Nothing -> []

standardValues :: [Value]
standardValues = [ BooleanLiteral True
                 , BooleanLiteral False
                 , Function (Declare UnknownName UnknownType) UnknownValue
                 , BinaryOperator UnknownValue Add UnknownValue
                 , BinaryOperator UnknownValue Multiply UnknownValue
                 , BinaryOperator UnknownValue GreaterThan UnknownValue
                 , BinaryOperator UnknownValue LessThan UnknownValue
                 , BinaryOperator UnknownValue Equal UnknownValue
                 , BinaryOperator UnknownValue Mod UnknownValue
                 , BinaryOperator UnknownValue And UnknownValue
                 , BinaryOperator UnknownValue Or UnknownValue]

-- i think the simplest way to enforce types is to add a function
-- need to include position in zipper for context. even though most things are
-- context independent (only really need variable)
typeOf :: Zipper -> Value -> Type
typeOf _ (StringLiteral _) = StringType
typeOf _ (IntLiteral _) = IntegerType
typeOf _ (BooleanLiteral _) = BooleanType
typeOf f (Variable w) = fromMaybe UnknownType (searchForVariableType f w)
  where
   searchForVariableType z v = searchAbove <|> searchBefore <|> current <|> args
    where 
    extractTypeFromDeclList (Declare (Name n) t) = if n == v then Just t else Nothing
    extractTypeFromDeclList (Declare UnknownName _) = Nothing
    args = case goToEnclosingFunction z of
        Just (ZipperVal (Function l _) _) -> extractTypeFromDeclList l
        _ -> Nothing
    extractTypeFromZipper y = case y of
        ZipperAs s _ -> extractType s
        _ -> Nothing
    extractType s = case s of
        Assign (Declare (Name n) u) _ -> if n == v then Just u else Nothing
        _ -> Nothing
    enclosingStatement = goToEnclosingStatement z
    previousStatements = case enclosingStatement of
        ZipperAs _ (TopLevel p _) -> p
        _ -> []
    current = extractTypeFromZipper enclosingStatement
    -- searchBefore = catMaybes (fmap extractType previousStatements)
    searchBefore = foldl (<|>) Nothing (fmap extractType previousStatements)
    searchAbove = case goup' enclosingStatement of
        Just z' -> searchForVariableType z' v
        Nothing -> Nothing
typeOf z (Function (Declare _ t) r) = FunctionType t (typeOf z r)
typeOf z (Call f _) = case typeOf z f of
        FunctionType _ t -> t
        _ -> UnknownType
typeOf _ (UnknownValue) = UnknownType
typeOf _ (BinaryOperator _ op _) = case op of
        Add -> IntegerType
        Multiply -> IntegerType
        Mod -> IntegerType
        And -> BooleanType
        Or -> BooleanType
        Equal -> BooleanType
        LessThan -> BooleanType
        GreaterThan -> BooleanType

expectedType :: Zipper -> Type
expectedType z@(ZipperVal _ ts) = case ts of
        CallName _ _ -> UnknownType -- WIP
        FnBody _ _ -> case expectedType (goup z) of
            FunctionType _ t -> t
            _ -> UnknownType -- WIP. should be type of the return value of the parent function
        CallArgs f _ -> case typeOf z f of
            FunctionType as _ -> as
            _ -> UnknownType -- WIP
        AssignVal (Declare _ t) _ -> t
        OpFirst op b _ -> case op of
            Add -> IntegerType
            Multiply -> IntegerType
            GreaterThan -> IntegerType
            LessThan -> IntegerType
            Mod -> IntegerType
            Equal -> typeOf z b
            And -> BooleanType
            Or -> BooleanType
        OpSecond a op _ -> case op of
            Add -> IntegerType
            Multiply -> IntegerType
            GreaterThan -> IntegerType
            LessThan -> IntegerType
            Mod -> IntegerType
            Equal -> typeOf z a
            And -> BooleanType
            Or -> BooleanType
expectedType z@(ZipperTyp _ ts) = case ts of
        DeclareType _ _ -> case goup (goup z) of
            z@(ZipperVal (Function _ _) _) -> case expectedType z of
                FunctionType t _ -> t
                _ -> UnknownType
            ZipperAs (Assign _ t) _ -> typeOf z t
            _ -> UnknownType
        FnTypeArgs _ _ -> case expectedType (goup z) of
            FunctionType t _ -> t
            _ -> UnknownType
        FnTypeRet _ _ -> case expectedType (goup z) of
            FunctionType _ t -> t
            _ -> UnknownType
expectedType _ = UnknownType

typesAreEqual :: Type -> Type -> Bool
typesAreEqual UnknownType _ = True
typesAreEqual _ UnknownType = True
typesAreEqual (FunctionType as r) (FunctionType bs s) = (typesAreEqual as bs) && (typesAreEqual r s)
typesAreEqual a b = a == b

possibleFunctionDefinitions :: Zipper -> [Value]
possibleFunctionDefinitions z = case expectedType z of 
    FunctionType args _ -> [Function (Declare UnknownName args) UnknownValue]
    _ -> []

allPossibleValues :: Zipper -> [Value]
allPossibleValues z = fmap Variable
                         (searchForNamedVariables z)
                      ++ standardValues
                      ++ possibleFunctionDefinitions z
                      ++ validFunctionCalls z

validFunctionCalls :: Zipper -> [Value]
validFunctionCalls z = functionCalls
  where
    names = searchForNamedVariables z
    nameFilter n = case typeOf z (Variable n) of
        FunctionType _ t -> typesAreEqual t (expectedType z)
        _ -> False
    goodNames = filter nameFilter names
    functionCalls = fmap (nameToFunctionCall . Variable) goodNames
    nameToFunctionCall name = case typeOf z name of
        FunctionType _ _ -> Call name UnknownValue
        _ -> UnknownValue -- this should never trigger. type weakness
  
-- check if a given value typechecks
valueTypeChecks :: Zipper -> Value -> Bool
valueTypeChecks z = typesAreEqual (expectedType z) . typeOf z

possibleValues :: Zipper -> [Value]
possibleValues z = filter (valueTypeChecks z) (allPossibleValues z)
