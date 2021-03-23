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

replaceWithStrType :: Zipper -> Zipper
replaceWithStrType = replaceWithType StringType

replaceWithBoolType :: Zipper -> Zipper
replaceWithBoolType = replaceWithType BooleanType

replaceWithIntType :: Zipper -> Zipper
replaceWithIntType = replaceWithType IntegerType

replaceWithFnType :: Zipper -> Zipper
replaceWithFnType = replaceWithType (FunctionType UnknownType UnknownType)

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

replaceWithTrue :: Zipper -> Zipper
replaceWithTrue = replaceWithValue (BooleanLiteral True)

replaceWithFalse :: Zipper -> Zipper
replaceWithFalse = replaceWithValue (BooleanLiteral False)

replaceWithCall :: Zipper -> Zipper
replaceWithCall = replaceWithValue (Call UnknownValue UnknownValue)

replaceWithFunction :: Zipper -> Zipper
replaceWithFunction z = case possibleFunctionDefinition z of
        a:_ -> replaceWithValue a z
        _ -> z

replaceWithOp :: Op -> Zipper -> Zipper
replaceWithOp op = replaceWithValue (BinaryOperator UnknownValue op UnknownValue)

replaceWithValue :: Value -> Zipper -> Zipper
replaceWithValue v z@(ZipperVal _ xs) = if valueTypeChecks z v
                                        then ZipperVal v xs
                                        else z
replaceWithValue _ z = z

possibleTypes :: Zipper -> [Type]
possibleTypes (ZipperTyp _ _) = [ FunctionType UnknownType UnknownType
                                , IntegerType
                                , BooleanType
                                , StringType
                                ]
possibleTypes _ = []

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
    searchAbove = case goup' enclosingStatement of
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

returnType :: Zipper -> Type
returnType z = case goToEnclosingFunction z of
        Just y -> case expectedType y of
          FunctionType _ t -> t
          _ -> UnknownType -- wtf? this should never happen. weakness in current datatypes
        Nothing -> StringType -- if not in a function definition, must be a string. for output. i guess

expectedType :: Zipper -> Type
expectedType (ZipperVal _ (CallName _ _)) = UnknownType
expectedType z@(ZipperVal _ (CallArgs f _)) = case typeOf z f of
        FunctionType as _ -> as
        _ -> UnknownType
expectedType (ZipperVal _ (AssignVal (Declare _ t) _)) = t
expectedType (ZipperVal _ (OpFirst op _ _)) = case op of
        Add -> IntegerType
        Multiply -> IntegerType
        GreaterThan -> IntegerType
        LessThan -> IntegerType
        Mod -> IntegerType
        Equal -> UnknownType
        And -> BooleanType
        Or -> BooleanType
expectedType (ZipperVal _ (OpSecond _ op _)) = case op of
        Add -> IntegerType
        Multiply -> IntegerType
        GreaterThan -> IntegerType
        LessThan -> IntegerType
        Mod -> IntegerType
        Equal -> UnknownType
        And -> BooleanType
        Or -> BooleanType
expectedType _ = UnknownType

typesAreEqual :: Type -> Type -> Bool
typesAreEqual UnknownType _ = True
typesAreEqual _ UnknownType = True
typesAreEqual (FunctionType as r) (FunctionType bs s) = (typesAreEqual as bs) && (typesAreEqual r s)
typesAreEqual a b = a == b

-- create a function that could fit in a given zipper
-- why is it a list? just to fit inside allPossibleValues, cuz I'm lazy atm and
-- not doing smart things
possibleFunctionDefinition :: Zipper -> [Value]
possibleFunctionDefinition z = case expectedType z of 
    FunctionType args _ -> [Function (typeListToDeclList args) UnknownValue]
    _ -> []
  where
    typeListToDeclList t = Declare UnknownName t

-- include function calls
-- include functions with appropriate arguments
allPossibleValues :: Zipper -> [Value]
allPossibleValues z = fmap Variable (searchForNamedVariables z) ++ standardValues ++ possibleFunctionDefinition z ++ validFunctionCalls z

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

