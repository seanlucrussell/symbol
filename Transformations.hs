{-# LANGUAGE OverloadedStrings #-}
module Transformations where

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

goToEnclosingStatement :: Zipper -> Zipper
goToEnclosingStatement z@(ZipperAs _ (TopLevel _ _))   = z
goToEnclosingStatement z = case goup' z of
        Just z' -> goToEnclosingStatement z'
        Nothing -> z

goToEnclosingFunction :: Zipper -> Maybe Zipper
goToEnclosingFunction z@(ZipperVal (Function _ _) _) = Just z
goToEnclosingFunction z = goup' z >>= goToEnclosingFunction

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

