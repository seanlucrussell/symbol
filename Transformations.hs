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
selectFirst' (ZipperDec (Declare n t) ts)
                                 = Just $ ZipperNam n (DeclareName t ts)
selectFirst' (ZipperVal value ts) = case value of
    Function (Declare n t:as) b -> Just $ ZipperNam n (DeclareName t (FnArgs [] as b ts))
    Function [] b               -> Just $ ZipperSt  b (FnBody [] ts) 
    Call f a                    -> Just $ ZipperVal f (CallName a ts)
    BinaryOperator a op b       -> Just $ ZipperVal a (OpFirst op b ts)
    StringLiteral _             -> Nothing
    IntLiteral _                -> Nothing
    BooleanLiteral _            -> Nothing
    Variable _                  -> Nothing
    UnknownValue                -> Nothing
selectFirst' (ZipperSt statement ts) = case statement of
    Assign (Declare n t) v      -> Just $ ZipperNam n (DeclareName t (AssignDecl v ts))
    Block (b:bs)                -> Just $ ZipperSt  b (Blk [] bs ts)
    Block []                    -> Just $ ZipperSt  statement ts
    Return v                    -> Just $ ZipperVal v (Ret ts)
    If v b                      -> Just $ ZipperVal v (IfCond b ts)
    UnknownStatement            -> Nothing
selectFirst' (ZipperNam _ _)     = Nothing
selectFirst' (ZipperTyp t ts) = case t of
    FunctionType (a:as) r       -> Just $ ZipperTyp a (FnTypeArgs [] as r ts)
    FunctionType [] r           -> Just $ ZipperTyp r (FnTypeRet [] ts)
    StringType                  -> Nothing
    BooleanType                 -> Nothing
    IntegerType                 -> Nothing
    UnknownType                 -> Nothing

selectPrev' :: Zipper -> Maybe Zipper
selectPrev' (ZipperNam n parent) = case parent of
    DeclareName _ (FnArgs [] _ _ _) -> Nothing
    DeclareName t (FnArgs (Declare m u:as) b s ts) -> Just $ ZipperTyp u (DeclareType m (FnArgs as (Declare n t:b) s ts))
    DeclareName _ (AssignDecl _ _) -> Nothing
selectPrev' (ZipperTyp t parent) = case parent of
    DeclareType n ts -> Just $ ZipperNam n (DeclareName t ts)
    FnTypeArgs (a:as) b r ts -> Just $ ZipperTyp a (FnTypeArgs as (t:b) r ts)
    FnTypeRet (a:as) ts -> Just $ ZipperTyp a (FnTypeArgs as [] t ts)
    FnTypeArgs [] _ _ _ -> Nothing
    FnTypeRet [] _ -> Nothing
selectPrev' (ZipperDec d parent) = case parent of 
    FnArgs (a:as) b s ts       -> Just $ ZipperDec a (FnArgs as (d:b) s ts)
    FnArgs [] _ _ _            -> Nothing
    AssignDecl _ _             -> Nothing
selectPrev' (ZipperSt s parent) = case parent of
    IfBody v ts                -> Just $ ZipperVal v (IfCond s ts)
    Blk (a:as) b ts            -> Just $ ZipperSt  a (Blk as (s:b) ts)
    TopLevel (a:as) b          -> Just $ ZipperSt  a (TopLevel as (s:b))
    FnBody (Declare n t:as) ts -> Just $ ZipperTyp t (DeclareType n (FnArgs as [] s ts))
    Blk [] _ _                 -> Nothing
    TopLevel [] _              -> Nothing
    FnBody [] _                -> Nothing
selectPrev' (ZipperVal v parent) = case parent of
    AssignVal (Declare n t) ts -> Just $ ZipperTyp t (DeclareType n (AssignDecl v ts))
    CallArgs f [] as ts        -> Just $ ZipperVal f (CallName (v:as) ts)
    CallArgs f (a:as) bs ts    -> Just $ ZipperVal a (CallArgs f as (v:bs) ts)
    OpSecond a op ts           -> Just $ ZipperVal a (OpFirst op v ts)
    CallName _ _               -> Nothing
    Ret _                      -> Nothing
    IfCond _ _                 -> Nothing
    OpFirst _ _ _              -> Nothing

selectNext' :: Zipper -> Maybe Zipper
selectNext' (ZipperNam n (DeclareName t ts)) = Just $ ZipperTyp t (DeclareType n ts)
selectNext' (ZipperDec d parent) = case parent of
    AssignDecl v ts                  -> Just $ ZipperVal v (AssignVal d ts)
    FnArgs a (b:bs) s ts             -> Just $ ZipperDec b (FnArgs (d:a) bs s ts)
    FnArgs a [] s ts                 -> Just $ ZipperSt  s (FnBody (d:a) ts)
selectNext' (ZipperTyp t parent) = case parent of
    DeclareType n (FnArgs a (Declare m u:bs) s ts)
                                     -> Just $ ZipperNam m (DeclareName u (FnArgs (Declare n t:a) bs s ts))
    DeclareType n (FnArgs a [] s ts) -> Just $ ZipperSt  s (FnBody (Declare n t:a) ts)
    DeclareType n (AssignDecl v ts)  -> Just $ ZipperVal v (AssignVal (Declare n t) ts)
    FnTypeArgs a [] r ts -> Just $ ZipperTyp r (FnTypeRet (t:a) ts)
    FnTypeArgs a (b:bs) r ts -> Just $ ZipperTyp b (FnTypeArgs (t:a) bs r ts)
    FnTypeRet _ _ -> Nothing
selectNext' (ZipperSt  s parent) = case parent of
    Blk a (b:bs) ts                  -> Just $ ZipperSt  b (Blk (s:a) bs ts)
    TopLevel a (b:bs)                -> Just $ ZipperSt  b (TopLevel (s:a) bs)
    FnBody _ _                       -> Nothing
    Blk _ [] _                       -> Nothing
    IfBody _ _                       -> Nothing
    TopLevel _ []                    -> Nothing
selectNext' (ZipperVal v parent) = case parent of
    IfCond s ts                      -> Just $ ZipperSt  s (IfBody v ts)
    CallName (a:as) ts               -> Just $ ZipperVal a (CallArgs v [] as  ts)
    CallArgs f as (b:bs) ts          -> Just $ ZipperVal b (CallArgs f (v:as) bs ts)
    OpFirst op b ts                  -> Just $ ZipperVal b (OpSecond v op ts)
    CallName [] _                    -> Nothing
    CallArgs _ _ [] _                -> Nothing
    Ret _                            -> Nothing
    AssignVal _ _                    -> Nothing
    OpSecond _ _ _                   -> Nothing

zipperOnHole :: Zipper -> Bool
zipperOnHole (ZipperSt UnknownStatement _) = True
zipperOnHole (ZipperVal UnknownValue _) = True
zipperOnHole (ZipperTyp UnknownType _) = True
zipperOnHole (ZipperNam UnknownName _) = True
zipperOnHole _ = False

nextHole' :: Zipper -> Maybe Zipper
nextHole' z = (searchStart z >>= searchDownRight) <|> searchUpRight z
  where
    searchStart = if zipperOnHole z then selectNext' else Just
    searchDownRight :: Zipper -> Maybe Zipper
    searchDownRight z = (searchChildrenRight z) <|> (selectNext' z >>= searchDownRight)
    searchChildrenRight :: Zipper -> Maybe Zipper
    searchChildrenRight z = if zipperOnHole z then Just z else selectFirst' z >>= searchDownRight
    searchUpRight :: Zipper -> Maybe Zipper
    searchUpRight z = (parent >>= selectNext' >>= searchDownRight) <|> (parent >>= searchUpRight)
      where parent = goup' z


previousHole' :: Zipper -> Maybe Zipper
previousHole' z = (selectPrev' z >>= searchDownLeft) <|> searchUpLeft z
  where
    searchDownLeft :: Zipper -> Maybe Zipper
    searchDownLeft z = searchChildrenLeft z <|> (selectPrev' z >>= searchDownLeft)
    searchChildrenLeft :: Zipper -> Maybe Zipper
    searchChildrenLeft z = if zipperOnHole z then Just z else selectLast' z >>= searchDownLeft
    searchUpLeft :: Zipper -> Maybe Zipper
    searchUpLeft z = (parent >>= selectPrev' >>= searchDownLeft) <|> (parent >>= searchUpLeft)
      where parent = goup' z

gouplist a b c = reverse a ++ [b] ++ c

goup' :: Zipper -> Maybe Zipper
goup' (ZipperNam n parent) = case parent of
    DeclareName t (AssignDecl v ts) -> Just $ ZipperSt (Assign (Declare n t) v) ts
    DeclareName t (FnArgs a b s ts) -> Just $ ZipperVal (Function (gouplist a (Declare n t) b) s) ts
goup' (ZipperTyp t parent) = case parent of
    DeclareType n (AssignDecl v ts) -> Just $ ZipperSt (Assign (Declare n t) v) ts
    DeclareType n (FnArgs a b s ts) -> Just $ ZipperVal (Function (gouplist a (Declare n t) b) s) ts
    FnTypeArgs a b r ts -> Just $ ZipperTyp (FunctionType (gouplist a t b) r) ts
    FnTypeRet a ts -> Just $ ZipperTyp (FunctionType (reverse a) t) ts
goup' (ZipperDec d parent) = case parent of
    FnArgs b a s ts -> Just $ ZipperVal (Function (gouplist b d a) s) ts
    AssignDecl v ts -> Just $ ZipperSt (Assign d v) ts
goup' (ZipperVal v parent) = case parent of
    CallName a ts -> Just $ ZipperVal (Call v a) ts
    CallArgs f b a ts -> Just $ ZipperVal (Call f (gouplist b v a)) ts
    AssignVal d ts -> Just $ ZipperSt (Assign d v) ts
    Ret ts -> Just $ ZipperSt (Return v) ts
    IfCond s ts -> Just $ ZipperSt (If v s) ts
    OpFirst op b ts -> Just $ ZipperVal (BinaryOperator v op b) ts
    OpSecond a op ts -> Just $ ZipperVal (BinaryOperator a op v) ts
goup' (ZipperSt s parent) = case parent of
    FnBody a ts -> Just $ ZipperVal (Function (reverse a) s) ts
    TopLevel _ _ -> Nothing 
    Blk b a ts -> Just $ ZipperSt (Block (gouplist b s a)) ts
    IfBody v ts -> Just $ ZipperSt (If v s) ts

-- alterations: these functions change the underlying AST. I think these should
-- maybe be broken into two groups: transformations that merely fill in a hole,
-- and transformations that replace a current thing with a hole. and also
-- removing a hole. I'm pretty sure with these 3 transformations we could
-- construct any transformation we want as a sequence of transformations, while
-- keeping the logic not too dificult

-- helper things
ins a l n = let (xs,ys) = splitAt n l in xs ++ [a] ++ ys
remove n l = take n l ++ drop (n+1) l
splitl l n = (take n l, l !! n, drop (n + 1) l)

insertBefore :: Zipper -> Zipper
insertBefore (ZipperSt s (Blk a b xs)) = ZipperSt s (Blk (UnknownStatement:a) b xs) .- selectPrev
insertBefore (ZipperSt s (TopLevel a b)) = ZipperSt s (TopLevel (UnknownStatement:a) b) .- selectPrev
insertBefore (ZipperSt s (FnBody a xs)) = ZipperSt s (FnBody (a ++ [Declare UnknownName UnknownType]) xs) .- selectPrev .- selectPrev
insertBefore (ZipperDec d (FnArgs a b s xs)) = ZipperDec d (FnArgs (Declare UnknownName UnknownType:a) b s xs) .- selectPrev
insertBefore (ZipperTyp t (DeclareType n (FnArgs a b s xs))) = ZipperTyp t (DeclareType n (FnArgs (Declare UnknownName UnknownType:a) b s xs)) .- selectPrev .- selectPrev .- selectPrev
insertBefore (ZipperTyp t (FnTypeRet as xs)) = ZipperTyp UnknownType (FnTypeArgs as [] t xs)
insertBefore (ZipperTyp t (FnTypeArgs as bs r xs)) = ZipperTyp UnknownType (FnTypeArgs as (t:bs) r xs)
insertBefore (ZipperNam n (DeclareName t (FnArgs a b s xs))) = ZipperNam n (DeclareName t (FnArgs (Declare UnknownName UnknownType:a) b s xs)) .- selectPrev .- selectPrev
insertBefore (ZipperVal v (CallArgs f a b xs)) = ZipperVal v (CallArgs f (UnknownValue:a) b xs) .- selectPrev
insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter (ZipperSt s (Blk a b xs)) = ZipperSt s (Blk a (UnknownStatement:b) xs) .- selectNext
insertAfter (ZipperSt s (TopLevel a b)) = ZipperSt s (TopLevel a (UnknownStatement:b)) .- selectNext
insertAfter (ZipperDec d (FnArgs a b s xs)) = ZipperDec d (FnArgs a (Declare UnknownName UnknownType:b) s xs) .- selectNext
insertAfter (ZipperNam n (DeclareName t (FnArgs a b s xs))) = ZipperNam n (DeclareName t (FnArgs a (Declare UnknownName UnknownType:b) s xs)) .- selectNext .- selectNext
insertAfter (ZipperTyp t (DeclareType n (FnArgs a b s xs))) = ZipperTyp t (DeclareType n (FnArgs a (Declare UnknownName UnknownType:b) s xs)) .- selectNext
insertAfter (ZipperTyp t (FnTypeArgs as bs r xs)) = ZipperTyp UnknownType (FnTypeArgs (t:as) bs r xs)
insertAfter (ZipperVal f (CallName a xs)) = ZipperVal f (CallName (UnknownValue:a) xs) .- selectNext
insertAfter (ZipperVal v (CallArgs f a b xs)) = ZipperVal v (CallArgs f a (UnknownValue:b) xs) .- selectNext
insertAfter z = z .- goup .- insertAfter

deleteAtCursor :: Zipper -> Zipper
deleteAtCursor (ZipperSt a (TopLevel [] [])) = ZipperSt a (TopLevel [] [])
deleteAtCursor (ZipperSt _ (TopLevel (a:as) [])) = ZipperSt a (TopLevel as [])
deleteAtCursor (ZipperSt _ (TopLevel a (b:bs))) = ZipperSt b (TopLevel a bs)
deleteAtCursor (ZipperSt a (Blk [] [] xs)) = (ZipperSt a (Blk [] [] xs))
deleteAtCursor (ZipperSt _ (Blk (a:as) [] xs)) = ZipperSt a (Blk as [] xs)
deleteAtCursor (ZipperSt _ (Blk a (b:bs) xs)) = ZipperSt b (Blk a bs xs)
deleteAtCursor z = z

replaceWithReturn :: Zipper -> Zipper
replaceWithReturn = replaceWithStatement (Return UnknownValue)

replaceWithIf :: Zipper -> Zipper
replaceWithIf = replaceWithStatement (If UnknownValue UnknownStatement)

replaceWithBlock :: Zipper -> Zipper
replaceWithBlock = replaceWithStatement (Block [UnknownStatement])

replaceWithAssignment :: Zipper -> Zipper
replaceWithAssignment = replaceWithStatement (Assign (Declare UnknownName UnknownType) UnknownValue)

replaceWithStatement :: Statement -> Zipper -> Zipper
replaceWithStatement s (ZipperSt _ xs) = ZipperSt s xs
replaceWithStatement _ z = z

replaceWithStrType :: Zipper -> Zipper
replaceWithStrType = replaceWithType StringType

replaceWithBoolType :: Zipper -> Zipper
replaceWithBoolType = replaceWithType BooleanType

replaceWithIntType :: Zipper -> Zipper
replaceWithIntType = replaceWithType IntegerType

replaceWithFnType :: Zipper -> Zipper
replaceWithFnType = replaceWithType (FunctionType [UnknownType] UnknownType)

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
replaceWithCall = replaceWithValue (Call UnknownValue [])

replaceWithFunction :: Zipper -> Zipper
replaceWithFunction z = case possibleFunctionDefinition z of
        a:_ -> replaceWithValue a z
        _ -> z
-- replaceWithFunction = replaceWithValue (Function [Declare UnknownName UnknownType] UnknownStatement)

replaceWithOp :: Op -> Zipper -> Zipper
replaceWithOp op = replaceWithValue (BinaryOperator UnknownValue op UnknownValue)

replaceWithValue :: Value -> Zipper -> Zipper
replaceWithValue v z@(ZipperVal _ xs) = if valueTypeChecks z v
                                        then ZipperVal v xs
                                        else z
replaceWithValue _ z = z

possibleTypes :: Zipper -> [Type]
possibleTypes (ZipperTyp _ _) = [ FunctionType [UnknownType] UnknownType
                                , IntegerType
                                , BooleanType
                                , StringType
                                ]
possibleTypes _ = []

possibleStatements :: Zipper -> [Statement]
possibleStatements (ZipperSt _ _) = [ If UnknownValue UnknownStatement
                                    , Block [UnknownStatement]
                                    , Return UnknownValue
                                    , Assign (Declare UnknownName UnknownType) UnknownValue
                                    ]
possibleStatements _ = []

goToEnclosingStatement :: Zipper -> Zipper
goToEnclosingStatement z@(ZipperSt _ (Blk _ _ _)) = z
goToEnclosingStatement z@(ZipperSt _ (TopLevel _ _))   = z
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
    extractNameFromDeclList (Declare (Name n) _:ls) = n:extractNameFromDeclList ls
    extractNameFromDeclList (Declare UnknownName _:ls) = extractNameFromDeclList ls
    extractNameFromDeclList [] = []
    args = case goToEnclosingFunction z of
        Just (ZipperVal (Function l _) _) -> extractNameFromDeclList l
        _ -> []
    extractNameFromZipper z = case z of
        ZipperSt s _ -> extractName s
        _ -> Nothing
    extractName s = case s of
        Assign (Declare (Name n) _) _ -> Just n
        _ -> Nothing
    enclosingStatement = goToEnclosingStatement z
    previousStatements = case enclosingStatement of
        ZipperSt _ (TopLevel p _) -> p
        ZipperSt _ (Blk p _ _) -> p
        _ -> []
    current = case extractNameFromZipper enclosingStatement of
        Just n -> [n]
        Nothing -> []
    searchBefore = catMaybes (fmap extractName previousStatements)
    searchAbove = case goup' enclosingStatement of
        Just z' -> searchForNamedVariables z'
        Nothing -> []

standardValues = [ BooleanLiteral True
                 , BooleanLiteral False
                 , Function [] UnknownStatement
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
typeOf z (Variable v) = fromMaybe UnknownType (searchForVariableType z v)
  where
   searchForVariableType z v = searchAbove <|> searchBefore <|> current <|> args
    where 
    extractTypeFromDeclList (Declare (Name n) t:ls) = if n == v then Just t else extractTypeFromDeclList ls
    extractTypeFromDeclList (Declare UnknownName _:ls) = extractTypeFromDeclList ls
    extractTypeFromDeclList [] = Nothing
    args = case goToEnclosingFunction z of
        Just (ZipperVal (Function l _) _) -> extractTypeFromDeclList l
        _ -> Nothing
    extractTypeFromZipper z = case z of
        ZipperSt s _ -> extractType s
        _ -> Nothing
    extractType s = case s of
        Assign (Declare (Name n) s) _ -> if n == v then Just s else Nothing
        _ -> Nothing
    enclosingStatement = goToEnclosingStatement z
    previousStatements = case enclosingStatement of
        ZipperSt _ (TopLevel p _) -> p
        ZipperSt _ (Blk p _ _) -> p
        _ -> []
    current = extractTypeFromZipper enclosingStatement
    -- searchBefore = catMaybes (fmap extractType previousStatements)
    searchBefore = foldl (<|>) Nothing (fmap extractType previousStatements)
    searchAbove = case goup' enclosingStatement of
        Just z' -> searchForVariableType z' v
        Nothing -> Nothing


typeOf _ (Function a b) = FunctionType (argumentTypes a) UnknownType
  where
    argumentTypes [] = []
    argumentTypes (Declare _ t:ts) = t:argumentTypes ts
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
-- and then ensure types are correct using this function when doing stuff in the
-- Transformations module. things aren't correct by construction in that case,
-- but it won't require a major restructure of the project. and should be simple
-- enough to implement. a better type system could get implemeneted later

-- getFunctionReturnType :: Value -> Type
-- getFunctionReturnType (Function _ body) = fromMaybe UnknownType (findReturnType body)
--   where
--     findReturnType (Block []) = Nothing
--     findReturnType (Block (a:as)) = findReturnType a <|> findReturnType (Block as)
--     findReturnType (Assign _ _) = Nothing
--     findReturnType (If _ b) = findReturnType b
--     findReturnType (Return v) = Just (typeOf v)
--     findReturnType (UnknownStatement) = Nothing
-- getFunctionReturnType _ = UnknownType

returnType :: Zipper -> Type
returnType z = case goToEnclosingFunction z of
        Just z -> case expectedType z of
          FunctionType _ t -> t
          _ -> UnknownType -- wtf? this should never happen. weakness in current datatypes
        Nothing -> StringType -- if not in a function definition, must be a string. for output. i guess

expectedType :: Zipper -> Type
expectedType z@(ZipperVal _ (CallName _ _)) = UnknownType
expectedType z@(ZipperVal _ (CallArgs f b _ _)) = case typeOf z f of
        FunctionType as _ -> as !! (length b) -- this should always be in bounds
        _ -> UnknownType
expectedType z@(ZipperVal _ (Ret _ )) = returnType z
expectedType (ZipperVal _ (IfCond _ _)) = BooleanType
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
typesAreEqual (FunctionType as r) (FunctionType bs s) =
     length as == length bs && or (zipWith typesAreEqual as bs) && typesAreEqual r s
typesAreEqual a b = a == b

-- create a function that could fit in a given zipper
-- why is it a list? just to fit inside allPossibleValues, cuz I'm lazy atm and
-- not doing smart things
possibleFunctionDefinition :: Zipper -> [Value]
possibleFunctionDefinition z = case expectedType z of 
    FunctionType args _ -> [Function (typeListToDeclList args) UnknownStatement]
    _ -> []
  where
    typeListToDeclList [] = []
    typeListToDeclList (t:ts) = Declare UnknownName t:typeListToDeclList ts

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
        FunctionType xs y -> Call name (replicate (length xs) UnknownValue)
        _ -> UnknownValue -- this should never trigger. type weakness
  


-- check if a given value typechecks
valueTypeChecks :: Zipper -> Value -> Bool
valueTypeChecks z = typesAreEqual (expectedType z) . typeOf z

possibleValues :: Zipper -> [Value]
possibleValues z = filter (valueTypeChecks z) (allPossibleValues z)

