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
insertBefore (Zipper s (TopLevel a b)) = Zipper s (TopLevel (Assignment UnknownTerm  UnknownTerm:a) b) .- selectPrev
insertBefore z = z .- goup .- insertBefore

insertAfter :: Zipper -> Zipper
insertAfter (Zipper s (TopLevel a b)) = Zipper s (TopLevel a (Assignment UnknownTerm UnknownTerm:b)) .- selectNext
insertAfter z = z .- goup .- insertAfter

replaceWithTerm :: Term -> Zipper -> Zipper
replaceWithTerm t z@(Zipper _ c) = let z' = Zipper t c in if validateZipper z' then z' else z

-- replaceWithType :: Type -> Zipper -> Zipper
-- replaceWithType t (Zipper _ xs) = Zipper t xs
-- replaceWithType _ z = z
-- 
-- replaceWithName :: T.Text -> Zipper -> Zipper
-- replaceWithName t (Zipper _ xs) = Zipper (Name t) xs
-- replaceWithName _ z = z
-- 
-- replaceWithInt :: Integer -> Zipper -> Zipper
-- replaceWithInt i = replaceWithValue (IntLiteral i)
-- 
-- replaceWithString :: T.Text -> Zipper -> Zipper
-- replaceWithString t = replaceWithValue (StringLiteral t)
-- 
-- replaceWithVariable :: T.Text -> Zipper -> Zipper
-- replaceWithVariable v = replaceWithValue (Variable v)
-- 
-- replaceWithValue :: Value -> Zipper -> Zipper
-- replaceWithValue v z@(Zipper _ xs) = if valueTypeChecks z v
--                                         then Zipper v xs
--                                         else z
-- replaceWithValue _ z = z
-- 
-- typeTypeChecks :: Zipper -> Type -> Bool
-- typeTypeChecks z t = typesAreEqual (expectedType z) t

-- allPossibleTypes :: Zipper -> [Type]
-- allPossibleTypes _ = [ FunctionType UnknownType UnknownType
--                      , IntegerType
--                      , BooleanType
--                      , StringType
--                      ]
-- 
-- possibleTypes :: Zipper -> [Type]
-- possibleTypes z = filter (typeTypeChecks z) (allPossibleTypes z)

-- searchForNamedVariables :: Zipper -> [T.Text]
-- -- go up to enclosing Block or Top
-- -- for each previous item in that enclosing Block or Top, check if the thing is
-- -- an assignment
-- -- if it is an assignment, check if it is already in the list
-- -- if not, add it to the list
-- -- continue searching
-- -- NEED TO INCLUDE FUNCTION DEFINITIONS!!!
-- -- ALSO NEED TO REMOVE DUPLICATE WORDS!!!
-- -- supper inefficient and poorly written, but it works for now
-- searchForNamedVariables z = sort (nub (searchAbove ++ searchBefore ++ current ++ args))
--   where
--     extractNameFromDeclList (Declare (Name n) _) = [n]
--     extractNameFromDeclList (Declare UnknownName _) = []
--     args = case goToEnclosingFunction z of
--         Just (Zipper (Function l _) _) -> extractNameFromDeclList l
--         _ -> []
--     extractNameFromZipper w = case w of
--         Zipper s _ -> extractName s
--         _ -> Nothing
--     extractName s = case s of
--         Assign (Declare (Name n) _) _ -> Just n
--         _ -> Nothing
--     enclosingStatement = goToEnclosingStatement z
--     previousStatements = case enclosingStatement of
--         Zipper _ (TopLevel p _) -> p
--         _ -> []
--     current = case extractNameFromZipper enclosingStatement of
--         Just n -> [n]
--         Nothing -> []
--     searchBefore = catMaybes (fmap extractName previousStatements)
--     searchAbove = case goToEnclosingFunction z >>= goup' of
--         Just z' -> searchForNamedVariables z'
--         Nothing -> []

standardTerms :: [Term]
standardTerms = [ BooleanLiteralTerm True
                , BooleanLiteralTerm False
                , FunctionTerm UnknownTerm UnknownTerm UnknownTerm
                , ApplicationTerm UnknownTerm UnknownTerm
                , ConditionalTerm UnknownTerm UnknownTerm UnknownTerm
                , UnknownTerm
                , FnTypeTerm UnknownTerm UnknownTerm
                , BoolTypeTerm
                , Assignment UnknownTerm UnknownTerm ]

-- possibleFunctionDefinitions :: Zipper -> [Value]
-- possibleFunctionDefinitions z = case expectedType z of 
--     FunctionType args _ -> [Function (Declare UnknownName args) UnknownValue]
--     _ -> []

allPossibleTerms :: Zipper -> [Term]
allPossibleTerms z = standardTerms
                      -- ++ fmap Variable
                      --    (searchForNamedVariables z)
                      -- ++ possibleFunctionDefinitions z
                      -- ++ validFunctionCalls z

-- validFunctionCalls :: Zipper -> [Value]
-- validFunctionCalls z = functionCalls
--   where
--     names = searchForNamedVariables z
--     nameFilter n = case typeOf z (Variable n) of
--         FunctionType _ t -> typesAreEqual t (expectedType z)
--         _ -> False
--     goodNames = filter nameFilter names
--     functionCalls = fmap (nameToFunctionCall . Variable) goodNames
--     nameToFunctionCall name = case typeOf z name of
--         FunctionType _ _ -> Call name UnknownValue
--         _ -> UnknownValue -- this should never trigger. type weakness

termTypeChecks :: Zipper -> Term -> Bool
termTypeChecks (Zipper _ c) t = validateZipper (Zipper t c)

possibleTerms :: Zipper -> [Term]
possibleTerms z = filter (termTypeChecks z) (allPossibleTerms z)
