{-# LANGUAGE OverloadedStrings #-}
module STLC.Transformations 
  ( possibleTerms
  , insertBefore
  , insertAfter
  , updateSymbolTable
  , replaceWithTermAndSelectNext
  , Transformation
  , swapUp
  , swapDown
  , remove
  , replaceWithTerm) where

import AST
import Movements
import STLC.TypeChecker
import Utilities

import STLC.SymbolData
import STLC.SymbolMovements

import qualified Data.Text as T
import qualified Data.Map as M

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

{-# LANGUAGE XOverloadedStrings #-}

type Transformation a = a -> Path -> Maybe (a, Path)

findValidAssignmentId :: Token -> Int
findValidAssignmentId z = firstNumberNotInList (findAllIds z)

swapAdjacent :: Int -> [a] -> Maybe [a]
swapAdjacent 0 (x:y:xs) = Just (y:x:xs)
swapAdjacent n (x:xs) = do xs' <- swapAdjacent (n-1) xs
                           return (x:xs')
swapAdjacent _ _ = Nothing

removeAtIndex :: Int -> [a] -> Maybe [a]
removeAtIndex 0 (_:xs) = Just xs
removeAtIndex n (x:xs) = do xs' <- removeAtIndex (n-1) xs
                            return (x:xs')
removeAtIndex _ _ = Nothing

-- NOTE: THIS ONLY SWAPS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
swapDown :: Tree a => Transformation a
swapDown t (p:ps) = do swapped <- swapAdjacent p (children t)
                       newTree <- update t swapped
                       return (newTree, p+1:ps)

-- NOTE: THIS ONLY SWAPS AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
swapUp :: Tree a => Transformation a
swapUp t (p:ps) = do swapped <- swapAdjacent (p-1) (children t)
                     newTree <- update t swapped
                     return (newTree, p-1:ps)

-- NOTE: THIS ONLY REMOVES AT TOP LEVEL! prolly needs new, more specific name to
-- avoid confusion.
remove :: Tree a => Transformation a
remove t (p:ps) = do removed <- removeAtIndex p (children t)
                     newTree <- update t removed
                     return (newTree, [p-1])

findAllIds :: Token -> [Int]
findAllIds (Identifier i) = [i]
findAllIds t = join (fmap findAllIds (children t))

insertBefore :: (Token, Path) -> (Token, Path)
insertBefore (t, p:ps) = case newToken of
                Just t' -> (t', 1+p:ps)
                Nothing -> (t, p:ps)
   where newToken = update t (insertAt p (Assignment Unknown Unknown Unknown) (children t))

insertAfter :: (Token, Path) -> (Token, Path)
insertAfter (t, p:ps) = case newToken of
                Just t' -> (t', p:ps)
                Nothing -> (t, p:ps)
   where newToken = update t (insertAt (p+1) (Assignment Unknown Unknown Unknown) (children t))

replaceWithTerm :: Token -> (Token, Path) -> (Token, Path)
replaceWithTerm replacement = try (replaceWithTerm' replacement)

replaceWithTermAndSelectNext :: Token -> (Token, Path) -> (Token, Path)
replaceWithTermAndSelectNext replacement = try (replaceWithTerm' replacement >=> (nextHole' <!> return))

replaceWithTerm' :: Token -> (Token, Path) -> Maybe ((Token, Path))
replaceWithTerm' t (x, p) = do replaced <- replaceAtPoint t p x
                               if validateProgram replaced
                               then return (replaced,p)
                               else Nothing

searchForNamedVariables :: Token -> [Token]
searchForNamedVariables = mapMaybe extractIdentifier . allIdentifierDefinitions

allIdentifierDefinitions :: Token -> [Token]
allIdentifierDefinitions tree = searchTree test tree
        where test (Function (Identifier _)  _ _) = True
              test (Assignment (Identifier _) _ _) = True
              test  _ = False

extractIdentifier :: Token -> Maybe Token
extractIdentifier (Function i _ _) = Just i
extractIdentifier (Assignment i _ _) = Just i
extractIdentifier _ = Nothing

extractType :: Token -> Maybe Token
extractType (Function _ t _) = Just t
extractType (Assignment _ t _) = Just t
extractType _ = Nothing

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
arityOfIdentifier :: Token -> Token -> Maybe Integer
arityOfIdentifier token tree = findIdentifierDefinition token tree >>= extractType >>= f
        where f :: Token -> Maybe Integer
              f (FunctionType _ t) = fmap (+1) (f t)
              f BoolType = Just 0
              f _ = Nothing

-- to compute arity, we need to find the original definition of the term. this
-- could be a function or it could be an assignment
findIdentifierDefinition :: Token -> Token -> Maybe Token
findIdentifierDefinition (Identifier id) tree = listToMaybe (searchTree test tree)
        where test (Function (Identifier id')  _ _) = id == id'
              test (Assignment (Identifier id') _ _) = id == id'
              test  _ = False
findIdentifierDefinition _ _ = Nothing

functionCalls :: Token -> [Token]
functionCalls t = concat [ fmap (app x) [0..(n x)] | x <- searchForNamedVariables t]
        where app x 0 = x
              app x n = Application (app x (n-1)) Unknown
              n x = case arityOfIdentifier x t of
                Just m -> m
                Nothing -> 0

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , Function Unknown Unknown Unknown
                , Conditional Unknown Unknown Unknown
                , FunctionType Unknown Unknown
                , BoolType
                , Unknown ]

allPossibleTerms :: Token -> [Token]
allPossibleTerms t = functionCalls t ++ standardTerms

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = isJust (replaceWithTerm' t (t',p))

possibleTerms :: (Token, Path) -> [Token]
possibleTerms (t,p) = filter (termTypeChecks t p) (allPossibleTerms t)

updateSymbolTable :: (Token, Path) -> T.Text -> SymbolTable -> Maybe SymbolTable
updateSymbolTable (t',p) t s = case treeUnderCursor p t' of
        Just (Identifier i) -> Just (M.insert i t s)
        _ -> Nothing
