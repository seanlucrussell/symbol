{-# LANGUAGE OverloadedStrings #-}
module STLC.Transformations 
  ( possibleTerms
  , swapAssignmentUp
  , swapAssignmentDown
  ) where

import AST
import Movements
import Utilities
import Transformations

import STLC.TypeChecker
import STLC.Data
import STLC.Movements

import qualified Data.Text as T
import qualified Data.Map as M

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

contextDepth :: Path -> Token -> Maybe Int
contextDepth p t = fmap length (contextAtPoint p t)

-- reindex :: (Int -> Int) -> Token -> Token
-- reindex f (Identifier i) = Identifier (f i)
-- reindex f (Function a b c) = Function a b (reindex c)
-- reindex f (Assignment a b c) = Assignment a b (reindex c)
-- reindex f (Application a b) = Application (reindex a) (reindex b)
-- reindex f (Conditional a b c) = Conditional (reindex a) (reindex b) (reindex c)
-- reindex f (Program ts) = Program (fmap (reindex f) ts)
-- reindex f t = t

applyToFreeVars :: (Int -> Int) -> Token -> Token
applyToFreeVars f t = app 0 t
  where app depth (Identifier i) = if i >= depth then Identifier (f i) else Identifier i
        app depth (Function a b c) = Function a b (app (depth + 1) c)
        app depth (Assignment a b c) = Assignment a b (app (depth + 1) c)
        app depth (Application a b) = Application (app depth a) (app depth b)
        app depth (Conditional a b c) = Conditional (app depth a) (app depth b) (app depth c)
        app depth (Program ts) = Program [app (depth + n) t | (n,t) <- zip [0..] ts]
        app _ t = t

swapAssignmentUp :: Transformation Token
swapAssignmentUp t (p:ps) = do swapped <- swapAdjacent (p-1) (children t)
                               newToken <- update t swapped
                               top <- treeUnderCursor [p-1,2] newToken
                               bottom <- treeUnderCursor [p,2] newToken
                               newToken' <- replaceAtPoint (applyToFreeVars (subtract 1) top) [p-1,2] newToken
                               newToken'' <- replaceAtPoint (applyToFreeVars (+1) bottom) [p,2] newToken'
                               return (newToken'', p-1:ps)
swapAssignmentUp _ _ = Nothing

swapAssignmentDown :: Transformation Token
swapAssignmentDown t (p:ps) = do swapped <- swapAdjacent p (children t)
                                 newToken <- update t swapped
                                 top <- treeUnderCursor [p,2] newToken
                                 bottom <- treeUnderCursor [p+1,2] newToken
                                 newToken' <- replaceAtPoint (applyToFreeVars (subtract 1) top) [p,2] newToken
                                 newToken'' <- replaceAtPoint (applyToFreeVars (+1) bottom) [p+1,2] newToken'
                                 return (newToken'', p+1:ps)
swapAssignmentDown _ _ = Nothing

extractType :: Token -> Maybe Token
extractType (Function _ t _) = Just t
extractType (Assignment _ t _) = Just t
extractType _ = Nothing

typeToArity :: Token -> Maybe Integer
typeToArity (FunctionType _ t) = fmap (+1) (typeToArity t)
typeToArity BoolType = Just 0
typeToArity _ = Nothing

-- If arity is not well defined (i.e. final type is Unknown), this evaluates to
-- Nothing. Otherwise, it evaluates to Just n, where n is a natural number
-- (which may be 0, for identifiers which refer to non-function values)
identifierArity :: Path -> Token -> Token -> Maybe Integer
identifierArity path id tree = findIdentifierDefinition path id tree >>= extractType >>= typeToArity

findIdentifierDefinition :: Path -> Token -> Token -> Maybe Token
findIdentifierDefinition path (Identifier n) tree = contextAtPoint path tree >>= safeListIndex n
findIdentifierDefinition _ _ _ = Nothing

contextAtPoint :: Path -> Token -> Maybe [Token]
contextAtPoint [] t = Just []
contextAtPoint (n:ns) (Program a) = do child <- select n (Program a)
                                       childContext <- contextAtPoint ns child
                                       return (reverse (fmap extractContext (take n a)) ++ childContext)
        where extractContext (Assignment _ t _) = t
              extractContext t = error ("Non-assignment found at top level: " ++ show t)
contextAtPoint (2:ns) (Function _ t b) = fmap (t:) (contextAtPoint ns b)
contextAtPoint (n:ns) t = do child <- select n t
                             contextAtPoint ns child

functionCalls :: Path -> Token -> [Token]
functionCalls p t = concat [ fmap (app i) [0..(n x)] | (x,i) <- zip context [0..]]
        where app x 0 = Identifier x
              app x n = Application (app x (n-1)) Unknown
              n x = fromMaybe 0 (identifierArity p x t)
              context = fromMaybe [] (contextAtPoint p t)

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , Function (Name Nothing) Unknown Unknown
                , Conditional Unknown Unknown Unknown
                , FunctionType Unknown Unknown
                , BoolType
                , Unknown ]

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = fromMaybe False (fmap validateProgram (replaceAtPoint t p t'))

possibleTerms :: Token -> Path -> [Token]
possibleTerms t p = filter (termTypeChecks t p) (functionCalls p t ++ standardTerms)
