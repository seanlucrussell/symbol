{-# LANGUAGE OverloadedStrings #-}
module STLC.Transformations 
  ( possibleTerms
  , swapAssignmentUp
  , swapAssignmentDown
  , insertAssignmentBefore
  , insertAssignmentAfter
  , removeAssignment
  ) where

import AST
import Utilities
import Transformations

import STLC.TypeChecker
import STLC.Data

import Data.Maybe

-- contextDepth :: Path -> Token -> Maybe Int
-- contextDepth p t = fmap length (contextAtPoint p t)
-- 
-- evalStep :: Transformation Token -- remember, we need to take into account changes to path
-- evalStep = undefined
-- 
-- evalUnderCursor :: Transformation Token
-- evalUnderCursor = undefined

removeAssignment :: Transformation Token
removeAssignment (Assignment a b c (Assignment _ _ _ EndOfProgram)) _ = return (Assignment a b c EndOfProgram, [0])
removeAssignment (Assignment a b c d) (3:ps) = do (d',ps') <- removeAssignment d ps
                                                  return (Assignment a b c d', 3:ps')
removeAssignment (Assignment _ _ _ d) _ = return (applyToFreeVars (subtract 1) d, [0])
removeAssignment _ _ = Nothing

insertAssignmentBefore :: Transformation Token
insertAssignmentBefore (Assignment a b c d) (3:ps) = do (d',ps') <- insertAssignmentBefore d ps
                                                        return (Assignment a b c d', 3:ps')
insertAssignmentBefore a [] = return (Assignment (Name Nothing) Unknown Unknown (applyToFreeVars (+1) a), [0])
insertAssignmentBefore a (p:ps) = return (Assignment (Name Nothing) Unknown Unknown (applyToFreeVars (+1) a), p+1:ps)

insertAssignmentAfter :: Transformation Token
insertAssignmentAfter (Assignment a b c d) (3:ps) = do (d',ps') <- insertAssignmentAfter d ps
                                                       return (Assignment a b c d', 3:ps')
insertAssignmentAfter (Assignment a b c d) [] = return (Assignment a b c (Assignment (Name Nothing) Unknown Unknown (applyToFreeVars (+1) d)), [])
insertAssignmentAfter (Assignment a b c d) (p:ps) = return (Assignment a b c (Assignment (Name Nothing) Unknown Unknown (applyToFreeVars (+1) d)), p:ps)
insertAssignmentAfter _ _ = Nothing


-- gotta be some way to wrap this up into some Token env typeclass or something.
-- Really we just need to know if the environment depth is less than i
applyToFreeVars :: (Int -> Int) -> Token -> Token
applyToFreeVars f = app 0
  where app depth (Identifier i) = if i >= depth then Identifier (f i) else Identifier i
        app depth (Function a b c) = Function a b (app (depth + 1) c)
        app depth (Assignment a b c d) = Assignment a b (app depth c) (app (depth + 1) d)
        app depth (Application a b) = Application (app depth a) (app depth b)
        app depth (Conditional a b c) = Conditional (app depth a) (app depth b) (app depth c)
        app _ x = x
-- i'd really like to write this as
--   applyToFreeVars f (Identifier env i) = if i >= length env then Identifier env (f i) else Identifier i
--   applyToFreeVars f t = fmap applyToFreeVars f t
-- so it sort of makes sense for each term to have an environment packaged up
-- with it or something. but then updates and transformations have to correctly
-- modify the environment. would be better if we could do this automatically
--
-- so perhaps what we really need is to remove the environment from the Token
-- structure completely! then we have some way of iterating over Tokens with
-- environment in mind
--
-- i think that would then mean that we need a way of introducing things to the
-- environment. e.g. a function or value that gets used whenever we enter into a
-- function or assignment body
--
-- ok maybe not. more like we need a function: (a -> env) -> (env -> Token a -> b) -> env -> Token a -> b
-- this function compiles existing data in the structure. er not quite. this
-- isn't the answer either.
--
-- what are our use cases?
--   - increment/decrement free variables
--   - get type annotations for identifiers
--   - info about names for the renderer

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

typeToArity :: Token -> Maybe Integer
typeToArity (FunctionType _ t) = fmap (+1) (typeToArity t)
typeToArity BoolType = Just 0
typeToArity _ = Nothing

-- need to fix this so identifiers which haven't been named cannot be used
contextAtPoint :: Path -> Token -> [Maybe Token]
contextAtPoint [] _ = []
contextAtPoint (3:ns) (Assignment _ t _ c) =  contextAtPoint ns c ++ [Just t]
contextAtPoint (2:ns) (Function _ t b) = contextAtPoint ns b ++ [Just t]
contextAtPoint (n:ns) t = fromMaybe [] (do child <- select n t
                                           return (contextAtPoint ns child))

functionCalls :: Path -> Token -> [Token]
functionCalls p t = concat [ fmap (app i) [0..(fromMaybe 0 (typeToArity x))] | (x,i) <- zip (catMaybes context) [0..]]
        where app x 0 = Identifier x
              app x m = Application (app x (m-1)) Unknown
              context = contextAtPoint p t

standardTerms :: [Token]
standardTerms = [ TrueTerm
                , FalseTerm
                , Function (Name Nothing) Unknown Unknown
                , Conditional Unknown Unknown Unknown
                , FunctionType Unknown Unknown
                , BoolType
                , Unknown ]

termTypeChecks :: Token -> Path -> Token -> Bool
termTypeChecks t' p t = maybe False (`validateProgram` p) (replaceAtPoint t p t')

possibleTerms :: Token -> Path -> [Token]
possibleTerms t p = filter (termTypeChecks t p) (functionCalls p t ++ standardTerms)
