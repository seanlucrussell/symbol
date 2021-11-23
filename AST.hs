{-# LANGUAGE OverloadedStrings #-}
module AST
  ( Path
  , Tree (..)
  , treeUnderCursor
  , select
  , isLeaf
  , searchTree
  , replaceAtPoint
  , validatePath
  , Zipper
  ) where
import qualified Data.Text as T
import Data.Maybe
import Control.Monad

class Tree a where
  children :: a -> [a]
  update :: a -> [a] -> Maybe a
type Path = [Int]

-- ZIPPER --
type Zipper a = (a, Path)
-- ZIPPER --

validatePath :: Tree a => a -> Path -> Bool
validatePath t [] = False
validatePath t [n] = isJust (select n t)
validatePath t (n:ns) = isJust (do t' <- select n t
                                   return (validatePath t' ns))

replaceAtIndex :: Tree a => a -> Int -> a -> Maybe a
replaceAtIndex t n t' = replaceAtIndex n (children t') >>= update t'
        where replaceAtIndex 0 (_:xs) = Just (t':xs)
              replaceAtIndex n [] = Nothing
              replaceAtIndex n (x:xs) = do xs' <- replaceAtIndex (n-1) xs
                                           return (x:xs')

replaceAtPoint :: Tree a => a -> Path -> a -> Maybe a
replaceAtPoint t []     = const (Just t)
replaceAtPoint t (p:ps) = (replaceAtPoint t ps) >> replaceAtIndex t p

isLeaf :: Tree a => a -> Bool
isLeaf t = case children t of
                [] -> True
                _ -> False

select :: Tree a => Int -> a -> Maybe a
select n t = findChild n (children t)
        where findChild 0 [t] = Just t
              findChild n (t:ts) = findChild (n-1) ts
              findChild _ _ = Nothing

searchTree :: Tree a => (a -> Bool) -> a -> [a]
searchTree f t = (if f t then [t] else []) ++ join (fmap (searchTree f) (children t))

treeUnderCursor :: Tree a => Path -> a -> Maybe a
treeUnderCursor [] t     = Just t
treeUnderCursor (x:xs) t = do t' <- select x t
                              treeUnderCursor xs t'
