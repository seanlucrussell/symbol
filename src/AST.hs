{-# LANGUAGE OverloadedStrings #-}
module AST
  ( Path
  , Tree (..)
  , treeUnderCursor
  , select
  , isLeaf
  , search
  , searchTree
  , replaceAtIndex
  , replaceAtPoint
  , validatePath
  ) where
import Utilities
import Control.Monad

class Tree a where
  children :: a -> [a]
  update :: a -> [a] -> Maybe a
type Path = [Int]

validatePath :: Tree a => a -> Path -> Bool
validatePath _ [] = True
validatePath t (n:ns) = case (do t' <- select n t
                                 return (validatePath t' ns)) of
                             Just True -> True
                             _ -> False

replaceAtIndex :: Tree a => a -> Int -> a -> Maybe a
replaceAtIndex t n t' = go n (children t') >>= update t'
        where go 0 (_:xs) = Just (t:xs)
              go _ [] = Nothing
              go m (x:xs) = do xs' <- go (m-1) xs
                               return (x:xs')

replaceAtPoint :: Tree a => a -> Path -> a -> Maybe a
replaceAtPoint t []     _  = Just t
replaceAtPoint t (p:ps) t' = do child <- select p t'
                                t'' <- replaceAtPoint t ps child
                                replaceAtIndex t'' p t'

isLeaf :: Tree a => a -> Bool
isLeaf t = case children t of
                [] -> True
                _ -> False

select :: Tree a => Int -> a -> Maybe a
select n = safeListIndex n . children

searchTree :: Tree a => (a -> Bool) -> a -> [a]
searchTree f t = (if f t then [t] else []) ++ join (fmap (searchTree f) (children t))

search :: Tree a => (a -> Bool) -> a -> [Path]
search test tree = if test tree then [[]] else []
                   ++ join [fmap (n:) p | (n,p) <- zip [0..] (fmap (search test) (children tree))]

treeUnderCursor :: Tree a => Path -> a -> Maybe a
treeUnderCursor [] t     = Just t
treeUnderCursor (x:xs) t = do t' <- select x t
                              treeUnderCursor xs t'
