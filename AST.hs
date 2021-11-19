{-# LANGUAGE OverloadedStrings #-}
module AST
  ( Zipper
  , Path
  , Tree (Tree)
  , zipperToTerm
  , termUnderCursor
  , extractToken
  , tokenUnderCursor
  ) where
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Tree a = Tree a [Tree a] deriving (Eq,Show)

type Path = [Int]
type Zipper a = (Tree a, Path)

termUnderCursor :: Zipper a -> Tree a
termUnderCursor (t, []) = t
termUnderCursor ((Tree _ ts), (x:xs)) = termUnderCursor (ts!!x, xs)

extractToken :: Tree a -> a
extractToken (Tree t _) = t

tokenUnderCursor :: Zipper a -> a
tokenUnderCursor = extractToken . termUnderCursor

zipperToTerm :: Zipper a -> Tree a
zipperToTerm (t, _) = t
