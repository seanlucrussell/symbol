{-# LANGUAGE OverloadedStrings #-}
module AST
  ( Zipper
  , Path
  , Term (Term)
  , zipperToTerm
  , termUnderCursor
  , tokenUnderCursor
  ) where
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Term a = Term a [Term a] deriving (Eq,Show)

type Path = [Int]
type Zipper a = (Term a, Path)

termUnderCursor :: Zipper a -> Term a
termUnderCursor (t, []) = t
termUnderCursor ((Term _ ts), (x:xs)) = termUnderCursor (ts!!x, xs)

extractToken :: Term a -> a
extractToken (Term t _) = t

tokenUnderCursor :: Zipper a -> a
tokenUnderCursor = extractToken . termUnderCursor

zipperToTerm :: Zipper a -> Term a
zipperToTerm (t, _) = t
