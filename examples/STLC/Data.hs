module STLC.Data
  ( Token (..)
  ) where

import AST

data Token = Identifier Int
           | Name (Maybe String)
           | Function Token Token Token
           | Assignment Token Token Token Token
           | EndOfProgram
           | Application Token Token
           | TrueTerm
           | FalseTerm
           | Conditional Token Token Token
           | Unknown
           | FunctionType Token Token
           | BoolType
           deriving (Eq,Show)

instance Tree Token where
   children (Function a b c) = [a,b,c]
   children (Application a b) = [a,b]
   children (Conditional a b c) = [a,b,c]
   children (FunctionType a b) = [a,b]
   children (Assignment a b c d) = [a,b,c,d]
   children _ = []

   update Function {} [a,b,c] = Just (Function a b c)
   update Application {} [a,b] = Just (Application a b)
   update Conditional {} [a,b,c] = Just (Conditional a b c)
   update FunctionType {} [a,b] = Just (FunctionType a b)
   update Assignment {} [a,b,c,d] = Just (Assignment a b c d)
   update _ _ = Nothing
