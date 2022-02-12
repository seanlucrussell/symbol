{-# LANGUAGE OverloadedStrings #-}
module STLC.Data
  ( Token (..)
  ) where

import AST
import Movements
import Utilities

import Control.Monad
import Data.Map
import Data.Maybe
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Token = Identifier Int
           | Name (Maybe String)
           | Function Token Token Token
           | Assignment Token Token Token
           | Application Token Token
           | TrueTerm
           | FalseTerm
           | Conditional Token Token Token
           | Unknown
           | FunctionType Token Token
           | BoolType
           | Program [Token]
           deriving (Eq,Show)

instance Tree Token where
   children (Function a b c) = [a,b,c]
   children (Application a b) = [a,b]
   children (Conditional a b c) = [a,b,c]
   children (FunctionType a b) = [a,b]
   children (Assignment a b c) = [a,b,c]
   children (Program ts) = ts
   children _ = []
   
   update (Function _ _ _) [a,b,c] = Just (Function a b c)
   update (Application _ _) [a,b] = Just (Application a b)
   update (Conditional _ _ _) [a,b,c] = Just (Conditional a b c)
   update (FunctionType _ _) [a,b] = Just (FunctionType a b)
   update (Assignment _ _ _) [a,b,c] = Just (Assignment a b c)
   update (Program _) ts = Just (Program ts)
   update _ _ = Nothing
