{-# LANGUAGE OverloadedStrings #-}
module SymbolRenderer
  ( zipperToWidget
  , renderDoc
  , renderTerm
  ) where

import SymbolData
import Renderer

import Data.Text.Prettyprint.Doc

-- things to remember from prettyprinter:
--   group
--   align
--   vsep
--   sep
--   vcat
--   puncutate
-- refer to
-- https://hackage.haskell.org/package/prettyprinter-1.7.0/docs/Prettyprinter.html
-- for more

instance Renderable Token where
  renderTerm' _ (IdentifierTerm idText) [] = annotate Cyan (pretty idText)
  renderTerm' _ FunctionTerm [a, b, c] = group (hang 1 (vcat ["λ" <> a <> ":" <> b <> ".", c]))
  renderTerm' (Just (ApplicationTerm,0)) ApplicationTerm [a, b] = align (sep [a, b])
  renderTerm' (Just (AssignmentTerm,2)) ApplicationTerm [a, b] = align (sep [a, b])
  renderTerm' _ ApplicationTerm [a, b] = parens (align (sep [a, b]))
  renderTerm' _ TrueTerm [] = annotate Red "True"
  renderTerm' _ FalseTerm [] = annotate Red "False"
  renderTerm' _ ConditionalTerm [a, b, c] = align (sep ["if" <+> a , "then" <+> b , "else" <+> c])
  renderTerm' _ UnknownTerm [] = "_____"
  renderTerm' (Just (FunctionTypeTerm,1)) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' (Just (AssignmentTerm,1)) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' (Just (FunctionTerm,1)) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' _ FunctionTypeTerm [a, b] = parens (align (sep [a, "->", b]))
  renderTerm' _ BoolTypeTerm [] = annotate Yellow "Bool"
  renderTerm' _ AssignmentTerm [a, b, c] = a <+> ":" <+> b <> line <> a <+> "=" <+> c
  renderTerm' _ Program a = vsep (punctuate line a)
  renderTerm' _ _ _ = "!!!RENDER ERROR!!!"

