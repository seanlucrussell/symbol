{-# LANGUAGE OverloadedStrings #-}
module SymbolRenderer
  ( zipperToWidget
  , renderDoc
  , renderTerm
  ) where

import SymbolData
import Renderer

import qualified Data.Map as M
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
  renderTerm' s _ (IdentifierTerm i) [] = annotate Cyan (case M.lookup i s of
                                                                   Just i' -> pretty i'
                                                                   Nothing -> pretty i)
  renderTerm' _ _ FunctionTerm [a, b, c] = group (hang 1 (vcat ["λ" <> a <> ":" <> b <> ".", c]))
  renderTerm' _ (RenderContext ApplicationTerm 0) ApplicationTerm [a, b] = align (sep [a, b])
  renderTerm' _ (RenderContext AssignmentTerm 2) ApplicationTerm [a, b] = align (sep [a, b])
  renderTerm' _ _ ApplicationTerm [a, b] = parens (align (sep [a, b]))
  renderTerm' _ _ TrueTerm [] = annotate Red "True"
  renderTerm' _ _ FalseTerm [] = annotate Red "False"
  renderTerm' _ _ ConditionalTerm [a, b, c] = align (sep ["if" <+> a , "then" <+> b , "else" <+> c])
  renderTerm' _ _ UnknownTerm [] = "_____"
  renderTerm' _ (RenderContext FunctionTypeTerm 1) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext AssignmentTerm 1) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext FunctionTerm 1) FunctionTypeTerm [a, b] = align (sep [a, "->", b])
  renderTerm' _ _ FunctionTypeTerm [a, b] = parens (align (sep [a, "->", b]))
  renderTerm' _ _ BoolTypeTerm [] = annotate Yellow "Bool"
  renderTerm' _ _ AssignmentTerm [a, b, c] = a <+> ":" <+> b <> line <> a <+> "=" <+> c
  renderTerm' _ _ Program a = vsep (punctuate line a)
  renderTerm' _ _ _ _ = "!!!RENDER ERROR!!!"

