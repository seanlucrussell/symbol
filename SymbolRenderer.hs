{-# LANGUAGE OverloadedStrings #-}
module SymbolRenderer where

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
                       Nothing -> error ("identifier lookup failed in renderer! identifier number was "
                                         ++ show i))
  renderTerm' _ _ (FunctionTerm _ _ _) [a, b, c] = group (hang 1 (vcat ["λ" <> a <> ":" <> b <> ".", c]))
  renderTerm' _ (RenderContext (ApplicationTerm _ _) 0) (ApplicationTerm _ _) [a, b] = align (sep [a, b])
  renderTerm' _ (RenderContext (AssignmentTerm _ _ _) 2) (ApplicationTerm _ _) [a, b] = align (sep [a, b])
  renderTerm' _ _ (ApplicationTerm _ _) [a, b] = parens (align (sep [a, b]))
  renderTerm' _ _ TrueTerm [] = annotate Red "True"
  renderTerm' _ _ FalseTerm [] = annotate Red "False"
  renderTerm' _ _ (ConditionalTerm _ _ _) [a, b, c] = align (sep ["if" <+> a , "then" <+> b , "else" <+> c])
  renderTerm' _ _ UnknownTerm [] = "_____"
  renderTerm' _ (RenderContext (FunctionTypeTerm _ _) 1) (FunctionTypeTerm _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext (AssignmentTerm _ _ _) 1) (FunctionTypeTerm _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext (FunctionTerm _ _ _) 1) (FunctionTypeTerm _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ _ (FunctionTypeTerm _ _) [a, b] = parens (align (sep [a, "->", b]))
  renderTerm' _ _ BoolTypeTerm [] = annotate Yellow "Bool"
  renderTerm' _ _ (AssignmentTerm _ _ _) [a, b, c] = a <+> ":" <+> b <> line <> a <+> "=" <+> c
  renderTerm' _ _ (Program _) a = vsep (punctuate line a)
  renderTerm' _ _ t u = error ("renderer fell through to unmatched case. term is: " ++ show t ++ show u)

