{-# LANGUAGE OverloadedStrings #-}
module STLC.Renderer where

import STLC.Data
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
  renderTerm' s _ (Identifier i) [] = annotate Cyan (case M.lookup i s of
                       Just "" -> " "
                       Just i' -> pretty i'
                       Nothing -> error ("id lookup failed in renderer! id number was " ++ show i))
  renderTerm' _ _ (Function _ _ _) [a, b, c] = group (hang 1 (vcat ["λ" <> a <> ":" <> b <> ".", c]))
  renderTerm' _ (RenderContext (Application _ _) 0) (Application _ _) [a, b] = align (sep [a, b])
  renderTerm' _ (RenderContext (Assignment _ _ _) 2) (Application _ _) [a, b] = align (sep [a, b])
  renderTerm' _ _ (Application _ _) [a, b] = parens (align (sep [a, b]))
  renderTerm' _ _ TrueTerm [] = annotate Red "True"
  renderTerm' _ _ FalseTerm [] = annotate Red "False"
  renderTerm' _ _ (Conditional _ _ _) [a, b, c] = align (sep ["if" <+> a , "then" <+> b , "else" <+> c])
  renderTerm' _ _ Unknown [] = "_____"
  renderTerm' _ (RenderContext (FunctionType _ _) 1) (FunctionType _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext (Assignment _ _ _) 1) (FunctionType _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ (RenderContext (Function _ _ _) 1) (FunctionType _ _) [a, b] = align (sep [a, "->", b])
  renderTerm' _ _ (FunctionType _ _) [a, b] = parens (align (sep [a, "->", b]))
  renderTerm' _ _ BoolType [] = annotate Yellow "Bool"
  renderTerm' _ _ (Assignment _ _ _) [a, b, c] = a <+> ":" <+> b <> line <> a <+> "=" <+> c
  renderTerm' _ _ (Program _) a = vsep (punctuate line a)
  renderTerm' _ _ t u = error ("renderer fell through to unmatched case. term is: " ++ show t ++ show u)

