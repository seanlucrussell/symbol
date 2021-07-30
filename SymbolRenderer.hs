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
  renderTerm' (IdentifierTerm idText) [] = renderIdentifier (pretty idText)
  renderTerm' t a = render a
          where render = case t of FunctionTerm -> renderFunction
                                   ApplicationTerm -> renderApplication 
                                   TrueTerm -> renderTrue
                                   FalseTerm -> renderFalse
                                   ConditionalTerm -> renderConditional 
                                   UnknownTerm -> renderUnknown
                                   FunctionTypeTerm -> renderFunctionType 
                                   BoolTypeTerm -> renderBoolType
                                   AssignmentTerm -> renderAssignment 
                                   Program -> renderProgram 

renderIdentifier = annotate Cyan
renderFunction [a, b, c] = group (hang 1 (vcat ["λ" <> a <> ":" <> b <> ".", c]))
renderApplication [a, b] = parens (align (sep [a, b]))
renderTrue _ = annotate Red "True"
renderFalse _ = annotate Red "False"
renderConditional [a, b, c] = align (sep ["if" <+> a , "then" <+> b , "else" <+> c])
renderUnknown _ = "_____"
renderFunctionType [a, b] = parens (align (sep [a, "->", b]))
renderBoolType _ = annotate Yellow "Bool"
renderAssignment [a, b, c] = a <+> ":" <+> b <> line <> a <+> "=" <+> c
renderProgram a = vsep (punctuate line a)
