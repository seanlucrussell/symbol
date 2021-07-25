{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( zipperToWidget
  , renderDoc
  , renderTerm
  ) where

import SymbolData
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr)
import Lens.Micro

renderZipper :: Zipper -> Doc Marking
renderZipper (Zipper t p) = rz p t

changeAtIndex :: Int -> a -> [a] -> [a]
changeAtIndex 0 f (y:ys) = f:ys
changeAtIndex n f (y:ys) = y:(changeAtIndex (n-1) f ys)

rz :: [Int] -> Term -> Doc Marking
rz [] t = annotate Highlight (renderTerm t)
rz (p:ps) (Term x ts) = renderTerm' x (changeAtIndex p (rz ps (ts!!p)) (fmap renderTerm ts))

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

renderTerm :: Term -> Doc Marking
renderTerm (Term t ts) = renderTerm' t (fmap renderTerm ts)

renderTerm' :: Token -> [Doc Marking] -> Doc Marking
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

data StackInstructions = StackLiteral String
                       | Push Marking
                       | Pop
                       | NewLine Int

data Marking = Highlight | Yellow | White | Green | Blue | Magenta | Cyan | Red

renderStack :: [StackInstructions] -> [Widget ()]
renderStack lines = renderStack' lines []
 where
 renderStack' [] _                  = [str ""]
 renderStack' (Push m:ts) s         = renderStack' ts (m:s)
 renderStack' (Pop:ts) (_:ss)       = renderStack' ts ss
 renderStack' (Pop:_) []            = []
 renderStack' (NewLine n:ts) s      = str " ":str (replicate n ' ') Brick.<+> a:as
   where a:as = renderStack' ts s
 renderStack' (StackLiteral l:ts) s = modifyDefAttr (\x -> x <> attributes s) (str l) Brick.<+> a:as
   where a:as = renderStack' ts s
         attributes []             = currentAttr
         attributes (Highlight:ss) = attributes ss `withBackColor` brightBlack
         attributes (Yellow:ss)    = attributes ss `withForeColor` yellow
         attributes (Green:ss)    = attributes ss `withForeColor` green
         attributes (Red:ss)    = attributes ss `withForeColor` red
         attributes (Blue:ss)    = attributes ss `withForeColor` blue
         attributes (Magenta:ss)    = attributes ss `withForeColor` magenta
         attributes (Cyan:ss)    = attributes ss `withForeColor` cyan
         attributes (White:ss)     = attributes ss `withForeColor` white
-- more colors:
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes-Color.html    
-- more info on vty styling (bold, underline, etc):
-- https://hackage.haskell.org/package/vty-5.29/docs/Graphics-Vty-Attributes.html

renderTree :: SimpleDocTree Marking -> [StackInstructions]
renderTree (STEmpty)           = []
renderTree (STChar c)          = [StackLiteral [c]]
renderTree (STText _ t)        = [StackLiteral (T.unpack t)]
renderTree (STLine i)          = [NewLine i]
renderTree (STAnn m content)   = [Push m] ++ (renderTree content) ++ [Pop]
renderTree (STConcat contents) = concat (fmap renderTree contents)

renderDoc :: Doc Marking -> Widget ()
renderDoc d = Brick.Widget Brick.Fixed Brick.Fixed
  (do ctx <- Brick.getContext
      Brick.render $ vBox $ renderStack $ renderTree $ treeForm $ layoutSmart
                    (LayoutOptions (AvailablePerLine (ctx^.Brick.availWidthL) 1.0)) d)

zipperToWidget :: Zipper -> Widget ()
zipperToWidget = renderDoc . renderZipper

