{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( Renderer
  , zipperToWidget
  , renderDoc
  , renderTerm
  , basicRenderer
  , verboseRenderer)  where

-- how do we make parens associative?

import SymbolData
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr)
import Lens.Micro

data Renderer = Renderer { renderIdentifier :: Doc Marking -> Doc Marking
                         , renderFunction :: Doc Marking -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderApplication :: Doc Marking -> Doc Marking -> Doc Marking
                         , renderBooleanLiteral :: Doc Marking -> Doc Marking
                         , renderConditional :: Doc Marking -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderUnknown :: Doc Marking
                         , renderFnType :: Doc Marking -> Doc Marking -> Doc Marking
                         , renderBoolType :: Doc Marking
                         , renderAssignment :: Doc Marking -> Doc Marking -> Doc Marking
                         , renderProgram :: [Doc Marking] -> Doc Marking }

renderZipper :: Renderer -> Zipper -> Doc Marking
renderZipper r (Zipper t c) = enclose (annotate Highlight (renderTerm r t)) c
        where enclose t (TopLevel a b) = renderProgram r (fmap (renderTerm r) (reverse a) ++ [t] ++ fmap (renderTerm r) b)
              enclose t (FunctionArg x y c) = enclose (renderFunction r t (renderTerm r x) (renderTerm r y)) c
              enclose t (FunctionArgType x y c) = enclose (renderFunction r (renderTerm r x) t (renderTerm r y)) c
              enclose t (FunctionBody x y c) = enclose (renderFunction r (renderTerm r x) (renderTerm r y) t) c
              enclose t (ApplicationFn x c) = enclose (renderApplication r t (renderTerm r x)) c
              enclose t (ApplicationArg x c) = enclose (renderApplication r (renderTerm r x) t) c
              enclose t (ConditionalCond x y c) = enclose (renderConditional r t (renderTerm r x) (renderTerm r y)) c
              enclose t (ConditionalOptOne x y c) = enclose (renderConditional r (renderTerm r x) t (renderTerm r y)) c
              enclose t (ConditionalOptTwo x y c) = enclose (renderConditional r (renderTerm r x) (renderTerm r y) t) c
              enclose t (AssignmentId x c) = enclose (renderAssignment r t (renderTerm r x)) c
              enclose t (AssignmentVal x c) = enclose (renderAssignment r (renderTerm r x) t) c
              enclose t (FnTypeArg x c) = enclose (renderFnType r t (renderTerm r x)) c
              enclose t (FnTypeRet x c) = enclose (renderFnType r (renderTerm r x) t) c


renderTerm :: Renderer -> Term -> Doc Marking
renderTerm r (IdentifierTerm i) = renderIdentifier r (pretty i)
renderTerm r (FunctionTerm a b c) = renderFunction r (renderTerm r a) (renderTerm r b) (renderTerm r c)
renderTerm r (ApplicationTerm a b) = renderApplication r (renderTerm r a) (renderTerm r b)
renderTerm r (BooleanLiteralTerm a) = renderBooleanLiteral r (pretty a)
renderTerm r (ConditionalTerm a b c) = renderConditional r (renderTerm r a) (renderTerm r b) (renderTerm r c)
renderTerm r (UnknownTerm) = renderUnknown r
renderTerm r (FnTypeTerm a b) = renderFnType r (renderTerm r a) (renderTerm r b)
renderTerm r (BoolTypeTerm) = renderBoolType r
renderTerm r (Assignment a b) = renderAssignment r (renderTerm r a) (renderTerm r b)
renderTerm r (Program a) = renderProgram r (fmap (renderTerm r) a)

-- specific renderers

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

basicRenderer :: Renderer 
basicRenderer = Renderer { renderIdentifier = \t -> annotate Cyan t
                         , renderFunction = \i t b -> group (hang 1 (vcat ["\\" <> i <> ":" <> t <> ".", b]))
                         , renderApplication = \f x -> parens (align (sep [f, x]))
                         , renderBooleanLiteral = \b -> annotate Red b
                         , renderConditional = \a b c -> align (sep ["if" <+> a, "then" <+> b, "else" <+> c])
                         , renderUnknown = "_____"
                         , renderFnType = \a b -> parens (a <+> "->" <+> b)
                         , renderBoolType = annotate Yellow "Bool"
                         , renderAssignment = \a b -> a <+> "=" <+> b
                         , renderProgram = \a -> vsep (punctuate line a) }

verboseRenderer :: Renderer 
verboseRenderer = Renderer { renderIdentifier = \t -> annotate White t
                         , renderFunction = \i t b -> "function of" <+> i <+> "element of" <+> t <+> ":" <+> b
                         , renderApplication = \f x -> "call" <+> f <+> "on" <+> x
                         , renderBooleanLiteral = \b -> annotate Red b
                         , renderConditional = \a b c -> "if" <+> a <+> "then" <+> b <+> "else" <+> c
                         , renderUnknown = "_____"
                         , renderFnType = \a b -> parens (a <+> "->" <+> b)
                         , renderBoolType = annotate Yellow "Bool"
                         , renderAssignment = \a b -> a <+> "=" <+> b
                         , renderProgram = \a -> vsep (punctuate line a) }

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

zipperToWidget :: Renderer -> Zipper -> Widget ()
zipperToWidget renderer = renderDoc . renderZipper renderer

