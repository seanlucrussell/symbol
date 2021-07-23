{-# LANGUAGE OverloadedStrings #-}
module Renderer
  ( Renderer
  , zipperToWidget
  , renderDoc
  , renderTerm
  , basicRenderer) where

-- , verboseRenderer)  where

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

data RenderContext = AppArg | TypeRet | Other

getRenderContext :: Container -> RenderContext
getRenderContext (ApplicationFn _ _) = AppArg
getRenderContext (FnTypeRet _ _) = TypeRet
getRenderContext _ = Other


renderZipper :: Renderer -> Zipper -> Doc Marking
-- renderZipper _ = renderAssoc . zipperToTerm
renderZipper r (Zipper t c) = enclose (annotate Highlight (renderTerm' (getRenderContext c) r t)) c
        where enclose t (TopLevel a b) = renderProgram r Other (fmap (renderTerm' Other r) (reverse a) ++ [t] ++ fmap (renderTerm' Other r) b)
              enclose t (FunctionArg x y c) = enclose (renderFunction r (getRenderContext c) t (renderTerm' Other r x) (renderTerm' Other r y)) c
              enclose t (FunctionArgType x y c) = enclose (renderFunction r (getRenderContext c) (renderTerm' Other r x) t (renderTerm' Other r y)) c
              enclose t (FunctionBody x y c) = enclose (renderFunction r (getRenderContext c) (renderTerm' Other r x) (renderTerm' Other r y) t) c
              enclose t (ApplicationFn x c) = enclose (renderApplication r (getRenderContext c) t (renderTerm' Other r x)) c
              enclose t (ApplicationArg x c) = enclose (renderApplication r (getRenderContext c) (renderTerm' AppArg r x) t) c
              enclose t (ConditionalCond x y c) = enclose (renderConditional r (getRenderContext c) t (renderTerm' Other r x) (renderTerm' Other r y)) c
              enclose t (ConditionalOptOne x y c) = enclose (renderConditional r (getRenderContext c) (renderTerm' Other r x) t (renderTerm' Other r y)) c
              enclose t (ConditionalOptTwo x y c) = enclose (renderConditional r (getRenderContext c) (renderTerm' Other r x) (renderTerm' Other r y) t) c
              enclose t (AssignmentId x y c) = enclose (renderAssignment r (getRenderContext c) t (renderTerm' Other r x) (renderTerm' Other r y)) c
              enclose t (AssignmentType x y c) = enclose (renderAssignment r (getRenderContext c) (renderTerm' Other r x) t (renderTerm' Other r y)) c
              enclose t (AssignmentVal x y c) = enclose (renderAssignment r (getRenderContext c) (renderTerm' Other r x) (renderTerm' Other r y) t) c
              enclose t (FnTypeArg x c) = enclose (renderFnType r (getRenderContext c) t (renderTerm' TypeRet r x)) c
              enclose t (FnTypeRet x c) = enclose (renderFnType r (getRenderContext c) (renderTerm' Other r x) t) c

-- renderZipper r (Zipper t c) = enclose (annotate Highlight (renderTerm r t)) c
--         where enclose t (TopLevel a b) = renderProgram r (fmap (renderTerm r) (reverse a) ++ [t] ++ fmap (renderTerm r) b)
--               enclose t (FunctionArg x y c) = enclose (renderFunction r t (renderTerm r x) (renderTerm r y)) c
--               enclose t (FunctionArgType x y c) = enclose (renderFunction r (renderTerm r x) t (renderTerm r y)) c
--               enclose t (FunctionBody x y c) = enclose (renderFunction r (renderTerm r x) (renderTerm r y) t) c
--               enclose t (ApplicationFn x c) = enclose (renderApplication r t (renderTerm r x)) c
--               enclose t (ApplicationArg x c) = enclose (renderApplication r (renderTerm r x) t) c
--               enclose t (ConditionalCond x y c) = enclose (renderConditional r t (renderTerm r x) (renderTerm r y)) c
--               enclose t (ConditionalOptOne x y c) = enclose (renderConditional r (renderTerm r x) t (renderTerm r y)) c
--               enclose t (ConditionalOptTwo x y c) = enclose (renderConditional r (renderTerm r x) (renderTerm r y) t) c
--               enclose t (AssignmentId x c) = enclose (renderAssignment r t (renderTerm r x)) c
--               enclose t (AssignmentVal x c) = enclose (renderAssignment r (renderTerm r x) t) c
--               enclose t (FnTypeArg x c) = enclose (renderFnType r t (renderTerm r x)) c
--               enclose t (FnTypeRet x c) = enclose (renderFnType r (renderTerm r x) t) c

-- lets do a renderer for terms that is associative
-- renderAssoc :: Term -> Doc Marking
-- renderAssoc (IdentifierTerm i) = annotate Cyan (pretty i)
-- renderAssoc (FunctionTerm a b c) = group (hang 1 (vcat ["\\" <> renderAssoc a <> ":" <> renderAssoc b <> ".", renderAssoc c]))
-- renderAssoc (ApplicationTerm a b) = parens (align (sep (fmap renderAssoc (terms a ++ [b]))))
--      where terms t = case t of
--                         (ApplicationTerm c d) -> terms c ++ [d]
--                         _ -> [t]
-- renderAssoc (BooleanLiteralTerm a) = annotate Red (pretty a)
-- renderAssoc (ConditionalTerm a b c) = align (sep ["if" <+> renderAssoc a, "then" <+> renderAssoc b, "else" <+> renderAssoc c])
-- renderAssoc (UnknownTerm) = "_____"
-- renderAssoc (FnTypeTerm a b) = parens (align (sep ([renderAssoc a] ++ (fmap ("->" <+>) (terms b)))))
--      where terms t = case t of
--                         (FnTypeTerm c d) -> [renderAssoc c] ++ terms d
--                         _ -> [renderAssoc t]
-- renderAssoc (BoolTypeTerm) = annotate Yellow "Bool"
-- renderAssoc (Assignment a b) = renderAssoc a <+> "=" <+> renderAssoc b
-- renderAssoc (Program a) = vsep (punctuate line (fmap (renderAssoc) a))


data Renderer = Renderer { renderIdentifier :: RenderContext -> Doc Marking -> Doc Marking
                         , renderFunction :: RenderContext -> Doc Marking -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderApplication :: RenderContext -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderBooleanLiteral :: RenderContext -> Doc Marking -> Doc Marking
                         , renderConditional :: RenderContext -> Doc Marking -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderUnknown :: RenderContext -> Doc Marking
                         , renderFnType :: RenderContext -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderBoolType :: RenderContext -> Doc Marking
                         , renderAssignment :: RenderContext -> Doc Marking -> Doc Marking -> Doc Marking -> Doc Marking
                         , renderProgram :: RenderContext -> [Doc Marking] -> Doc Marking }

renderTerm' :: RenderContext -> Renderer -> Term -> Doc Marking
-- renderTerm _ = renderAssoc
renderTerm' context r (IdentifierTerm i) = renderIdentifier r context (pretty i)
renderTerm' context r (FunctionTerm a b c) = renderFunction r context (renderTerm' Other r a) (renderTerm' Other r b) (renderTerm' Other r c)
renderTerm' context r (ApplicationTerm a b) = renderApplication r context (renderTerm' AppArg r a) (renderTerm' Other r b)
renderTerm' context r (BooleanLiteralTerm a) = renderBooleanLiteral r context (pretty a)
renderTerm' context r (ConditionalTerm a b c) = renderConditional r context (renderTerm' Other r a) (renderTerm' Other r b) (renderTerm' Other r c)
renderTerm' context r (UnknownTerm) = renderUnknown r context
renderTerm' context r (FnTypeTerm a b) = renderFnType r context (renderTerm' Other r a) (renderTerm' TypeRet r b)
renderTerm' context r (BoolTypeTerm) = renderBoolType r context
renderTerm' context r (Assignment a b c) = renderAssignment r context (renderTerm' Other r a) (renderTerm' Other r b) (renderTerm' Other r c)
renderTerm' context r (Program a) = renderProgram r context (fmap (renderTerm' Other r) a)


renderTerm :: Renderer -> Term -> Doc Marking
renderTerm = renderTerm' Other
-- renderTerm _ = renderAssoc
-- renderTerm r (IdentifierTerm i) = renderIdentifier r (pretty i)
-- renderTerm r (FunctionTerm a b c) = renderFunction r (renderTerm r a) (renderTerm r b) (renderTerm r c)
-- renderTerm r (ApplicationTerm a b) = renderApplication r (renderTerm r a) (renderTerm r b)
-- renderTerm r (BooleanLiteralTerm a) = renderBooleanLiteral r (pretty a)
-- renderTerm r (ConditionalTerm a b c) = renderConditional r (renderTerm r a) (renderTerm r b) (renderTerm r c)
-- renderTerm r (UnknownTerm) = renderUnknown r
-- renderTerm r (FnTypeTerm a b) = renderFnType r (renderTerm r a) (renderTerm r b)
-- renderTerm r (BoolTypeTerm) = renderBoolType r
-- renderTerm r (Assignment a b) = renderAssignment r (renderTerm r a) (renderTerm r b)
-- renderTerm r (Program a) = renderProgram r (fmap (renderTerm r) a)

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
basicRenderer = Renderer { renderIdentifier = \context t -> annotate Cyan t
                         , renderFunction = \context i t b -> group (hang 1 (vcat ["\\" <> i <> ":" <> t <> ".", b]))
                         , renderApplication = \context f x -> case context of
                                AppArg -> align (sep [f, x])
                                _ -> parens (align (sep [f, x]))
                         , renderBooleanLiteral = \context b -> annotate Red b
                         , renderConditional = \context a b c -> align (sep ["if" <+> a, "then" <+> b, "else" <+> c])
                         , renderUnknown = \context -> "_____"
                         , renderFnType = \context a b -> case context of 
                                TypeRet -> a <+> "->" <+> b
                                _ -> parens (a <+> "->" <+> b)
                         , renderBoolType = \context -> annotate Yellow "Bool"
                         , renderAssignment = \context a b c -> a <+> ":" <+> b <> line <> a <+> "=" <+> c
                         , renderProgram = \context a -> vsep (punctuate line a) }

-- renderWContext context (FunctionTerm a b c) = group (hang 1 (vcat ["\\" <> renderWContext Other a <> ":" <> renderWContext Other b <> ".", renderWContext Other c]))
-- renderWContext context (ApplicationTerm a b) = case context of
--         AppArg -> renderWContext AppArg a <+> renderWContext Other b
--         _ -> parens (renderWContext AppArg a <+> renderWContext Other b)
-- renderWContext context (BooleanLiteralTerm a) = annotate Red (pretty a)
-- renderWContext context (ConditionalTerm a b c) = align (sep ["if" <+> renderWContext Other a, "then" <+> renderWContext Other b, "else" <+> renderWContext Other c])
-- renderWContext context (UnknownTerm) = "_____"
-- renderWContext context (FnTypeTerm a b) = case context of
--         TypeRet -> renderWContext Other a <+> renderWContext TypeRet b
--         _ -> parens (renderWContext Other a <+> renderWContext TypeRet b)
-- renderWContext context (BoolTypeTerm) = annotate Yellow "Bool"
-- renderWContext context (Assignment a b) = renderWContext Other a <+> "=" <+> renderWContext Other b
-- renderWContext context (Program a) = vsep (punctuate line (fmap (renderWContext Other) a))


-- verboseRenderer :: Renderer 
-- verboseRenderer = Renderer { renderIdentifier = \t -> annotate White t
--                          , renderFunction = \i t b -> "function of" <+> i <+> "element of" <+> t <+> ":" <+> b
--                          , renderApplication = \f x -> "call" <+> f <+> "on" <+> x
--                          , renderBooleanLiteral = \b -> annotate Red b
--                          , renderConditional = \a b c -> "if" <+> a <+> "then" <+> b <+> "else" <+> c
--                          , renderUnknown = "_____"
--                          , renderFnType = \a b -> parens (a <+> "->" <+> b)
--                          , renderBoolType = annotate Yellow "Bool"
--                          , renderAssignment = \a b -> a <+> "=" <+> b
--                          , renderProgram = \a -> vsep (punctuate line a) }

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

