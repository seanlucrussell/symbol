{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import SymbolData
import Data.Text.Prettyprint.Doc
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Graphics.Vty
import qualified Brick
import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, modifyDefAttr)
import Lens.Micro

class Renderer a where
  renderDeclaration :: a -> Doc Marking -> Doc Marking -> Doc Marking
  renderFunction :: a -> Doc Marking -> Doc Marking -> Doc Marking
  renderCall :: a -> Doc Marking -> Doc Marking -> Doc Marking
  renderAssignment :: a -> Doc Marking -> Doc Marking -> Doc Marking
  renderTop :: a -> [Doc Marking] -> Doc Marking
  renderOp :: a -> Doc Marking -> Op -> Doc Marking -> Doc Marking
  renderFunctionType :: a -> Doc Marking -> Doc Marking -> Doc Marking
  renderIntegerType :: a -> Doc Marking
  renderBooleanType :: a -> Doc Marking
  renderStringType :: a -> Doc Marking
  renderIntegerLiteral :: a -> Doc Marking -> Doc Marking
  renderStringLiteral :: a -> Doc Marking -> Doc Marking
  renderBooleanLiteral :: a -> Doc Marking -> Doc Marking
  renderVariable :: a -> Doc Marking -> Doc Marking
  renderUnknown :: a -> Doc Marking
  renderName :: a -> Doc Marking -> Doc Marking

data SymbolRenderer = SymbolRenderer
instance Renderer SymbolRenderer where
  renderDeclaration _ n t = n <+> ":" <+> t
  renderFunction _ a b = "\\" <+> a <+> "." <+> b
  renderCall _ f a = parens (f <+> a)
  renderAssignment _ d v = d <+> "=" <+> v
  renderTop _ a = vsep (punctuate line a)
  renderOp _ a op b = parens (a <+> pop <+> b)
   where pop = case op of
               Add -> "+"
               Multiply -> "*"
               Equal -> "=="
               LessThan -> "<"
               GreaterThan -> ">"
               Mod -> "mod"
               And -> "and"
               Or -> "or"
  renderFunctionType _ args result = group (align $ parens (args <+> "->" <+> result))
  renderIntegerType _ = annotate Yellow $ pretty ("Nat" :: T.Text)
  renderBooleanType _ = annotate Yellow $ pretty ("Bool" :: T.Text)
  renderStringType _ = annotate Yellow $ pretty ("Str" :: T.Text)
  renderIntegerLiteral _ l = annotate Red l
  renderStringLiteral _ l = annotate Cyan ("\"" <> l <> "\"")
  renderBooleanLiteral _ l = annotate Red l
  renderVariable _ l = annotate White l
  renderUnknown _ = pretty ("_____" :: T.Text)
  renderName _ n = annotate White n

class Prettify a where
  prettify :: Renderer b => b -> a -> Doc Marking

instance Prettify Name where
  prettify r (Name n)    = renderName r (pretty n)
  prettify r UnknownName = renderUnknown r

instance Prettify Type where
  prettify r (FunctionType args result) = renderFunctionType r (prettify r args) (prettify r result)
  prettify r IntegerType                = renderIntegerType r
  prettify r StringType                 = renderStringType r
  prettify r BooleanType                = renderBooleanType r
  prettify r UnknownType                = renderUnknown r

instance Prettify Value where
  prettify r (StringLiteral v)       = renderStringLiteral r  (pretty v)
  prettify r (IntLiteral v)          = renderIntegerLiteral r     (pretty v)
  prettify r (BooleanLiteral v)      = renderBooleanLiteral r (pretty v)
  prettify r (Variable v)            = renderVariable r       (pretty v)
  prettify r (Function a b)          = renderFunction r                  (prettify r a)     (prettify r b)
  prettify r (Call f a)              = renderCall r                  (prettify r f) (prettify r a)
  prettify r UnknownValue            = renderUnknown r
  prettify r (BinaryOperator a op b) = renderOp r                  (prettify r a) op           (prettify r b)

instance Prettify Declare where
  prettify r (Declare n t) = renderDeclaration r (prettify r n) (prettify r t)

instance Prettify Assignment where
  prettify r (Assign d v)     = renderAssignment r (prettify r d) (prettify r v)

pMap :: (Renderer r, Prettify a) => r -> [a] -> [Doc Marking]
pMap renderer = fmap (prettify renderer)
rMap :: (Renderer r, Prettify a) => r -> [a] -> [Doc Marking]
rMap renderer = pMap renderer . reverse
pJoin renderer a b c = pMap renderer (reverse a) ++ [b] ++ pMap renderer c

prettyZip :: Renderer r => r -> Zipper -> Doc Marking
prettyZip renderer z = prettyZip' z
 where
  prettyZip' (ZipperAs  t p) = encloseAs p (annotate Highlight $ prettify renderer t)
  prettyZip' (ZipperDec t p) = encloseDc p (annotate Highlight $ prettify renderer t)
  prettyZip' (ZipperVal t p) = encloseVl p (annotate Highlight $ prettify renderer t)
  prettyZip' (ZipperNam t p) = encloseNm p (annotate Highlight $ prettify renderer t)
  prettyZip' (ZipperTyp t p) = encloseTy p (annotate Highlight $ prettify renderer t)
  encloseNm (DeclareName t ts) n = encloseDc ts $ renderDeclaration renderer n (prettify renderer t)
  encloseTy (DeclareType n ts) t = encloseDc ts $ renderDeclaration renderer (prettify renderer n) t
  encloseTy (FnTypeArgs r ts) t = encloseTy ts $ renderFunctionType renderer t (prettify renderer r)
  encloseTy (FnTypeRet a ts) r = encloseTy ts $ renderFunctionType renderer (prettify renderer a) r
  encloseDc (FnArgs s ts) d = encloseVl ts $ renderFunction renderer d (prettify renderer s)
  encloseDc (AssignDecl v ts) d = encloseAs ts $ renderAssignment renderer d (prettify renderer v)
  encloseVl (FnBody a ts) f = encloseVl ts $ renderFunction renderer (prettify renderer a) f 
  encloseVl (CallName a ts) f = encloseVl ts $ renderCall renderer f (prettify renderer a)
  encloseVl (CallArgs f ts) v = encloseVl ts $ renderCall renderer (prettify renderer f) v
  encloseVl (AssignVal d ts) v = encloseAs ts $ renderAssignment renderer (prettify renderer d) v
  encloseVl (OpFirst op b ts) v = encloseVl ts $ renderOp renderer v op (prettify renderer b)
  encloseVl (OpSecond a op ts) v = encloseVl ts $ renderOp renderer (prettify renderer a) op v
  encloseAs (TopLevel a b) s = renderTop renderer (pJoin renderer a s b)

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
         -- attributes []             = defAttr
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

zipperToWidget :: Renderer r => r -> Zipper -> Widget ()
zipperToWidget renderer = renderDoc . prettyZip renderer

