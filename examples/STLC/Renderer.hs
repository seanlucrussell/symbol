{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module STLC.Renderer 
        (renderContextAtPoint)
        where

import AST
import STLC.Data
import qualified Renderer

import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Data.Text

-- concept
--
-- what if we could automatically derive the correct parenthesis to disambiguate
-- an expression, but only as much as necessary? some considerations
--
-- binary operations can associate left or right
-- binary operations can commute
-- some of this may require a syntactic understanding (e.g. when operations are
-- infix, prefix, postfix, mixfix)
-- operators have a total order for operator precedence which will help
-- determine parens
--
-- one more thought: an n-ary operation with arguments a0,a1,...an can be
-- represented with n+1 separators labeled s0,s1,...sn+1 as follows
--
--   s0 a0 s1 a1 ... an sn+1
--
-- dunno what impact that has on this theory, but it may simplify
-- considerations. for instance, an infix binary operation has s0 and s2 empty,
-- with s1 the operator name. this concept should cover the large majority of
-- syntaxes that need to be considered.

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

renderContextAtPoint :: Path -> Token -> [String]
renderContextAtPoint [] _ = []
renderContextAtPoint (2:ps) (Function n _ t) = renderContextAtPoint ps t ++ [extractName n]
renderContextAtPoint (p:ps) t = renderContextAtPoint ps (children t !! p)

instance Renderer.Render (Token, [String]) where
  render width (token,context) = fromDoc width (renderToken token context [])

fromDoc :: Int -> Doc Annotation -> Renderer.Rendering
fromDoc width doc = generatePathMap [] (0,0) (Renderer.layout width doc)

forward :: Int -> (Int,Int) -> (Int,Int)
forward n (x,y) = (x+n,y)

down :: Int -> (Int,Int) -> (Int,Int)
down n (_,y) = (n,y+1)

generatePathMap :: [Annotation] -> (Int,Int) -> SimpleDocStream Annotation -> Renderer.Rendering
generatePathMap _ _ SFail                      = M.empty
generatePathMap _ _ SEmpty                     = M.empty
generatePathMap p x (SChar c s)                = M.insert x (Renderer.Cell c (style p) (paths p)) (generatePathMap p (forward 1 x) s)
generatePathMap p x (SText n t s)              = M.union (M.fromList [((forward m x),Renderer.Cell c (style p) (paths p)) | (c,m) <- Prelude.zip (unpack t) [0..]])
                                                           (generatePathMap p (forward n x) s)
generatePathMap p x (SLine n s)                = generatePathMap p (down n x) s
generatePathMap p x (SAnnPush p' s)            = generatePathMap (p':p) x s
generatePathMap (p:ps) x (SAnnPop s)           = generatePathMap ps x s
generatePathMap [] x    n                          = error ("unmatched pattern in generatePathMap: "
                                                             ++ "empty path, " ++ show x ++ show n)

paths :: [Annotation] -> [Path]
paths (Location p:as) = p:paths as
paths (_:as)          = paths as
paths []              = []

style :: [Annotation] -> Renderer.Style
style (Location _:as) = style as
style (Highlight:as) = Renderer.Highlight
style (Yellow:as) = Renderer.Yellow
style (White:as) = Renderer.White
style (Green:as) = Renderer.Green
style (Blue:as) = Renderer.Blue
style (Magenta:as) = Renderer.Magenta
style (Cyan:as) = Renderer.Cyan
style (Red:as) = Renderer.Red
style [] = Renderer.Default

data Annotation = Highlight | Yellow | White | Green | Blue | Magenta | Cyan | Red | Location Path deriving (Show)

extractName :: Token -> String
extractName (Name (Just n)) = n
extractName (Name Nothing) = "?"
extractName t = error "trying to extract name from token " ++ show t

renderToken :: Token -> [String] -> Path -> Doc Annotation
renderToken (Identifier i) context p = annotate (Location p) $ annotate Cyan (pretty (context!!i))
renderToken (Name (Just "")) context p = annotate (Location p) $ " "
renderToken (Name (Just n)) context p = annotate (Location p) $ annotate Cyan (pretty n)
renderToken (Name Nothing) context p = annotate (Location p) $ annotate Cyan ("_____")
renderToken (Function x y z) context p  = annotate (Location p) $ Data.Text.Prettyprint.Doc.group (hang 1 (vcat ["λ" <> x' <> ":" <> y' <> ".", z' ]))
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
              z' = renderToken z (extractName x:context) (p ++ [2])
renderToken (Application x y) context p  = annotate (Location p) $ align (sep [x', y'])
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
renderToken TrueTerm context p = annotate (Location p) $ annotate Red "True"
renderToken FalseTerm context p = annotate (Location p) $ annotate Red "False"
renderToken (Conditional x y z) context p  = annotate (Location p) $ align (sep ["if", x', "then", y', "else", z'])
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
              z' = renderToken z context (p ++ [2])
renderToken Unknown context p = annotate (Location p) $ "_____"
renderToken (FunctionType x y) context p  = annotate (Location p) $ align (sep [x', "->", y'])
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
renderToken BoolType context p = annotate (Location p) $ annotate Yellow "Bool"
renderToken EndOfProgram _ _  = ""
renderToken (Assignment x y z w) context p  = annotate (Location p) $ x' <+> ":" <+> y' <> line <> x' <+> "=" <+> z' <> line <> line <> w'
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
              z' = renderToken z context (p ++ [2])
              w' = renderToken w (extractName x:context) (p ++ [3])
