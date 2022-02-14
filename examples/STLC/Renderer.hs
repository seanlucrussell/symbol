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
renderContextAtPoint (2:ps) (Function (Name i) _ t) = (case i of
                Just n -> n
                Nothing -> ""):renderContextAtPoint ps t
renderContextAtPoint (2:ps) (Assignment (Name i) _ t) = (case i of
                Just n -> n
                Nothing -> ""):renderContextAtPoint ps t
renderContextAtPoint (p:ps) t = renderContextAtPoint ps (children t !! p)

forward :: Int -> (Int,Int) -> (Int,Int)
forward n (x,y) = (x+n,y)

down :: Int -> (Int,Int) -> (Int,Int)
down n (_,y) = (n,y+1)

instance Renderer.Render (Token, [String]) where
  render width (token,context) = fromDoc width (renderToken token context [])

fromDoc :: Int -> Doc Annotation -> Renderer.Rendering
fromDoc width doc = generatePathMap [] (0,0) (Renderer.layout width doc)

-- push/pop requires that we ignore annotations that aren't markings. currently
-- fixed in termToPathMap but more robust solution would be preferred
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
extractName (Function (Name (Just n)) _ _) = n
extractName (Function (Name Nothing) _ _) = "?"
extractName (Assignment (Name (Just n)) _ _) = n
extractName (Assignment (Name Nothing) _ _) = "?"
extractName t = error "trying to extract name from token " ++ show t

-- renderTerm' _ (Identifier i)  = annotate Cyan (case M.lookup i s of
--                    Just "" -> " "
--                    Just i' -> pretty i'
--                    Nothing -> error ("id lookup failed in renderer! id number was " ++ show i))
renderToken :: Token -> [String] -> Path -> Doc Annotation
renderToken (Identifier i) context p = annotate (Location p) $ annotate Cyan (pretty (context!!i))
renderToken (Name (Just "")) context p = annotate (Location p) $ " "
renderToken (Name (Just n)) context p = annotate (Location p) $ annotate Cyan (pretty n)
renderToken (Name Nothing) context p = annotate (Location p) $ annotate Cyan ("_____")
renderToken (Function x y z) context p  = annotate (Location p) $ Data.Text.Prettyprint.Doc.group (hang 1 (vcat ["λ" <> renderToken x context (p ++ [0]) <> ":" <> renderToken y context (p ++ [1]) <> ".", renderToken z (show x:context) (p ++ [2])]))
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
renderToken (FunctionType x y) context p  = annotate (Location p) $ align (sep [renderToken x context (p ++ [0]), "->", renderToken y context (p ++ [1])])
renderToken BoolType context p = annotate (Location p) $ annotate Yellow "Bool"
renderToken (Assignment x y z) context p  = annotate (Location p) $ x' <+> ":" <+> y' <> line <> x' <+> "=" <+> z'
        where x' = renderToken x context (p ++ [0])
              y' = renderToken y context (p ++ [1])
              z' = renderToken z (case x of 
                                Name Nothing -> "?":context
                                Name (Just n) -> n:context) (p ++ [2])
-- renderToken (Program x) context p = annotate (Location p) $ vsep (punctuate line x)
renderToken (Program x) context p = annotate (Location p) $ vsep (punctuate line (f context x 0))
        where f context (t@(Assignment (Name n) _ _):ts) i = renderToken t context [i]:f ((case n of
                                Just s -> s
                                Nothing -> "?"):context) ts (i+1)
              f _ _ _ = []
