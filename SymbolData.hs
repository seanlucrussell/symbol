{-# LANGUAGE OverloadedStrings #-}
module SymbolData
  ( Term
    ( IdentifierTerm
    , FunctionTerm
    , ApplicationTerm
    , BooleanLiteralTerm
    , ConditionalTerm
    , UnknownTerm
    , FnTypeTerm
    , BoolTypeTerm
    , Assignment
    , Program)
  , Container
    ( TopLevel
    , FunctionArg
    , FunctionArgType
    , FunctionBody
    , ApplicationFn
    , ApplicationArg
    , ConditionalCond
    , ConditionalOptOne
    , ConditionalOptTwo
    , AssignmentId
    , AssignmentType
    , AssignmentVal
    , FnTypeArg
    , FnTypeRet)
  , Zipper (Zipper)
  , validateZipper
  , zipperToTerm
  , z) where
import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}

data Term = IdentifierTerm T.Text
          | FunctionTerm Term Term Term
          | ApplicationTerm Term Term
          | BooleanLiteralTerm Bool
          | ConditionalTerm Term Term Term
          | UnknownTerm
          | FnTypeTerm Term Term
          | BoolTypeTerm
          | Assignment Term Term Term
          | Program [Term]
          deriving (Eq,Show)

data Type = BooleanType
          | FunctionType Type Type
          | UnknownType
          deriving (Eq,Show)

-- checks that a whole program is specified correctly
validateProgram :: Term -> Bool
validateProgram (Program t) = validateProgram' baseContext t
        where validateProgram' c (Assignment (IdentifierTerm a) t' b:ts) = case typeOf c b of
                Just t -> case termToType t' of
                        Just t'' -> if typeEquality t t'' then validateProgram' (updateContext c a t) ts else False
                        Nothing -> False
                Nothing -> False
              validateProgram' c (Assignment UnknownTerm t' b:ts) = case typeOf c b of
                Just t -> case termToType t' of
                        Just t'' -> if typeEquality t t'' then validateProgram' c ts else False
                        Nothing -> False
                Nothing -> False
              validateProgram' _ [] = True
              validateProgram' _ _ = False
validateProgram _ = False

type Context = T.Text -> Type

updateContext :: Context -> T.Text -> Type -> Context
updateContext c i t = \n -> if n == i then t else c n

baseContext :: Context
baseContext = \x -> UnknownType

typeEquality :: Type -> Type -> Bool
typeEquality UnknownType _ = True
typeEquality _ UnknownType = True
typeEquality BooleanType BooleanType = True
typeEquality (FunctionType a b) (FunctionType c d) = typeEquality a c && typeEquality b d
typeEquality _ _ = False

-- maps syntactic terms like FnTermType into the corresponding type representation
termToType :: Term -> Maybe Type
termToType (FnTypeTerm a b) = do a' <- termToType a
                                 b' <- termToType b
                                 return (FunctionType a' b')
termToType BoolTypeTerm = Just BooleanType
termToType UnknownTerm = Just UnknownType
termToType _ = Nothing

-- returns Nothing if the type checking is invalid
typeOf :: Context -> Term -> Maybe Type
typeOf c (IdentifierTerm i) = Just (c i)
typeOf c (FunctionTerm UnknownTerm t b) = do argType <- termToType t
                                             bodyType <- typeOf c b
                                             return (FunctionType argType bodyType)
typeOf c (FunctionTerm (IdentifierTerm i) t b) = do argType <- termToType t
                                                    bodyType <- typeOf (updateContext c i argType) b
                                                    return (FunctionType argType bodyType)
typeOf c (ApplicationTerm f x) = do fType <- typeOf c f
                                    xType <- typeOf c x
                                    case fType of   
                                      FunctionType a b -> if typeEquality xType a then return b else Nothing
                                      UnknownType -> Just UnknownType
                                      _ -> Nothing
typeOf _ (BooleanLiteralTerm _) = Just BooleanType
typeOf c (ConditionalTerm b x y) = do bType <- typeOf c b
                                      xType <- typeOf c x
                                      yType <- typeOf c y
                                      if typeEquality bType BooleanType && typeEquality xType yType
                                         then typeOf c x -- what if x is UnknownType but y isn't?
                                         else Nothing
typeOf _ UnknownTerm = Just UnknownType
typeOf _ _ = Nothing

data Container = TopLevel [Term] [Term]
               | FunctionArg Term Term Container
               | FunctionArgType Term Term Container
               | FunctionBody Term Term Container
               | ApplicationFn Term Container
               | ApplicationArg Term Container
               | ConditionalCond Term Term Container
               | ConditionalOptOne Term Term Container
               | ConditionalOptTwo Term Term Container
               | AssignmentId Term Term Container
               | AssignmentType Term Term Container
               | AssignmentVal Term Term Container
               | FnTypeArg Term Container
               | FnTypeRet Term Container
               deriving (Eq,Show)

data Zipper = Zipper Term Container deriving (Eq,Show)

goUp :: Term -> Container -> (Term, Maybe Container)
goUp t (TopLevel as bs) = (Program ((reverse as) ++ [t] ++ bs), Nothing)
goUp t (FunctionArg a b c) = (FunctionTerm t a b, Just c)
goUp t (FunctionArgType a b c) = (FunctionTerm a t b, Just c)
goUp t (FunctionBody a b c) = (FunctionTerm a b t, Just c)
goUp t (ApplicationFn a c) = (ApplicationTerm t a, Just c)
goUp t (ApplicationArg a c) = (ApplicationTerm a t, Just c)
goUp t (ConditionalCond a b c) = (ConditionalTerm t a b, Just c)
goUp t (ConditionalOptOne a b c) = (ConditionalTerm a t b, Just c)
goUp t (ConditionalOptTwo a b c) = (ConditionalTerm a b t, Just c)
goUp t (AssignmentId a b c) = (Assignment t a b, Just c)
goUp t (AssignmentType a b c) = (Assignment a t b, Just c)
goUp t (AssignmentVal a b c) = (Assignment a b t, Just c)
goUp t (FnTypeArg a c) = (FnTypeTerm t a, Just c)
goUp t (FnTypeRet a c) = (FnTypeTerm a t, Just c)

zipperToTerm :: Zipper -> Term
zipperToTerm (Zipper t c) = goToTop t (Just c)
        where goToTop t' (Just c') = let (t'',c'') = goUp t' c' in goToTop t'' c''
              goToTop t' Nothing = t'

validateZipper :: Zipper -> Bool
validateZipper = validateProgram . zipperToTerm

-- t = BooleanLiteralTerm True
-- f = BooleanLiteralTerm False
-- cond = ConditionalTerm
-- app = ApplicationTerm
-- fun = FunctionTerm
assign = Assignment
-- ident = IdentifierTerm
-- g = fun (ident "a") (BoolTypeTerm) (cond (ident "a") f t)
-- h = fun (ident "f") (FnTypeTerm BoolTypeTerm BoolTypeTerm) (ApplicationTerm (ident "f") t)
-- p = Program [assign (ident "g") g, assign (ident "h") h, assign (ident "result") (app (ident "h") (ident "g"))]

-- z = Zipper (assign (ident "h") h) (TopLevel [assign (ident "g") g] [assign (ident "result") (app (ident "h") (ident "g"))])
z = Zipper (assign UnknownTerm UnknownTerm UnknownTerm) (TopLevel [] [])
