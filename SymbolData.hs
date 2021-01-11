{-# LANGUAGE OverloadedStrings #-}
module SymbolData where

import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}


data Name = Name T.Text | UnknownName deriving (Eq)
data Declare = Declare Name Type deriving (Eq)

data Type = FunctionType [Type] Type
          | IntegerType
          | BooleanType
          | StringType
          | UnknownType
          deriving (Eq)

data Op = Add | Multiply | GreaterThan | LessThan | Equal | Mod | And | Or deriving (Eq)

data Value = StringLiteral T.Text
           | IntLiteral Integer
           | BooleanLiteral Bool
           | Variable T.Text
           | Function [Declare] Statement
           | Call Value [Value]
           | UnknownValue
           | BinaryOperator Value Op Value
           deriving (Eq)

data Statement = Assign Declare Value
               | Block [Statement]
               | Return Value
               | If Value Statement
               | UnknownStatement
               deriving (Eq)

-- not really necessary for the editor, but probably important for converting
-- the zipper into the real ast representation
data Top = Top [Statement]

data StatementContainer = FnBody [Declare] ValueContainer
                        | Blk [Statement] [Statement] StatementContainer
                        | IfBody Value StatementContainer
                        | TopLevel [Statement] [Statement]
                        deriving (Eq)
data ValueContainer = CallName [Value] ValueContainer
                    | CallArgs Value [Value] [Value] ValueContainer
                    | Ret StatementContainer
                    | IfCond Statement StatementContainer
                    | AssignVal Declare StatementContainer
                    | OpFirst Op Value ValueContainer
                    | OpSecond Value Op ValueContainer
                    deriving (Eq)
data DeclareContainer = FnArgs [Declare] [Declare] Statement ValueContainer
                      | AssignDecl Value StatementContainer
                      deriving (Eq)
data NameContainer = DeclareName Type DeclareContainer deriving (Eq)
data TypeContainer = DeclareType Name DeclareContainer
                   | FnTypeArgs [Type] [Type] Type TypeContainer
                   | FnTypeRet [Type] TypeContainer
                   deriving (Eq)
data Zipper = ZipperSt Statement StatementContainer
            | ZipperVal Value ValueContainer
            | ZipperDec Declare DeclareContainer
            | ZipperNam Name NameContainer
            | ZipperTyp Type TypeContainer
            deriving (Eq)

-- containers are basically a linked list with proper typing that can be used to
-- reconstruct the datastructure. For instance, a StatementContainer together
-- with a Statement can be used to go up 1 level
-- concept for making list stuff simpler. a lot of list-related functions are
-- the same, so having one type to encapsulate all that would prolly be a help
-- data ListContainer a b = ListContainer [a] [a] b

-- a = If (StringLiteral "is it Sunday?") UnknownStatement
a = Assign (Declare (Name "max") (FunctionType [UnknownType, IntegerType] IntegerType))
           UnknownValue

b = Block [ Assign (Declare UnknownName UnknownType) (IntLiteral 365)
          , a
          , If (BinaryOperator UnknownValue Equal (StringLiteral "Sonata"))
               (Return (BooleanLiteral False))
          ]

c = ZipperSt UnknownStatement (TopLevel [a] [b])

blankZipper = ZipperSt UnknownStatement (TopLevel [] [])

-- new typing concept; make sure type of things match! e.g.
--
-- data Declare a = Declare Name (Type a)
-- data Assign a = Assign (Declare a) (Value a)
-- class (Arguments a, Type b) => Function a b

-- gadt for zippers? e.g. define a zipper as
--
-- data Zipper a = Zipper a (Parent a)
--
-- question: what would the type of goup be then? maybe something like 
--
-- goup :: Zipper a -> Zipper (Container a)
--
-- ? maybe something like
--
-- class Zippable a where
--   Reassemble :: a -> Container a b -> b
