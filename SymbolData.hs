{-# LANGUAGE OverloadedStrings #-}
module SymbolData where

import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}


data Name = Name T.Text | UnknownName deriving (Eq)
data Declare = Declare Name Type deriving (Eq)

data Type = FunctionType Type Type
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
           | Function Declare Value
           | Call Value Value
           | UnknownValue
           | BinaryOperator Value Op Value
           deriving (Eq)

data Assignment = Assign Declare Value
                  deriving (Eq)

data Top = Top [Assignment]

data AssignmentContainer = TopLevel [Assignment] [Assignment]
                           deriving (Eq)
data ValueContainer = CallName Value ValueContainer
                    | CallArgs Value ValueContainer
                    | AssignVal Declare AssignmentContainer
                    | OpFirst Op Value ValueContainer
                    | FnBody Declare ValueContainer
                    | OpSecond Value Op ValueContainer
                    deriving (Eq)
data DeclareContainer = FnArgs Value ValueContainer
                      | AssignDecl Value AssignmentContainer
                      deriving (Eq)
data NameContainer = DeclareName Type DeclareContainer deriving (Eq)
data TypeContainer = DeclareType Name DeclareContainer
                   | FnTypeArgs Type TypeContainer
                   | FnTypeRet Type TypeContainer
                   deriving (Eq)
data Zipper = ZipperAs Assignment AssignmentContainer
            | ZipperVal Value ValueContainer
            | ZipperDec Declare DeclareContainer
            | ZipperNam Name NameContainer
            | ZipperTyp Type TypeContainer
            deriving (Eq)

exampleFunction :: Value
exampleFunction = Function
                    (Declare
                       (Name "f")
                       (FunctionType IntegerType IntegerType))
                    (Call
                       (Variable "f")
                       (Call
                          (Variable "f") 
                          (IntLiteral 0)))

exampleAssignment :: Assignment
exampleAssignment = Assign (Declare UnknownName UnknownType) exampleFunction

example :: Zipper
example = ZipperAs exampleAssignment (TopLevel [exampleAssignment] [])

