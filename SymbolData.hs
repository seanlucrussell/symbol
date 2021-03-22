{-# LANGUAGE OverloadedStrings #-}
module SymbolData where

import qualified Data.Text as T

{-# LANGUAGE XOverloadedStrings #-}


data Name = Name T.Text | UnknownName deriving (Eq,Show)
data Declare = Declare Name Type deriving (Eq,Show)

data Type = FunctionType Type Type
          | IntegerType
          | BooleanType
          | StringType
          | UnknownType
          deriving (Eq,Show)

data Op = Add | Multiply | GreaterThan | LessThan | Equal | Mod | And | Or deriving (Eq,Show)

data Value = StringLiteral T.Text
           | IntLiteral Integer
           | BooleanLiteral Bool
           | Variable T.Text
           | Function Declare Value
           | Call Value Value
           | UnknownValue
           | BinaryOperator Value Op Value
           deriving (Eq,Show)

data Assignment = Assign Declare Value
                  deriving (Eq,Show)

data Top = Top [Assignment]

data AssignmentContainer = TopLevel [Assignment] [Assignment]
                           deriving (Eq,Show)
data ValueContainer = CallName Value ValueContainer
                    | CallArgs Value ValueContainer
                    | AssignVal Declare AssignmentContainer
                    | OpFirst Op Value ValueContainer
                    | FnBody Declare ValueContainer
                    | OpSecond Value Op ValueContainer
                    deriving (Eq,Show)
data DeclareContainer = FnArgs Value ValueContainer
                      | AssignDecl Value AssignmentContainer
                      deriving (Eq,Show)
data NameContainer = DeclareName Type DeclareContainer deriving (Eq,Show)
data TypeContainer = DeclareType Name DeclareContainer
                   | FnTypeArgs Type TypeContainer
                   | FnTypeRet Type TypeContainer
                   deriving (Eq,Show)
data Zipper = ZipperAs Assignment AssignmentContainer
            | ZipperVal Value ValueContainer
            | ZipperDec Declare DeclareContainer
            | ZipperNam Name NameContainer
            | ZipperTyp Type TypeContainer
            deriving (Eq,Show)

exampleFunction :: Value
-- exampleFunction = Function
--                     (Declare
--                        (Name "f")
--                        (FunctionType IntegerType IntegerType))
--                     (Call
--                        (Variable "f")
--                        (Call
--                           (Variable "f") 
--                           (IntLiteral 0)))
exampleFunction = Function
                    (Declare
                       (Name "f")
                       IntegerType)
                    (Call
                       (Variable "f")
                       UnknownValue)

exampleAssignment :: Assignment
exampleAssignment = Assign (Declare UnknownName UnknownType) exampleFunction

blankAssignment = Assign (Declare UnknownName UnknownType) UnknownValue

example :: Zipper
-- example = ZipperAs exampleAssignment (TopLevel [] [blankAssignment])
example = ZipperVal (Variable "f") (CallName UnknownValue (FnBody (Declare (Name "f") IntegerType) (AssignVal (Declare UnknownName UnknownType) (TopLevel [] [Assign (Declare UnknownName UnknownType) UnknownValue]))))
-- example = ZipperAs blankAssignment (TopLevel [] [blankAssignment])
--
--
--
-- ZipperVal
--   (Call
--     (Variable "f")
--     UnknownValue)
--   (FnBody
--     (Declare
--       (Name "f")
--       IntegerType)
--     (AssignVal
--       (Declare
--         UnknownName
--         UnknownType)
--       (TopLevel
--         []
--         [Assign (Declare UnknownName UnknownType) UnknownValue])))
-- ZipperVal (Variable "f") (CallName UnknownValue (FnBody (Declare (Name "f") IntegerType) (AssignVal (Declare UnknownName UnknownType) (TopLevel [] [Assign (Declare UnknownName UnknownType) UnknownValue]))))
-- ZipperDec (Declare (Name "f") IntegerType) (FnArgs (Call (Variable "f") UnknownValue) (AssignVal (Declare UnknownName UnknownType) (TopLevel [] [Assign (Declare UnknownName UnknownType) UnknownValue])))
