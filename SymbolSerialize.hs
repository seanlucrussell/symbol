module SymbolSerialize 
        ( serialize
        , deserialize
        )
   where

import SymbolData
import AST hiding (Tree)

import qualified Data.Map
import qualified Data.Text
import Text.Read
import Text.ParserCombinators.Parsec

data Tree a = Tree a [Tree a]

type S = (SymbolTable, Token, Path)

tokenToString :: Token -> String
tokenToString (IdentifierTerm n) = "Id:" ++ show n
tokenToString (FunctionTerm _ _ _) = "Fn"
tokenToString (ApplicationTerm _ _) = "App"
tokenToString (TrueTerm        ) = "T"
tokenToString (FalseTerm       ) = "F"
tokenToString (ConditionalTerm _ _ _) = "If"
tokenToString (UnknownTerm     ) = "Unknown"
tokenToString (FunctionTypeTerm _ _) = "FnT"
tokenToString (BoolTypeTerm    ) = "BoolT"
tokenToString (AssignmentTerm  _ _ _) = "Assign"
tokenToString (Program         _ ) = "Prog"

treeToToken :: Tree String -> Maybe Token
treeToToken (Tree ('I':'d':':':n) []     ) = readMaybe n >>= (Just . IdentifierTerm)
treeToToken (Tree "Fn"            [a,b,c]) = do a' <- treeToToken a
                                                b' <- treeToToken b
                                                c' <- treeToToken c
                                                return (FunctionTerm a' b' c')
treeToToken (Tree "App"           [a,b]  ) = do a' <- treeToToken a
                                                b' <- treeToToken b
                                                return (ApplicationTerm a' b')
treeToToken (Tree "If"            [a,b,c]) = do a' <- treeToToken a
                                                b' <- treeToToken b
                                                c' <- treeToToken c
                                                return (ConditionalTerm a' b' c')
treeToToken (Tree "FnT"           [a,b]  ) = do a' <- treeToToken a
                                                b' <- treeToToken b
                                                return (FunctionTypeTerm a' b')
treeToToken (Tree "Assign"        [a,b,c]) = do a' <- treeToToken a
                                                b' <- treeToToken b
                                                c' <- treeToToken c
                                                return (AssignmentTerm a' b' c')
treeToToken (Tree "Prog"          ts     ) = do ts' <- sequence (fmap treeToToken ts)
                                                return (Program ts')
treeToToken (Tree "BoolT"         []     ) = Just BoolTypeTerm     
treeToToken (Tree "T"             []     ) = Just TrueTerm         
treeToToken (Tree "F"             []     ) = Just FalseTerm        
treeToToken (Tree "Unknown"       []     ) = Just UnknownTerm      
treeToToken _                              = Nothing

-- format:
--  (Data
--   (SymbolTable
--     (3929294 'var_name')
--     (4423911 'other_var'))
--   (Program
--     (Token
--       (Child-Token)
--       (Other-Child
--          (IntLit 8)
--          (BoolLit True))))
--   (Path (2) (1) (3 (4)))

-- serializer

serializeTree :: Tree String -> String
serializeTree t = serializeWithIndents t 0
        where serializeWithIndents :: Tree String -> Int -> String
              serializeWithIndents (Tree value subtrees) n = replicate n ' ' ++ foldl (subtreeFold (n+1)) ("(" ++ value) subtrees ++ ")"
              subtreeFold n b a = b ++ "\n" ++ serializeWithIndents a n

tableToTree :: SymbolTable -> Tree String
tableToTree table = Tree "SymbolTable" (fmap keyValToTree (Data.Map.toList table))
        where keyValToTree (k,v) = Tree "Key" [Tree (show k) [], Tree (Data.Text.unpack v) []]

programToTree :: Token -> Tree String
programToTree token = Tree (tokenToString token) (fmap programToTree (children token))

pathToTree :: Path -> Tree String
pathToTree path = Tree "Path" (fmap intToTree path)
        where intToTree n = Tree (show n) []

serialize :: S -> String
serialize (table, program, path) = serializeTree (Tree "Data" [ tableToTree table
                                                              , programToTree program
                                                              , pathToTree path])


-- deserializer

whitespace :: Parser [Char]
whitespace = many $ oneOf " \n\t"

-- <tree>  := (<token> <tree>*)
-- <token> := [a-zA-Z0-9:]+
-- need to do strings too

treeParser :: Parser (Tree String)
treeParser = do char '('
                whitespace
                token <- many1 (letter <|> digit <|> char ':')
                whitespace
                subTrees <- treeParser `sepBy` whitespace
                char ')'
                return (Tree token subTrees)

deserializeTree :: String -> Maybe (Tree String)
deserializeTree s = case parse treeParser "tree" s of
                        Left _ -> Nothing
                        Right t -> Just t

treeToPath :: Tree String -> Maybe Path
treeToPath (Tree "Path" subtrees) = mapM treeToInt subtrees
        where treeToInt (Tree n []) = readMaybe n
              treeToInt _ = Nothing
treeToPath _ = Nothing

treeToTable :: Tree String -> Maybe SymbolTable
treeToTable (Tree "SymbolTable" subtrees) = do keyValueList <- mapM treeToKeyValuePair subtrees
                                               return (Data.Map.fromList keyValueList)
        where treeToKeyValuePair (Tree "Key" [Tree k [],Tree v []]) = do n <- readMaybe k
                                                                         return (n, Data.Text.pack v)
              treeToKeyValuePair _ = Nothing
treeToTable _ = Nothing

-- need to make sure program is valid
treeToProgram :: Tree String -> Maybe Token
treeToProgram = treeToToken
-- treeToProgram (Tree string subtrees) = do token <- treeToToken string
--                                           subPrograms <- mapM treeToProgram subtrees
--                                           return (Tree token subPrograms)

-- path should point to a valid location in the program
-- every symbol used in program should be defined in symbol table
deserialize :: String -> Maybe S
deserialize s = do tree <- deserializeTree s
                   case tree of
                        Tree "Data" [a,b,c] -> do table <- treeToTable a
                                                  program <- treeToProgram b
                                                  path <- treeToPath c
                                                  return (table, program, path)
                        _ -> Nothing
                        

-- test serializer:
-- putStrLn $ serialize ((Data.Map.fromList [(43466,"not"),(932005,"and")]), ((Tree Program [Tree AssignmentTerm [Tree (IdentifierTerm 4245) [], Tree UnknownTerm [], Tree TrueTerm []]]), [4,3,2,1,0,0,0,3]))
