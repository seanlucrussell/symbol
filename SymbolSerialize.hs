module SymbolSerialize where

import SymbolData
import AST

import qualified Data.Map
import qualified Data.Text
import Text.Read
import Text.ParserCombinators.Parsec

type S = (SymbolTable, Tree Token, Path)

tokenToString :: Token -> String
tokenToString (IdentifierTerm n) = "Id:" ++ show n
tokenToString (FunctionTerm    ) = "Fn"
tokenToString (ApplicationTerm ) = "App"
tokenToString (TrueTerm        ) = "T"
tokenToString (FalseTerm       ) = "F"
tokenToString (ConditionalTerm ) = "If"
tokenToString (UnknownTerm     ) = "Unknown"
tokenToString (FunctionTypeTerm) = "FnT"
tokenToString (BoolTypeTerm    ) = "BoolT"
tokenToString (AssignmentTerm  ) = "Assign"
tokenToString (Program         ) = "Prog"

stringToToken :: String -> Maybe Token
stringToToken ('I':'d':':':n) = readMaybe n >>= (Just . IdentifierTerm)
stringToToken "Fn"            = Just FunctionTerm     
stringToToken "App"           = Just ApplicationTerm  
stringToToken "T"             = Just TrueTerm         
stringToToken "F"             = Just FalseTerm        
stringToToken "If"            = Just ConditionalTerm  
stringToToken "Unknown"       = Just UnknownTerm      
stringToToken "FnT"           = Just FunctionTypeTerm 
stringToToken "BoolT"         = Just BoolTypeTerm     
stringToToken "Assign"        = Just AssignmentTerm   
stringToToken "Prog"          = Just Program          
stringToToken _               = Nothing

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

programToTree :: Tree Token -> Tree String
programToTree (Tree token subtrees) = Tree (tokenToString token) (fmap programToTree subtrees)

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
-- <token> := [a-zA-Z0-9]+

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
treeToProgram :: Tree String -> Maybe (Tree Token)
treeToProgram (Tree string subtrees) = do token <- stringToToken string
                                          subPrograms <- mapM treeToProgram subtrees
                                          return (Tree token subPrograms)

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
