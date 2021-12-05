module STLC.Serialize 
        ( serialize
        , deserialize
        )
   where

import STLC.Data
import AST hiding (Tree)

import qualified Data.Map
import qualified Data.Text
import Text.Read
import Text.ParserCombinators.Parsec

data Tree a = Tree a [Tree a]

type S = (SymbolTable, Token, Path)

tokenToTree :: Token -> Tree String
tokenToTree (Identifier n) = Tree ("Id:" ++ show n) []
tokenToTree (Function a b c) = Tree "Fn" (fmap tokenToTree [a,b,c])
tokenToTree (Application a b) = Tree "App" (fmap tokenToTree [a,b])
tokenToTree TrueTerm         = Tree "T" []
tokenToTree FalseTerm        = Tree "F" []
tokenToTree (Conditional a b c) = Tree "If" (fmap tokenToTree [a,b,c])
tokenToTree Unknown      = Tree "Unknown" []
tokenToTree (FunctionType a b) = Tree "FnT" (fmap tokenToTree [a,b])
tokenToTree BoolType     = Tree "BoolT" []
tokenToTree (Assignment  a b c) = Tree "Assign" (fmap tokenToTree [a,b,c])
tokenToTree (Program         ts ) = Tree "Prog" (fmap tokenToTree ts)

treeToToken :: Tree String -> Maybe Token
treeToToken (Tree ('I':'d':':':n) []) = readMaybe n >>= (Just . Identifier)
treeToToken (Tree "Fn"            ts) = do [a,b,c] <- mapM treeToToken ts
                                           return (Function a b c)
treeToToken (Tree "App"           ts) = do [a,b] <- mapM treeToToken ts
                                           return (Application a b)
treeToToken (Tree "If"            ts) = do [a,b,c] <- mapM treeToToken ts
                                           return (Conditional a b c)
treeToToken (Tree "FnT"           ts) = do [a,b] <- mapM treeToToken ts
                                           return (FunctionType a b)
treeToToken (Tree "Assign"        ts) = do [a,b,c] <- mapM treeToToken ts
                                           return (Assignment a b c)
treeToToken (Tree "Prog"          ts) = do ts' <- mapM treeToToken ts
                                           return (Program ts')
treeToToken (Tree "BoolT"         []) = Just BoolType     
treeToToken (Tree "T"             []) = Just TrueTerm         
treeToToken (Tree "F"             []) = Just FalseTerm        
treeToToken (Tree "Unknown"       []) = Just Unknown      
treeToToken _                         = Nothing

-- serializer

serializeTree :: Tree String -> String
serializeTree t = serializeWithIndents t 0
        where serializeWithIndents :: Tree String -> Int -> String
              serializeWithIndents (Tree value subtrees) n = replicate n ' ' ++ foldl (subtreeFold (n+1)) ("(" ++ value) subtrees ++ ")"
              subtreeFold n b a = b ++ "\n" ++ serializeWithIndents a n

tableToTree :: SymbolTable -> Tree String
tableToTree table = Tree "SymbolTable" (fmap keyValToTree (Data.Map.toList table))
        where keyValToTree (k,v) = Tree "Key" [Tree (show k) [], Tree (Data.Text.unpack v) []]

pathToTree :: Path -> Tree String
pathToTree path = Tree "Path" (fmap intToTree path)
        where intToTree n = Tree (show n) []

serialize :: S -> String
serialize (table, program, path) = serializeTree (Tree "Data" [ tableToTree table
                                                              , tokenToTree program
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
-- putStrLn $ serialize ((Data.Map.fromList [(43466,"not"),(932005,"and")]), ((Tree Program [Tree Assignment [Tree (Identifier 4245) [], Tree Unknown [], Tree TrueTerm []]]), [4,3,2,1,0,0,0,3]))
