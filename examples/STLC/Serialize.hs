module STLC.Serialize 
        ( serialize
        , deserialize
        )
   where

import STLC.Data
import AST hiding (Tree)

import Text.Read
import Text.ParserCombinators.Parsec

data Tree a = Tree a [Tree a]

type S = (Token, Path)

tokenToTree :: Token -> Tree String
tokenToTree (Identifier n) = Tree ("Id:" ++ show n) []
tokenToTree (Name Nothing) = Tree "EmptyName" []
tokenToTree (Name (Just n)) = Tree ("Name:" ++ n) []
tokenToTree (Function a b c) = Tree "Fn" (fmap tokenToTree [a,b,c])
tokenToTree (Application a b) = Tree "App" (fmap tokenToTree [a,b])
tokenToTree TrueTerm         = Tree "T" []
tokenToTree FalseTerm        = Tree "F" []
tokenToTree (Conditional a b c) = Tree "If" (fmap tokenToTree [a,b,c])
tokenToTree Unknown      = Tree "Unknown" []
tokenToTree (FunctionType a b) = Tree "FnT" (fmap tokenToTree [a,b])
tokenToTree BoolType     = Tree "BoolT" []
tokenToTree (Assignment a b c d) = Tree "Assign" (fmap tokenToTree [a,b,c,d])
tokenToTree EndOfProgram     = Tree "EOP" []

treeToToken :: Tree String -> Maybe Token
treeToToken (Tree ('I':'d':':':n) []) = readMaybe n >>= (Just . Identifier)
treeToToken (Tree ('N':'a':'m':'e':':':n) []) = Just (Name (Just n))
treeToToken (Tree "EmptyName" []) = Just (Name Nothing)
treeToToken (Tree "Fn"            ts) = do [a,b,c] <- mapM treeToToken ts
                                           return (Function a b c)
treeToToken (Tree "App"           ts) = do [a,b] <- mapM treeToToken ts
                                           return (Application a b)
treeToToken (Tree "If"            ts) = do [a,b,c] <- mapM treeToToken ts
                                           return (Conditional a b c)
treeToToken (Tree "FnT"           ts) = do [a,b] <- mapM treeToToken ts
                                           return (FunctionType a b)
treeToToken (Tree "Assign"        ts) = do [a,b,c,d] <- mapM treeToToken ts
                                           return (Assignment a b c d)
treeToToken (Tree "BoolT"         []) = Just BoolType     
treeToToken (Tree "T"             []) = Just TrueTerm         
treeToToken (Tree "F"             []) = Just FalseTerm        
treeToToken (Tree "EOP"             []) = Just EndOfProgram
treeToToken (Tree "Unknown"       []) = Just Unknown      
treeToToken _                         = Nothing

-- serializer

serializeTree :: Tree String -> String
serializeTree t = serializeWithIndents t 0
        where serializeWithIndents :: Tree String -> Int -> String
              serializeWithIndents (Tree value subtrees) n = replicate n ' ' ++ foldl (subtreeFold (n+1)) ("(" ++ value) subtrees ++ ")"
              subtreeFold n b a = b ++ "\n" ++ serializeWithIndents a n

pathToTree :: Path -> Tree String
pathToTree path = Tree "Path" (fmap intToTree path)
        where intToTree n = Tree (show n) []

serialize :: S -> String
serialize (program, path) = serializeTree (Tree "Data" [ tokenToTree program
                                                              , pathToTree path])


-- deserializer

whitespace :: Parser [Char]
whitespace = many $ oneOf " \n\t"

-- <tree>  := (<token> <tree>*)
-- <token> := [a-zA-Z0-9:]+
-- need to do strings too

treeParser :: Parser (Tree String)
treeParser = do _ <- char '('
                _ <- whitespace
                treeToken <- many1 (letter <|> digit <|> char ':')
                _ <- whitespace
                subTrees <- treeParser `sepBy` whitespace
                _ <- char ')'
                return (Tree treeToken subTrees)

deserializeTree :: String -> Maybe (Tree String)
deserializeTree s = case parse treeParser "tree" s of
                        Left _ -> Nothing
                        Right t -> Just t

treeToPath :: Tree String -> Maybe Path
treeToPath (Tree "Path" subtrees) = mapM treeToInt subtrees
        where treeToInt (Tree n []) = readMaybe n
              treeToInt _ = Nothing
treeToPath _ = Nothing

-- path should point to a valid location in the program
deserialize :: String -> Maybe S
deserialize s = do tree <- deserializeTree s
                   case tree of
                        Tree "Data" [b,c] -> do program <- treeToToken b
                                                path <- treeToPath c
                                                return (program, path)
                        _ -> Nothing
                        

-- test serializer:
-- putStrLn $ serialize ((Data.Map.fromList [(43466,"not"),(932005,"and")]), ((Tree Program [Tree Assignment [Tree (Identifier 4245) [], Tree Unknown [], Tree TrueTerm []]]), [4,3,2,1,0,0,0,3]))
