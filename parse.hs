module Main where
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Control.Monad.State 
import Control.Applicative ((<*))
import Data.List (intercalate)
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import qualified Data.Map as M

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse p s inp = runIndent s $ runParserT p () s inp

data Tree = Tree Expression [Tree]
    deriving (Show)

type Attrs = [(String, String)]
type InlineContent = String

data Expression = Comment String 
    | PlainText String
    | RubyStartBlock String
    | RubyMidBlock String
    | RubyExp String
    | Tag String Attrs InlineContent
    | GenericExpression String 
    deriving (Show)

container :: IParser Tree
container = do
  b <- withBlock Tree expression container
  spaces
  return b

expression :: IParser Expression
expression = comment <|> startPlainText <|> rubyBlock <|> rubyExp <|> tag <|> genericExpression
  
rubyBlock = do
    char '-'
    spaces
    k <- rubyKeyword
    rest <- manyTill anyChar newline <* spaces
    if (k `elem` midBlockKeywords)
    then return (RubyMidBlock $ k ++ rest)
    else return (RubyStartBlock $ k ++ rest) 
  where midBlockKeywords = ["else", "elsif", "rescue", "ensure", "when", "end"]

rubyExp = RubyExp <$> ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)

tag :: IParser Expression
tag = do
    tag <- explicitTag <|> return "div"
    cs <- many dotClass
    hs <- option [] (hashAttrs)
    s <- manyTill anyChar newline
    spaces
    return $ Tag tag (attrs cs hs) s
  where 
    attrs cs hs = filter (\(k, v) -> v /= "") $ 
      M.toList $ 
      M.unionWith (\a b -> a ++ " " ++ b) 
        (M.fromList (makeClassAttrs cs)) 
        (M.fromList hs)

explicitTag = do
  char '%'
  tag <- many alphaNum
  return tag

dotClass = char '.' >> (many (alphaNum <|> char '-'))

hashAttrs = do
  char '{' 
  xs <- kvPair `sepBy` (spaces >> char ',' >> spaces)
  char '}'
  return xs

rubyVar = many (alphaNum <|> char '_')

rubyKeyword = many alphaNum

rubyString1 = do
  char '\'' 
  xs <-  many (noneOf "'")
  char '\''
  -- we don't need to include the quotes
  return xs 

rubyString2 = do
  char '"'
  xs <- many (noneOf "\"")
  char '"'
  -- TODO interpolation markers should change from #{ } to <% %>
  return $  "FIX: " ++ xs 

rubyString = rubyString1 <|> rubyString2

rubySymbol =  char ':' >> rubyVar

rubyInlineCode = do
  xs <- many (noneOf "}")
  return $ "<%= " ++ xs ++ " %>"

aKey = rubyString <|> rubySymbol
  
kvPair :: IParser (String, String)
kvPair = do
  k <- aKey
  spaces >> string "=>" >> spaces
  v <- rubyString <|> rubyInlineCode
  return (k, v)

makeClassAttrs :: [String] -> [(String, String)]
makeClassAttrs cs = [("class", vals)]
    where vals = intercalate " " cs 

comment :: IParser Expression
comment = do
  char '/' 
  s <- manyTill anyChar newline
  return $ Comment s

startPlainText = do 
  spaces
  a <- noneOf "-=.#%" 
  b <- manyTill anyChar newline
  spaces
  return $ PlainText (a:b)

genericExpression :: IParser Expression
genericExpression = do
  spaces
  s <- manyTill anyChar newline
  spaces
  return $ GenericExpression s


------------------------------------------------------------------------
-- output ERB
-- turn tree structure into an array of lines, including closing tags and indentation level


type Nesting = Int

erb ::  Nesting -> Tree -> [String]
erb n tree@(Tree (Tag t a i) xs) = 
    (pad n ++ startTag tree) : (processChildren n xs ++ [pad n ++ endTag tree])

erb n tree@(Tree (RubyStartBlock s) xs) = 
    (pad n ++ "<% " ++ s ++ " %>") : processChildren n xs
erb n tree@(Tree (RubyMidBlock s) xs) = 
    (pad n ++ "<% " ++ s ++ " %>") : processChildren n xs

erb n tree@(Tree (RubyExp s) _) = [pad n ++ "<%= " ++ s ++ " %>"] 
erb n tree@(Tree (PlainText s) _) = [pad n ++ s] 
erb n tree@(Tree (Comment s) _) = [pad n ++ "<!-- " ++ s ++ " -->"] 

erb n x@_ = [pad n ++ show x]

processChildren n xs = concat $ map (erb (n + 1)) $ endtags xs

-- This function tries to insert "<% end %>" tags correctly
endtags (x@(Tree (RubyStartBlock _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(endtags (y:xs))  -- just shift the cursor to the right
endtags (x@(Tree (RubyMidBlock _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(endtags (y:xs))
endtags (x@(Tree (RubyStartBlock _) _):xs) = x:(Tree (PlainText "<% end %>") []):(endtags xs)
endtags (x@(Tree (RubyMidBlock _) _):xs) = x:(Tree (PlainText "<% end %>") []):(endtags xs)
endtags (x:xs) = x : (endtags xs)
endtags [] = []


startTag :: Tree -> String
startTag (Tree (Tag t a i) _) = "<" ++ t ++ showAttrs a ++ ">" ++ inline i

endTag :: Tree -> String
endTag (Tree (Tag t _ _) _) = "</" ++ t ++ ">"

showAttrs xs = case map makeAttr xs of 
      [] -> ""
      xs' -> " " ++ intercalate " " xs'
    where makeAttr (k,v) =  intercalate "=" [k, "\"" ++ v ++ "\"" ]

inline x = x
    
pad :: Int -> String
pad n = take (n * 2) $ repeat ' ' 

------------------------------------------------------------------------

main = do
    s <- getContents
    case iParse container "" s of 
        Left err -> putStrLn (show err)
        Right s' -> do 
            -- putStrLn (show s')
            putStrLn . unlines $ erb 0 s'




