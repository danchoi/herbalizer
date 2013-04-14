{-# LANGUAGE DoAndIfThenElse #-}
module Main where
import Control.Applicative ((<$>), (<*>), (<*), (<$))
import Control.Monad (liftM)
import Control.Monad.State 
import Control.Applicative ((<*))
import Data.List (intercalate)
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Text.Parsec.Pos
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Data.List.Utils
import Data.List (isInfixOf)
import Text.Regex.Posix
import System.Environment

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse p s inp = runIndent s $ runParserT p () s inp

data Tree = Tree Expression [Tree]
    deriving (Show)

type Attrs = [(String, String)]
data InlineContent = RubyInlineContent String 
    | PlainInlineContent String
    | NullInlineContent
    | HamlFilterContent String
    deriving (Show, Eq)


type IsFormTag = Bool

data Expression = 
      DocType String
    | Comment String 
    | PlainText String
    | RubyStartBlock String IsFormTag
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
expression = docType <|> comment <|> hamlFilter <|> startPlainText <|> rubyBlock <|> rubyExp <|> tag <|>  genericExpression
  
rubyBlock = do
    char '-'
    spaces
    k <- rubyKeyword
    rest <- manyTill anyChar newline <* spaces
    if (k `elem` midBlockKeywords)
    then return (RubyMidBlock $ k ++ rest)
    -- TODO : we need to recognize Ruby expression expression included purely for a side effect,
    -- e.g. "- localvar = Time.now"
    else return (RubyStartBlock (k ++ rest) False)
  where midBlockKeywords = ["else", "elsif", "rescue", "ensure", "when", "end"]

rubyExp = do
  line <- ((:) <$> char '=' >> spaces >> manyTill anyChar newline <* spaces)
  return (RubyExp line)

tag :: IParser Expression
tag = do
    tag <- explicitTag <|> return "div"
    as <- many (dotClass <|> idHash)
    hs <- option [] (hashAttrs)
    many $ oneOf " \t"
    c <- parseInlineContent 
    spaces
    return $ Tag tag (attrs as hs) c
  where 
    attrs as hs = filter (\(k, v) -> v /= "") $ 
      M.toList $ 
      M.unionWith (\a b -> intercalate " " (filter (/= "") [a,b]))
        (M.fromList hs)
        (M.fromList (makeClassIdAttrs as)) 
    parseInlineContent = (RubyInlineContent <$> (char '=' >> spaces >> manyTill anyChar newline)) <|> 
        (PlainInlineContent <$> (manyTill anyChar newline)) 
        <|> return NullInlineContent

makeClassIdAttrs :: [String] -> [(String, String)]
makeClassIdAttrs cs = classes : [("id", ids)]
    where classes = ("class", intercalate " " $ map tail $ filter ("." `isPrefixOf`) cs )
          ids = intercalate " " $ map tail $ filter (isPrefixOf "#") cs


explicitTag = do
  char '%'
  tag <- many alphaNum
  return tag

dotClass = (:) <$> char '.' <*> cssClassOrId
idHash = (:) <$> char '#' <*> cssClassOrId

hashAttrs = do
  char '{' 
  xs <- kvPair `sepBy` (spaces >> char ',' >> spaces)
  char '}'
  return xs

cssClassOrId = many (alphaNum <|> oneOf "-_")
rubyIdentifier = many (alphaNum <|> char '_')

rubyKeyword = many alphaNum

singleQuotedStr = do
    between (char '\'') (char '\'') (many stringChar)
  where stringChar = ('\'' <$ string "\\'") <|> (noneOf "'")

doubleQuotedStr = do
    between (char '"') (char '"') (many stringChar)
  where stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"")

--- Ruby interpolation delimiters crudley replaced by ERB style
rubyString = do
    between (char '"') (char '"') rString
  where 
    rString = liftM replaceInterpolationDelim $ many stringChar
    stringChar = ('"' <$ string "\\\"") <|> (noneOf "\"") 
    replaceInterpolationDelim = (replace "#{" "<%= ") . (replace "}" " %>")

rubySymbol =  char ':' >> rubyIdentifier
rubySymbolKey = rubyIdentifier <* char ':'

-- really, we need to parse full-blown Ruby expressions
rubyValue = do
    xs <- many (noneOf ",([ \t")  <* spaces
    rest <- ((lookAhead (oneOf ",}") >> return ""))
            <|> (betweenStuff '(' ')' )
            <|> (betweenStuff '[' ']' )
    return $ "<%= " ++ xs ++ rest ++ " %>"
  where 
    betweenStuff x y = do
      xs' <- between (char x) (char y) (many $ noneOf [y])
      return $ [x] ++ xs' ++ [y]

rocket = spaces >> string "=>" >> spaces 
aKey = (singleQuotedStr <* rocket)
  <|> (doubleQuotedStr <* rocket)
  <|> (rubySymbol <* rocket)
  <|> (rubySymbolKey <* spaces)

aValue = singleQuotedStr <|> rubyString <|> many1 digit <|> rubyValue

kvPair :: IParser (String, String)
kvPair = do
  k <- (many $ oneOf " \t") >> aKey 
  v <- spaces >> aValue <* (many $ oneOf " \t")
  return (k, v)

-- TODO HTML Comments are not rendered like HAML renders them
-- also HAML -# style comments could be rendered
comment :: IParser Expression
comment = do
  char '/' 
  s <- manyTill anyChar newline
  spaces
  return $ Comment s

docType :: IParser Expression
docType = do
    string "!!!"
    many $ char ' '
    s <- option [] $ many alphaNum
    newline
    return $ DocType s

filterBlock p = withPos $ do
    r <- many (checkIndent >> p)
    return r

hamlFilter = do
  withPos $ do
    char ':' 
    s <- many $ alphaNum
    many (oneOf " \t")
    newline
    xs <- indentedOrBlank
    return $ Tag (convertToTag s) [] (HamlFilterContent $ concat xs)
  where convertToTag "javascript" = "script"
        convertToTag s = s

indentedOrBlank = many1 (try blankLine <|> try indentedLine)

indentedLine :: IParser String
indentedLine = do
    a <- many $ oneOf " \t"
    indented
    xs <- manyTill anyChar newline 
    return $ a ++ xs ++ "\n"

blankLine = do
    a <- many $ oneOf " \t"
    newline 
    return $ a ++ "\n"

-- TODO; check how this deals with plain text that actually starts with these characters
-- Not sure what HAML's escaping rules are here; again HAML makes things unclear & make you
-- to look at docs

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

-- This is the main processing entrypoint
processChildren :: Nesting -> [Tree] -> [String]
processChildren n xs = concat $ map (erb n) $ (rubyEnd xs)

erb ::  Nesting -> Tree -> [String]

erb n tree@(Tree (Tag t a i) []) 
    | t `elem` selfClosingTags = [pad n ++ selfClosingTag tree]
    -- basically ignores inline content
  where selfClosingTags = ["br", "img", "hr", "meta"]

-- no children; no padding, just tack closing tag on end
erb n tree@(Tree (Tag t a i) []) = [pad n ++ startTag tree ++ endTag 0 tree] 

erb n tree@(Tree (Tag t a i) xs) = (pad n ++ startTag tree) : ((processChildren (n + 1) xs) ++ [endTag n tree])

erb n tree@(Tree (RubyStartBlock s isform) xs) = 
    (pad n ++ (starttag isform) ++ s ++ " %>") : (processChildren (n + 1) xs)
  where 
    starttag True = "<%= "
    starttag False = "<% "


erb n tree@(Tree (RubyMidBlock s) xs) = 
    (pad n ++ "<% " ++ s ++ " %>") : (processChildren (n + 1) xs)

erb n tree@(Tree (RubyExp s) _) = [pad n ++ "<%= " ++ s ++ " %>"] 

erb n tree@(Tree (PlainText s) _) = [pad n ++ s] 
erb n tree@(Tree (Comment s) xs) = (pad n ++ "<!--" ++ s) : ((processChildren (n + 1) xs) ++ [pad n  ++ "-->"])

-- DocTypes
erb n tree@(Tree (DocType s) _) = [d s]
  where
    d "" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    d "Strict" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    d "Frameset" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
    d "1.1" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
    d "Basic" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\">"
    d "Mobile" = "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">"
    d "RDFa" = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML+RDFa 1.0//EN\" \"http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd\">"
    d "5" = "<!DOCTYPE html>"


    d _ = "TEST"

erb n x@_ = [pad n ++ show x]


endTagTree = Tree (PlainText "<% end %>") []

-- Try to insert "<% end %>" tags correctly
rubyEnd (x@(Tree (RubyStartBlock _ _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(rubyEnd (y:xs))  -- just shift the cursor to the right
rubyEnd (x@(Tree (RubyMidBlock _) _):y@(Tree (RubyMidBlock _) _):xs) = 
    x:(rubyEnd (y:xs))
rubyEnd (x@(Tree (RubyStartBlock _ _) _):xs) = x:endTagTree:(rubyEnd xs)
rubyEnd (x@(Tree (RubyMidBlock _) _):xs) = x:endTagTree:(rubyEnd xs)

-- RubyExp with children with probably a form_for or the like; convert to a RubyStartBlock
rubyEnd (x@(Tree (RubyExp s) children@(c:cs)):xs) = rubyEnd $ (Tree (RubyStartBlock s True) children):xs

-- Move inline Ruby expressions to child tree
rubyEnd (x@(Tree (Tag t a (RubyInlineContent s)) ts):xs) = 
  (Tree (Tag t a NullInlineContent) ((Tree (RubyExp s) []):ts)):(rubyEnd xs)



-- erb content should pass through
rubyEnd (x@(Tree (Tag "erb" a (HamlFilterContent s)) ts):xs) = (Tree (PlainText s) []):(rubyEnd xs)

-- Move HamlFilterContent to child tree
rubyEnd (x@(Tree (Tag t a (HamlFilterContent s)) ts):xs) = (Tree (Tag t a NullInlineContent) ((Tree (PlainText ('\n':s)) []):ts)):(rubyEnd xs)





rubyEnd (x:xs) = x : (rubyEnd xs)
rubyEnd [] = []



startTag :: Tree -> String
startTag (Tree (Tag t a i) _) = "<" ++ t ++ showAttrs a ++ ">" ++ showInlineContent i

endTag :: Int -> Tree -> String
endTag n (Tree (Tag t _ _) _) = pad n ++ "</" ++ t ++ ">"

selfClosingTag :: Tree -> String
selfClosingTag (Tree (Tag t a _) _) = "<" ++ t ++ showAttrs a ++ "/>"

showAttrs xs = case map makeAttr xs of 
      [] -> ""
      xs' -> " " ++ intercalate " " xs'
    where makeAttr (k,v) =  intercalate "=" [k, "\"" ++ v ++ "\"" ]

showInlineContent (PlainInlineContent s) = s
showInlineContent (NullInlineContent) = ""
-- should not be reached:
showInlineContent (RubyInlineContent s) = "RUBY: " ++ s 

showInlineContent s = "\nERROR: No showInlineContent for " ++ (show s) ++ "\n"

    
pad :: Int -> String
pad n = take (n * 2) $ repeat ' ' 

------------------------------------------------------------------------

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
    Left err -> Left err
    Right ys -> case f x of 
                  Left err -> Left err
                  Right y -> Right (y:ys)
mapEithers _ _ = Right []

------------------------------------------------------------------------

parse1 s = iParse container "" s 

-- http://stackoverflow.com/questions/15549050/haskell-parsec-how-do-you-use-the-functions-in-text-parsec-indent
runIndentParser :: (SourcePos -> SourcePos) 
          -> IndentParser String () a 
          -> String -> Either ParseError a
runIndentParser f p src = fst $ flip runState (f $ initialPos "") $ runParserT p () "" src

topLevelsParser1 = many1 (topLevelItem)

topLevelItem = do
  withPos $ do
    as <- manyTill anyChar newline
    xs <- option [] (try indentedOrBlank)
    return $ as ++ "\n" ++ concat xs

parseTopLevels s =
    case (runIndentParser id topLevelsParser1 s) of
      Left err -> putStrLn (show err)
      Right chunks -> do
        case (mapEithers parse1 chunks) of
          Left err -> putStrLn . show $ err
          Right trees -> do
            mapM_ putStrLn $ processChildren 0 trees

main = do
    args <- getArgs
    if (null args)
    then
      getContents >>= parseTopLevels 
    else
      mapM_ (\f -> readFile f >>= parseTopLevels) args
      
