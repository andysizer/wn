-- file: parseWml.hs

module ParseWml
(
  parseWml
) 
    where

import Data.List
import Text.Parsec.Prim (unexpected)
import Text.Parsec.Pos (newPos)
import Numeric

import ApplicativeParsec

-- state for parser

data ParseState = ParseState
    {
      pDomain :: !String
    , pHistory :: !String
    }
        deriving (Eq, Show)

initState = ParseState "" ""

type WmlParser = GenParser Char ParseState [NodeItem]
type WmlNodeParser = GenParser Char ParseState NodeItem

-- default node type including top level
data Node = Node
    {
      nName :: !String
    , nBody :: !NodeBody
    }
        deriving (Eq, Show)

emptyNode = NNode $ N $ Node "" []

-- merge nodes 
data MergeNode = MergeNode
    {
      mName :: !String
    , mBody :: !NodeBody
    }
        deriving (Eq, Show)

type NodeBody = [NodeItem]

data WmlConfig = N !Node
               | M !MergeNode
        deriving (Eq, Show)

data NodeItem = NAtt !Attribute
              | NNode !WmlConfig
        deriving (Eq, Show)

parseWml s = do
    case runParser topLevel initState "" s of
        Right r -> r
        Left e -> error $ show e

-- topLevel = 

topLevel :: WmlParser
topLevel = manyTill (spaces *> topLevelItem) eof

--topLevelItem :: WmlNodeParser
topLevelItem =
        marker *> sourceOrDomain *> spaces *> topLevelItem
    <|> hash *> domain *> spaces *> topLevelItem
    <|> topLevelNode

sourceOrDomain =
        char 'l' *> sourceInfo
    <|> domain

sourceInfo = do
    string "ine "
    setSrcPos
    h <- many (noneOf "\n")
    s <- getState
    setState $ ParseState (pDomain s) h
    anyChar

setSrcPos = do
    (src, line, column) <- historyItem
    setPosition $ newPos src line column

historyItem = do
    line <- lineNumber
    column <- columnNumber
    src <- fileName 
    return (src, line, column)

lineNumber = do
    spaces
    ds <- many digit
    spaces
    let [(n,_)] = readDec ds
    return n

columnNumber = lineNumber

fileName = do
    spaces
    between (char '"') (char '"') (many (noneOf "\""))

domain = do
    string "textdomain "
    d <- many (noneOf "\n")
    st <- getState
    setState $ ParseState d (pHistory st)
    anyChar

-- Main node parser
parseNode :: (a -> WmlConfig) -> (String -> NodeBody -> a) -> WmlNodeParser
parseNode c1 c2 = do
    start <- tagName
    body <- nodeBody
    end <- tagName
    mkNode c1 c2 start body end

tagName = manyTill namechars rb <* spaces

mkNode c1 c2 s b e
    | s == e = return $ NNode $ c1 $ c2 s b
    | otherwise = unexpected (": end tag '" ++ e ++ "' does not match '" ++ s ++ "'")

-- 'constructors' for different node types
node :: WmlNodeParser
node = parseNode N Node

mergeNode :: WmlNodeParser
mergeNode = parseNode M MergeNode

-- Top Level Node parser
topLevelNode :: WmlNodeParser
topLevelNode = do
    lb
    internalNode

-- parsing node bodies. NB no 'try's.
nodeBody :: WmlParser
nodeBody = 
        lb *> maybeEndBody
    <|> marker *> sourceOrDomain *> spaces *> nodeBody
    <|> hash *> domain *> spaces *> nodeBody
    <|> attributeThenNodeBody

-- maybeEndBody
-- we just hit [, so its either an end tag (NB don't consume the tag name)  
-- or a node followed by...
maybeEndBody =
        char '/' *> (return [])
    <|> do n <- internalNode
           spaces
           r <- nodeBody
           return (n : r)

-- attributeThenNodeBody
-- we didn't see [, so must be an attribute followed by ...
attributeThenNodeBody = do
    a <- attribute
    spaces
    b <- nodeBody
    return (NAtt a : b)

internalNode :: WmlNodeParser
internalNode =
        char '+' *> mergeNode
    <|> node

-- Attribute
data Attribute = Attribute
    {
      aKeys  :: ![String]
    , aValues :: ![[AttributeValue]]
    }
        deriving (Eq, Show)

attribute = do
    spaces
    keys <- attributeKeys
    char '='
    spaces
    values <- attributeValues
    return $ Attribute keys values

attributeKeys = attributeKey `sepBy1` comma

attributeKey = many namechars <* spaces

attributeValues = concatenatedAttributeValue `sepBy` comma

concatenatedAttributeValue = attributeValue `sepBy` plus

data AttributeValue = UnquotedString !String
                    | Formula !String
                    | QuotedString !String
                    | Translatable !TranslatableString
                    | Code String
        deriving (Eq, Show)

attributeValue =
        annotation attributeValue
    <|> UnquotedString <$> leadingUnderscore
    <|> Formula <$> formulaValue
    <|> QuotedString <$> quotedStringValue
    <|> Translatable <$> translatableStringValue
    <|> Code <$> literalCodeValue
    <|> UnquotedString <$> unquotedStringValue

annotation p = marker *> sourceOrDomain *> spaces *> p

optionalAnnotation =
        marker *> sourceOrDomain *> spaces *> optionalAnnotation
    <|> return ()

leadingUnderscore = do
    try(char '_' *> noneOf " \"\n[" >>= leadingUnderscore')

leadingUnderscore' c = do
    v <- many (noneOf "\n[")
    optionalAnnotation
    return $ ['_', c] ++ v

formulaValue = try(string "\"$(") *> many (noneOf ")") <* anyChar <* optionalAnnotation

quotedStringValue = do
    char '"'
    s <- manyTill (noneOf "\"") (char '"')
    r <- quotedStringValueRest
    optionalAnnotation
    return $ s ++ r

quotedStringValueRest =
        do { s <- quotedStringValue; return ('"' : s)}
    <|> return ""

data TranslatableString = TranslatableString
    {
      tString :: !String
    , tDomain :: !String
    }
        deriving (Eq, Show)

translatableStringValue = do
    char '_'
    spaces 
    s <- quotedStringValue
    st <- getState
    return $ TranslatableString s (pDomain st)

literalCodeValue = do
    char '<'
    char '<'
    c <- manyTill anyChar (try (string ">>"))
    optionalAnnotation
    return c
    

unquotedStringValue = do
    v <- many  (noneOf ", \n[\376")
    r <- unquotedStringValueRest
    optionalAnnotation
    return $ v ++ r

unquotedStringValueRest =
        char ' ' *> do {r <- many  (noneOf "\n[\376"); return (' ': r) }
    <|> char '[' *> do {r <- many  (noneOf " \n\376"); return ('[': r) }
    <|> return ""

lb = char '['
rb = char ']'

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','[',']','$']
namechars = oneOf namechars' 

marker = char '\376'
hash = char '#'

comma = try(option "" (many (char ' ')) *> char ',') <* spaces
plus = try(option "" (many (char ' ')) *> char '+') <* spaces

-----------------------------

t1 = "[tag]"
t2 = "key"
t4 = "\"value\"\n"
t5 = "\"va\"\"lue\"\n"
t6 = "\"va\"\"lue\" + \" of god\"\n"
t7 = " + \" of god\"\n "
t3 = "key=\"value\"\n[/tag] "

t10 = "[tag]\n     key=value\n[/tag]"
t11 = "[tag]\n     key=value\n[/ta]"
t12 = "[tag]\n     [tag1]\n     key1=value1\n[/tag1] \n [/tag]"
t13 = "[tag]\n     key=value\n   [xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t14= "[tag]\n     key=value\n   [tag1]\n     key1=value1\n[/tag] \n [/tag]"
t15= "[+xxx]\n     key1=value1\n[/xxx]\n"
t16= "[tag]\n     key=value\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t17= "[tag]\n     key=\"value\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t18= "[tag]\n     key=\"va\"\"lue\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t19= "[tag]\n     key=\"va\" + \"lue\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t20= "[tag]\n     key=_\"value\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t21= "[tag]\n     key=$value\n   [+xxx]\n     key1=_\"value1\"\n[/xxx] \n [/tag]"
t22= "[tag]\n     key=\"$(value)\"\n   [+xxx]\n     key1=_\"value1\"\n[/xxx] \n [/tag]"


p = parseWml
