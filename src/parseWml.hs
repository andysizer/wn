-- file: parseWml.hs

module ParseWml
(
  parseWml
) 
    where

import Data.List
import Text.Parsec.Prim (unexpected)

import ApplicativeParsec

-- default node type including top level
data Node = Node
    {
      nName :: String
    , nBody :: NodeBody
    }
        deriving (Eq, Show)

emptyNode = Node "" []

-- merge nodes 
data MergeNode = MergeNode
    {
      mName :: String
    , mBody :: NodeBody
    }
        deriving (Eq, Show)

type NodeBody = [NodeItem]

data NodeItem = NAtt Attribute
              | NMatt Mattribute 
              | NNode Node
              | NMNode MergeNode
        deriving (Eq, Show)

parseWml s = parse topLevel "" s

-- topLevel = 
topLevel = manyTill (spaces *> topLevelItem) eof

topLevelItem =
        marker *> sourceOrDomain *> spaces *> return emptyNode  -- deal with \376
    <|> hash *> domain *> spaces *> return emptyNode            -- deal with #
    <|> topLevelNode

sourceOrDomain =
        char 'l' *> sourceInfo
    <|> domain

sourceInfo = do
    many (noneOf "\n")
    anyChar

domain = do
    many (noneOf "\n")
    anyChar

-- Main node parser
parseNode :: (a -> NodeItem) -> (String -> NodeBody -> a) -> CharParser () NodeItem
parseNode c1 c2 = do
    start <- tagName
    body <- nodeBody
    end <- tagName
    mkNode c1 c2 start body end

tagName = manyTill namechars rb <* spaces

mkNode c1 c2 s b e
    | s == e = return $ c1 $ c2 s b
    | otherwise = unexpected (": end tag '" ++ e ++ "' does not match '" ++ s ++ "'")

-- 'constructors' for different node types
node = parseNode NNode Node

mergeNode = parseNode NMNode MergeNode

-- Top Level Node parser 
topLevelNode = do
    spaces
    lb
    (NNode n) <- node
    return n

-- parsing node bodies. NB no 'try's.
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
    return (a : b)

internalNode =
        char '+' *> mergeNode
    <|> node

-- Attributes
data Attribute = Attribute
    {
      aKey  :: String
    , aValue :: AttributeValue
    }
        deriving (Eq, Show)

data Mattribute = Mattribute
    {
      mKeys  :: [String]
    , mValues :: [AttributeValue]
    }
        deriving (Eq, Show)

attribute = do
    spaces
    key <- attName
    keys <- maybeKeys
    finishAtrribute key keys

attName = many namechars <* spaces

maybeKeys = do
        char ','
        spaces
        key <- attName
        keys <- maybeKeys
        return (key : keys)
    <|> return [] 

finishAtrribute key [] = do
    char '='
    spaces
    value <- attributeValue
    return $ NAtt $ Attribute key value
finishAtrribute key keys = do
    char '='
    spaces
    value <- attributeValue
    values <- maybeValues
    return $ NMatt $ Mattribute (key:keys) (value:values)

maybeValues = do 
        char ','
        spaces
        value <- attributeValue
        values <- maybeValues
        return (value : values)
    <|> return [] 

data AttributeValue = Variable String
                    | Formula String
                    | String [StringValue]
        deriving (Eq, Show)

attributeValue =
        marker *> sourceOrDomain *> spaces *> attributeValue
    <|> Variable <$> wmlVariableValue
    <|> Formula <$> formulaValue
    <|> String <$> stringValue
    <|> String <$> defaultLineValue

wmlVariableValue = char '$' *> many namechars <* spaces

formulaValue = try(string "\"$(") *> many (noneOf ")") <* anyChar

data StringValue = Translatable String
                 | QuotedString String
        deriving (Eq, Show)
 
stringValue = do 
    s <- quotedStringValue
    r <- stringValueRest
    return (s:r)

stringValueRest = 
        try(plus) *>
        do
        { s <- quotedStringValue;
          r <- stringValueRest;
          return (s:r)
        }
    <|> return []

quotedStringValue = 
        translatableString
    <|> quotedString

translatableString = do
    char '_' 
    spaces 
    s <- simpleQuotedString
    return $ Translatable s

quotedString = do
    s <- simpleQuotedString
    return $ QuotedString s

simpleQuotedString = do
    char '"'
    s <- manyTill (noneOf "\"") (char '"')
    r <- maybeQuotedString
    return $ s ++ r

maybeQuotedString =
        do { s <- simpleQuotedString; return ('"' : s)}
    <|> return ""

defaultLineValue = do
    v <- anyChar `manyTill`  (char '\n')
    return $ [QuotedString v]

lb = char '['
rb = char ']'

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

marker = char '\376'
hash = char '#'

plus = spaces *> char '+' <* spaces

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
