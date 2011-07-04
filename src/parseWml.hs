-- file: parseWml.hs

module ParseWml
(
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


-- Main node parser
-- need type decl to get it to type check
parseNode :: (a -> NodeItem) -> (String -> NodeBody -> a) -> CharParser () NodeItem
parseNode c1 c2 =
    do start <- tagName
       body <- nodeBody
       end <- tagName
       mkNode c1 c2 start body end

mkNode c1 c2 s b e
    | s == e = return $ c1 $ c2 s b
    | otherwise 
    = unexpected (": end tag '" ++ e ++ "' does not match '" ++ s ++ "'")

-- 'constructors' for different node types
node = parseNode NNode Node

mergeNode = parseNode NMNode MergeNode

-- Top Level Node parser 
topLevelNode = 
    do spaces
       lb
       (NNode n) <- node
       return n

-- parsing node bodies. NB no 'try's.
nodeBody = 
        lb *> maybeEndBody
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
attributeThenNodeBody =
    do a <- attribute
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

data AttributeValue = Translatable String
                    | Substitution String
                    | Formula String
                    | String String
        deriving (Eq, Show)

attribute = 
    do spaces
       key <- attName
       keys <- maybeKeys
       finishAtrribute key keys

maybeKeys =
        do char ','
           spaces
           key <- attName
           keys <- maybeKeys
           return (key : keys)
    <|> return [] 

finishAtrribute key [] = 
   do char '='
      spaces
      value <- attValue
      return $ NAtt $ Attribute key value
finishAtrribute key keys = 
   do char '='
      spaces
      value <- attValue
      values <- maybeValues
      return $ NMatt $ Mattribute (key:keys) (value:values)

maybeValues = 
        do char ','
           spaces
           value <- attValue
           values <- maybeValues
           return (value : values)
    <|> return [] 

attValue = 
        char '_' *> translatableAttValue
    <|> char '$' *> substitionAttValue
    <|> char '"' *> quotedAttValue
    <|> defaultAttValue
    <?> "Invalid attribute value"

translatableAttValue = char '"' *> (Translatable <$> pString)

substitionAttValue = Substitution <$> wmlVarName

quotedAttValue = 
        char '$' *> (Formula <$> formulaAttValue) <* (spaces *> char '"')
    <|> String <$> quotedAttValue'

quotedAttValue' = 
    do s <- pString
       spaces
       r <- pConcatString
       return $ s ++ r

pString = 
    do s <- anyChar `manyTill` (char '"')
       r <- pString'
       return $ s ++ r

pString' = 
        char '"' *> ((:) <$> (return '"') <*> pString)
    <|> return []

pConcatString = 
        plus *> spaces *> char '"' *> quotedAttValue'
    <|> return []
            

formulaAttValue = between (char '(') (char ')') (many (noneOf ")"))

-- TODO should we 'trim' this. What about the others?
defaultAttValue = String <$> anyChar `manyTill` eol

tagName = manyTill namechars rb <* spaces
attName = many namechars <* spaces
wmlVarName = many namechars <* spaces

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

lb = char '['
rb = char ']'

plus = char '+'

eol =   
        try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

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
