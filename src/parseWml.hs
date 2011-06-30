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

topLevelNode = 
    do spaces
       lbracket
       (NNode n) <- node
       return n

node = parseNode NNode Node

mergeNode = parseNode NMNode MergeNode

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

-- Next three control paring node bodies. NB no 'try's.
nodeBody = 
        lbracket *> maybeEndBody
    <|> attributeThenNodeBody

maybeEndBody =
        fslash *> (return [])
    <|> do n <- internalNode
           spaces
           r <- nodeBody
           return $ (n : r)

attributeThenNodeBody =
    do a <- attribute
       spaces
       b <- nodeBody
       return $ (a : b)

internalNode =
        char '+' *> mergeNode
    <|> node


data Attribute = Attribute
    {
      aKey  :: String
    , aValue :: String
    }
        deriving (Eq, Show)

data Mattribute = Mattribute
    {
      mKeys  :: [String]
    , mValues :: [String]
    }
        deriving (Eq, Show)

attribute = singleAttribute

singleAttribute = lookAhead isSingleAttribute *> singleAttribute' 

isSingleAttribute = attName *> equals

singleAttribute' = 
    do name <- attName
       equals
       value <- singleAttValue
       return $ NAtt $ Attribute name value

singleAttValue = 
        char '_' *> translatableAttValue
    <|> char '$' *> substitionAttValue
    <|> char '"' *> quotedAttValue
    <|> defaultAttValue
    <?> "Invalid attribute value"

translatableAttValue = 
    do char '_'
       v <- pString
       return $ v

substitionAttValue = wmlVarName

quotedAttValue = 
        char '$' *> formulaAttValue
    <|> quotedAttValue'

quotedAttValue' = 
    do s <- pString
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
        plus *> char '"' *> quotedAttValue'
    <|> return []
            

formulaAttValue = undefined

defaultAttValue = anyChar `manyTill` eol

tagName = manyTill namechars rbracket <* spaces
attName = many namechars <* spaces
wmlVarName = many namechars <* spaces

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

lbracket = char '['
rbracket = char ']'
fslash = char '/'

equals = char '=' <* spaces
plus = try(spaces *> char '+' <* spaces)

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

