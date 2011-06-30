-- file: parseWml.hs

module ParseWml
(
) 
    where

import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec.Error
import Text.Parsec.Prim (unexpected)

import ApplicativeParsec

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


data Node = Node
    {
      nName :: String
    , nBody :: NodeBody
    }
        deriving (Eq, Show)

data MergeNode = MergeNode
    {
      mName :: String
    , mBody :: NodeBody
    }
        deriving (Eq, Show)

data NodeItem = NAtt Attribute
              | NMatt Mattribute 
              | NNode Node
              | NMNode MergeNode
        deriving (Eq, Show)

type NodeBody = [NodeItem]

--node :: CharParser () (Either [Char] Node)
node = parseNode NNode Node startTag

node' = parseNode NNode Node startTag'

mergeNode = parseNode NMNode MergeNode startTag'

-- need type decl to get it to type check
parseNode :: (a -> NodeItem) -> (String -> NodeBody -> a) -> CharParser () String  -> CharParser () NodeItem
parseNode c1 c2 tp =
    do start <- tp
       body <- nodeBody
       end <- endTag
       mkNode c1 c2 start body end

mkNode c1 c2 s b e
    | s == e = return $ c1 $ c2 s b
    | otherwise 
    = unexpected (": end tag '" ++ e ++ "' does not match '" ++ s ++ "'")


startTag = try (lbracket *> startTag')

startTag' = tagName <* rbracket

endTag = try(lbracket *> fslash *> tagName <* rbracket)

nodeBody = 
        emptyBody
    <|> nodeBody'

nodeBody' = 
    do i <- nodeItem
       r <- nodeBody
       return $ (i : r)

emptyBody = try(spaces *> lookAhead endTag) *> (return $ [])

nodeItem = 
        try( spaces *> (NAtt <$> singleAttribute))
    <|> lbracket *> internalNode

internalNode =
        char '+' *> mergeNode
    <|> node'

singleAttribute = lookAhead isSingleAttribute *> singleAttribute' 

isSingleAttribute = attName *> equals

singleAttribute' = 
    do name <- attName
       equals
       value <- singleAttValue
       return $ Attribute name value

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

defaultAttValue = do anyChar `manyTill` eol

tagName = many namechars
attName = many namechars
wmlVarName = many namechars

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

lbracket = spaces *> char '['
rbracket = char ']'
fslash = char '/'

equals = try(spaces *> char '=' <* spaces)
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

