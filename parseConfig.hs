-- file: parseWml.hs

module ParseWml
(
) 
    where

import Data.List
import Control.Monad

import ApplicativeParsec

data Attribute = Attribute
    {
      aName  :: String
    , aValue :: String
    }
        deriving (Eq, Show)

data Mattribute = Mattribute
    {
      mNames  :: [String]
    , mValues :: [String]
    }
        deriving (Eq, Show)


data Node = Node
    {
      nName :: String
    , nBody :: NodeBody
    }
        deriving (Eq, Show)

data NodeItem = NAtt Attribute
              | NMatt Mattribute 
              | NNode Node
        deriving (Eq, Show)

type NodeBody = [NodeItem]

node :: CharParser () (Either [Char] Node)
node = do spaces
          start <- startTag
          body <- nodeBody
          end <- endTag
          return $ mkNode start body end

startTag = lbracket *> tagName <* rbracket

endTag = lbracket *> fslash *> tagName <* rbracket

nodeBody = liftM many nodeItem

nodeItem = liftM NAtt <$> singleAttribute
       <|> liftM NNode <$> node

singleAttribute = spaces *> lookAhead isSingleAttribute *> singleAttribute 

isSingleAttribute = attName *> equals

singleAttribute' = do name <- attName
                      equals
                      value <- singleAttValue
                      return $ mkSingleAttribute name value

singleAttValue = char '_' *> translatableAttValue
             <|> char '$' *> substitionAttValue
             <|> char '"' *> quotedAttValue
             <|> defaultAttValue
             <?> "Invalid attribute value"

translatableAttValue = do char '_'
                          v <- pString
                          return $ v

substitionAttValue = wmlVarName

quotedAttValue = char '$' *> formulaAttValue
             <|> quotedAttValue'

quotedAttValue' = do s <- pString
                     r <- pConcatString
                     return $ s ++ r
pString = do s <- anyChar `manyTill` (char '"')
             char '"'
             r <- pString'
             return $ s ++ r

pString' = char '"' *> ((:) <$> (return '"') <*> pString)
       <|> return []

pConcatString = plus *> char '"' *> quotedAttValue'
            

formulaAttValue = undefined

defaultAttValue = do anyChar `manyTill` eol

mkNode s (Right b) e
    | s == e = Right $ Node s b
    | otherwise = Left "xx"
mkNode _ (Left e) _ = Left e

mkSingleAttribute n v = Right $ Attribute n v

tagName = many namechars
attName = many namechars
wmlVarName = many namechars

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

lbracket = char '['
rbracket = char ']'
fslash = char '/'

equals = spaces *> char '=' <* spaces
plus = spaces *> char '+' <* spaces

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

t1 = "[tag]\n     key=value\n[/tag]"
