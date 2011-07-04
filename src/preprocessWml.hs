-- file: preprocessWml.hs


module PreProcessWml
(
) 
    where

import Data.List
import Data.Map as M
import Text.Parsec.Prim (unexpected)

import Text.ParserCombinators.Parsec.Prim (getState, setState)

import ApplicativeParsec

----------------------------------------------------------------------------------

type DefMap = M.Map String Define

data PreprocessState = PreprocessState
    {
      state :: [String]
    , defines :: DefMap
    }
        deriving (Eq, Show)


preprocess :: CharParser PreprocessState Char
preprocess =
        char '#' *> directive
    <|> passThru

passThru = anyChar

directive =
        comment
    <|> define
    <|> ifDirective
    <|> undef
    <|> file
    <?> "unexpected preprocessor directive"

comment =
    do char ' '
       restOfLine
       return '\n'

restOfLine = many (noneOf "\n\r") <* eol

define = 
    do char 'd'
       string "efine"
       define'

define' =
    do s <- defineSig
       b <- defineBody
       updateDefines s b
       return '\n'
       
defineSig = 
    do l <- restOfLine
       (name, args) <- pdefsig l
       return (DefSig name args)

data DefSig = DefSig
    {
      defName :: String
    , defArgs :: [String]
    }
        deriving (Eq, Show)

pdefsig l = pdefsig' $ words l

pdefsig' [] = unexpected "#define must define a name"
pdefsig' (('#':_): _) = unexpected "#define must define a name"
pdefsig' (name:args) = return (name, pdefargs args)

pdefargs [] = []
pdefargs (('#':_): _) = []
pdefargs (x:xs) = (x : pdefargs xs)

defineBody = many (noneOf "#") <* string "#enddef"

data Define = Define
    {
      sig :: DefSig
    , body :: String
    }
        deriving (Eq, Show)

updateDefines s b =
    do d <- return $ Define s b
       n <- return $ defName s
       st <-getState
       nm <- return $ M.insert n d (defines st)
       ns <- return $ PreprocessState (state st) nm
       setState ns

ifDirective = 
    do char 'i'
       char 'f'
       ifDirective'

ifDirective' =
        ifdef
    <|> ifhave
    <|> ifver
    <|> char 'n' *> ifn

ifdef = if' "def" defCondition evalDefCondition

defCondition = undefined

evalDefCondition = undefined

ifhave = if' "def" haveCondition evalHaveCondition

haveCondition = undefined

evalHaveCondition = undefined

ifver = if' "ver" verCondition evalVerCondition

verCondition = undefined

evalVerCondition = undefined

ifn =
        ifndef
    <|> ifnhave
    <|> ifnver

ifndef = if' "def" defCondition (not . evalDefCondition)

ifnhave = if' "def" haveCondition (not . evalHaveCondition)

ifnver = if' "ver" verCondition (not . evalVerCondition)

if' s c e =
    do string s
       c <- c
       r <- return (e c)
       ifThen r
       ifElse r

ifThen = undefined
ifElse = undefined

undef = undefined

file = undefined

eol =   
        try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
