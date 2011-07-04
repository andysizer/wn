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

data DState = ProcessingIf
           | SkippingIf
           | ProcessingElse
           | SkippingElse
        deriving (Eq, Show)

data PreprocessState = PreprocessState
    {
      state :: [DState]
    , defines :: DefMap
    }
        deriving (Eq, Show)

preprocess:: CharParser PreprocessState String
preprocess =
    do s <- skipping
       preprocess' s

skipping =
    do st <- getState
       skipping' $ state st

skipping' [] = return False
skipping' (SkippingIf:_) = return True
skipping' (SkippingElse:_) = return True
skipping' _ = return False

preprocess' True =
    do c <- process
       return [c]
preprocess' False =
        char '{' *> substitute
    <|> do c <- process
           return $ [c]

substitute =
    do pat <- manyTill anyChar (char '}')
       char '}'
       st <- getState
       (s: args) <- return $ words pat
       d <- return $ M.lookup s (defines st)
       substitute' d pat args

substitute' Nothing pat args = undefined
substitute' (Just d) pat args =
    do args <- getArgs $ concat args
       return $ substituteArgs d args

getArgs args = 
    do savedState <- getState
       (Right l) <- return $ parse parseArgs "" args
       setState savedState
       return l

parseArgs = sepBy parseArg space

parseArg = 
        spaces *> compoundArg 
    <|> spaces *> many (noneOf " \n\r\t")

compoundArg = between (char '(') (char ')') (many (noneOf ")"))

substituteArgs :: Define -> [String] -> String 
substituteArgs d a = undefined

process =
        char '#' *> directive
    <|> processChar

processChar = 
    do st <- getState
       processChar' $ state st

processChar' [] = anyChar
processChar' (SkippingIf:_) = retnl
processChar' (SkippingElse:_) = retnl
processChar' _ = anyChar

directive =
        comment
    <|> define
    <|> ifDirective
    <|> elseEnd
    <|> undef
    <|> file
    <?> "unexpected preprocessor directive"

comment =
    do char ' '
       restOfLine
       retnl

restOfLine = many (noneOf "\n\r") <* eol

define = 
    do char 'd'
       string "efine"
       define'

define' =
    do s <- defineSig
       b <- defineBody
       updateDefines s b
       retnl
       
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
       st <- getState
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
       pred <- c
       pushIfState(e pred)
       retnl

pushIfState True = pushDState ProcessingIf
pushIfState False = pushDState SkippingIf

pushDState s =
    do st <- getState
       ns <- return $ PreprocessState (s : state st) (defines st)
       setState ns

elseEnd =
        char 'l' *> elseDir
    <|> endif

elseDir =
    do string "se"
       st <- getState
       switchIfState (state st) (defines st)
       restOfLine
       retnl

switchIfState (ProcessingIf : xs) d = return $ PreprocessState (SkippingElse : xs) d
switchIfState (SkippingIf : xs) d = return $ PreprocessState (ProcessingElse : xs) d
switchIfState _ _ = unexpected ": #else nested incorrectly"

endif =
    do string "ndif"
       st <- getState
       endif' (state st) (defines st)
       restOfLine
       retnl

endif' (ProcessingIf : xs) d = return $ PreprocessState xs d
endif' (SkippingIf : xs) d = return $ PreprocessState xs d
endif' (ProcessingElse : xs) d = return $ PreprocessState xs d
endif' (SkippingElse : xs) d = return $ PreprocessState xs d
endif' _ _ = unexpected ": #endif nested incorrectly"

undef =
    do char 'u'
       string "ndef"
       l <- restOfLine
       undef' $ words l
       retnl

undef' [] = unexpected ": #undef missing symbol"
undef' (k: _) =
    do st <- getState
       nm <- return $ M.delete k (defines st)
       return $ PreprocessState (state st) nm

file = undefined

retnl = return '\n'
eol =   
        try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
