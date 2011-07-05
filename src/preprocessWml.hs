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

data DState = Top
           | ProcessingIf
           | SkippingIf
           | ProcessingElse
           | SkippingElse
           | Defining
        deriving (Eq, Show)

data PreprocessState = PreprocessState
    {
      state :: [DState]
    , defines :: DefMap
    , pendingDefine :: DefSig
    , pendingBody :: String
    }
        deriving (Eq, Show)

initState = PreprocessState [Top] M.empty (DefSig "" []) []

preprocess:: CharParser PreprocessState String
preprocess =
    do st <- getState
       preprocess' $ state st

preprocess' s@(SkippingIf : _ ) =
    do c <- process s
       return [c]
preprocess' s@(SkippingElse :_ ) =
    do c <- process s
       return [c]
preprocess' s =
        char '{' *> substitute
    <|> do c <- process s
           return $ [c]

----------------------------------------------------------------------
-- Substitution - this can be macro expansion or file inclusion
-- the text '{xxxx}' gets substituted.
-- NB. We have already consumed the leading '{'
----------------------------------------------------------------------
substitute =
    do pat <- manyTill (noneOf "}") (char '}')
       st <- getState
       (s: args) <- return $ words pat
       d <- return $ M.lookup s (defines st)
       substitute' d pat args

substitute' Nothing pat args = undefined
substitute' (Just d) pat args =
    do args <- getArgs $ unwords args
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
substituteArgs d a = r
    where r = substituteArgs' (defArgs (sig d)) a (body d)

substituteArgs' [] _ b = b
substituteArgs' (x:xs) [] b = substituteArgs' xs [] (replace x [] b)
substituteArgs' (x:xs) (y:ys) b = substituteArgs' xs ys (replace x y b)

replace _ _ [] = []
replace old new xs@(y:ys) =
    case stripPrefix old xs of
         Nothing -> y : replace old new ys
         Just ys' -> new ++ replace old new ys'

-----------------------------------------------
-- non-substitution code 
----------------------------------------------

process s =
        char '#' *> directive s
    <|> processChar s

skip s = many (noneOf "#\n\r") *> process s

processChar s@(SkippingIf : _) =  skip s
processChar s@(SkippingElse : _) = skip s
processChar s@(Defining : _) =
    do b <- many (noneOf "\n\r#")
       pendBody b
       char '#' *> directive s <|> retnl
processChar _ = anyChar

directive s =
        comment
    <|> define s
    <|> ifDirective s
    <|> elseEnd s
    <|> undef
    <?> "unexpected preprocessor directive"

comment =
    do char ' '
       restOfLine
       retnl

restOfLine = many (noneOf "\n\r") <* eol

define s@(SkippingIf : _) =  skip s
define s@(SkippingElse  :_) =  skip s
define s  = 
    do char 'd'
       string "efine"
       s <- defineSig
       pendDefine s
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
pdefargs (x:xs) = (("{" ++ x ++ "}"): pdefargs xs)

pendDefine s =
    do st <- getState
       setState $ PreprocessState (Defining : (state st)) (defines st) s []

pendBody b =
    do st <- getState
       setState $ PreprocessState (state st) (defines st) (pendingDefine st) (pendingBody st ++ b ++ "\n")

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
       ns <- return $ PreprocessState (state st) nm (pendingDefine st) (pendingBody st)
       setState ns

ifDirective s = 
    do char 'i'
       char 'f'
       ifDirective' s

ifDirective' s@(SkippingIf : _) =  pushDState SkippingIf *> skip s
ifDirective' s@(SkippingElse  :_) =  pushDState SkippingIf *> skip s
ifDirective' s@(Defining  :_) =  unexpected ": #if not supported inside #define"
ifDirective' s =
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
       setState $ PreprocessState (s : state st) (defines st) (pendingDefine st) (pendingBody st)

elseEnd s =
        char 'l' *> elseDir s
    <|> string "nd" *> end s

elseDir s =
    do string "se"
       st <- getState
       switchIfState s st
       restOfLine
       retnl

switchIfState (ProcessingIf : xs) st = setPState (SkippingElse : xs) st
switchIfState (SkippingIf : (SkippingElse : xs)) st = setPState (SkippingElse : (SkippingElse : xs)) st
switchIfState (SkippingIf : (SkippingIf : xs)) st = setPState (SkippingElse : (SkippingIf : xs)) st
switchIfState (SkippingIf : xs) st = setPState (ProcessingElse : xs) st
switchIfState _ _ = unexpected ": #else nested incorrectly"

setPState s st = setState $ PreprocessState s (defines st) (pendingDefine st) (pendingBody st)

end s =
        endif s
    <|> enddef s

endif s =
    do string "if"
       st <- getState
       endif' s st
       restOfLine
       retnl

endif' (ProcessingIf : _) st = popState st
endif' (SkippingIf : _) st = popState st
endif' (ProcessingElse : _) st = popState st
endif' (SkippingElse : _) st = popState st
endif' _ _ = unexpected ": #endif nested incorrectly"

popState st = return $ PreprocessState (tail $ state st) (defines st) (pendingDefine st) (pendingBody st)

enddef s = xxxxx

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

retnl = return '\n'

eol =   
        try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
