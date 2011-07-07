-- file: preprocessWml.hs


module PreprocessWml
(
  preprocessWml
, initState
) 
    where

import Data.List
import Data.Map as M

import Text.Parsec.Prim (unexpected)

import Text.ParserCombinators.Parsec.Prim (getState, setState)
import ApplicativeParsec

----------------------------------------------------------------------------------
-- PreprocessState
-- Holds the state of the preprocessor
-- state :       
--   describes what the preprocess is doing e.g. processing #if or #define.See PState.
--   It is a stack, implemented as list, that reflects conditional nesting.
--   Many of the parsing functions match against the top of the stack
--   NB it is never empty (see initState) so 'head' and 'tail' are always safe.
-- defines:
--   a map from name to a Define structure that describes a textual substitution
-- pendingDefine:
--   Only valid in Defining State. Contains the 'signature' of the #define being
--   processed.
-- pendingBody: 
--   Only valid in Defining State. Accumulates the body of the #define being
--   processed.

data PreprocessState = PreprocessState
    {
      state :: [PState]
    , defines :: DefMap
    , pendingDefine :: DefSig
    , pendingBody :: String
    }
        deriving (Eq, Show)

----------------------------------------------------------------------------------
-- PState
-- An enumeration of the possible states of the preprocessor.

data PState = Top
           | ProcessingIf
           | SkippingIf
           | ProcessingElse
           | SkippingElse
           | Defining
        deriving (Eq, Show)


type DefMap = M.Map String Define

data Define = Define
    {
      sig :: DefSig
    , body :: String
    }
        deriving (Eq, Show)

data DefSig = DefSig
    {
      defName :: String
    , defArgs :: [String]
    }
        deriving (Eq, Show)

initState = PreprocessState [Top] M.empty (DefSig "" []) []

-----------------------------------------------------------------------------------
-- preprocess consumes one of more source chars from the input and returns a string
-- The string can contain
-- 1. The character consumed
-- 2. A new line i.e. when skipping a failed If branch
-- 3. The result of a substition.
-- It is assumed preprocess will called repeatedly on given input and the results
-- concat'ed to form the result of the preprocess all the input.

preprocessWml :: CharParser PreprocessState String
preprocessWml = 
    do s <- preprocess
       preprocessWml' s

preprocessWml' :: String -> CharParser PreprocessState String
preprocessWml' "" = return ""
preprocessWml' c =
    do cs <- preprocessWml
       return $ c ++ cs
       
preprocess :: CharParser PreprocessState String
preprocess =
        eof *> return ""
    <|> do st <- getState
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

check :: CharParser PreprocessState String
check =
    do s <- getState
       unexpected $ "current state " ++ (show $ head $ state s)
       return ""
----------------------------------------------------------------------
-- Substitution - this can be macro expansion or file inclusion
-- the text '{xxxx}' gets substituted.
-- NB. We have already consumed the leading '{'
-- We don't deal with nested substitions ....
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
       (char '#' *> directive s) <|> (restOfLine *> retnl)
processChar _ = anyChar

directive s =
        comment
    <|> define s
    <|> ifDirective s
    <|> elseEnd s
    <|> undef
    <?> "preprocessor directive"

comment =
    do char ' '
       restOfLine
       retnl

lineEnd = eol <|> (eof *> return "\n")

restOfLine = manyTill (noneOf "\n\r") lineEnd

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

updateDefines =
    do st <- getState
       s <- return $ pendingDefine st
       nm <- return $ M.insert (defName s) (Define s (pendingBody st)) (defines st)
       ns <- return $ PreprocessState (state st) nm (pendingDefine st) (pendingBody st)
       setState ns

ifDirective s = 
    do char 'i'
       char 'f'
       ifDirective' s

ifDirective' s@(SkippingIf : _) =  pushPState SkippingIf  s *> skip s
ifDirective' s@(SkippingElse  :_) =  pushPState SkippingIf s *> skip s
ifDirective' s@(Defining  :_) =  unexpected ": #if not supported inside #define"
ifDirective' s =
        ifdef s
    <|> ifhave s
    <|> ifver s
    <|> char 'n' *> ifn s

ifdef s = if' "def" defCondition evalDefCondition s
ifndef s = ifn' "def" defCondition evalDefCondition s

defCondition = symbol <* restOfLine

symbol = spaces *> many (noneOf " \n\r\t")

evalDefCondition s = 
    do st <-getState
       case M.member s (defines st) of
           True -> return True
           _ -> unexpected "symbol not found"

ifhave s = if' "def" haveCondition evalHaveCondition s
ifnhave s = ifn' "def" haveCondition evalHaveCondition s

haveCondition = undefined

evalHaveCondition = undefined

ifver s = if' "ver" verCondition evalVerCondition s
ifnver s = ifn' "ver" verCondition evalVerCondition s

verCondition = undefined

evalVerCondition = undefined

ifn s =
        ifndef s
    <|> ifnhave s
    <|> ifnver s

if' r c e s =
    do string r
       pred <- c
       b <- e pred
       pushIfState b s
       retnl

ifn' r c e s =
    do string r
       pred <- c
       b <- e pred
       pushIfState (not b) s
       retnl

elseEnd s = char 'e' *> elseEnd' s
elseEnd' s =
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

pushIfState True s = pushPState ProcessingIf s
pushIfState False s = pushPState SkippingIf s

pushPState n s =
    do st <- getState
       setPState (n : s) st

popState = 
    do st <- getState
       setPState (tail $ state st) st

setPState s st = setState $ PreprocessState s (defines st) (pendingDefine st) (pendingBody st)

end s =
        endif s
    <|> enddef s

endif s =
    do string "if"
       endif' s
       restOfLine
       retnl

endif' (ProcessingIf : _) = popState
endif' (SkippingIf : _) = popState
endif' (ProcessingElse : _) = popState
endif' (SkippingElse : _) = popState
endif' _ = unexpected ": #endif nested incorrectly"

enddef s = 
    do string "def"
       st <- getState
       enddef' s
       restOfLine
       retnl

enddef' (SkippingIf : _) = popState
enddef' (SkippingElse : _) = popState
enddef' (Defining : _) = 
    do updateDefines
       popState

enddef' _ = unexpected ": #enddef"

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

tn = 
   [
     "xxx" -- pass
   , "#" -- fail
   , "# gsgsg"
   , "zzz# sdsd"
   , "#define foo x y # sdsd\nv1={x}\nv2={y}\n#enddef\n{foo 1 2}" -- pass
   ]

main = runParser preprocessWml initState "" "#define foo \n#enddef\n#ifndef foo\nbar\n#endif