-- file: preprocessWml.hs


module PreProcessWml
(
  preProcessWmlFile
) 
    where

import Data.List
import Data.Map as M
import Data.IORef

import Text.Parsec.Prim (unexpected)

import Text.ParserCombinators.Parsec.Prim (getState, setState)
import ApplicativeParsec


----------------------------------------------------------------------------------
-- PreProcessorState
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


data PreProcessorState = PreProcessorState
    {
      files :: [FilePath]
    , continuations :: [Continuation]
    , defines :: DefMap
    , state :: [LPPState]
    , pendingDefine :: DefSig
    , pendingBody :: String
    }
        -- deriving (Eq, Show)

----------------------------------------------------------------------------------
-- PState
-- An enumeration of the possible states of the preprocessor processing a single file

data LPPState = Top
           | ProcessingIf
           | SkippingIf
           | ProcessingElse
           | SkippingElse
           | Defining
        deriving (Eq, Show)

----------------------------------------------------------------------------------
-- utility functions for manipulation PState

setLPPState s st = setState $ mkPPState (files st) (continuations st) (defines st) s (pendingDefine st) (pendingBody st)

popLPPState = 
    do st <- getState
       setLPPState (tail $ state st) st

pushLPPState n s =
    do st <- getState
       setLPPState (n : s) st

-- next two used by #if directives

pushIfState True s = pushLPPState ProcessingIf s
pushIfState False s = pushLPPState SkippingIf s

switchIfState (ProcessingIf : xs) st = setLPPState (SkippingElse : xs) st
switchIfState (SkippingIf : (SkippingElse : xs)) st = setLPPState (SkippingElse : (SkippingElse : xs)) st
switchIfState (SkippingIf : (SkippingIf : xs)) st = setLPPState (SkippingElse : (SkippingIf : xs)) st
switchIfState (SkippingIf : xs) st = setLPPState (ProcessingElse : xs) st
switchIfState _ _ = unexpected ": #else nested incorrectly"

----------------------------------------------------------------------------------
-- Types for dealing with #define
----------------------------------------------------------------------------------
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

-----------------------------------------------------------
-- Initial Preprocessor State
initState f = mkPPState [f] [] M.empty [Top] (DefSig "" []) []

mkPPState fs cs ds s pd pb =
    PreProcessorState {
        files = fs,
        continuations = cs,
        defines = ds,
        state = s,
        pendingDefine = pd,
        pendingBody = pb
    }
-----------------------------------------------------------------------------------
preProcessWmlFile f = result
    where pps = initState f
          result = driver pps

preProcessWmlFile' st n s = do
    case runParser preprocessWmlFile'' st n s of
        Right r -> r
        Left e -> error $ show e

preprocessWmlFile'' = 
    do r <- preprocessWml
       st <- getState
       return (st, r)

driver :: PreProcessorState -> IO (String)
driver (PreProcessorState [] [] _ _ _ _) = return ""
driver (PreProcessorState [] (cont:conts) defines _ _ _) =
    do let (pps', result) = cont defines
       result' <- driver pps'
       return $ result ++ result'
driver (PreProcessorState (file: files) conts defines lpps pd pb) =
    do s <-readFile file
       let pps = mkPPState files conts defines lpps pd pb
       let (pps', result) = preProcessWmlFile' pps file s
       result' <- driver pps'
       return $ result ++ result'

-- preprocess consumes one of more source chars from the input and returns a string
-- The string can contain
-- 1. The character consumed
-- 2. A new line i.e. when skipping a failed If branch
-- 3. The result of a substition.
-- It is assumed preprocess will called repeatedly on given input and the results
-- concat'ed to form the result of the preprocess all the input.

--preprocessWml :: CharParser PreprocessState String
preprocessWml = 
    do s <- preprocess
       preprocessWml' s

--preprocessWml' :: String -> CharParser PreprocessState String
preprocessWml' "" = return ""
preprocessWml' c =
    do cs <- preprocessWml
       return $ c ++ cs
       
--preprocess :: CharParser PreprocessState String
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

check :: CharParser PreProcessorState String
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
substitute :: CharParser PreProcessorState String
substitute =
    do pat <- manyTill (noneOf "}") (char '}')
       st <- getState
       (s: args) <- return $ words pat
       d <- return $ M.lookup s (defines st)
       n <- substitute' d pat args
       return $ case runParser preprocessWml st "" n of
                    Right s -> s
                    Left e -> fail $ show e

type Continuation = DefMap -> (PreProcessorState, String)
substitute' Nothing pat _ =
    do let pat' = expandPat pat
       st <- getState
       pos <- getPosition
       i <- getInput
       let preprocessWmlFile''' pos i =
               do setPosition pos
                  setInput i
                  preprocessWmlFile''
       let cont = (\d -> 
                      do let pps = mkPPState (files st) (continuations st) d
                                             (state st) (pendingDefine st) (pendingBody st)
                         
                         case runParser (preprocessWmlFile''' pos i) pps "" "" of
                                    Right r -> r
                                    Left e -> error $ show e
                         )
       let fs = pat' ++ (files st)
       let cs = cont : (continuations st)
       let pps = mkPPState fs cs (defines st) 
                           [Top] (DefSig "" []) []
       setState pps
       return ""
substitute' (Just d) pat args =
    do args <- getArgs $ unwords args
       return $ substituteArgs d args

expandPat pat = [pat]

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

skip s =
        (char '#' *> directive s)
    <|> eol *> retnl
    <|> eof *> retnl
    <|> anyChar *> skip s
    <?> "fell off end"

processChar s@(SkippingIf : _) =  skip s
processChar s@(SkippingElse : _) = skip s
processChar s@(Defining : _) =
    do b <- many (noneOf "\n\r#")
       pendBody b
       (char '#' *> directive s) <|> (restOfLine *> retnl)
processChar _ = anyChar

directive s =
        char ' ' *> comment
    <|> char 'd' *> define s
    <|> char 'i' *> ifDirective s
    <|> char 'e' *> elseEnd s
    <|> char 'u' *> undef s 
    <?> "preprocessor directive"

comment =
    do restOfLine
       retnl

lineEnd = eol <|> (eof *> return "\n")

restOfLine = manyTill (noneOf "\n\r") lineEnd

define s@(SkippingIf : _) =  skip s
define s@(SkippingElse  :_) =  skip s
define s  = 
    do string "efine"
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
       setState $ mkPPState (files st) (continuations st) (defines st)
                            (Defining : (state st)) s []

pendBody b =
    do st <- getState
       setState $ mkPPState (files st) (continuations st) (defines st)
                            (state st) (pendingDefine st) (pendingBody st ++ b ++ "\n")

updateDefines =
    do st <- getState
       s <- return $ pendingDefine st
       nm <- return $ M.insert (defName s) (Define s (pendingBody st)) (defines st)
       setState $ mkPPState (files st) (continuations st) nm
                            (state st) (pendingDefine st) (pendingBody st)

ifDirective s = 
    do char 'f'
       ifDirective' s

ifDirective' s@(SkippingIf : _) =  pushLPPState SkippingIf  s *> skip s
ifDirective' s@(SkippingElse  :_) =  pushLPPState SkippingIf s *> skip s
ifDirective' s@(Defining  :_) =  unexpected ": #if not supported inside #define"
ifDirective' s =
        char 'd' *> ifdef s
    <|> char 'h' *> ifhave s
    <|> char 'v' *> ifver s
    <|> char 'n' *> ifn s

ifdef s = if' "ef" defCondition evalDefCondition s
ifndef s = ifn' "ef" defCondition evalDefCondition s

defCondition = symbol <* restOfLine

symbol = spaces *> many (noneOf " \n\r\t")

evalDefCondition s = 
    do st <-getState
       case M.member s (defines st) of
           True -> return True
           _ -> return False

ifhave s = if' "ave" haveCondition evalHaveCondition s
ifnhave s = ifn' "ave" haveCondition evalHaveCondition s

haveCondition = undefined

evalHaveCondition = undefined

ifver s = if' "er" verCondition evalVerCondition s
ifnver s = ifn' "er" verCondition evalVerCondition s

verCondition = undefined

evalVerCondition = undefined

ifn s =
        char 'd' *> ifndef s
    <|> char 'h' *> ifnhave s
    <|> char 'v' *> ifnver s

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

elseEnd s =
        char 'l' *> elseDir s
    <|> string "nd" *> end s

elseDir s =
    do string "se"
       st <- getState
       switchIfState s st
       restOfLine
       retnl


end s =
        char 'i' *> endif s
    <|> enddef s

endif s =
    do restOfLine
       endif' s
       retnl

endif' (ProcessingIf : _) = popLPPState
endif' (SkippingIf : _) = popLPPState
endif' (ProcessingElse : _) = popLPPState
endif' (SkippingElse : _) = popLPPState
endif' _ = unexpected ": #endif nested incorrectly"

enddef s = 
    do string "def"
       st <- getState
       enddef' s
       restOfLine
       retnl

enddef' (SkippingIf : _) = popLPPState
enddef' (SkippingElse : _) = popLPPState
enddef' (Defining : _) = 
    do updateDefines
       popLPPState

enddef' _ = unexpected ": #enddef"

undef (SkippingIf : _) = restOfLine *> retnl
undef (SkippingElse : _) = restOfLine *> retnl
undef s =
    do string "ndef"
       l <- restOfLine
       undef' $ words l
       retnl

undef' [] = unexpected ": #undef missing symbol"
undef' (k: _) =
    do st <- getState
       nm <- return $ M.delete k (defines st)
       return $ mkPPState (files st) (continuations st) nm
                          (state st) (pendingDefine st) (pendingBody st)


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
   , "#define d1\n#enddef\n#define d2\n#enddef\n#ifdef d1\nxxx\n#ifdef d2\nyyy\n#else\nbbb\n#endif\n#else\naaa\n#endif\n"
   , "#define x a b c\n#ifndef d\nx1 = {a}\n#else\nx2={b}\n#endif\n#enddef\n"
   , "#define foo x y\n  bar {x}+1 {y} {x}\n#enddef\n#define bar x y z\n  x={x}\n  y={y}\n  z={z}\n#enddef\n{foo 1 3}\n"
   ]

test = runParser preprocessWml (initState "") "" 

