-- file: preprocessWml.hs

module PreProcessWml
(
  preProcessWmlFile
, preProcessWml
, preProcess
, check
, pp
, initState
) 
    where

import Control.Monad

import Data.List as L
import Data.Map as M

import System.IO 
import System.Directory
import System.FilePath

import Text.Parsec.Prim (unexpected)
import Text.ParserCombinators.Parsec.Prim (getState, setState)

import ApplicativeParsec
import GameConfig
import Utf8ReadWriteFile


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

data WorkItem =
     File !FilePath
   | Cont !Continuation !FilePath

data PreProcessorState = PreProcessorState
    {
      path :: Maybe FilePath
    , work :: ![WorkItem]
    , defines :: !DefMap
    , state :: ![LPPState]
    , pendingDefine :: !DefSig
    , pendingBody :: !String
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
           | TextDomain
        deriving (Eq, Show)

----------------------------------------------------------------------------------
-- constructor for PreProcessorState
mkPPState p w ds s pd pb =
    PreProcessorState {
        path = p,
        work = w,
        defines = ds,
        state = s,
        pendingDefine = pd,
        pendingBody = pb
    }

-- Initial Preprocessor State
initState path = mkPPState (Just path) [] M.empty [Top] (DefSig "" []) []

-- utility functions for manipulation PState

setLPPState s st = setState $ mkPPState (path st) (work st) (defines st) 
                                        s (pendingDefine st) (pendingBody st)

popLPPState = do
    st <- getState
    setLPPState (tail $ state st) st

pushLPPState n s = do
    st <- getState
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
-----------------------------------------------------------------------------------
-- expand a path according to the following rules from
-- http://wiki.wesnoth.org/PreprocessorRef
-- 1. pathname - a path under the wesnoth data/ subdirectory
-- 2. ~pathname - a path relative to the data/ subdirectory of the user's data directory
-- 3. ./pathname - a path relative to the directory containing the file in which the pattern appears.

expandPath _ ('~':path) = expandPath' $ userDataPath </> path
expandPath rel ('.':('/':path)) = expandRelativePath rel path
expandPath _ path = expandPath' $ systemDataPath </> path

expandRelativePath "" path = error $ "Invalid relative path name ./" ++ path
expandRelativePath f path = expandPath' $ takeDirectory f </> path

expandPath' filePath = do
    dir <- doesDirectoryExist filePath
    if dir
    then expandDirectory filePath
    else return $ checkFile filePath (takeExtension filePath)

checkFile f ".cfg" = [f]
checkFile _ _ = []

expandPath'' _ ".." = return []
expandPath'' _ "." = return []
expandPath'' p f = expandPath' $ normalise $ p </> f

expandDirectory filePath = do
    let _main = normalise $ filePath </> "_main.cfg"
    exist <- doesFileExist $ _main
    if exist
    then return $ [_main]
    else expandDirectory' filePath

expandDirectory' filePath = do
    c <- getDirectoryContents filePath
    let (i,c') = L.partition ((==) "_initial.cfg") c
    let (f,c'') = L.partition ((==) "_final.cfg") c'
    i' <- mapM (expandPath'' filePath) $ i 
    f' <- mapM (expandPath'' filePath) $ f 
    c''' <- mapM (expandPath'' filePath) $ L.sort c'' 
    return $ concat i' ++ concat c''' ++ concat f'

preProcessWmlFile f = driver $ initState f

driver :: PreProcessorState -> IO (String)
driver (PreProcessorState (Just path) [] defines lpps pd pb) = do
    files <- expandPath "" path
    driver $ mkPPState Nothing (L.map File files) defines lpps pd pb
driver (PreProcessorState (Just path) work@((Cont _ file): _) defines lpps pd pb) = do
    files <- expandPath file path
    driver $ mkPPState Nothing (L.map File files ++ work) defines lpps pd pb
driver (PreProcessorState Nothing [] _ _ _ _) = return ""
driver (PreProcessorState Nothing ((Cont cont _): _) defines _ _ _) = do
    let (pps', result) = cont defines 
    result' <- driver pps'
    return $ result ++ result'

driver (PreProcessorState Nothing ((File file): work) defines lpps pd pb) = do
    s <- readFileUtf8 file
    let pps = mkPPState Nothing work defines lpps pd pb
    let (pps', result) = preProcessWmlFile' pps file s
    result' <- driver pps'
    return $ result ++ result'

preProcessWmlFile' st n s = do
    case runParser preProcessWmlFile'' st n s of
        Right r -> r
        Left e -> error $ show e

preProcessWmlFile'' = do
    l <- lineInfo
    r <- preProcessWml
    st <- getState
    return (st, l ++ r)

lineInfo = do
    pos <- getPosition
    return $ "\n#line \"" ++ (sourceName pos) ++ "\" " 
                          ++ (show $ sourceLine pos) ++ " " 
                          ++ (show $ sourceColumn pos) ++ "\n"


-- preProcess consumes one of more source chars from the input and returns a string
-- The string can contain
-- 1. The character consumed
-- 2. A new line i.e. when skipping a failed If branch
-- 3. The result of a substition.
-- It is assumed preProcess will called repeatedly on given input and the results
-- concat'ed to form the result of the preProcess all the input.

preProcessWml = do
    s <- preProcess
    preProcessWml' s

preProcessWml' "" = return ""
preProcessWml' c = do
    cs <- preProcessWml
    return $ c ++ cs
       
preProcess =
        eof *> return ""
    <|> do st <- getState
           preProcess' $ state st

preProcess' s@(SkippingIf : _ ) = do
    c <- process s
    return [c]
preProcess' s@(SkippingElse :_ ) = do
    c <- process s
    return [c]
preProcess' s@(Defining :_ ) = do
    c <- process s
    return [c]
preProcess' s@(TextDomain :_ ) = do
    popLPPState
    c <- process s
    return $ "textdomain" ++ [c]
preProcess' s =
        char '{' *> substitute '}'
    <|> do c <- process s
           return $ [c]

check :: CharParser PreProcessorState String
check = do
    s <- getState
    unexpected $ "current state " ++ (show $ state s)
    return ""
----------------------------------------------------------------------
-- Substitution - this can be macro expansion or file inclusion
-- the text '{xxxx}' gets substituted.
-- NB. We have already consumed the leading '{'
-- We don't deal with nested substitions ....
----------------------------------------------------------------------

substitute c = do
    (h : t) <- spaces *> manyTill substituteItem (char c)
    st <- getState
    d <- return $ M.lookup h (defines st)
    n <- substitute' d h t
    return $ case runParser preProcessWml st "" n of
                 Right s -> s
                 Left e -> fail $ show e

substituteItem =
      char '(' *> bracketItem <* spaces
    <|> char '{' *> substitute '}' <* spaces
    <|> many (noneOf " )}\n\r\t") <* spaces

bracketItem = do
    l <- substitute ')'
    return $ "(" ++ l ++ ")" 

type Continuation = DefMap -> (PreProcessorState, String)

substitute' Nothing pat _ = do
    st <- getState
    pos <- getPosition
    let file= sourceName pos
    i <- getInput
    let preProcessWmlFile = do
            setPosition pos
            setInput i
            preProcessWmlFile''
    let cont d = do 
            let pps = mkPPState (path st) (work st) d
                                (state st) (pendingDefine st) (pendingBody st)
            case runParser preProcessWmlFile pps "" "" of
                Right r -> r
                Left e -> error $ show e
    let pps = mkPPState (Just pat) ((Cont cont file) : (work st)) (defines st) [Top] (DefSig "" []) []
    setState pps
    return ""
substitute' (Just d) pat args = return $ substituteArgs  (defArgs (sig d)) args (body d)

substituteArgs [] _ b = b
substituteArgs (x:xs) [] b = substituteArgs xs [] (replace x [] b)
substituteArgs (x:xs) (y:ys) b = substituteArgs xs ys (replace x y b)

replace _ _ [] = []
replace old new xs@(y:ys) =
    case L.stripPrefix old xs of
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
    -- <|> eol *> retnl
    <|> eof *> retnl
    <|> anyChar *> skip s
    <?> "fell off end"

processChar s@(SkippingIf : _) =  skip s
processChar s@(SkippingElse : _) = skip s
processChar s
    | Defining `elem` s = do
        b <- many (noneOf "\n\r#")
        pendBody b
        (char '#' *> directive s) <|> (restOfLine *> retnl)
    | otherwise = anyChar

directive s =
        char ' ' *> comment
    <|> try (string "textdomain") *> textDomain s
    <|> try (char 'd' *> define s)
    <|> try (char 'i' *> ifDirective s)
    <|> try (char 'e' *> elseEnd s)
    <|> try (char 'u' *> undef s)
    <|> comment -- hmm seems like there isn't always a space first thing
    <?> "preprocessor directive"

comment = do
    restOfLine
    retnl

lineEnd = eol <|> (eof *> return '\n')

restOfLine = manyTill (noneOf "\n\r") lineEnd

textDomain s = pushLPPState TextDomain s *> return '#'

define s@(SkippingIf : _) =  skip s
define s@(SkippingElse  :_) =  skip s
define s  = do
    string "efine"
    s <- defineSig
    pendDefine s
    retnl

defineSig = do
    l <- restOfLine
    (name, args) <- pdefsig l
    return (DefSig name args)

pdefsig l = pdefsig' $ words l

pdefsig' [] = unexpected "#define must define a name"
pdefsig' (('#':_): _) = unexpected "#define must define a name"
pdefsig' (name:args) = return (name, pdefargs args)

pdefargs [] = []
pdefargs (('#':_): _) = []
pdefargs (x:xs) = (("{" ++ x ++ "}"): pdefargs xs)

pendDefine s = do
    st <- getState
    setState $ mkPPState (path st) (work st) (defines st)
                         (Defining : (state st)) s []

pendBody b = do
    st <- getState
    setState $ mkPPState (path st) (work st) (defines st)
                         (state st) (pendingDefine st) (pendingBody st ++ b ++ "\n")

updateDefines = do
    st <- getState
    s <- return $ pendingDefine st
    nm <- return $ M.insert (defName s) (Define s (pendingBody st)) (defines st)
    setState $ mkPPState (path st) (work st) nm
                         (state st) (pendingDefine st) (pendingBody st)

ifDirective s = do
    char 'f'
    ifDirective' s

ifDirective' s@(SkippingIf : _) =  pushLPPState SkippingIf  s *> skip s
ifDirective' s@(SkippingElse  :_) =  pushLPPState SkippingIf s *> skip s
-- ifDirective' s@(Defining  :_) =  unexpected ": #if not supported inside #define"
ifDirective' s =
        char 'd' *> ifdef s
    <|> char 'h' *> ifhave s
    <|> char 'v' *> ifver s
    <|> char 'n' *> ifn s

ifdef s = if' "ef" defCondition evalDefCondition s
ifndef s = ifn' "ef" defCondition evalDefCondition s

defCondition = symbol <* restOfLine

symbol = spaces *> many (noneOf " \n\r\t")

evalDefCondition s = do
    st <-getState
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

if' r c e s = do
    string r
    pred <- c
    b <- e pred
    pushIfState b s
    retnl

ifn' r c e s = do
    string r
    pred <- c
    b <- e pred
    pushIfState (not b) s
    retnl

elseEnd s =
        char 'l' *> elseDir s
    <|> string "nd" *> end s

elseDir s = do
    string "se"
    st <- getState
    switchIfState s st
    restOfLine
    retnl


end s =
        char 'i' *> endif s
    <|> enddef s

endif s = do
    restOfLine
    endif' s
    retnl

endif' (ProcessingIf : _) = popLPPState
endif' (SkippingIf : _) = popLPPState
endif' (ProcessingElse : _) = popLPPState
endif' (SkippingElse : _) = popLPPState
endif' _ = unexpected ": #endif nested incorrectly"

enddef s = do
    string "def"
    st <- getState
    enddef' s
    restOfLine
    retnl

enddef' (SkippingIf : _) = popLPPState
enddef' (SkippingElse : _) = popLPPState
enddef' (Defining : _) = do
    updateDefines
    popLPPState
enddef' _ = unexpected ": #enddef"

undef (SkippingIf : _) = restOfLine *> retnl
undef (SkippingElse : _) = restOfLine *> retnl
undef s = do
    string "ndef"
    l <- restOfLine
    undef' $ words l
    retnl

undef' [] = unexpected ": #undef missing symbol"
undef' (k: _) = do
    st <- getState
    nm <- return $ M.delete k (defines st)
    return $ mkPPState (path st) (work st) nm
                       (state st) (pendingDefine st) (pendingBody st)

retnl = return '\n'

{--
eol =   
        try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
--}
eol = char '\n'

-- A little utility
pp x y = do { p <- preProcessWmlFile x; writeFileUtf8 y p;}
