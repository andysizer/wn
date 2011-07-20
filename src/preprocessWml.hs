-- file: preprocessWml.hs

module PreProcessWml
(
  preProcessWmlFile
, preProcessWml
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
    , skippingDepth :: !Int
    , pendingDefine :: Maybe DefSig
    , pendingBody :: ![String]
    }
        -- deriving (Eq, Show)

----------------------------------------------------------------------------------
-- constructor for PreProcessorState
mkPPState p w ds s pd pb =
    PreProcessorState {
        path = p,
        work = w,
        defines = ds,
        skippingDepth = s,
        pendingDefine = pd,
        pendingBody = pb
    }

type PreProcessorParser = GenParser Char PreProcessorState String

-- Initial Preprocessor State
initState path = mkPPState (Just path) [] M.empty 0 Nothing []

----------------------------------------------------------------------------------
-- Types for dealing with #define
----------------------------------------------------------------------------------
type DefMap = M.Map String Define

data Define = Define
    {
      sig :: !DefSig
    , body :: !String
    }
        deriving (Eq, Show)

data DefSig = DefSig
    {
      defName :: !String
    , defArgs :: ![String]
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
expandPath'' _ "images" = return []
expandPath'' _ "sounds" = return []
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
driver (PreProcessorState (Just path) [] defines sdepth pd pb) = do
    files <- expandPath "" path
    driver $ mkPPState Nothing (L.map File files) defines sdepth pd pb
driver (PreProcessorState (Just path) work@((Cont _ file): _) defines sdepth pd pb) = do
    files <- expandPath file path
    driver $ mkPPState Nothing (L.map File files ++ work) defines sdepth pd pb
driver (PreProcessorState Nothing [] _ _ _ _) = return ""
driver (PreProcessorState Nothing ((Cont cont _): _) defines _ _ _) = do
    -- error $ "After inclusion"
    let (pps', result) = cont defines 
    result' <- driver pps'
    return $ result ++ result'

driver (PreProcessorState Nothing ((File file): work) defines sdepth pd pb) = do
    s <- readFileUtf8 file
    let pps = mkPPState Nothing work defines sdepth pd pb
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

preProcessWml :: PreProcessorParser
preProcessWml = do
    st <- getState
    cs <- getInput
    preProcessWml' (skippingDepth st) cs

preProcessWml' :: Int -> String -> PreProcessorParser
preProcessWml' 0 [] = do
    -- pos <- getPosition
    -- error $ " reached eof @ " ++ show pos
    return ""
preProcessWml' 0 ('{': _) = char '{' *> substitute '}'
preProcessWml' 0 ('#': _) = char '#' *> directive 0
preProcessWml' 0 (_: cs) = do
    c <- anyChar
    st <- getState
    case pendingDefine st of
        Nothing ->  do { rest <- preProcessWml' 0 cs;
                         return $ (c:rest)
                       }
        _ -> do { b <- many (noneOf "#");
                  pendBody b;
                  inp <- getInput;
                  preProcessWml' 0 inp
                }
preProcessWml' _ [] = return ""
{--
preProcessWml' _ [] = do
    pos <- getPosition
    error $ "dangling #if " ++ show pos
--}
preProcessWml' sd ('#': _) = do
    -- inp <- getInput
    -- error $ "pp # " ++ show sd ++ inp
    char '#' *> directive sd
preProcessWml' sd _ = skip sd

continue sd = do
    inp <- getInput
    preProcessWml' sd inp

----------------------------------------------------------------------
-- Substitution - this can be macro expansion or file inclusion
-- the text '{xxxx}' gets substituted.
-- NB. We have already consumed the leading '{'
----------------------------------------------------------------------
substitute :: Char -> PreProcessorParser
substitute r = do
    items <- spaces *> manyTill substituteItem (char r)
    case items of
        (h:t) -> do { st <- getState;
                      d <- return $ M.lookup h (defines st);
                      substitute' d h t
                    }
        [] -> do { pos <- getPosition;
                   error $ "empty {} not allowed " ++ show pos
                 }
substituteItem :: PreProcessorParser
substituteItem = 
        spaces *> bracketItem <* spaces
    <|> braceItem <* spaces
    <|> many (noneOf " (){}\n\r\t") <* spaces

bracketItem = char '(' *> substituteItem' '(' ')' 

braceItem = char '{' *> substituteItem' '{' '}'

substituteItem' :: Char -> Char -> PreProcessorParser
substituteItem' l r = do
    items <- spaces *> manyTill substituteItem (char r)
    case items of
        [] -> error "empty {} not allowed"
        (h : t) -> return $ [l] ++ items ++ [r]
	           where items = h ++ foldr (\x y -> (' ': x ++ y)) "" t
    

type Continuation = DefMap -> (PreProcessorState, String)

substitute' Nothing pat _ = do
    st <- getState
    pos <- getPosition
    let file= sourceName pos
    inp <- getInput
    let preProcessWmlFile = do
            setPosition pos
            setInput inp
            -- error $ "continuing @ " ++ show pos ++ "\n" ++ inp
            preProcessWmlFile''
    let cont d = do 
            let pps = mkPPState (path st) (work st) d
                                (skippingDepth st) (pendingDefine st) (pendingBody st)
            case runParser preProcessWmlFile pps "" "" of
                Right r -> r
                Left e -> error $ show e
    let pps = mkPPState (Just pat) ((Cont cont file) : (work st)) (defines st) 0 Nothing []
    setState pps
    return ""
substitute' (Just d) pat args = do
    let scs = substituteArgs  (defArgs (sig d)) args (body d)
    st <- getState
    let pps = mkPPState Nothing [] (defines st) 0 Nothing []
    let scs' = case runParser preProcessWml pps "" scs of
                   Right r -> r
                   Left e -> fail $ show e
    let sd = skippingDepth st
    inp <- getInput
    -- pos <- getPosition
    --error $ "macro substitution @ " ++ show sd ++ " " ++ show pos ++ "\n" ++ scs' ++ "\n" ++ inp
    cs <- preProcessWml' sd inp
    return $ scs' ++ cs

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
skip :: Int -> PreProcessorParser
skip sd = do
    many (noneOf "#") 
    inp <- getInput;
    -- error $ "skip " ++ show sd ++ " " ++ inp
    preProcessWml' sd inp

directive :: Int -> PreProcessorParser
directive sd =
        textDomain sd
    <|> try (char 'd' *> define sd)
    <|> try (char 'i' *> ifDirective sd)
    <|> try (char 'e' *> elseEnd sd)
    <|> try (char 'u' *> undef sd)
    <|> comment sd -- hmm seems like there isn't always a space first thing
    <?> "preprocessor directive"

comment sd  = do
    restOfLine
    continue sd

restOfLine = many (noneOf "\n")

textDomain sd = do
    r <- getInput
    textDomain' "textdomain" r sd

textDomain' [] _ 0 = do
    rest <- continue 0
    return ('#': rest)
textDomain' [] _ sd = skip sd
textDomain' (t:ts) (r:rs) sd
    | t == r = textDomain' ts rs sd
    | otherwise = fail $ "expecting " ++ [t]

define 0  = do
    string "efine"
    s <- defineSig
    pendDefine s
    continue 0
define sd =  skip sd

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
                         (skippingDepth st) (Just s) []

pendBody b = do
    st <- getState
    setState $ mkPPState (path st) (work st) (defines st)
                         (skippingDepth st) (pendingDefine st) (b : (pendingBody st))

updateDefines = do
    st <- getState
    let (Just s) = pendingDefine st
    let body = dropWhile ((==) ' ') $ concat $ reverse $ (pendingBody st)
    nm <- return $ M.insert (defName s) (Define s body) (defines st)
    setState $ mkPPState (path st) (work st) nm
                         (skippingDepth st) Nothing []

ifDirective sd = do
    char 'f'
    ifDirective' sd

ifDirective' sd =
        char 'd' *> ifdef sd
    <|> char 'h' *> ifhave sd
    <|> char 'v' *> ifver sd
    <|> char 'n' *> ifn sd

ifdef sd = if' sd "ef" defCondition evalDefCondition id
ifndef sd = if' sd "ef" defCondition evalDefCondition not

defCondition = symbol <* restOfLine

symbol = spaces *> many (noneOf " \n\r\t")

evalDefCondition c st = M.member c (defines st)

ifhave sd = undefined
ifnhave sd = undefined
{--
ifhave sd = if' sd "ave" haveCondition evalHaveCondition (not.not)
ifnhave sd = if' sd "ave" haveCondition evalHaveCondition not

haveCondition = undefined

evalHaveCondition = undefined
--}

ifver sd = undefined
ifnver sd = undefined
{--
ifver sd = if' sd "er" verCondition evalVerCondition not.not
ifnver sd = if' sd "er" verCondition evalVerCondition not

verCondition = undefined

evalVerCondition = undefined
--}

ifn s =
        char 'd' *> ifndef s
    <|> char 'h' *> ifnhave s
    <|> char 'v' *> ifnver s

type IfConditionParser = PreProcessorParser
type IfConditionEvaluator = String -> PreProcessorState -> Bool
type Sense = Bool -> Bool

if' :: Int -> String -> IfConditionParser -> IfConditionEvaluator -> Sense -> PreProcessorParser
if' 0 key cond eval sense = do
    c <- string key *> cond 
    st <- getState
    let skip = sense $ eval c st
    let skipd = b2d skip
    setState $ mkPPState (path st) (work st) (defines st) 
                         skipd (pendingDefine st) (pendingBody st)
    -- error $ "if " ++ show skipd
    continue skipd
if' sd _ _ _ _ = changeSkipDepth $ sd + 1

b2d True = 0
b2d False = 1

changeSkipDepth skipd= do
    st <- getState
    setState $ mkPPState (path st) (work st) (defines st) 
                         skipd (pendingDefine st) (pendingBody st)
    continue skipd

elseEnd sd =
        char 'l' *> string "se" *> else' sd
    <|> string "nd" *> end sd

else' 0 = changeSkipDepth 1
else' 1 = changeSkipDepth $ 0
else' sd = skip sd

end sd =
        char 'i' *> endif sd
    <|> enddef sd

endif 0 = do
    restOfLine
    -- inp <- getInput
    -- error $ "#endif' " ++ show 0 ++ " " ++ inp
    continue 0
endif sd = do
    -- error $ "#endif' " ++ show sd
    changeSkipDepth $ sd - 1

enddef 0 = do
    string "def"
    updateDefines
    restOfLine
    continue 0
enddef sd = do
    -- error $ "#enddef' " ++ show sd
    skip sd

undef 0 = do
    string "ndef"
    l <- restOfLine
    undef' $ words l
    continue 0
undef sd = skip sd

undef' [] = unexpected ": #undef missing symbol"
undef' (k: _) = do
    st <- getState
    nm <- return $ M.delete k (defines st)
    return $ mkPPState (path st) (work st) nm
                       (skippingDepth st) (pendingDefine st) (pendingBody st)

-- A little utility
pp x y = do { p <- preProcessWmlFile x; writeFileUtf8 y p;}
