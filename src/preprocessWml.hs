-- file: preprocessWml.hs

module PreProcessWml
(
  preProcessWmlFile
, initState
, PreProcessorState(..)
, WorkItem(..)
) 
    where

import Control.Monad

import Data.List as L
import Data.Map as M

import Text.Parsec.Prim (unexpected)
import Text.ParserCombinators.Parsec.Prim (getState, setState)

import ApplicativeParsec

import WesConfig as WCgf

data WorkItem =
     File !FilePath
   | Cont !Continuation !FilePath

data PreProcessorState = PreProcessorState
    {
      path :: Maybe FilePath
    , work :: ![WorkItem]
    , defines :: !DefMap
    , domain :: !String
    , skippingDepth :: !Int
    , pendingDefine :: Maybe DefSig
    , pendingBody :: ![String]
    , inString :: !Bool
    }
        -- deriving (Eq, Show)

updatePath st p = PreProcessorState p (work st) (defines st) (domain st) (skippingDepth st) (pendingDefine st) (pendingBody st) (inString st)
updateWork st w = PreProcessorState (path st) w (defines st) (domain st) (skippingDepth st) (pendingDefine st) (pendingBody st) (inString st)
updateDefineMap st nm = PreProcessorState (path st) (work st) nm (domain st) (skippingDepth st) (pendingDefine st) (pendingBody st) (inString st)
updateDomain st d = PreProcessorState (path st) (work st) (defines st) d (skippingDepth st) (pendingDefine st) (pendingBody st) (inString st)
updateSDepth st d = PreProcessorState (path st) (work st) (defines st) (domain st) d (pendingDefine st) (pendingBody st) (inString st)
updateDefine st d = PreProcessorState (path st) (work st) (defines st) (domain st) (skippingDepth st) d [] (inString st)
updateBody st b = PreProcessorState (path st) (work st) (defines st) (domain st) (skippingDepth st) (pendingDefine st) b (inString st) 
toggleInString st = PreProcessorState (path st) (work st) (defines st) (domain st) (skippingDepth st) (pendingDefine st) (pendingBody st) (not (inString st))

type PreProcessorParser = GenParser Char PreProcessorState String

-- Initial Preprocessor State
initState path = PreProcessorState (Just path)
                                   []           -- no work
                                   M.empty      -- no defines
                                   WCgf.package -- the default textdomain 
                                   0            -- skip depth
                                   Nothing      -- no define sig
                                   []           -- or body
                                   False        -- not in a string

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
    , defDomain :: !String
    }
        deriving (Eq, Show)

preProcessWmlFile st n s = do
    case runParser preProcessWmlFile' st n s of
        Right r -> r
        Left e -> error $ show e

preProcessWmlFile' = do
    st <- getState
    l <- if (inString st)
         then return ""
         else lineInfo
    r <- preProcessWml
    st' <- getState
    return (st', l ++ r)

lineInfo = do
    pos <- getPosition
    st <- getState
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
    return ""
preProcessWml' 0 ('{': _) = char '{' *> substitute '}'
preProcessWml' 0 ('#': cs) = do
    st <- getState
    if inString st
    then preProcessWml' 0 ('_':cs) -- ignore the #
    else char '#' *> directive 0
preProcessWml' 0 (_: cs) = do
    c <- anyChar
    st <- getState
    case pendingDefine st of
        Nothing ->  if c == '"'
                    then do { setState $ toggleInString st;
                              rest <- preProcessWml' 0 cs;
                              return $ (c:rest)
                            }
                    else do { rest <- preProcessWml' 0 cs;
                              return $ (c:rest)
                            }
        _ -> do { b <- many (noneOf "#");
                  pendBody b;
                  inp <- getInput;
                  preProcessWml' 0 inp
                }
preProcessWml' _ [] = return ""
preProcessWml' sd ('#': _) = do
    st <- getState
    if inString st
    then skip sd  
    else char '#' *> char '#' *> directive sd
preProcessWml' sd ('"':_) = do
    st <- getState
    setState $ toggleInString st
    anyChar
    skip sd
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
    let landt = do
        if (inString st)
        then return ("","")
        else do
             {
               linfo <- lineInfo;
               return (linfo, "#textdomain " ++ (domain st) ++ "\n")
             }
    let preProcessWmlFilePostInclude = do
            setPosition pos
            setInput inp
            (l , td) <- landt
            r <- preProcessWml
            st' <- getState
            return (st', l ++ td ++ r)
    let cont d = do 
            let pps = updateDefineMap st d
            case runParser preProcessWmlFilePostInclude pps "" "" of
                Right r -> r
                Left e -> error $ show e
    let pps = PreProcessorState (Just pat) ((Cont cont file) : (work st)) (defines st) (domain st) 0 Nothing [] (inString st)
    setState pps
    return ""
substitute' (Just def) pat args = do
    let md = (defDomain (sig def))
    st <- getState
    let d = (domain st)
    let (mtd, td) = if (md == d) || (inString st)
                    then ("","")
                    else ("\376textdomain " ++ md ++ " ", 
                          "\376textdomain " ++ d ++ " ")
    let scs = substituteArgs  (defArgs (sig def)) args (body def)
    let pps = PreProcessorState Nothing [] (defines st) d 0 Nothing [] False
    let scs' = case runParser preProcessWml pps "" scs of
                   Right r -> r
                   Left e -> fail $ show e
    let sd = skippingDepth st
    inp <- getInput
    cs <- preProcessWml' sd inp
    return $ mtd ++ scs' ++ td ++ cs

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
    many (noneOf "#\"") 
    inp <- getInput;
    preProcessWml' sd inp

directive :: Int -> PreProcessorParser
directive sd =
        try (char 't' *> textDomain sd)
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

textDomain 0 = do
    string "extdomain"
    s <- symbol
    st <- getState
    setState $ updateDomain st s
    rest <- continue 0
    return $ "#textdomain " ++ s ++ rest
textDomain sd = skip sd

define 0  = do
    string "efine"
    s <- defineSig
    pendDefine s
    continue 0
define sd =  skip sd

defineSig = do
    l <- restOfLine
    (name, args) <- pdefsig l
    st <- getState
    return (DefSig name args (domain st))

pdefsig l = pdefsig' $ words l

pdefsig' [] = unexpected "#define must define a name"
pdefsig' (('#':_): _) = unexpected "#define must define a name"
pdefsig' (name:args) = return (name, pdefargs args)

pdefargs [] = []
pdefargs (('#':_): _) = []
pdefargs (x:xs) = (("{" ++ x ++ "}"): pdefargs xs)

pendDefine s = do
    st <- getState
    setState $ updateDefine st (Just s)

pendBody b = do
    st <- getState
    setState $ updateBody st (b : (pendingBody st))

updateDefines = do
    st <- getState
    let (Just s) = pendingDefine st
    let body = dropWhile ((==) ' ') $ concat $ reverse $ (pendingBody st)
    nm <- return $ M.insert (defName s) (Define s body) (defines st)
    setState $ PreProcessorState (path st) (work st) nm (domain st)
                                 (skippingDepth st) Nothing [] (inString st)

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
    setState $ updateSDepth st skipd
    continue skipd
if' sd _ _ _ _ = changeSkipDepth $ sd + 1

b2d True = 0
b2d False = 1

changeSkipDepth skipd= do
    st <- getState
    setState $ updateSDepth st skipd
    continue skipd

elseEnd sd =
        char 'l' *> string "se" *> else' sd
    <|> string "nd" *> end sd

else' 0 = changeSkipDepth 1
else' 1 = changeSkipDepth $ 0
else' sd = skip sd

end sd =
        char 'i' *> char 'f' *> endif sd
    <|> enddef sd

endif 0 = do
    restOfLine
    continue 0
endif sd = changeSkipDepth $ sd - 1

enddef 0 = do
    string "def"
    updateDefines
    restOfLine
    continue 0
enddef sd = skip sd

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
    return $ updateDefineMap st nm

