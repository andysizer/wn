-- file: parseTerrain.hs

module ParseTerrain
( TerrainMap
, TerrainRow
, Terrain(..)
, TerrainSpec(..)
, parseMap
, parseRow
, parseTerrain
, parseBuilderMap
, noTerrain
, Layer
, toLayer
, wildCard
, noLayer
, builderStarLayer
, builderDotLayer
, module Data.Word
, module Data.Bits
, ParseError
) 
    where

import Data.Word
import Data.Bits
import Data.List
import Numeric

import ApplicativeParsec

------------------------------------------------------------------------------
--                      Types and constants
------------------------------------------------------------------------------
type Layer = Word32

wildCard :: Layer
wildCard = 0x2A000000
noLayer :: Layer
noLayer = 0xFFFFFFFF

builderStarLayer = shift (fromIntegral (fromEnum '*') :: Layer) 24
builderDotLayer  = shift (fromIntegral (fromEnum '.') :: Layer) 24

toLayer :: String -> Layer
toLayer "" = noLayer
toLayer s
    | length s >= 5 = noLayer
    | otherwise = foldl add 0 s
        where add acc y = shift acc 8 + fromIntegral (fromEnum y) :: Layer

data Terrain = Terrain 
    {
      terrainBase    :: Layer
    , terrainOverlay :: Layer
    } 
        deriving (Eq, Ord, Show)

data TerrainSpec = TerrainSpec 
    {
      tsStartingPos :: Int
    , tsTerrain    :: Terrain
    } 
       deriving (Eq, Ord, Show)

mkTerrain b Nothing = Terrain b noLayer
mkTerrain b (Just o) = Terrain b o

type TerrainRow = [TerrainSpec]
type TerrainMap = [TerrainRow]

noTerrain = Terrain 0 noLayer

------------------------------------------------------------------------------------
--                     Exported parse functions
------------------------------------------------------------------------------------
parseMap s = m
    where r = parse terrainMap "(Invalid Map)" s
          m = validate r

validate m@(Right []) = m
validate r@(Right m) = result
    where x = length $ head m
          offenders = findIndices (((/=) x).length) m
          e l = newErrorMessage (Message ("Map not rectangular in rows: " ++ (show l))) 
                                (initialPos "validation")
          result = case offenders of [] -> r
                                     _ -> Left (e offenders)
validate m = m

parseRow = parse row "(Invalid terrain list)"

parseTerrain = parse terrain "(Invalid terrain string)"

parseBuilderMap = parse builderMap "(Invalid builder map)"

-------------------------------------------------------------------------------------
--                     Parsers
-------------------------------------------------------------------------------------
terrainMap = endBy row eol

row = eol *> row'
  <|> row'

row' = terrainSpec `sepBy` (char ',')

terrainSpec = TerrainSpec <$> (spacesOrTabs *> (startingPos <* spacesOrTabs)) <*> terrain

startingPos = do s <- getInput
                 case readDec s of
                     [(n, s')] -> n <$ setInput s'
                     _         -> return (-1)

terrain = mkTerrain <$> (spacesOrTabs *> layer) <*> try (optionMaybe (char '^' >> layer)) <* spacesOrTabs

layer = val
    where token = choice [layer4, layer3, layer2, layer1]
          val = toLayer <$> token

layer1 = try ((:) <$> legal <*> return [])
layer2 = try((:) <$> legal <*> layer1)
layer3 = try((:) <$> legal <*> layer2)
layer4 = try((:) <$> legal <*> layer3)

legal = noneOf "\ \,^\n"

spaceOrTab = oneOf " \t"

spacesOrTabs = optionMaybe(many spaceOrTab)

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

builderMap = endBy builderRow eol

builderRow = eol *> builderRow'
  <|> builderRow'

builderRow' = (spacesOrTabs *> builderSpec) `sepBy` (spacesOrTabs *> (char ','))

builderSpec = do s <- getInput
                 case readDec s of
                     [(n, s')] -> (Terrain 0 n) <$ setInput s'
                     _         -> (Terrain (fromIntegral(shift (fromEnum (head s)) 24)::Layer) 0) <$ setInput (tail s)



