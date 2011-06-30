-- file: terrainTranslation.hs

module TerrainTranslation
(

-- Re-exports from ParseTerrain
  P.Terrain(..)
, P.ParseError

-- Types
, TerrainList
, TerrainMap
, TerrainMatchCache
, Coordinate(..)
, ReadMapResult(..)

-- Terrain constants
, offMapUser
, voidTerrain
, fogged
, humanCastle
, humanKeep
, shallowWater
, deepWater
, gressLand
, forest
, mountain
, hill
, caveWall
, cave
, undergroundVillage
, dwarvenCastle
, dwarvenKeep
, plusTerrain
, minusTerrain
, notTerrain
, starTerrain
, baseTerrain

-- Match constansts
, allForestsMatch
, allHillsMatch
, allMountainsMatch
, allSwampsMatch

-- Terrain reading and writing
, readTerrainCode
, writeTerrainCode
, TerrainTranslation.readList
, writeList
, readGameMap
, writeGameMap

-- Terrain matching
, terrainMatches
, hasWildCard
, hasWildCards

-- Support for builder maps
, readBuilderMap

) 
    where

import Data.Word
import Data.Bits
import Data.List
import qualified Data.Map as Map

import Control.Applicative

import qualified ParseTerrain as P

mkTerrain :: P.Layer -> P.Layer -> P.Terrain
mkTerrain base overlay
    | overlay == P.wildCard && base == P.terrainBase notTerrain = P.Terrain base P.noLayer
    | overlay == P.wildCard && base == P.terrainBase starTerrain = P.Terrain base P.noLayer
    | otherwise = P.Terrain base overlay

getLayerMask :: P.Layer -> P.Layer
getLayerMask t
    | (t .&. 0xFF000000) == 0x2A000000 = 0x00000000
    | (t .&. 0x00FF0000) == 0x002A0000 = 0xFF000000
    | (t .&. 0x0000FF00) == 0x00002A00 = 0xFFFF0000
    | (t .&. 0x000000FF) == 0x0000002A = 0xFFFFFF00
    | otherwise                        = 0xFFFFFFFF

overlayIsNoLayer t = P.terrainOverlay t == P.noLayer

getMask :: P.Terrain -> P.Terrain
getMask t
    | overlayIsNoLayer t = P.Terrain (getLayerMask (P.terrainBase t)) 0xFFFFFFFF
    | otherwise =  P.Terrain (getLayerMask (P.terrainBase t)) (getLayerMask (P.terrainOverlay t))

layerMaskIsNoLayer :: P.Layer -> Bool
layerMaskIsNoLayer layer = getLayerMask layer == P.noLayer

hasWildCard :: P.Terrain -> Bool
hasWildCard t
    | overlayIsNoLayer t = not (layerMaskIsNoLayer (P.terrainBase t))
    | otherwise = not (layerMaskIsNoLayer (P.terrainBase t) 
                       && layerMaskIsNoLayer (P.terrainOverlay t))

type TerrainList = [P.Terrain]

hasWildCards :: TerrainList -> Bool
hasWildCards [] = False
hasWildCards s = any hasWildCard s

----------------------------------------------------------------------------------
--                              Terrain Map reading and writing                 --
----------------------------------------------------------------------------------
type TerrainMap = [TerrainList]

type StartingPosMap = Map.Map Int Coordinate
data ReadMapResult = ReadMapResult 
    {
      gameMap           :: TerrainMap
    , startingPositions :: StartingPosMap
    } 
        deriving (Eq, Show)

readGameMap :: String -> StartingPosMap -> Either P.ParseError ReadMapResult
readGameMap s spm = result
    where m = P.parseMap s
          mkres (Right m) spm = Right (ReadMapResult m' spm')
              where m' = map (map P.tsTerrain) m
                    spm' = updateStartingPs m spm
          check m = case m of (Left x) -> Left x
                              _ -> mkres m spm
          result = check m

updateStartingPs :: P.TerrainMap -> StartingPosMap -> StartingPosMap
updateStartingPs m spm = spm'
    where addTerrain ts (x, y, smp) = (x - 1, y, smp')
              where sp = P.tsStartingPos ts
                    smp' = if sp < 1 
                    then smp
                    else Map.insert sp (Coordinate x y) smp
          addRow r (x, y, smp) = (x', y', smp')
              where xlim = (length r) - 1
                    y' = y - 1
                    (x', y'', smp') = foldr addTerrain (xlim, y, smp) r
          ylim = (length m) - 1
          (_, _, spm') = foldr addRow (0, ylim, spm) m

readBuilderMap = P.parseBuilderMap

writeGameMap :: TerrainMap -> StartingPosMap -> Int -> String
writeGameMap tm sm minSize = result
    where pl = Map.toList sm
          y = fromIntegral(length tm)
          acc = (y - 1, pl, minSize, "")
          (_, _, _, result) = foldr writeGameRow acc tm

writeGameRow row (y, sm, minSize, result) = (y - 1, sm', minSize, result' ++ result)
    where rl = fromIntegral(length row)
          writeTerrain t (x, s , r) = (x', s', r')
              where x' = x - 1
		    c = (Coordinate x y)
                    e = find (((==)c).snd) sm
                    (sp, s') = case e of Nothing -> (-1, sm)
					 Just e'@(k, _) -> (k, delete e' sm)
		    sep = if rl - 1 == x then "" else ", "
                    r' = unparseTerrain t sp minSize ++ sep ++ r 
          (_, sm', result') = foldr writeTerrain (rl - 1, sm, "\n") row

----------------------------------------------------------------------------------
--                              Terrain Matching                                --
----------------------------------------------------------------------------------
data TerrainMatchCacheEntry = TerrainMatchCacheEntry 
    {
      cacheEntryTerrain       :: P.Terrain
    , cacheEntryMask          :: P.Terrain
    , cacheEntryMaskedTerrain :: P.Terrain
    , cacheEntryHasWildCard   :: Bool
    } 
        deriving (Eq, Show)

type TerrainMatchCache = [TerrainMatchCacheEntry]

data TMArgs = TMNull
            | TMString String
            | TMTerrain P.Terrain
            | TMTerrainList TerrainList
            deriving (Eq, Show)

mkTMatch :: TMArgs -> TerrainMatchCache
mkTMatch TMNull = []
mkTMatch (TMString s) =
    let r =  TerrainTranslation.readList s
    in case r of (Right tl) -> mkTMatch (TMTerrainList tl)
                 _ -> []
mkTMatch (TMTerrain t) = mkTMatch $ TMTerrainList [t]
mkTMatch (TMTerrainList tl) = zipWith4 TerrainMatchCacheEntry tl ml mtl wl
    where wl = map hasWildCard tl
          ml = map getMask tl
	  f t1 t2 = P.Terrain  ((P.terrainBase t1) .&. (P.terrainBase t2))
                               ((P.terrainOverlay t1) .&. (P.terrainOverlay t2))
	  mtl = zipWith f tl ml

matchFrom :: String -> TerrainMatchCache
matchFrom s = mkTMatch (TMString s)

data MatchTerrainArgs = T_T P.Terrain P.Terrain
                  | T_TL P.Terrain TerrainList
                  | T_TMC P.Terrain TerrainMatchCache
                  deriving (Eq, Show)

terrainMatches :: P.Terrain -> TerrainList -> Bool
terrainMatches t tl = matchTerrain (T_TL t tl)

matchTerrain :: MatchTerrainArgs -> Bool
matchTerrain (T_T s d ) = matchTerrain $ T_TL s [d]
matchTerrain (T_TL _ []) = False
matchTerrain (T_TL s tl) = matchTerrain $ T_TMC s $ mkTMatch $ TMTerrainList tl
matchTerrain (T_TMC s m) = matchTMatch s m True

matchTMatch :: P.Terrain -> TerrainMatchCache -> Bool -> Bool
matchTMatch s [] r = not r
matchTMatch s (m:ms) r
    | cacheEntryTerrain m == starTerrain = r
    | cacheEntryTerrain m == notTerrain = matchTMatch s ms (not r)
    | s == cacheEntryTerrain m = r
    | cacheEntryHasWildCard m && matchTWild s m = r
    | otherwise = matchTMatch s ms r

matchTWild s m = baseMatch && overlayMatch
    where mask = cacheEntryMask m
          maskedTerrain = cacheEntryMaskedTerrain m
          baseMatch = ((P.terrainBase s) .&. (P.terrainBase mask)) == (P.terrainBase maskedTerrain)
	  overlayMatch = ((P.terrainOverlay s) .&. (P.terrainOverlay mask)) == (P.terrainOverlay maskedTerrain)

-------------------------------------------
--            Coordinates                --
-------------------------------------------
data Coordinate = Coordinate 
    {
      xCoord :: Int
    , yCoord :: Int
    } 
        deriving (Eq, Ord, Show)


---------------------------------------------------------------
--                         Terrain Parsing                   --
---------------------------------------------------------------
readTerrainCode = P.parseTerrain

parseTerrain_ s = 
    case P.parseTerrain s of (Right t) -> t
                             _         -> P.noTerrain



writeTerrainCode :: P.Terrain -> String
writeTerrainCode t = unparseTerrain t (-1) 1

readList s = map P.tsTerrain  <$> P.parseRow s

writeList :: TerrainList -> String
writeList [] = ""
writeList (t: []) = writeTerrainCode t
writeList (t:ts) = (writeTerrainCode t) ++ (foldr w "" ts)
    where w t s = ", " ++ (writeTerrainCode t) ++ s 

unparseTerrain :: P.Terrain -> Int -> Int -> String
unparseTerrain terrain startingPos minSize =
    let ps = if startingPos > 0 then show startingPos ++ " " else ""
	extractChar f (m, s) l = let x = shift((f terrain) .&. m) s
                               in if x == 0 then l else ((toEnum ((fromIntegral x) :: Int) :: Char) : l)
	ms = [(0xFF000000, -24),(0x00FF0000, -16),(0x0000FF00, -8),(0x000000FF, 0)]
        bs = foldr (extractChar P.terrainBase) [] ms
	os = if P.terrainOverlay terrain == P.noLayer then "" else ('^' : foldr (extractChar P.terrainOverlay) [] ms )
	spec = ps ++ bs ++ os
	l = length spec
	padding = if l < minSize then replicate (minSize - l) ' ' else []
    in spec ++ padding

-----------------------------------------------------
--                   Terrain Constants             --
-----------------------------------------------------
offMapUser         = parseTerrain_ "_off^_usr"

voidTerrain        = parseTerrain_ "_s"
fogged             = parseTerrain_ "_f"

humanCastle        = parseTerrain_ "Ch"
humanKeep          = parseTerrain_ "Kh"
shallowWater       = parseTerrain_ "Ww"
deepWater          = parseTerrain_ "Wo"
gressLand          = parseTerrain_ "Gg"
forest             = parseTerrain_ "Gg^Ff"
mountain           = parseTerrain_ "Mm"
hill               = parseTerrain_ "Hh"

caveWall           = parseTerrain_ "Xu"
cave               = parseTerrain_ "Uu"
undergroundVillage = parseTerrain_ "Uu^Vu"
dwarvenCastle      = parseTerrain_ "Cud"
dwarvenKeep        = parseTerrain_ "Kud"

plusTerrain        = parseTerrain_ "+"
minusTerrain       = parseTerrain_ "-"
notTerrain         = parseTerrain_ "!"
starTerrain        = parseTerrain_ "*"
baseTerrain        = parseTerrain_ "_bas"

allForestsMatch   = matchFrom "F*,*^F*"
allHillsMatch     = matchFrom "!,*^V*,!,H*"
allMountainsMatch = matchFrom "!,*^V*,!,M*"
allSwampsMatch    = matchFrom "!,*^V*,*^B*,!,S*"


{- test data

testMap = "\nGs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Re          , Gs^Fds      , Re          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Re          , Gs^Fds      , Re          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Re          , Re          , Ce          , Re          , Re          , Re          , Re          , Gs^Fds      , Re          , Gs^Fds      , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Ggf         , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      \nRe          , Re          , Re          , Ce          , 2 Ke        , Ce          , Re          , Re          , Gs^Fds      , Re          , Re          , Re          , Gg          , Re          , Gg          , Ggf         , Gg          , Gg          , Gg          , Hh          , Hh          , Hh          \nRe          , Re          , Re          , Ce          , Ce          , Ce          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Re          , Gg          , Gg          , Gs^Fds      , Gg          , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Hh          , Hh          , Hh          \nGg          , Gg          , Gg          , Re          , Re          , Re          , Re          , Gs^Fds      , Re          , Gg          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Hh          , Hh          , Hh          \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Gs^Fds      , Re          , Re          , Gg          , Re          , Re          , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Ggf         , Gg          , Gg^Fet      , Gs^Fds      , Gg^Fet      , Hh          , Hh          \nGs^Fds      , Gs^Fds      , Re          , Re          , Re          , Gs^Fds      , Gs^Fds      , Gg          , Re          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      \nRe          , Re          , Gg^Ve       , Re          , Re          , Re          , Gg          , Gg          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Ggf         , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Re          , Re          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Ggf         , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      , Ggf         , Gg          , Gg          , Gg          , Gg          , Gg          , Gg^Fet      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Gg^Fet      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Fet      , Ggf         , Gs^Fds      , Gg          , Gg          , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gs^Fds      , Ggf         , Ggf         , Gs^Fds      , Gs^Fds      , Gg          , Ggf         , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gg^Fet      , Gs^Fds      , Gg          , Gg          , Ggf         , Ggf         , Gg          , Ggf         , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Ggf         , Gg          , Gs^Fds      , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      \nGs^Fds      , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Ve       , Gg          , Ggf         , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Ggf         , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \nHh          , Hh          , Gs^Fds      , Gg^Fet      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      , Gs^Fds      \nHh          , Hh          , Hh          , Gs^Fds      , Hh          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg          , Ggf         , Gs^Fds      , Ggf         , Gg          , Gs^Fds      , Gg          , Gg          , Cv          , Gg          , Ggf         , Gs^Fds      , Gs^Fds      , Gs^Fds      \nMm          , Mm          , Mm          , Hh          , Hh          , Gg^Fet      , Ggf         , Gs^Fds      , Ggf         , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Ggf         , Gg          , Cv          , 1 Kv        , Cv          , Gg          , Gg          , Gs^Fds      , Gs^Fds      \nHh          , Hh          , Hh          , Hh          , Hh          , Hh          , Ggf         , Gg          , Gg^Ve       , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Ve       , Gs^Fds      , Gg          , Cv          , Cv          , Cv          , Gg          , Ggf         , Gg^Fet      , Gs^Fds      \nHh          , Hh          , Gg          , Hh          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gg^Fet      , Gs^Fds      , Gs^Fds      , Gg          , Gg          , Gg          , Gg          , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGg          , Gg          , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \nGg          , Gg          , Gs^Fds      , Gg          , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      , Gs^Fds      \n"::String

-}