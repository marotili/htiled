{-# LANGUAGE Arrows, UnicodeSyntax, RecordWildCards, NamedFieldPuns #-}
module Data.Tiled.Load (loadMapFile, loadMap) where

import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Data.Bits (testBit, clearBit)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (fromDistinctAscList, Map, fromList)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Word (Word32)

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Text.XML.HXT.Core

import Data.Tiled.Types

-- | Load a map from a string
loadMap ∷ String → IO TiledMap
loadMap str = load (readString [] str) "binary"

-- | Load a map file.
loadMapFile ∷ FilePath → IO TiledMap
loadMapFile fp = load (readDocument [] fp) fp

load ∷ IOStateArrow () XmlTree XmlTree -> FilePath -> IO TiledMap
load a fp = head `fmap` runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> a
    >>> getChildren >>> isElem
    >>> doMap fp)

properties ∷ IOSArrow XmlTree Properties
properties = listA $ getChildren >>> isElem >>> hasName "properties"
            >>> getChildren >>> isElem >>> hasName "property"
            >>> getAttrValue "name" &&& getAttrValue "value"

getAttrR ∷ (Read α, Num α) ⇒ String → IOSArrow XmlTree α
getAttrR a = arr read . getAttrValue0 a

getAttrMaybe ∷ (Read α, Num α) ⇒ String → IOSArrow XmlTree (Maybe α)
getAttrMaybe a = arr tm . getAttrValue a
    where
        tm "" = Nothing
        tm s = Just $ read s

doMap ∷ FilePath → IOSArrow XmlTree TiledMap
doMap mapPath = proc m → do
    _mapOrientation ← arr (\x → case x of "orthogonal" → Orthogonal
                                          "isometric" → Isometric
                                          _ → error "unsupported orientation")
                     . getAttrValue "orientation" ⤙ m
    _mapWidth       ← getAttrR "width"      ⤙ m
    _mapHeight      ← getAttrR "height"     ⤙ m
    _mapTileWidth   ← getAttrR "tilewidth"  ⤙ m
    _mapTileHeight  ← getAttrR "tileheight" ⤙ m
    _mapProperties  ← properties            ⤙ m
    _mapTilesets    ← tilesets              ⤙ m
    _mapLayers      ← layers                ⤙ (m, (_mapWidth, _mapHeight))
    returnA        ⤙ TiledMap {..} { _mapPath = mapPath }

layers ∷ IOSArrow (XmlTree, (Int, Int)) [Layer]
layers = listA (first (getChildren >>> isElem) >>> doObjectGroup <+> doLayer <+> doImageLayer)
  where
    doObjectGroup = arr fst >>> hasName "objectgroup" >>> id &&& (listA object >>> arr Right) >>> common

    object = getChildren >>> isElem >>> hasName "object"
         >>> proc obj → do
        _objectName     ← arr listToMaybe . listA (getAttrValue "name") ⤙ obj
        _objectType     ← arr listToMaybe . listA (getAttrValue "type") ⤙ obj
        _objectX        ← getAttrR "x"                                  ⤙ obj
        _objectY        ← getAttrR "y"                                  ⤙ obj
        _objectWidth    ← arr listToMaybe . listA (getAttrR "width")    ⤙ obj
        _objectHeight   ← arr listToMaybe . listA (getAttrR "height")   ⤙ obj
        _objectGid      ← arr listToMaybe . listA (getAttrR "gid")      ⤙ obj
        _objectPolygon  ← arr listToMaybe . polygon                     ⤙ obj
        _objectPolyline ← arr listToMaybe . polyline                    ⤙ obj
        _objectProperties ← properties                                  ⤙ obj
        returnA      ⤙ Object {..}

    polygon ∷ IOSArrow XmlTree [Polygon]
    polygon = listA $ getChildren >>> isElem >>> hasName "polygon"
          >>> getAttrValue "points" >>> arr (Polygon . points)
    polyline ∷ IOSArrow XmlTree [Polyline]
    polyline = listA $ getChildren >>> isElem >>> hasName "polyline"
          >>> getAttrValue "points" >>> arr (Polyline . points)

    points :: String → [(Int, Int)]
    points s = (x, y):if null rest then [] else points rest
        where (p, rest) = drop 1 `fmap` break (==' ') s
              (x', y') = drop 1 `fmap` break (==',') p
              x = read x'
              y = read y'

    doImageLayer = arr fst >>> hasName "imagelayer" >>> id &&& image >>> proc (l, layerImage) → do
        _layerName ← getAttrValue "name" ⤙ l
        _layerOpacity ← arr (fromMaybe 1 . listToMaybe) . listA (getAttrR "opacity") ⤙ l
        _layerIsVisible ← arr (isNothing . listToMaybe) . listA (getAttrValue "visible") ⤙ l
        _layerProperties ← properties ⤙ l
        returnA ⤙ ImageLayer{..}

    doLayer = first (hasName "layer") >>> arr fst &&& (doData >>> arr Left) >>> common

    doData = first (getChildren >>> isElem >>> hasName "data")
         >>> proc (dat, (w, h)) → do
                liftArrIO (\inp -> print "Enter") -< dat
                encoding    ← getAttrValue "encoding"        ⤙ dat
                liftArrIO (\inp -> print "Enter1") -< dat
                compression ← getAttrValue "compression"     ⤙ dat
                liftArrIO (\inp -> print "Enter2") -< dat
                --text        ← getText . isText . getChildren ⤙ dat
                --liftArrIO (\inp -> print "Enter3") -< dat

                tiles <- listA (getChildren >>> isElem >>> hasName "tile" >>> tileProc) -< (dat :: XmlTree)
                liftArrIO (\_ -> print "Tiles") -< dat

                returnA ⤙ (toMap w h tiles)-- `orElse` (dataToTiles w h encoding compression text)

      where
        tileProc = proc tile -> do
          gid <- getAttrR "gid" -< (tile :: XmlTree)
          returnA -< Tile 
            { _tileGid=gid
            , _tileIsVFlipped=False 
            , _tileIsHFlipped=False
            , _tileIsDiagFlipped=False 
            }
                  

    -- Width → Height → Encoding → Compression → Data → [Tile]
    dataToTiles ∷ Int → Int → String → String → String → Map (Int, Int) Tile
    dataToTiles w h "base64" "gzip" = toMap w h . base64 GZip.decompress
    dataToTiles w h "base64" "zlib" = toMap w h . base64 Zlib.decompress
    dataToTiles w h "" "" = toMap w h . bytesToTiles . LBS.unpack . LBS.fromChunks . (:[]) . BS.pack
    dataToTiles _ _ b c = error ("unsupported tile data format, only base64 and \
                               \gzip/zlib is supported at the moment." ++ show (b, c))

    toMap :: Int -> Int -> [Tile] -> Map (Int, Int) Tile
    toMap w h = fromDistinctAscList . sort . filter (\(_, x) → _tileGid x /= 0)
                . zip [(x, y) | y ← [0..h-1], x ← [0..w-1]]

    base64 f = bytesToTiles . LBS.unpack . f . LBS.fromChunks
                            . (:[]) . B64.decodeLenient . BS.pack

    bytesToTiles (a:b:c:d:xs) = Tile { .. } : bytesToTiles xs
      where n = f a + f b * 256 + f c * 65536 + f d * 16777216
            f = fromIntegral . fromEnum ∷ Char → Word32
            _tileGid = n `clearBit` 30 `clearBit` 31 `clearBit` 29
            _tileIsVFlipped = n `testBit` 30
            _tileIsHFlipped = n `testBit` 31
            _tileIsDiagFlipped = n `testBit` 29
    bytesToTiles [] = []
    bytesToTiles rest = error ("number of bytes not a multiple of 4." ++ show rest)

    common = proc (l, x) → do
        _layerName       ← getAttrValue "name"              ⤙ l
        _layerOpacity    ← arr (fromMaybe 1 . listToMaybe)
                          . listA (getAttrR "opacity")     ⤙ l
        _layerIsVisible  ← arr (isNothing . listToMaybe)
                          . listA (getAttrValue "visible") ⤙ l
        _layerProperties ← properties                      ⤙ l
        returnA ⤙ case x of Left  _layerData    → Layer {..}
                            Right _layerObjects → ObjectLayer {..}

tilesets ∷ IOSArrow XmlTree [Tileset]
tilesets = listA $ getChildren >>> isElem >>> hasName "tileset"
         >>> tileData Nothing

liftArrIO :: (a -> IO b) -> IOSLA s a b
liftArrIO f = IOSLA $ \s x -> fmap (\y -> (s, [y])) (f x)

data TileSetHead = TileSetHead 
  { tshSource :: String
  , tshInitialGid :: Word32
  }

tileData :: Maybe TileSetHead -> IOSArrow XmlTree Tileset
tileData headData = proc ts → do
      t <- path1 `orElse` tData -< ts
      returnA -< t

  where tileProperties ∷ IOSArrow XmlTree (Word32, Properties)
        tileProperties = getChildren >>> isElem >>> hasName "tile"
                     >>> getAttrR "id" &&& properties

        images = listA (getChildren >>> image)
        --tiles = listA ()

        path1 :: IOSArrow XmlTree Tileset
        path1 = hasAttr "source" >>> headDataA >>> liftArrIO (\hData ->
              loadTileSet (readDocument [] ("data/" ++ tshSource hData)) hData)

        headDataA = proc ts -> do
          tshSource <- getAttrValue "source" -< ts
          tshInitialGid <- getAttrR "firstgid" -< ts
          returnA -< TileSetHead{..}

        tData :: IOSArrow XmlTree Tileset
        tData = proc ts -> do
          _tsName        ← getAttrValue "name"     ⤙ ts
          _tsInitialGid  ← (case headData of 
            Just (TileSetHead { tshInitialGid }) -> liftArrIO (return . return tshInitialGid)
            _ -> getAttrR "firstgid")     ⤙ ts
          _tsTileWidth   ← getAttrR "tilewidth"    ⤙ ts
          _tsTileHeight  ← getAttrR "tileheight"   ⤙ ts
          _tsMargin      ← (arr $ fromMaybe 0) . getAttrMaybe "margin" ⤙ ts
          _tsSpacing     ← (arr $ fromMaybe 0) . getAttrMaybe "spacing" ⤙ ts
          _tsImages      ← images                  ⤙ ts
          _tsTileProperties ← listA tileProperties ⤙ ts
          _tileIds <- listA (getChildren >>> tileId) -< ts
          _tileImages <- listA (getChildren >>> tileImage) -< ts
          let tileimgs = fromList $ zip _tileIds (concat _tileImages)
          returnA ⤙ (Tileset {..}) { _tsTileImages = tileimgs, _tsSource = ""}

loadTileSet ∷ IOStateArrow () XmlTree XmlTree -> TileSetHead -> IO Tileset
loadTileSet a headData = head `fmap` runX (
        configSysVars [withValidate no, withWarnings yes]
    >>> a
    >>> getChildren >>> isElem
    >>> tileData (Just headData)
    )

tileId :: IOSArrow XmlTree Word32
tileId = isElem >>> hasName "tile" >>> proc tile -> do
  id <- getAttrR "id" -< tile
  returnA -< id

tileImage :: IOSArrow XmlTree [Image]
tileImage = isElem >>> hasName "tile" >>> listA (getChildren >>> proc tile -> do
    image <- image -< tile
    returnA -< image
  )

image ∷ IOSArrow XmlTree Image
image = isElem >>> hasName "image" >>> proc img → do
    _iSource ← getAttrValue "source"   ⤙ img
    _iTrans  ← arr (fmap colorToTriplet . listToMaybe) . listA (getAttrValue0 "trans") ⤙ img
    _iWidth  ← getAttrR "width"        ⤙ img
    _iHeight ← getAttrR "height"       ⤙ img
    returnA ⤙ Image {..} { _iSource = "data/" ++ _iSource}
    where
        colorToTriplet x = (h x, h $ drop 2 x, h $ drop 4 x)
            where h (y:z:_) = fromIntegral $ digitToInt y * 16 + digitToInt z
                  h _ = error "invalid color in an <image ...> somewhere."
