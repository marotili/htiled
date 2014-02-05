{-# LANGUAGE UnicodeSyntax, RecordWildCards, TemplateHaskell #-}
module Data.Tiled.Types where

import Data.Map (Map)
import Data.Word (Word8, Word32)

import Control.Lens

-- | Orientations.
data MapOrientation = Orthogonal | Isometric deriving (Show, Eq)

-- | Properties.
type Properties = [(String, String)]

-- | A polygon.
data Polygon = Polygon [(Int, Int)] deriving (Show, Eq)

-- | A polyline.
data Polyline = Polyline [(Int, Int)] deriving (Show, Eq)

-- | An image containing tiles.
data Image = Image
           { _iSource         ∷ FilePath
           , _iTrans          ∷ Maybe (Word8, Word8, Word8)
           , _iWidth, _iHeight ∷ Int
           } deriving (Show, Eq)
makeLenses ''Image

-- | An object, usable for stuff not repetitively aligned on a grid.
data Object = Object
            { _objectName                ∷ Maybe String
            , _objectType                ∷ Maybe String
            , _objectProperties          ∷ Properties
            , _objectX, _objectY          ∷ Int
            , _objectWidth, _objectHeight ∷ Maybe Int
            , _objectGid                 ∷ Maybe Word32
            , _objectPolygon             ∷ Maybe Polygon
            , _objectPolyline            ∷ Maybe Polyline
            } deriving (Show, Eq)
makeLenses ''Object

-- | A single tile as is stored in a layer.
data Tile = Tile { _tileGid                        ∷ Word32
                 , _tileIsVFlipped, _tileIsHFlipped, _tileIsDiagFlipped ∷ Bool
                 } deriving (Show, Eq, Ord)
makeLenses ''Tile

-- | Either a tile layer or an object layer.
data Layer = Layer
           { _layerName       ∷ String
           , _layerOpacity    ∷ Float
           , _layerIsVisible  ∷ Bool
           , _layerProperties ∷ Properties
           , _layerData       ∷ Map (Int, Int) Tile
           }
           | ObjectLayer
           { _layerName       ∷ String
           , _layerOpacity    ∷ Float
           , _layerIsVisible  ∷ Bool
           , _layerProperties ∷ Properties
           , _layerObjects    ∷ [Object]
           }
           | ImageLayer
           { _layerName       ∷ String
           , _layerOpacity    ∷ Float
           , _layerIsVisible  ∷ Bool
           , _layerProperties ∷ Properties
           , _layerImage      ∷ Image
           } deriving Eq
makeLenses ''Layer
--makePrisms ''Layer
_Layer :: Prism' Layer Layer
_Layer = prism' id (\l -> case l of Layer {} -> Just l; _ -> Nothing) 
_ObjectLayer :: Prism' Layer Layer
_ObjectLayer = prism' id (\l -> case l of ObjectLayer {} -> Just l; _ -> Nothing) 
_ImageLayer :: Prism' Layer Layer
_ImageLayer = prism' id (\l -> case l of ImageLayer {} -> Just l; _ -> Nothing) 


instance Show Layer where
    show Layer {..} = "Layer { layerName = " ++ show _layerName ++
                            ", layerOpacity = " ++ show _layerOpacity ++
                            ", layerIsVisible = " ++ show _layerIsVisible ++
                            ", layerProperties = " ++ show _layerProperties ++
                            ", layerData = \"...\" }"
    show ObjectLayer {..} = "ObjectLayer { layerName = " ++ show _layerName ++
                                        ", layerOpacity = " ++ show _layerOpacity ++
                                        ", layerIsVisible = " ++ show _layerIsVisible ++
                                        ", layerProperties = " ++ show _layerProperties ++
                                        ", layerObjects = " ++ show _layerObjects ++ " }"
    show ImageLayer {..} = "ObjectLayer { layerName = " ++ show _layerName ++
                                       ", layerOpacity = " ++ show _layerOpacity ++
                                       ", layerIsVisible = " ++ show _layerIsVisible ++
                                       ", layerProperties = " ++ show _layerProperties ++
                                       ", layerImage = " ++ show _layerImage ++ " }"

-- | A set of tiles that can be used.
data Tileset = Tileset
             { _tsName                    ∷ String
             , _tsInitialGid              ∷ Word32
             , _tsSource                  :: String
             , _tsTileWidth, _tsTileHeight ∷ Int
             , _tsSpacing, _tsMargin       ∷ Int
             , _tsTileImages :: Map Word32 Image
             , _tsImages                  ∷ [Image] -- ^ Multiple images not
                                                   -- yet supported in tiled.
             , _tsTileProperties          ∷ [(Word32, Properties)]
             } deriving (Show, Eq)
makeLenses ''Tileset

-- | A tiled map.
data TiledMap = TiledMap
         { _mapPath             ∷ FilePath -- ^ The file path of the map file.
         , _mapOrientation      ∷ MapOrientation
         , _mapWidth, _mapHeight ∷ Int
         , _mapTileWidth        ∷ Int
         , _mapTileHeight       ∷ Int
         , _mapProperties       ∷ Properties
         , _mapTilesets         ∷ [Tileset]
         , _mapLayers           ∷ [Layer]
         } deriving (Show, Eq)
makeLenses ''TiledMap


