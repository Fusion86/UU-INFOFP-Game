module TileSet where

import Codec.Picture (Image (imageHeight, imageWidth), PixelRGBA8, convertRGBA8, readImage)
import Codec.Picture.Extra (crop)
import Common
import Coordinates
import Data.Map (empty, fromList, insert)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Model

loadTileSet :: FilePath -> IO TileSet
loadTileSet f = do
  tileSetImg <- readImage f
  case tileSetImg of
    Left err -> error err
    Right img ->
      let tileSet = cutImageToTileSet $ convertRGBA8 img
       in return $ trace ("Loaded tileSet with " ++ show (length tileSet) ++ " tiles.") tileSet
  where
    cutImageToTileSet :: Image PixelRGBA8 -> TileSet
    cutImageToTileSet img = helper 1 (0, 0) -- IDs start at 1, because 0 = no tile.
      where
        helper :: Int -> (Int, Int) -> TileSet
        helper id (x, y)
          -- If end of row, go to next row.
          | x >= imageWidth img = helper id (0, y + 8)
          -- If y > height, aka end of the image.
          | y >= imageHeight img = empty
          -- Otherwise, cut/crop the tile from the source image.
          | otherwise =
            let tile = fromImageRGBA8 $ crop x y 8 8 img
             in insert id tile (helper (id + 1) (x + 8, y))
