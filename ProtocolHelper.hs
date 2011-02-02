{-# LANGUAGE TemplateHaskell #-}
module ProtocolHelper where

import Generator
import Control.Applicative
import Control.Monad
import Codec.Compression.Zlib
import qualified Codec.Compression.Zlib.Internal as ZI
import Data.Int
import Data.Word
import Data.Bits
import Data.Foldable
import JavaBinary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

type BlockLoc = (Int8, Int8, Int8)

enum "BlockId"
  "UnknownBlock"
  [ (0x00,"Air")
  , (0x01,"Stone")
  , (0x02,"Grass")
  , (0x03,"Dirt")
  , (0x04,"Cobblestone")
  , (0x05,"WoodenPlank")
  , (0x06,"Sapling")
  , (0x07,"Bedrock")
  , (0x08,"Water")
  , (0x09,"StationaryWater")
  , (0x0A,"Lava")
  , (0x0B,"StationaryLava")
  , (0x0C,"Sand")
  , (0x0D,"Gravel")
  , (0x0E,"Goldore")
  , (0x0F,"Ironore")
  , (0x10,"Coalore")
  , (0x11,"Wood")
  , (0x12,"Leaves")
  , (0x13,"Sponge")
  , (0x14,"Glass")
  , (0x15,"LapisLazuliOre")
  , (0x16,"LapisLazuliBlock")
  , (0x17,"Dispenser")
  , (0x18,"Sandstone")
  , (0x19,"NoteBlock")
  , (0x23,"Wool")
  , (0x25,"YellowFlower")
  , (0x26,"RedRose")
  , (0x27,"BrownMushroom")
  , (0x28,"RedMushroom")
  , (0x29,"GoldBlock")
  , (0x2A,"IronBlock")
  , (0x2B,"DoubleStoneSlab")
  , (0x2C,"StoneSlab")
  , (0x2D,"Brick")
  , (0x2E,"TNT")
  , (0x2F,"Bookshelf")
  , (0x30,"MossStone")
  , (0x31,"Obsidian")
  , (0x32,"Torch")
  , (0x33,"Fire")
  , (0x34,"MonsterSpawner")
  , (0x35,"WoodenStairs")
  , (0x36,"Chest")
  , (0x37,"RedstoneWire")
  , (0x38,"DiamondOre")
  , (0x39,"DiamondBlock")
  , (0x3A,"Workbench")
  , (0x3B,"Crops")
  , (0x3C,"Soil")
  , (0x3D,"Furnace")
  , (0x3E,"BurningFurnace")
  , (0x3F,"SignPost")
  , (0x40,"WoodenDoor")
  , (0x41,"Ladder")
  , (0x42,"MinecartTracks")
  , (0x43,"CobblestoneStairs")
  , (0x44,"WallSign")
  , (0x45,"Lever")
  , (0x46,"StonePressurePlate")
  , (0x47,"IronDoor")
  , (0x48,"WoodenPressurePlate")
  , (0x49,"RedstoneOre")
  , (0x4A,"GlowingRedstoneOre")
  , (0x4B,"RedstoneTorchOff")
  , (0x4C,"RedstoneTorchOn")
  , (0x4D,"StoneButton")
  , (0x4E,"Snow")
  , (0x4F,"Ice")
  , (0x50,"SnowBlock")
  , (0x51,"Cactus")
  , (0x52,"Clay")
  , (0x53,"SugarCane")
  , (0x54,"Jukebox")
  , (0x55,"Fence")
  , (0x56,"Pumpkin")
  , (0x57,"Netherrack")
  , (0x58,"SoulSand")
  , (0x59,"Glowstone")
  , (0x5A,"Portal")
  , (0x5B,"JackOLantern")
  , (0x5C,"Cake")
  ]


mapchunkDataGet :: Get (Maybe (Int8,Int8,Int8,[BlockId],ByteString,ByteString,ByteString))
mapchunkDataGet =
            do (sx,sy,sz) <- getJ
               let block_count = (fromIntegral sx + 1)
                               * (fromIntegral sy + 1)
                               * (fromIntegral sz + 1)
               len <- getJ :: Get Int32
               compressed <- getLazyByteString (fromIntegral len)
               return $ case safeDecompress compressed of
                 Left _ -> Nothing
                 Right uncompressed ->
                  let parser = (,,,,,,) sx sy sz
                           <$> replicateM (fromIntegral block_count) getJ
                           <*> getLazyByteString (block_count `div` 2)
                           <*> getLazyByteString (block_count `div` 2)
                           <*> getLazyByteString (block_count `div` 2)
                  in Just $ runGet parser uncompressed

-- | Get a lazy ByteString prefixed with a 32-bit length.
getLazyByteString32 :: Get ByteString
getLazyByteString32 = getLazyByteString . fromIntegral =<< getWord32be

putMaybe16 :: (a -> Put) -> Maybe a -> Put
putMaybe16 p Nothing = putJ( -1 :: Int16)
putMaybe16 p (Just x) = p x

getMaybe16 :: Get a -> Get (Maybe a)
getMaybe16 p = do
  mb <- lookAheadM $ isNil <$> getJ
  case mb of
    Nothing -> Just <$> p
    Just () -> return Nothing
  where
  isNil :: Int16 -> Maybe ()
  isNil x = guard (x == (-1))


mapchunkDataPut :: Maybe (Int8,Int8,Int8,[BlockId],ByteString,ByteString,ByteString) -> Put
mapchunkDataPut Nothing = mapchunkDataPut (Just (0,0,0,[Air],L.singleton 0,L.singleton 0,L.singleton 0))
mapchunkDataPut (Just (szx,szy,szz,bs,ms,b,s)) = do
  putJ szx
  putJ szy
  putJ szz
  let compressed = compress $ runPut $ traverse_ putJ bs
                                     *> putLazyByteString ms
                                     *> putLazyByteString b
                                     *> putLazyByteString s
  putWord32be (fromIntegral (L.length compressed))
  putLazyByteString compressed

putChanges :: [((Int8,Int8,Int8),BlockId, Int8)] -> Put
putChanges xs = do
  putWord16be (fromIntegral (length xs))
  let (coords, tys, metas) = unzip3 xs
  traverse_ (putJ . packCoords) coords
  traverse_ putJ tys
  traverse_ putJ metas

  where
  packCoords :: BlockLoc -> Int16
  packCoords (x,y,z) = fromIntegral (fromIntegral x `shiftL` 12
                                 .|. fromIntegral z `shiftL` 8
                                 .|. fromIntegral y :: Word16)

getChanges :: Get [((Int8,Int8,Int8),BlockId, Int8)]
getChanges = do
              sz <- getJ :: Get Int16
              zip3 <$> (replicateM (fromIntegral sz) (splitCoord <$> getJ))
                   <*> replicateM (fromIntegral sz) getJ
                   <*> replicateM (fromIntegral sz) getJ
  where
            splitCoord :: Int16 -> (Int8, Int8, Int8)
            splitCoord c = (fromIntegral (c' `shiftR` 12),
                            fromIntegral (c' .&. 0x7f),
                            fromIntegral (c' `shiftR` 8 .&. 0xf))
               where c' :: Word16
                     c' = fromIntegral c

getCoords :: Get  [(Int8,Int8,Int8)]
getCoords = do len <- getJ :: Get Int32
               replicateM (fromIntegral len) getJ

putCoords :: [(Int8,Int8,Int8)] -> Put
putCoords xs = do
  putWord32be (fromIntegral (length xs))
  traverse_ putJ xs


safeDecompress :: ByteString -> Either String ByteString
safeDecompress
  = ZI.foldDecompressStream (fmap . LI.Chunk) (Right LI.Empty) (\ _ str -> Left str)
  . ZI.decompressWithErrors ZI.zlibFormat ZI.defaultDecompressParams

