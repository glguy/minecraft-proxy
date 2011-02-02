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

import ProtocolBlocks

type BlockLoc = (Int8, Int8, Int8)

enum "BlockId"
  "UnknownBlock"
  blocks

enum16 "ItemId"
  "OtherItem" $
  map (\(tag,name) -> (tag,"Item"++name)) blocks ++
  [(0x100,"IronShovel")
  ,(0x101,"IronPickaxe")
  ,(0x102,"IronAxe")
  ,(0x103,"FlintAndSteel")
  ,(0x104,"Apple")
  ,(0x105,"Bow")
  ,(0x106,"Arrow")
  ,(0x107,"Coal")
  ,(0x108,"Diamond")
  ,(0x109,"IronIngot")
  ,(0x10A,"GoldIngot")
  ,(0x10B,"IronSword")
  ,(0x10C,"WoodenSword")
  ,(0x10D,"WoodenShovel")
  ,(0x10E,"WoodenPickaxe")
  ,(0x10F,"WoodenAxe")
  ,(0x110,"StoneSword")
  ,(0x111,"StoneShovel")
  ,(0x112,"StonePickaxe")
  ,(0x113,"StoneAxe")
  ,(0x114,"DiamondSword")
  ,(0x115,"DiamondShovel")
  ,(0x116,"DiamondPickaxe")
  ,(0x117,"DiamondAxe")
  ,(0x118,"Stick")
  ,(0x119,"Bowl")
  ,(0x11A,"MushroomSoup")
  ,(0x11B,"GoldSword")
  ,(0x11C,"GoldShovel")
  ,(0x11D,"GoldPickaxe")
  ,(0x11E,"GoldAxe")
  ,(0x11F,"String")
  ,(0x120,"Feather")
  ,(0x121,"Sulphur")
  ,(0x122,"WoodenHoe")
  ,(0x123,"StoneHoe")
  ,(0x124,"IronHoe")
  ,(0x125,"DiamondHoe")
  ,(0x126,"GoldHoe")
  ,(0x127,"Seeds")
  ,(0x128,"Wheat")
  ,(0x129,"Bread")
  ,(0x12A,"LeatherHelmet")
  ,(0x12B,"LeatherChestplate")
  ,(0x12C,"LeatherLeggings")
  ,(0x12D,"LeatherBoots")
  ,(0x12E,"ChainmailHelmet")
  ,(0x12F,"ChainmailChestplate")
  ,(0x130,"ChainmailLeggings")
  ,(0x131,"ChainmailBoots")
  ,(0x132,"IronHelmet")
  ,(0x133,"IronChestplate")
  ,(0x134,"IronLeggings")
  ,(0x135,"IronBoots")
  ,(0x136,"DiamondHelmet")
  ,(0x137,"DiamondChestplate")
  ,(0x138,"DiamondLeggings")
  ,(0x139,"DiamondBoots")
  ,(0x13A,"GoldHelmet")
  ,(0x13B,"GoldChestplate")
  ,(0x13C,"GoldLeggings")
  ,(0x13D,"GoldBoots")
  ,(0x13E,"Flint")
  ,(0x13F,"RawPorkchop")
  ,(0x140,"CookedPorkchop")
  ,(0x141,"Paintings")
  ,(0x142,"GoldenApple")
  ,(0x143,"Sign")
  ,(0x144,"WoodenDoorItem")
  ,(0x145,"Bucket")
  ,(0x146,"WaterBucket")
  ,(0x147,"LavaBucket")
  ,(0x148,"Minecart")
  ,(0x149,"Saddle")
  ,(0x14A,"IronDoorItem")
  ,(0x14B,"Redstone")
  ,(0x14C,"Snowball")
  ,(0x14D,"Boat")
  ,(0x14E,"Leather")
  ,(0x14F,"Milk")
  ,(0x150,"ClayBrick")
  ,(0x151,"ClayBalls")
  ,(0x152,"SugarCaneItem")
  ,(0x153,"Paper")
  ,(0x154,"Book")
  ,(0x155,"Slimeball")
  ,(0x156,"StorageMinecart")
  ,(0x157,"PoweredMinecart")
  ,(0x158,"Egg")
  ,(0x159,"Compass")
  ,(0x15A,"FishingRod")
  ,(0x15B,"Clock")
  ,(0x15C,"GlowstoneDust")
  ,(0x15D,"RawFish")
  ,(0x15E,"CookedFish")
  ,(0x15F,"InkSac")
  ,(0x160,"Bone")
  ,(0x161,"Sugar")
  ,(0x162,"CakeItem")
  ,(0x8D0,"GoldMusicDisc")
  ,(0x8D1,"GreenMusicDisc")
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

