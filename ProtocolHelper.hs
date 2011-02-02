{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProtocolHelper where

import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.Int
import Data.Word
import qualified Codec.Compression.Zlib.Internal as ZI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

import Generator
import JavaBinary
import Protocol.Tables

type BlockLoc = (Int8, Int8, Int8)

type MessageTag = Int8

type ChunkLoc = (Int32, Int32)

newtype EntityId = EID Int32
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype SlotId = SID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype WindowId = WID Int8
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype TransactionId = TID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype ProgressBarId = PID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype GraphicId = GID Int32
  deriving (Eq, Ord, Show,Read,JavaBinary)

enum "InstrumentType"
  "OtherInstrument"
  [ (0, "Harp"       )
  , (1, "DoubleBass" )
  , (2, "SnareDrum"  )
  , (3, "Sticks"     )
  , (4, "BassDrum"   )
  ]

enum "Face"
  "UnknownFace"
  [ (-1,"None")
  , (0,"Y1")
  , (1,"Y2")
  , (2,"Z1")
  , (3,"Z2")
  , (4,"X1")
  , (5,"X2")
  ]

enum "MobId"
  "OtherMob"
  [ (50, "Creeper")
  , (51, "Skeleton")
  , (52, "Spider")
  , (53, "GiantSpider")
  , (54, "Zombie")
  , (55, "Slime")
  , (56, "Ghast")
  , (57, "ZombiePigman")
  , (90, "Pig")
  , (91, "Sheep")
  , (92, "Cow")
  , (93, "Hen")
  , (94, "Squid")
  ]

enum "EntityStatus"
  "OtherStatus"
  [ (2,"Damaged")
  , (3,"Died")
  ]

enum "InventoryType"
  "UnknownInventory"
  [ (0,"BasicInventory")
  , (1,"WorkbenchInventory")
  , (2,"FurnaceInventory")
  , (3,"DispenserInventory")
  ]

enum "Action"
  "ActionOther"
  [ (1,"ActionCrouch")
  , (2,"ActionUncrouch")
  ]

enum "Animate"
  "OtherAnimate"
  [ (0,"NoAnimate")
  , (1,"SwingArm")
  , (2,"DamageAnimation")
  , (104,"Crouch")
  , (105,"Uncrouch")
  ]

enum "DiggingStatus"
  "OtherDigging"
  [ (0,"StartedDigging")
  , (1,"Digging")
  , (2,"StoppedDigging")
  , (3,"BlockBroken")
  , (4,"DropItem")
  ]

enum "PrechunkStatus"
  "OtherPrechunk"
  [ (0,"UnloadChunk")
  , (1,"LoadChunk")
  ]

enum "BlockId" "UnknownBlock" blocks

enum16 "ItemId" "OtherItem"   items


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
putMaybe16 _ Nothing = putJ( -1 :: Int16)
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

