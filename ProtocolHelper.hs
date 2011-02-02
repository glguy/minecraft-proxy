{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ProtocolHelper where

import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Array (Array, array, bounds, (!))
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


mapchunkDataGet :: Get (ChunkLoc, Maybe (Array (Int8,Int8,Int8) BlockId,ByteString,ByteString,ByteString))
mapchunkDataGet =
  do (x,y,z,sx,sy,sz) <- getJ :: Get (Int32, Int16, Int32, Int8, Int8, Int8)
     let (chunk,(bx,by,bz)) = decomposeCoords x (fromIntegral y) z
     let block_count = (fromIntegral sx + 1)
                     * (fromIntegral sy + 1)
                     * (fromIntegral sz + 1)
         toArray :: [BlockId] -> Array (Int8,Int8,Int8) BlockId
         toArray xs = array ((bx,by,bz),(bx+sx,by+sy,bz+sz)) (zip (coords bx by bz sx sy sz) xs)
     len <- getJ :: Get Int32
     compressed <- getLazyByteString (fromIntegral len)
     return $! case safeDecompress compressed of
       Left _ -> (chunk,Nothing)
       Right uncompressed ->
         let parser = do (,,,)
                  <$> (toArray <$> replicateM (fromIntegral block_count) getJ)
                  <*> getLazyByteString (block_count `div` 2)
                  <*> getLazyByteString (block_count `div` 2)
                  <*> getLazyByteString (block_count `div` 2)
         in (chunk,Just $ runGet parser uncompressed)

coords bx by bz sx sy sz = do -- The x z y order is intentional
  x' <- take (fromIntegral sx + 1) [fromIntegral bx ..]
  z' <- take (fromIntegral sz + 1) [fromIntegral bz ..]
  y' <- take (fromIntegral sy + 1) [fromIntegral by ..]
  return (x',y',z')


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


mapchunkDataPut :: (ChunkLoc,Maybe (Array (Int8,Int8,Int8) BlockId,ByteString,ByteString,ByteString)) -> Put
mapchunkDataPut ((cx,cz),mbRest) =
  do let (blockArr, metas, blights, slights) = case mbRest of
           Nothing -> error "Can't put bad chunk"
           Just r -> r
     let ((bx,by,bz),(bx',by',bz')) = bounds blockArr
     let x :: Int32
         x = cx `shiftL` 4 .|. fromIntegral bx
         y :: Int16
         y = fromIntegral by
         z :: Int32
         z = cz `shiftL` 4 .|. fromIntegral bz
         sx = (bx' - bx)
         sy = (by' - by)
         sz = (bz' - bz)

     putJ x
     putJ y
     putJ z
     putJ sx
     putJ sy
     putJ sz
     let bs = map (blockArr !) (coords bx by bz sx sy sz)
     let compressed = compress $ runPut $ traverse_ putJ bs
                                       *> putLazyByteString metas
                                       *> putLazyByteString blights
                                       *> putLazyByteString slights
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

-- | 'decomposeCoords' computes the chunk coordinates and the
-- coordinates within that chunk.
decomposeCoords ::
  Int32 {- ^ X -} ->
  Int8  {- ^ Y -} ->
  Int32 {- ^ Z -} ->
  (ChunkLoc, BlockLoc)
decomposeCoords x y z = ((x `shiftR` 4
                        ,z `shiftR` 4)
                        ,(fromIntegral $ x .&. 0xf
                        ,fromIntegral $ y .&. 0x7f
                        ,fromIntegral $ z .&. 0xf)
                        )
