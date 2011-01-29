module GameState where

import Data.Array.IO
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Int
import Data.Word
import Data.Bits
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Protocol
import JavaBinary

type EntityMap = Map EntityId (Either String MobId, Int32, Int32, Int32)

type ChunkLoc = (Int32, Int32)
type BlockLoc = (Int8, Int8, Int8)

type BlockMap  = Map ChunkLoc (IOArray BlockLoc BlockId, IOUArray BlockLoc Word8)

data GameState = GS
  { entityMap :: !EntityMap 
  , blockMap  :: !BlockMap
  , health    :: Int16
  , time      :: Maybe Int64
  , spawnLocation :: Maybe (Int32, Int32, Int32)
  }

newGameState = GS Map.empty Map.empty 19 Nothing Nothing

updateEntityMap :: (EntityMap -> EntityMap)
                -> GameState -> GameState
updateEntityMap f gs = gs { entityMap = f (entityMap gs) }


updateBlockMap :: (BlockMap -> IO BlockMap)
               -> GameState -> IO GameState
updateBlockMap f gs = do
  m <- f $ blockMap gs
  return gs { blockMap = m }


updateHealth :: (Int16 -> Int16)
               -> GameState -> GameState
updateHealth f gs = gs { health = f (health gs) }


updateGameState :: Message -> GameState -> IO (Maybe EntityId, GameState)

updateGameState (NamedEntitySpawn eid name x y z _ _ _) gs
  = return (Just eid, updateEntityMap (Map.insert eid (Left name, x, y, z)) gs)

updateGameState (MobSpawn eid ty x y z _ _ _) gs
  = return (Just eid, updateEntityMap (Map.insert eid (Right ty, x, y, z)) gs)

updateGameState (EntityTeleport eid x y z _ _) gs
  = return (Just eid,
     updateEntityMap (Map.update (\ (ty,_,_,_) -> Just (ty, x, y, z)) eid) gs)

updateGameState (EntityRelativeMove eid dX dY dZ) gs
  = return (Just eid, gs')
  where
   gs' = updateEntityMap (Map.update aux eid) gs
   aux (ty,x,y,z) = x' `seq` y' `seq` z' `seq` Just (ty, x', y', z')
          where x' = x + fromIntegral dX
                y' = y + fromIntegral dY
                z' = z + fromIntegral dZ

updateGameState (EntityLookMove eid dX dY dZ _ _) gs
  = return (Just eid, updateEntityMap (Map.update aux eid) gs)
  where aux (ty,x,y,z) = Just (ty, x + fromIntegral dX,
                                   y + fromIntegral dY,
                                   z + fromIntegral dZ)

updateGameState (DestroyEntity eid) gs
  = return (Just eid, updateEntityMap (Map.delete eid) gs)

updateGameState (UpdateHealth x) gs
  = return (Nothing, gs { health = x })

updateGameState (SpawnPosition x y z) gs
  = return (Nothing, gs { spawnLocation = Just (x,y,z) })

updateGameState (TimeUpdate t) gs
  = return (Nothing, gs { time = Just t })

updateGameState (Mapchunk x y z sx sy sz bs ms b c) gs
  = do gs' <- updateBlockMap (setChunk x y z sx sy sz bs ms) gs
       return (Nothing, gs')

updateGameState (MultiblockChange x z changes) gs
  = do gs' <- updateBlockMap (setBlocks x z changes) gs
       return (Nothing, gs')

updateGameState (Prechunk x z False) gs
  = do gs' <- updateBlockMap (return . Map.delete (x,z)) gs
       return (Nothing, gs')

updateGameState (BlockChange x y z blockid meta) gs
  = do gs' <- updateBlockMap (setBlock x y z blockid meta) gs
       return (Nothing, gs')

updateGameState _ gs = return (Nothing, gs)


decomposeCoords :: Int32 -> Int8 -> Int32 -> ((Int32, Int32), (Int8, Int8, Int8))
decomposeCoords x y z = ((x `shiftR` 4
                        ,z `shiftR` 4)
                        ,(fromIntegral $ x .&. 0xf
                        ,fromIntegral $ y .&. 0x7f
                        ,fromIntegral $ z .&. 0xf)
                        )

setChunk x y z sx sy sz bs ms bm = do
  (blockArray,metaArray,bm') <- case Map.lookup chunk bm of
                 Nothing -> do
                   blockArray <- newArray ((0,0,0),(0xf,0x7f,0xf)) Air
                   metaArray  <- newArray ((0,0,0),(0xf,0x7f,0xf)) (0 :: Word8)
                   return (blockArray, metaArray, Map.insert chunk (blockArray, metaArray) bm)
                 Just (blockArray, metaArray) -> return (blockArray, metaArray, bm)
  
  zipWithM_ (writeArray blockArray) coords bs
  zipWithM_ (writeMetaData metaArray) (pairs coords) (L.unpack ms)
  
  return bm'
  where
  (chunk,(bx,by,bz)) = decomposeCoords x (fromIntegral y) z
  pairs (x:y:z) = (x,y) : pairs z
  pairs _ = []

  writeMetaData arr (x,y) m =  writeArray arr y (m `shiftR` 4)
                            *> writeArray arr x (m .&. 0xf)

  coords = do x <- take (fromIntegral sx + 1) [bx ..]
              z <- take (fromIntegral sz + 1) [bz ..]
              y <- take (fromIntegral sy + 1) [by ..]
              return (x,y,z)


setBlocks x z changes bm = do
  case Map.lookup (x,z) bm of
    Nothing -> return ()
    Just arr -> mapM_ (aux arr) changes
  return bm

  where
  aux (blockArray, metaArray) (coord, ty, meta) = let c = splitCoord coord
                            in writeArray blockArray c ty
                            *> writeArray metaArray c (fromIntegral meta)

  splitCoord :: Int16 -> (Int8, Int8, Int8)
  splitCoord c = (fromIntegral (c' `shiftR` 12), fromIntegral (c' .&. 0x7f), fromIntegral (c' `shiftR` 8 .&. 0xf))
   where c' :: Word16
         c' = fromIntegral c

setBlock :: Int32 -> Int8 -> Int32 -> BlockId -> Int8 -> BlockMap -> IO BlockMap
setBlock x y z blockid meta bm = do
  case Map.lookup chunk bm of
    Nothing -> return ()
    Just (blockArray, metaArray) -> writeArray blockArray block blockid
                                 *> writeArray metaArray  block (fromIntegral meta)
  return bm
  where
  (chunk,block) = decomposeCoords x (fromIntegral y) z
