module GameState where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.Map (Map)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Int
import Data.Bits
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Protocol
import JavaBinary

type EntityMap = Map EntityId (Either String MobId, Int32, Int32, Int32)

type BlockMap  = Map (Int32, Int32, Int32) (ByteString, ByteString)

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


updateBlockMap :: (BlockMap -> BlockMap)
               -> GameState -> GameState
updateBlockMap f gs = gs { blockMap = f (blockMap gs) }


updateHealth :: (Int16 -> Int16)
               -> GameState -> GameState
updateHealth f gs = gs { health = f (health gs) }


updateGameState :: Message -> GameState -> (Maybe EntityId, GameState)

updateGameState (NamedEntitySpawn eid name x y z _ _ _) gs
  = (Just eid, updateEntityMap (Map.insert eid (Left name, x, y, z)) gs)

updateGameState (MobSpawn eid ty x y z _ _ _) gs
  = (Just eid, updateEntityMap (Map.insert eid (Right ty, x, y, z)) gs)

updateGameState (EntityTeleport eid x y z _ _) gs
  = (Just eid,
     updateEntityMap (Map.update (\ (ty,_,_,_) -> Just (ty, x, y, z)) eid) gs)

updateGameState (EntityRelativeMove eid dX dY dZ) gs
  = (Just eid, gs')
  where
   gs' = updateEntityMap (Map.update aux eid) gs
   aux (ty,x,y,z) = x' `seq` y' `seq` z' `seq` Just (ty, x', y', z')
          where x' = x + fromIntegral dX
                y' = y + fromIntegral dY
                z' = z + fromIntegral dZ

updateGameState (EntityLookMove eid dX dY dZ _ _) gs
  = (Just eid, updateEntityMap (Map.update aux eid) gs)
  where aux (ty,x,y,z) = Just (ty, x + fromIntegral dX,
                                   y + fromIntegral dY,
                                   z + fromIntegral dZ)

updateGameState (DestroyEntity eid) gs
  = (Just eid, updateEntityMap (Map.delete eid) gs)

updateGameState (UpdateHealth x) gs
  = (Nothing, gs { health = x })

updateGameState (SpawnPosition x y z) gs
  = (Nothing, gs { spawnLocation = Just (x,y,z) })

updateGameState (TimeUpdate t) gs
  = (Nothing, gs { time = Just t })

{-
updateGameState (Mapchunk x y z sx sy sz bs) gs
  = (Nothing, updateBlockMap (setChunk x y z sx sy sz bs) gs)

updateGameState (MultiblockChange x z changes) gs
  = (Nothing, updateBlockMap (setBlocks x z changes) gs)

updateGameState (BlockChange x y z blockid meta) gs
  = (Nothing, updateBlockMap (setBlock x y z blockid meta) gs)
-}

updateGameState _ gs = (Nothing, gs)


decomposeCoords x y z = (x `shiftR` 4
                        ,y `shiftR` 7
                        ,z `shiftR` 4
                        ,x .&. 0xf
                        ,y .&. 0x7f
                        ,z .&. 0xf
                        )
{-
setBlocks x z changes = Map.update (Just . aux) (x,0,z)
  where
  splitCoord c = (c `shiftR` 12, c .&. 0x7f, (c `shiftR` 8) .&. 0xf)
  aux (bs, ms) = (alterByteString bs $ \ p ->
                  for_ changes $ \ (coord, ty, meta) ->
                  let (x,y,z) = splitCoord coord in
                  pokeElemOff p (fromIntegral $ (x * 16 + z) * 16 + y) (L.head (encode ty))
                 ,alterByteString ms $ \ p ->
                  for_ changes $ \ (coord, ty, meta) ->
                  let (x,y,z) = splitCoord coord in
                  pokeElemOff p (fromIntegral $ (x * 16 + z) * 16 + y) (fromIntegral meta))

setBlock x y z blockid meta = Map.update (Just . aux) (cx,cy,cz)
  where
  (cx,cy,cz,bx,by,bz) = decomposeCoords x (fromIntegral y) z
  blockbyte = L.head $ encode blockid
  aux (a,b) = (updateByteString (bx * 16 * 16 + bz * 16 + by) blockbyte a
              ,updateByteString (bx * 16 * 16 + bz * 16 + by) (fromIntegral meta) b)
-}
