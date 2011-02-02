{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Protocol where

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
import Debug.Trace
import Network.Socket (ServiceName)
import qualified Codec.Compression.Zlib.Internal as ZI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

import JavaBinary
import Generator

defaultMinecraftPort :: ServiceName
defaultMinecraftPort = "25565"


type MessageTag = Int8
type ChunkLoc = (Int32, Int32)
type BlockLoc = (Int8, Int8, Int8)

newtype EntityId = EID Int32
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype SlotId = SID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype WindowId = WID Int8
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype TransactionId = TID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype ItemId = IID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype ProgressBarId = PID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype GraphicId = GID Int32
  deriving (Eq, Ord, Show,Read,JavaBinary)

$(enum "InstrumentType"
  "OtherInstrument"
  [ (0, "Harp"       )
  , (1, "DoubleBass" )
  , (2, "SnareDrum"  )
  , (3, "Sticks"     )
  , (4, "BassDrum"   )
  ])

$(enum "Face"
  "UnknownFace"
  [ (-1,"None")
  , (0,"Y1")
  , (1,"Y2")
  , (2,"Z1")
  , (3,"Z2")
  , (4,"X1")
  , (5,"X2")
  ])

$(enum "MobId"
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
  ])

$(enum "EntityStatus"
  "OtherStatus"
  [ (2,"Damaged")
  , (3,"Died")
  ])

$(enum "InventoryType"
  "UnknownInventory"
  [ (0,"BasicInventory")
  , (1,"WorkbenchInventory")
  , (2,"FurnaceInventory")
  , (3,"DispenserInventory")
  ]) 

$(enum "BlockId"
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
  ])

$(enum "Action"
  "ActionOther"
  [ (1,"ActionCrouch")
  , (2,"ActionUncrouch")
  ])

$(enum "Animate"
  "OtherAnimate"
  [ (0,"NoAnimate")
  , (1,"SwingArm")
  , (2,"DamageAnimation")
  , (104,"Crouch")
  , (105,"Uncrouch")
  ])

$(enum "DiggingStatus"
  "OtherDigging"
  [ (0,"StartedDigging")
  , (1,"Digging")
  , (2,"StoppedDigging")
  , (3,"BlockBroken")
  , (4,"DropItem")
  ])


data Message

  = KeepAliv --  Keep alives are sent every 1200 ticks

  | LoginRequest Int32  --  Protocol Version
                 String --  Username
                 String --  Password
                 Int64  --  Map Seed
                 Int8   --  Dimension

  | Handshake    String --  Session ID

  | Chat         String --  Message

  | TimeUpdate   Int64 --  The world time in minutes

  | EntityEquipment EntityId
                    SlotId
                    ItemId --  Item ID
                    Int16 --  Damage?

  | SpawnPosition Int32 --  X
                  Int32 --  Y
                  Int32 --  Z

  | UseEntity EntityId --  User
              EntityId --  Target
              Bool --  Left-click?

  | UpdateHealth Int16 --  Health: 0--20

  | Respawn

  | Player Bool --  On Ground

  | PlayerPosition Double --  X
                   Double --  Y
                   Double --  Stance
                   Double --  Z
                   Bool   --  On ground

  | PlayerLook Float --  Yaw
               Float --  Pitch
               Bool  --  On Ground

  | PlayerPositionLook
                   Double --  X
                   Double --  Y
                   Double --  Stance
                   Double --  Z
                   Float  --  Yaw
                   Float  --  Pitch
                   Bool   --  On Ground

  | PlayerDigging DiggingStatus
                  Int32 --  X
                  Int8  --  Y
                  Int32 --  Z
                  Face

  | PlayerBlockPlacement
                  Int32 --  X
                  Int8  --  Y
                  Int32 --  Z
                  Face
                  (Maybe (ItemId, Int8, Int16)) --  Optional block, count, and use

  | HoldingChange SlotId

  | Animation EntityId --  Player ID
              Animate

  | EntityAction EntityId --  Player ID
                 Action

  | NamedEntitySpawn EntityId --  Player ID
                     String --  Player Name
                     Int32 --  X
                     Int32 --  Y
                     Int32 --  Z
                     Int8 --  Rotation
                     Int8 --  Pitch
                     ItemId --  Current Item

  | PickupSpawn EntityId --  Player ID
                ItemId
                Int8  --  Count
                Int16  --  Damage?
                Int32 --  X
                Int32 --  Y
                Int32 --  Z
                Int8 --  Rotation
                Int8 --  Pitch
                Int8 --  Roll

  | CollectItem EntityId --  Collected
                EntityId --  Collector

  | AddObject EntityId
              Int8  --  Type ID
              Int32 --  X
              Int32 --  Y
              Int32 --  Z

  | MobSpawn EntityId
             MobId --  Mob ID
             Int32 --  X
             Int32 --  Y
             Int32 --  Z
             Int8  --  Yaw
             Int8  --  Pitch
             Metadata

  | Painting EntityId
             String --  Name of painting
             Int32 --  X
             Int32 --  Y
             Int32 --  Z
             GraphicId

  | EntityVelocity EntityId
                   Int16 --  X Velocity
                   Int16 --  Y Velocity
                   Int16 --  Z Velocity

  | DestroyEntity EntityId

  | Entity EntityId

  | EntityRelativeMove EntityId
                       Int8 --  Change in X
                       Int8 --  Change in Y
                       Int8 --  Change in Z

  | EntityLook EntityId
               Int8 --  Yaw
               Int8 --  Pitch

  | EntityLookMove EntityId
                   Int8  --  dX
                   Int8  --  dY
                   Int8  --  dZ
                   Int8  --  Yaw
                   Int8  --  Pitch

  | EntityTeleport EntityId
                   Int32 --  X
                   Int32 --  Y
                   Int32 --  Z
                   Int8  --  Yaw
                   Int8  --  Pitch

  | EntityStatus EntityId
                 EntityStatus

  | AttachEntity EntityId --  Entity ID
                 EntityId --  Vehicle ID

  | EntityMetadata EntityId Metadata

  | Prechunk ChunkLoc PrechunkStatus

  | Mapchunk Int32 --  X
             Int16 --  Y
             Int32 --  Z
             Int8  --  X length
             Int8  --  Y length
             Int8  --  Z length
             [BlockId]
             ByteString
             ByteString
             ByteString

  | MultiblockChange ChunkLoc
                     [(BlockLoc, BlockId, Int8)] --  Coordinate, Block type, Meta

  | BlockChange Int32 --  Block X
                Int8  --  Block Y
                Int32 --  Block Z
                BlockId
                Int8  --  Block metadata

  | PlayNote Int32 --  Block X
             Int16 --  Block Y
             Int32 --  Block Z
             InstrumentType
             Int8  --  Pitch

  | Explosion Double --  X
              Double --  Y
              Double --  Z
              Float  --  Radius?
              [(Int8,Int8,Int8)] --  Relative X,Y,Z of affected blocks

  | OpenWindow WindowId
               InventoryType
               String --  Title
               Int8 --  Number of slots

  | CloseWindow WindowId

  | WindowClick WindowId
                SlotId
                Bool --  Right-click
                TransactionId
                (Maybe (ItemId, Int8, Int16)) --  Optional Item, Count, Uses

  | SetSlot WindowId
            SlotId
            (Maybe (ItemId, Int8, Int16)) --  Item, Count and Use

  | WindowItems WindowId
                [Maybe (ItemId, Int8, Int16)] --  List of slots (Item ID, Count, Uses)
  | UpdateProgressBar WindowId
                      ProgressBarId
                      Int16 --  Value

  | Transaction WindowId
                TransactionId
                Bool --  Success

  | UpdateSign Int32  --  X
               Int16  --  Y
               Int32  --  Z
               String --  Text on line 1
               String --  Text on line 2
               String --  Text on line 3
               String --  Text on line 4

  | Disconnect String --  Reason

  deriving (Show, Read)

-- | 'AutoGet' provides a short-cut to writing 'getJ' implementations
-- for simple message constructors.
class AutoGet a where
  autoGet :: a -> Get Message

instance AutoGet Message where
  autoGet m = return m

instance (JavaBinary x, AutoGet y) => AutoGet (x -> y) where
  autoGet f = do x <- getJ
                 autoGet (f x)

instance JavaBinary Message where
  getJ = getMessage
  putJ = putMessage

data PrechunkStatus
  = LoadChunk
  | UnloadChunk
  deriving (Read, Show, Eq)

instance JavaBinary PrechunkStatus where
  getJ = do
    tag <- getJ :: Get Int8
    return $! case tag of
      0 -> UnloadChunk
      _ -> LoadChunk
  putJ UnloadChunk = putJ (0 :: Int8)
  putJ LoadChunk   = putJ (1 :: Int8)

data Metadata = Metadata [(Int8, MetadataEntry)]
 deriving (Show, Read)

data MetadataEntry
  = MetadataByte Int8
  | MetadataShort Int16
  | MetadataInt Int32
  | MetadataFloat Float
  | MetadataString String
  | MetadataTriple (Int16, Int8, Int16)
  deriving (Show, Read)

instance JavaBinary Metadata where
  getJ = Metadata <$> aux
    where
    aux =
     do tag <- getJ :: Get Int8
        if tag == 127 then return []
         else do
          let ix = tag .&. 0x1f
          x <- case tag `shiftR` 5 of
                 0 -> MetadataByte   <$> getJ
                 1 -> MetadataShort  <$> getJ
                 2 -> MetadataInt    <$> getJ
                 3 -> MetadataFloat  <$> getJ
                 4 -> MetadataString <$> getJ
                 5 -> MetadataTriple <$> getJ
                 _ -> error $ "Unknown metadata tag " ++ show tag
          xs <- aux
          return ((ix,x) : xs)

  putJ (Metadata xs) = traverse_ aux xs *> putJ (127 :: Int8)
    where putTag fieldType ix = putJ (fieldType `shiftL` 5 .|. ix .&. 0x1f)
          aux (ix, MetadataByte   x) = putTag 0 ix *> putJ x
          aux (ix, MetadataShort  x) = putTag 1 ix *> putJ x
          aux (ix, MetadataInt    x) = putTag 2 ix *> putJ x
          aux (ix, MetadataFloat  x) = putTag 3 ix *> putJ x
          aux (ix, MetadataString x) = putTag 4 ix *> putJ x
          aux (ix, MetadataTriple x) = putTag 5 ix *> putJ x


-- | Get a lazy ByteString prefixed with a 32-bit length.
getLazyByteString32 :: Get ByteString
getLazyByteString32 = getLazyByteString . fromIntegral =<< getWord32be

getMaybe16 :: Get a -> Get (Maybe a)
getMaybe16 p = do
  mb <- lookAheadM $ isNil <$> getJ
  case mb of
    Nothing -> Just <$> p
    Just () -> return Nothing
  where
  isNil :: Int16 -> Maybe ()
  isNil x = guard (x == (-1))



putMaybe16 :: (a -> Put) -> Maybe a -> Put
putMaybe16 = maybe (putJ (-1 :: Int16))


getMessage :: Get Message
getMessage = do
  tag <- getJ
  case tag :: Int8 of
    0x00 -> autoGet KeepAliv
    0x01 -> autoGet LoginRequest
    0x02 -> autoGet Handshake
    0x03 -> autoGet Chat
    0x04 -> autoGet TimeUpdate
    0x05 -> autoGet EntityEquipment
    0x06 -> autoGet SpawnPosition
    0x07 -> autoGet UseEntity
    0x08 -> autoGet UpdateHealth
    0x09 -> autoGet Respawn
    0x0a -> autoGet Player
    0x0b -> autoGet PlayerPosition
    0x0c -> autoGet PlayerLook
    0x0d -> autoGet PlayerPositionLook
    0x0e -> autoGet PlayerDigging
    0x0f -> PlayerBlockPlacement <$> getJ <*> getJ <*> getJ <*> getJ
                                 <*> getMaybe16 getJ
    0x10 -> autoGet HoldingChange
    0x12 -> autoGet Animation
    0x13 -> autoGet EntityAction
    0x14 -> autoGet NamedEntitySpawn
    0x15 -> autoGet PickupSpawn
    0x16 -> autoGet CollectItem
    0x17 -> autoGet AddObject
    0x18 -> autoGet MobSpawn
    0x19 -> autoGet Painting
    0x1c -> autoGet EntityVelocity
    0x1d -> autoGet DestroyEntity
    0x1e -> autoGet Entity
    0x1f -> autoGet EntityRelativeMove
    0x20 -> autoGet EntityLook
    0x21 -> autoGet EntityLookMove
    0x22 -> autoGet EntityTeleport
    0x26 -> autoGet EntityStatus
    0x27 -> autoGet AttachEntity
    0x28 -> autoGet EntityMetadata
    0x32 -> autoGet Prechunk
    0x33 -> do (x,y,z,sx,sy,sz) <- getJ
               let block_count = (fromIntegral sx + 1)
                               * (fromIntegral sy + 1)
                               * (fromIntegral sz + 1)
               len <- getJ :: Get Int32
               compressed <- getLazyByteString (fromIntegral len)
               return $ case safeDecompress compressed of
                 Left _ -> proxyChat "Bad chunk, attempting to continue"
                 Right uncompressed ->
                  let parser = Mapchunk x y z sx sy sz
                           <$> replicateM (fromIntegral block_count) getJ
                           <*> getLazyByteString (block_count `div` 2)
                           <*> getLazyByteString (block_count `div` 2)
                           <*> getLazyByteString (block_count `div` 2)
                  in runGet parser uncompressed

    0x34 -> MultiblockChange <$> getJ <*> changes
      where changes = do
              sz <- getJ :: Get Int16
              zip3 <$> (replicateM (fromIntegral sz) (splitCoord <$> getJ))
                   <*> replicateM (fromIntegral sz) getJ
                   <*> replicateM (fromIntegral sz) getJ
            splitCoord :: Int16 -> (Int8, Int8, Int8)
            splitCoord c = (fromIntegral (c' `shiftR` 12),
                            fromIntegral (c' .&. 0x7f),
                            fromIntegral (c' `shiftR` 8 .&. 0xf))
               where c' :: Word16
                     c' = fromIntegral c
    0x35 -> autoGet BlockChange
    0x36 -> autoGet PlayNote
    0x3c -> Explosion            <$> getJ <*> getJ <*> getJ <*> getJ <*> coords
      where coords = do
              len <- getJ :: Get Int32
              replicateM (fromIntegral len) getJ
    0x64 -> autoGet OpenWindow
    0x65 -> autoGet CloseWindow
    0x66 -> WindowClick          <$> getJ <*> getJ <*> getJ <*> getJ
                                 <*> getMaybe16 getJ
    0x67 -> SetSlot              <$> getJ <*> getJ <*> getMaybe16 getJ
    0x68 -> WindowItems          <$> getJ <*> items
      where items = do
              count <- getJ :: Get Int16
              replicateM (fromIntegral count) (getMaybe16 getJ)
    0x69 -> autoGet UpdateProgressBar
    0x6a -> autoGet Transaction
    0x82 -> autoGet UpdateSign
    0xff -> autoGet Disconnect
    _ -> error $ "Unknown packet " ++ show tag

putMessage :: Message -> Put
putMessage KeepAliv = putJ (0x00 :: MessageTag)
putMessage (LoginRequest ver usr pw x y) = do
  putJ (0x01 :: MessageTag)
  putJ ver
  putJ usr
  putJ pw
  putJ x
  putJ y
putMessage (Handshake usr) = do
  putJ (0x02 :: MessageTag)
  putJ usr
putMessage (Chat msg) = do
  putJ (0x03 :: MessageTag)
  putJ msg
putMessage (TimeUpdate time) = do
  putJ (0x04 :: MessageTag)
  putJ time
putMessage (EntityEquipment eid slot iid damage) = do
  putJ (0x05 :: MessageTag)
  putJ eid
  putJ slot
  putJ iid
  putJ damage

putMessage (SpawnPosition x y z) = do
  putJ (0x06 :: MessageTag)
  putJ x
  putJ y
  putJ z

putMessage (UseEntity user target left) = do
  putJ (0x07 :: MessageTag)
  putJ user
  putJ target
  putJ left

putMessage (UpdateHealth health) = do
  putJ (0x08 :: MessageTag)
  putJ health

putMessage Respawn = putJ (0x09 :: MessageTag)

putMessage (Player ground) = do
  putJ (0x0a :: MessageTag)
  putJ ground

putMessage (PlayerPosition x y stance z ground) = do
  putJ (0x0b :: MessageTag)
  putJ x
  putJ y
  putJ stance
  putJ z
  putJ ground

putMessage (PlayerLook yaw pitch ground) = do
  putJ (0x0c :: MessageTag)
  putJ yaw
  putJ pitch
  putJ ground

putMessage (PlayerPositionLook x stance y z yaw pitch ground) = do
  putJ (0x0d :: MessageTag)
  putJ x
  putJ stance
  putJ y
  putJ z
  putJ yaw
  putJ pitch
  putJ ground

putMessage (PlayerDigging status x y z face) = do
  putJ (0x0e :: MessageTag)
  putJ status
  putJ x
  putJ y
  putJ z
  putJ face

putMessage (PlayerBlockPlacement x y z face mbstuff) = do
  putJ (0x0f :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ face
  putMaybe16 putJ mbstuff

putMessage (HoldingChange slot) = do
  putJ (0x10 :: MessageTag)
  putJ slot

putMessage (Animation pid ani) = do
  putJ (0x12 :: MessageTag)
  putJ pid
  putJ ani

putMessage (EntityAction eid act) = do
  putJ (0x13 :: MessageTag)
  putJ eid
  putJ act

putMessage (NamedEntitySpawn eid name x y z rot pitch item) = do
  putJ (0x14 :: MessageTag)
  putJ eid
  putJ name
  putJ x
  putJ y
  putJ z
  putJ rot
  putJ pitch
  putJ item

putMessage (PickupSpawn eid iid cnt dmg x y z rot pitch roll) = do
  putJ (0x15 :: MessageTag)
  putJ eid
  putJ iid
  putJ cnt
  putJ dmg
  putJ x
  putJ y
  putJ z
  putJ rot
  putJ pitch
  putJ roll

putMessage (CollectItem collected collector) = do
  putJ (0x16 :: MessageTag)
  putJ collected
  putJ collector

putMessage (AddObject eid ty x y z) = do
  putJ (0x17 :: MessageTag)
  putJ eid
  putJ ty
  putJ x
  putJ y
  putJ z

putMessage (MobSpawn eid ty x y z yaw pitch meta) = do
  putJ (0x18 :: MessageTag)
  putJ eid
  putJ ty
  putJ x
  putJ y
  putJ z
  putJ yaw
  putJ pitch
  putJ meta

putMessage (Painting eid title x y z ty) = do
  putJ (0x19 :: MessageTag)
  putJ eid
  putJ title
  putJ x
  putJ y
  putJ z
  putJ ty

putMessage (EntityVelocity eid x y z) = do
  putJ (0x1c :: MessageTag)
  putJ eid
  putJ x
  putJ y
  putJ z

putMessage (DestroyEntity eid) = do
  putJ (0x1d :: MessageTag)
  putJ eid

putMessage (Entity eid) = do
  putJ (0x1e :: MessageTag)
  putJ eid

putMessage (EntityRelativeMove eid x y z) = do
  putJ (0x1f :: MessageTag)
  putJ eid
  putJ x
  putJ y
  putJ z


putMessage (EntityLook eid yaw pitch) = do
  putJ (0x20 :: MessageTag)
  putJ eid
  putJ yaw
  putJ pitch

putMessage (EntityLookMove eid x y z yaw pitch) = do
  putJ (0x21 :: MessageTag)
  putJ eid
  putJ x
  putJ y
  putJ z
  putJ yaw
  putJ pitch

putMessage (EntityTeleport eid x y z yaw pitch) = do
  putJ (0x22 :: MessageTag)
  putJ eid
  putJ x
  putJ y
  putJ z
  putJ yaw
  putJ pitch

putMessage (EntityStatus eid status) = do
  putJ (0x26 :: MessageTag)
  putJ eid
  putJ status

putMessage (AttachEntity eid vid) = do
  putJ (0x27 :: MessageTag)
  putJ eid
  putJ vid

putMessage (EntityMetadata eid meta) = do
  putJ (0x28 :: MessageTag)
  putJ eid
  putJ meta

putMessage (Prechunk chunk mode) = do
  putJ (0x32 :: MessageTag)
  putJ chunk
  putJ mode

putMessage (Mapchunk x y z szx szy szz bs ms b s) = do
  putJ (0x33 :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ szx
  putJ szy
  putJ szz
  let compressed = compress $ runPut $ traverse_ putJ bs
                                     *> putLazyByteString ms
                                     *> putLazyByteString b
                                     *> putLazyByteString s
  putWord32be (fromIntegral (L.length compressed))
  putLazyByteString compressed


putMessage (MultiblockChange chunkLoc xs) = do
  putJ (0x34 :: MessageTag)
  putJ chunkLoc
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

putMessage (BlockChange x y z ty meta) = do
  putJ (0x35 :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ ty
  putJ meta

putMessage (PlayNote x y z ty pitch) = do
  putJ (0x36 :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ ty
  putJ pitch

putMessage (Explosion x y z r xs) = do
  putJ (0x3c :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ r
  putWord32be (fromIntegral (length xs))
  traverse_ putJ xs

putMessage (OpenWindow wid ty title slots) = do
  putJ (0x64 :: MessageTag)
  putJ wid
  putJ ty
  putJ title
  putJ slots

putMessage (CloseWindow wid) = do
  putJ (0x65 :: MessageTag)
  putJ wid

putMessage (WindowClick win slot right act mbstuff) = do
  putJ (0x66 :: MessageTag)
  putJ win
  putJ slot
  putJ right
  putJ act
  putMaybe16 putJ mbstuff

putMessage (SetSlot win slot mbstuff) = do
  putJ (0x67 :: MessageTag)
  putJ win
  putJ slot
  putMaybe16 putJ mbstuff

putMessage (WindowItems win xs) = do
  putJ (0x68 :: MessageTag)
  putJ win
  putWord16be (fromIntegral (length xs))
  traverse_ (putMaybe16 putJ) xs

putMessage (UpdateProgressBar win bar val) = do
  putJ (0x69 :: MessageTag)
  putJ win
  putJ bar
  putJ val

putMessage (Transaction win act accept) = do
  putJ (0x6a :: MessageTag)
  putJ win
  putJ act
  putJ accept

putMessage (UpdateSign x y z tx1 tx2 tx3 tx4) = do
  putJ (0x82 :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ tx1
  putJ tx2
  putJ tx3
  putJ tx4

putMessage (Disconnect reason) = do
  putJ (0xff :: MessageTag)
  putJ reason

toMessages :: ByteString -> [Message]
toMessages bs = msg : toMessages rest
  where
  (msg, rest) = runGet (liftA2 (,) getJ getRemainingLazyByteString) bs

safeDecompress :: ByteString -> Either String ByteString
safeDecompress
  = ZI.foldDecompressStream (fmap . LI.Chunk) (Right LI.Empty) (\ _ str -> Left str)
  . ZI.decompressWithErrors ZI.zlibFormat ZI.defaultDecompressParams

proxyChat :: String -> Message
proxyChat text = Chat $ "\194\167\&6" ++ text

highlight :: String -> String
highlight text = "\194\167\&b" ++ text

