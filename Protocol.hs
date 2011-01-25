{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Protocol where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Int
import Data.Foldable
import Data.Traversable
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import JavaBinary

type MessageTag = Int8

newtype EntityId = EID Int32
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype SlotId = SID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype WindowId = WID Int8
  deriving (Eq, Ord, Show,Read,JavaBinary)

newtype TransactionId = TID Int16
  deriving (Eq, Ord, Show,Read,JavaBinary)

data Message

  = KeepAliv -- ^ Keep alives are sent every 1200 ticks

  | LoginRequest Int32  -- ^ Protocol Version
                 String -- ^ Username
                 String -- ^ Password
                 Int64  -- ^ Map Seed
                 Int8   -- ^ Dimension

  | Handshake    String -- ^ Username

  | Chat         String -- ^ Message

  | TimeUpdate   Int64 -- ^ The world time in minutes

  | EntityEquipment EntityId
                    SlotId
                    Int16 -- ^ Item ID
                    Int16 -- ^ Damage?

  | SpawnPosition Int32 -- ^ X
                  Int32 -- ^ Y
                  Int32 -- ^ Z

  | UseEntity EntityId -- ^ User
              EntityId -- ^ Target
              Bool -- ^ Left-click?

  | UpdateHealth Int16 -- ^ Health: 0--20

  | Respawn

  | Player Bool -- ^ On Ground

  | PlayerPosition Double -- ^ X
                   Double -- ^ Y
                   Double -- ^ Stance
                   Double -- ^ Z
                   Bool   -- ^ On ground

  | PlayerLook Float -- ^ Yaw
               Float -- ^ Pitch
               Bool -- ^ On Ground

  | PlayerPositionLook
                   Double -- ^ X
                   Double -- ^ Y
                   Double -- ^ Stance
                   Double -- ^ Z
                   Float -- ^ Yaw
                   Float -- ^ Pitch
                   Bool -- ^ On Ground

  | PlayerDigging DiggingStatus
                  Int32 -- ^ X
                  Int8 -- ^ Y
                  Int32 -- ^ Z
                  Face

  | PlayerBlockPlacement
                  Int32 -- ^ X
                  Int8 -- ^ Y
                  Int32 -- ^ Z
                  Face
                  (Maybe (Int16, Int8, Int16)) -- ^ Optional block, count, and use

  | HoldingChange Int16 -- ^ Slot

  | Animation EntityId -- ^ Player ID
              Animate

  | EntityAction EntityId -- ^ Player ID
                 Action

  | NamedEntitySpawn EntityId -- ^ Player ID
                     String -- ^ Player Name
                     Int32 -- ^ X
                     Int32 -- ^ Y
                     Int32 -- ^ Z
                     Int8 -- ^ Rotation
                     Int8 -- ^ Pitch
                     Int16 -- ^ Current Item

  | PickupSpawn EntityId -- ^ Player ID
                Int16 -- ^ Item ID
                Int8  -- ^ Count
                Int16  -- ^ Damage?
                Int32 -- ^ X
                Int32 -- ^ Y
                Int32 -- ^ Z
                Int8 -- ^ Rotation
                Int8 -- ^ Pitch
                Int8 -- ^ Roll

  | CollectItem EntityId -- ^ Collected
                EntityId -- ^ Collector

  | AddObject EntityId
              Int8  -- ^ Type ID
              Int32 -- ^ X
              Int32 -- ^ Y
              Int32 -- ^ Z

  | MobSpawn EntityId
             MobId -- ^ Mob ID
             Int32 -- ^ X
             Int32 -- ^ Y
             Int32 -- ^ Z
             Int8  -- ^ Yaw
             Int8  -- ^ Pitch
             Metadata

  | Painting EntityId
             String -- ^ Name of painting
             Int32 -- ^ X
             Int32 -- ^ Y
             Int32 -- ^ Z
             Int32 -- ^ Graphic ID

  | EntityVelocity EntityId
                   Int16 -- ^ X Velocity
                   Int16 -- ^ Y Velocity
                   Int16 -- ^ Z Velocity

  | DestroyEntity EntityId

  | Entity EntityId

  | EntityRelativeMove EntityId
                       Int8 -- ^ Change in X
                       Int8 -- ^ Change in Y
                       Int8 -- ^ Change in Z

  | EntityLook EntityId
               Int8 -- ^ Yaw
               Int8 -- ^ Pitch

  | EntityLookMove EntityId
                   Int8  -- ^ dX
                   Int8  -- ^ dY
                   Int8  -- ^ dZ
                   Int8  -- ^ Yaw
                   Int8  -- ^ Pitch

  | EntityTeleport EntityId
                   Int32 -- ^ X
                   Int32 -- ^ Y
                   Int32 -- ^ Z
                   Int8  -- ^ Yaw
                   Int8  -- ^ Pitch

  | EntityStatus EntityId
                 Int8 -- ^ Status

  | AttachEntity EntityId -- ^ Entity ID
                 EntityId -- ^ Vehicle ID

  | EntityMetadata EntityId Metadata

  | Prechunk Int32 -- ^ X
             Int32 -- ^ Z
             Bool  -- ^ Load on True, Unload on False

  | Mapchunk Int32 -- ^ X
             Int16 -- ^ Y
             Int32 -- ^ Z
             Int8  -- ^ X length
             Int8  -- ^ Y length
             Int8  -- ^ Z length
             Data.ByteString.Lazy.ByteString -- ^ ZLib Deflate compressed data

  | MultiblockChange Int32 -- ^ Chunk X
                     Int32 -- ^ Chunk Z
                     [(Int16, BlockId, Int8)] -- ^ Coordinate, Block type, Meta

  | BlockChange Int32 -- ^ Block X
                Int8  -- ^ Block Y
                Int32 -- ^ Block Z
                BlockId
                Int8  -- ^ Block metadata

  | PlayNote Int32 -- ^ Block X
             Int16 -- ^ Block Y
             Int32 -- ^ Block Z
             InstrumentType
             Int8  -- ^ Pitch

  | Explosion Double -- ^ X
              Double -- ^ Y
              Double -- ^ Z
              Float  -- ^ Radius?
              [(Int8,Int8,Int8)] -- ^ Relative X,Y,Z of affected blocks

  | OpenWindow WindowId
               InventoryType
               String -- ^ Title
               Int8 -- ^ Number of slots

  | CloseWindow WindowId

  | WindowClick WindowId
                SlotId
                Bool -- ^ Right-click
                TransactionId
                (Maybe (Int16, Int8, Int16)) -- ^ Optional Item, Count, Uses

  | SetSlot WindowId
            SlotId
            (Maybe (Int16, Int8, Int16)) -- ^ Item, Count and Use

  | WindowItems WindowId
                [Maybe (Int16, Int8, Int16)] -- ^ List of slots (Item ID, Count, Uses)
  | UpdateProgressBar WindowId
                      Int16 -- ^ Progress bar ID
                      Int16 -- ^ Value

  | Transaction WindowId
                TransactionId
                Bool -- ^ Success

  | UpdateSign Int32 -- ^ X
               Int16 -- ^ Y
               Int32 -- ^ Z
               String -- ^ Text on line 1
               String -- ^ Text on line 2
               String -- ^ Text on line 3
               String -- ^ Text on line 4

  | Disconnect   String -- ^ Reason

  deriving (Show, Read)

class AutoGet a where
  autoGet :: a -> Get Message

instance AutoGet Message where
  autoGet m = return m

instance (JavaBinary x, AutoGet y) => AutoGet (x -> y) where
  autoGet f = autoGet . f =<< getJ

instance JavaBinary Message where
  getJ = getMessage
  putJ = putMessage

data InstrumentType
  = DoubleBass
  | SnareDrum
  | Sticks
  | BassDrum
  | Harp
  | OtherInstrument Int8
  deriving (Show, Read)

instance JavaBinary InstrumentType where
  getJ = fmap (\tag -> case tag of
      1 -> DoubleBass
      2 -> SnareDrum
      3 -> Sticks
      4 -> BassDrum
      5 -> Harp
      _ -> OtherInstrument tag
    ) getJ

  putJ DoubleBass            = putJ (1 :: Int8)
  putJ SnareDrum             = putJ (2 :: Int8)
  putJ Sticks                = putJ (3 :: Int8)
  putJ BassDrum              = putJ (4 :: Int8)
  putJ Harp                  = putJ (5 :: Int8)
  putJ (OtherInstrument tag) = putJ tag

data InventoryType
  = BasicInventory
  | WorkbenchInventory
  | FurnaceInventory
  | DispenserInventory
  deriving (Show, Read)

instance JavaBinary InventoryType where
  getJ = do
    tag <- getJ
    case tag :: Int8 of
      0 -> return BasicInventory
      1 -> return WorkbenchInventory
      2 -> return FurnaceInventory
      3 -> return DispenserInventory
      _ -> error ("Unknown inventory " ++ show tag)

  putJ BasicInventory     = putJ (0 :: Int8)
  putJ WorkbenchInventory = putJ (1 :: Int8)
  putJ FurnaceInventory   = putJ (2 :: Int8)
  putJ DispenserInventory = putJ (3 :: Int8)


data MobId
  = Creeper
  | Skeleton
  | Spider
  | GiantSpider
  | Zombie
  | Slime
  | Ghast
  | ZombiePigman
  | Pig
  | Sheep
  | Cow
  | Hen
  | OtherMob Int8
  deriving (Show, Read, Eq)

instance JavaBinary MobId where

  getJ = do
    tag <- getJ
    case tag of 
      50 -> return Creeper
      51 -> return Skeleton
      52 -> return Spider
      53 -> return GiantSpider
      54 -> return Zombie
      55 -> return Slime
      56 -> return Ghast
      57 -> return ZombiePigman
      90 -> return Pig
      91 -> return Sheep
      92 -> return Cow
      93 -> return Hen
      _  -> return $ OtherMob tag
  putJ Creeper        = putJ (50 :: Int8)
  putJ Skeleton       = putJ (51 :: Int8)
  putJ Spider         = putJ (52 :: Int8)
  putJ GiantSpider    = putJ (53 :: Int8)
  putJ Zombie         = putJ (54 :: Int8)
  putJ Slime          = putJ (55 :: Int8)
  putJ Ghast          = putJ (56 :: Int8)
  putJ ZombiePigman   = putJ (57 :: Int8)
  putJ Pig            = putJ (90 :: Int8)
  putJ Sheep          = putJ (91 :: Int8)
  putJ Cow            = putJ (92 :: Int8)
  putJ Hen            = putJ (93 :: Int8)
  putJ (OtherMob tag) = putJ (tag :: Int8)

data BlockId
  = Air
  | Stone
  | Grass
  | Dirt
  | Cobblestone
  | WoodenPlank
  | Sapling
  | Bedrock
  | Water
  | StationaryWater
  | Lava
  | StationaryLava
  | Sand
  | Gravel
  | Goldore
  | Ironore
  | Coalore
  | Wood
  | Leaves
  | Sponge
  | Glass
  | LapisLazuliOre
  | LapisLazuliBlock
  | Dispenser
  | Sandstone
  | NoteBlock
  | Wool
  | YellowFlower
  | RedRose
  | BrownMushroom
  | RedMushroom
  | GoldBlock
  | IronBlock
  | DoubleStoneSlab
  | StoneSlab
  | Brick
  | TNT
  | Bookshelf
  | MossStone
  | Obsidian
  | Torch
  | Fire
  | MonsterSpawner
  | WoodenStairs
  | Chest
  | RedstoneWire
  | DiamondOre
  | DiamondBlock
  | Workbench
  | Crops
  | Soil
  | Furnace
  | BurningFurnace
  | SignPost
  | WoodenDoor
  | Ladder
  | MinecartTracks
  | CobblestoneStairs
  | WallSign
  | Lever
  | StonePressurePlate
  | IronDoor
  | WoodenPressurePlate
  | RedstoneOre
  | GlowingRedstoneOre
  | RedstoneTorchOff
  | RedstoneTorchOn
  | StoneButton
  | Snow
  | Ice
  | SnowBlock
  | Cactus
  | Clay
  | SugarCane
  | Jukebox
  | Fence
  | Pumpkin
  | Netherrack
  | SoulSand
  | Glowstone
  | Portal
  | JackOLantern
  | Cake
     deriving (Show, Read)

instance JavaBinary BlockId where
  getJ = do
    tag <- getJ
    case tag :: Int8 of
      0x00 -> return Air
      0x01 -> return Stone
      0x02 -> return Grass
      0x03 -> return Dirt
      0x04 -> return Cobblestone
      0x05 -> return WoodenPlank
      0x06 -> return Sapling
      0x07 -> return Bedrock
      0x08 -> return Water
      0x09 -> return StationaryWater
      0x0A -> return Lava
      0x0B -> return StationaryLava
      0x0C -> return Sand
      0x0D -> return Gravel
      0x0E -> return Goldore
      0x0F -> return Ironore
      0x10 -> return Coalore
      0x11 -> return Wood
      0x12 -> return Leaves
      0x13 -> return Sponge
      0x14 -> return Glass
      0x15 -> return LapisLazuliOre
      0x16 -> return LapisLazuliBlock
      0x17 -> return Dispenser
      0x18 -> return Sandstone
      0x19 -> return NoteBlock
      0x23 -> return Wool
      0x25 -> return YellowFlower
      0x26 -> return RedRose
      0x27 -> return BrownMushroom
      0x28 -> return RedMushroom
      0x29 -> return GoldBlock
      0x2A -> return IronBlock
      0x2B -> return DoubleStoneSlab
      0x2C -> return StoneSlab
      0x2D -> return Brick
      0x2E -> return TNT
      0x2F -> return Bookshelf
      0x30 -> return MossStone
      0x31 -> return Obsidian
      0x32 -> return Torch
      0x33 -> return Fire
      0x34 -> return MonsterSpawner
      0x35 -> return WoodenStairs
      0x36 -> return Chest
      0x37 -> return RedstoneWire
      0x38 -> return DiamondOre
      0x39 -> return DiamondBlock
      0x3A -> return Workbench
      0x3B -> return Crops
      0x3C -> return Soil
      0x3D -> return Furnace
      0x3E -> return BurningFurnace
      0x3F -> return SignPost
      0x40 -> return WoodenDoor
      0x41 -> return Ladder
      0x42 -> return MinecartTracks
      0x43 -> return CobblestoneStairs
      0x44 -> return WallSign
      0x45 -> return Lever
      0x46 -> return StonePressurePlate
      0x47 -> return IronDoor
      0x48 -> return WoodenPressurePlate
      0x49 -> return RedstoneOre
      0x4A -> return GlowingRedstoneOre
      0x4B -> return RedstoneTorchOff
      0x4C -> return RedstoneTorchOn
      0x4D -> return StoneButton
      0x4E -> return Snow
      0x4F -> return Ice
      0x50 -> return SnowBlock
      0x51 -> return Cactus
      0x52 -> return Clay
      0x53 -> return SugarCane
      0x54 -> return Jukebox
      0x55 -> return Fence
      0x56 -> return Pumpkin
      0x57 -> return Netherrack
      0x58 -> return SoulSand
      0x59 -> return Glowstone
      0x5A -> return Portal
      0x5B -> return JackOLantern
      0x5C -> return Cake
      _ -> error ("block id " ++ show tag)

  putJ Air                = putJ (0x00 :: Int8)
  putJ Stone              = putJ (0x01 :: Int8)
  putJ Grass              = putJ (0x02 :: Int8)
  putJ Dirt               = putJ (0x03 :: Int8)
  putJ Cobblestone        = putJ (0x04 :: Int8)
  putJ WoodenPlank        = putJ (0x05 :: Int8)
  putJ Sapling            = putJ (0x06 :: Int8)
  putJ Bedrock            = putJ (0x07 :: Int8)
  putJ Water              = putJ (0x08 :: Int8)
  putJ StationaryWater    = putJ (0x09 :: Int8)
  putJ Lava               = putJ (0x0A :: Int8)
  putJ StationaryLava     = putJ (0x0B :: Int8)
  putJ Sand               = putJ (0x0C :: Int8)
  putJ Gravel             = putJ (0x0D :: Int8)
  putJ Goldore            = putJ (0x0E :: Int8)
  putJ Ironore            = putJ (0x0F :: Int8)
  putJ Coalore            = putJ (0x10 :: Int8)
  putJ Wood               = putJ (0x11 :: Int8)
  putJ Leaves             = putJ (0x12 :: Int8)
  putJ Sponge             = putJ (0x13 :: Int8)
  putJ Glass              = putJ (0x14 :: Int8)
  putJ LapisLazuliOre     = putJ (0x15 :: Int8)
  putJ LapisLazuliBlock   = putJ (0x16 :: Int8)
  putJ Dispenser          = putJ (0x17 :: Int8)
  putJ Sandstone          = putJ (0x18 :: Int8)
  putJ NoteBlock          = putJ (0x19 :: Int8)
  putJ Wool               = putJ (0x23 :: Int8)
  putJ YellowFlower       = putJ (0x25 :: Int8)
  putJ RedRose            = putJ (0x26 :: Int8)
  putJ BrownMushroom      = putJ (0x27 :: Int8)
  putJ RedMushroom        = putJ (0x28 :: Int8)
  putJ GoldBlock          = putJ (0x29 :: Int8)
  putJ IronBlock          = putJ (0x2A :: Int8)
  putJ DoubleStoneSlab    = putJ (0x2B :: Int8)
  putJ StoneSlab          = putJ (0x2C :: Int8)
  putJ Brick              = putJ (0x2D :: Int8)
  putJ TNT                = putJ (0x2E :: Int8)
  putJ Bookshelf          = putJ (0x2F :: Int8)
  putJ MossStone          = putJ (0x30 :: Int8)
  putJ Obsidian           = putJ (0x31 :: Int8)
  putJ Torch              = putJ (0x32 :: Int8)
  putJ Fire               = putJ (0x33 :: Int8)
  putJ MonsterSpawner     = putJ (0x34 :: Int8)
  putJ WoodenStairs       = putJ (0x35 :: Int8)
  putJ Chest              = putJ (0x36 :: Int8)
  putJ RedstoneWire       = putJ (0x37 :: Int8)
  putJ DiamondOre         = putJ (0x38 :: Int8)
  putJ DiamondBlock       = putJ (0x39 :: Int8)
  putJ Workbench          = putJ (0x3A :: Int8)
  putJ Crops              = putJ (0x3B :: Int8)
  putJ Soil               = putJ (0x3C :: Int8)
  putJ Furnace            = putJ (0x3D :: Int8)
  putJ BurningFurnace     = putJ (0x3E :: Int8)
  putJ SignPost           = putJ (0x3F :: Int8)
  putJ WoodenDoor         = putJ (0x40 :: Int8)
  putJ Ladder             = putJ (0x41 :: Int8)
  putJ MinecartTracks     = putJ (0x42 :: Int8)
  putJ CobblestoneStairs  = putJ (0x43 :: Int8)
  putJ WallSign           = putJ (0x44 :: Int8)
  putJ Lever              = putJ (0x45 :: Int8)
  putJ StonePressurePlate = putJ (0x46 :: Int8)
  putJ IronDoor           = putJ (0x47 :: Int8)
  putJ WoodenPressurePlate= putJ (0x48 :: Int8)
  putJ RedstoneOre        = putJ (0x49 :: Int8)
  putJ GlowingRedstoneOre = putJ (0x4A :: Int8)
  putJ RedstoneTorchOff   = putJ (0x4B :: Int8)
  putJ RedstoneTorchOn    = putJ (0x4C :: Int8)
  putJ StoneButton        = putJ (0x4D :: Int8)
  putJ Snow               = putJ (0x4E :: Int8)
  putJ Ice                = putJ (0x4F :: Int8)
  putJ SnowBlock          = putJ (0x50 :: Int8)
  putJ Cactus             = putJ (0x51 :: Int8)
  putJ Clay               = putJ (0x52 :: Int8)
  putJ SugarCane          = putJ (0x53 :: Int8)
  putJ Jukebox            = putJ (0x54 :: Int8)
  putJ Fence              = putJ (0x55 :: Int8)
  putJ Pumpkin            = putJ (0x56 :: Int8)
  putJ Netherrack         = putJ (0x57 :: Int8)
  putJ SoulSand           = putJ (0x58 :: Int8)
  putJ Glowstone          = putJ (0x59 :: Int8)
  putJ Portal             = putJ (0x5A :: Int8)
  putJ JackOLantern       = putJ (0x5B :: Int8)
  putJ Cake               = putJ (0x5C :: Int8)

data Metadata = Metadata [MetadataEntry]
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
    where aux = do
            tag <- getJ :: Get Int8
            if tag == 127 then return []
             else (:) <$> case tag `shiftR` 5 of
                            0 -> MetadataByte   <$> getJ
                            1 -> MetadataShort  <$> getJ
                            2 -> MetadataInt    <$> getJ
                            3 -> MetadataFloat  <$> getJ
                            4 -> MetadataString <$> getJ
                            5 -> MetadataTriple <$> getJ
                            _ -> error $ "Unknown metadata tag " ++ show tag
                      <*> aux
  putJ (Metadata xs) = traverse aux xs *> putJ (127 :: Int8)
    where aux (MetadataByte   x) = putJ (0 :: Int8) *> putJ x
          aux (MetadataShort  x) = putJ (1 :: Int8) *> putJ x
          aux (MetadataInt    x) = putJ (2 :: Int8) *> putJ x
          aux (MetadataFloat  x) = putJ (3 :: Int8) *> putJ x
          aux (MetadataString x) = putJ (4 :: Int8) *> putJ x
          aux (MetadataTriple x) = putJ (5 :: Int8) *> putJ x

data Action
  = ActionCrouch
  | ActionUncrouch
  | ActionOther Int8
  deriving (Show, Read)

instance JavaBinary Action where
  getJ = do
    tag <- getJ
    case tag of
      1 -> return ActionCrouch
      2 -> return ActionUncrouch
      _ -> return $ ActionOther tag
  putJ ActionCrouch      = putJ (1 :: Int8)
  putJ ActionUncrouch    = putJ (2 :: Int8)
  putJ (ActionOther tag) = putJ tag

data Animate
  = NoAnimate
  | SwingArm
  | DamageAnimation
  | Crouch
  | Uncrouch
  | OtherAnimate Int8
  deriving (Show, Read)

instance JavaBinary Animate where
  getJ = do
    tag <- getJ
    case tag of
      0   -> return NoAnimate
      1   -> return SwingArm
      2   -> return DamageAnimation
      104 -> return Crouch
      105 -> return Uncrouch
      _   -> return $ OtherAnimate tag
  putJ NoAnimate          = putJ (0 :: Int8)
  putJ SwingArm           = putJ (1 :: Int8)
  putJ DamageAnimation    = putJ (2 :: Int8)
  putJ Crouch             = putJ (104 :: Int8)
  putJ Uncrouch           = putJ (105 :: Int8)
  putJ (OtherAnimate tag) = putJ (tag :: Int8)

data DiggingStatus
  = StartedDigging
  | Digging
  | StoppedDigging
  | BlockBroken
  | DropItem
  deriving (Show, Read)

instance JavaBinary DiggingStatus where
  getJ = do
    tag <- getJ
    case tag :: Int8 of
      0 -> return StartedDigging
      1 -> return Digging
      2 -> return StoppedDigging
      3 -> return BlockBroken
      4 -> return DropItem
      _ -> error $ "unknown digging status " ++ show tag
  putJ StartedDigging = putJ (0 :: Int8)
  putJ Digging        = putJ (1 :: Int8)
  putJ StoppedDigging = putJ (2 :: Int8)
  putJ BlockBroken    = putJ (3 :: Int8)
  putJ DropItem       = putJ (4 :: Int8)

data Face
  = Y1 | Y2 | Z1 | Z2 | X1 | X2 | None
  deriving (Show, Read)

instance JavaBinary Face where
  getJ = do
    tag <- getJ
    case tag :: Int8 of
      -1 -> return None
      0 -> return Y1
      1 -> return Y2
      2 -> return Z1
      3 -> return Z2
      4 -> return X1
      5 -> return X2
      _ -> error ("Bad face? " ++ show tag)
  putJ None = putJ (-1 :: Int8)
  putJ Y1   = putJ (0  :: Int8)
  putJ Y2   = putJ (1  :: Int8)
  putJ Z1   = putJ (2  :: Int8)
  putJ Z2   = putJ (3  :: Int8)
  putJ X1   = putJ (4  :: Int8)
  putJ X2   = putJ (5  :: Int8)

-- | Get a lazy ByteString prefixed with a 32-bit length.
getLazyByteString32 :: Get Data.ByteString.Lazy.ByteString
getLazyByteString32 = getLazyByteString . fromIntegral =<< getWord32be

getItemTriple :: Get (Maybe (Int16, Int8, Int16))
getItemTriple = do
  itemid <- getJ
  if itemid == (-1) then return Nothing
    else Just <$> ((itemid,,) <$> getJ <*> getJ)

putItemTriple :: Maybe (Int16, Int8, Int16) -> Put
putItemTriple Nothing = putJ (-1 :: Int16)
putItemTriple (Just x) = putJ x


getMessage :: Get Message
getMessage = do
  tag <- getJ
  case tag :: Int8 of
    0x00 -> return KeepAliv
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
                                 <*> getItemTriple
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
    0x33 -> Mapchunk             <$> getJ <*> getJ <*> getJ <*> getJ <*> getJ
                                 <*> getJ <*> getLazyByteString32
    0x34 -> MultiblockChange <$> getJ <*> getJ <*> changes
      where changes = do
              sz <- getJ :: Get Int16
              zip3 <$> replicateM (fromIntegral sz) getJ
                   <*> replicateM (fromIntegral sz) getJ
                   <*> replicateM (fromIntegral sz) getJ
    0x35 -> autoGet BlockChange
    0x36 -> autoGet PlayNote
    0x3c -> Explosion            <$> getJ <*> getJ <*> getJ <*> getJ <*>
               (getWord32be >>= \ len -> replicateM (fromIntegral len) getJ)
    0x64 -> autoGet OpenWindow
    0x65 -> autoGet CloseWindow
    0x66 -> WindowClick          <$> getJ <*> getJ <*> getJ <*> getJ
                                 <*> getItemTriple
    0x67 -> SetSlot              <$> getJ <*> getJ <*> getItemTriple
    0x68 -> WindowItems          <$> getJ <*> items
      where items = do count <- getWord16be
                       replicateM (fromIntegral count) getItemTriple
                         
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
  putItemTriple mbstuff

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

putMessage (Prechunk x z mode) = do
  putJ (0x32 :: MessageTag)
  putJ x
  putJ z
  putJ mode

putMessage (Mapchunk x y z szx szy szz bs) = do
  putJ (0x33 :: MessageTag)
  putJ x
  putJ y
  putJ z
  putJ szx
  putJ szy
  putJ szz
  putWord32be (fromIntegral (Data.ByteString.Lazy.length bs))
  putLazyByteString bs


putMessage (MultiblockChange x z xs) = do
  putJ (0x34 :: MessageTag)
  putJ x
  putJ z
  putWord16be (fromIntegral (length xs))
  let (coords, tys, metas) = unzip3 xs
  traverse_ putJ coords
  traverse_ putJ tys
  traverse_ putJ metas

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
  putItemTriple mbstuff

putMessage (SetSlot win slot mbstuff) = do
  putJ (0x67 :: MessageTag)
  putJ win
  putJ slot
  putItemTriple mbstuff

putMessage (WindowItems win xs) = do
  putJ (0x68 :: MessageTag)
  putJ win
  putWord16be (fromIntegral (length xs))
  traverse_ putItemTriple xs  

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

splitMessage :: Data.ByteString.Lazy.ByteString ->
                (Message, Data.ByteString.Lazy.ByteString)
splitMessage = runGet $ liftM2 (,) getJ getRemainingLazyByteString

