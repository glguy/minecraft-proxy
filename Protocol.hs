{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Protocol where

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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

import JavaBinary
import Generator
import ProtocolHelper
import Language.Haskell.TH

defaultMinecraftPort :: ServiceName
defaultMinecraftPort = "25565"


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
 deriving (Show, Read, Eq)

data MetadataEntry
  = MetadataByte Int8
  | MetadataShort Int16
  | MetadataInt Int32
  | MetadataFloat Float
  | MetadataString String
  | MetadataTriple (Int16, Int8, Int16)
  deriving (Show, Read,Eq)

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


highlight :: String -> String
highlight text = "\194\167\&b" ++ text

$(packetData "Message"
  [ con0 0x00 "KeepAliv" --  Keep alives are sent every 1200 ticks
  , con' 0x01 "LoginRequest"
      [''Int32  --  Protocol Version
      ,''String --  Username
      ,''String --  Password
      ,''Int64  --  Map Seed
      ,''Int8   --  Dimension
      ]

  , con' 0x02 "Handshake"
      [''String --  Session ID
      ]
  , con' 0x03 "Chat"
      [''String --  Message
      ]
  , con' 0x04 "TimeUpdate"
      [''Int64 --  The world time in minutes
      ]
  , con' 0x05 "EntityEquipment"
      [''EntityId
      ,''SlotId
      ,''ItemId --  Item ID
      ,''Int16 --  Damage?
      ]
  , con' 0x06 "SpawnPosition"
      [''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ]
  , con' 0x07 "UseEntity"
      [''EntityId --  User
      ,''EntityId --  Target
      ,''Bool --  Left-click?
      ]
  , con' 0x08 "UpdateHealth"
      [''Int16 --  Health: 0--20
      ]
  , con0 0x09 "Respawn"
  , con' 0x0a "Player"
      [''Bool --  On Ground
      ]
  , con' 0x0b "PlayerPosition"
      [''Double --  X
      ,''Double --  Y
      ,''Double --  Stance
      ,''Double --  Z
      ,''Bool   --  On ground
      ]
  , con' 0x0c "PlayerLook"
      [''Float --  Yaw
      ,''Float --  Pitch
      ,''Bool  --  On Ground
      ]
  , con' 0x0d "PlayerPositionLook"
      [''Double --  X
      ,''Double --  Y
      ,''Double --  Stance
      ,''Double --  Z
      ,''Float  --  Yaw
      ,''Float  --  Pitch
      ,''Bool   --  On Ground
      ]
  , con' 0x0e "PlayerDigging"
      [''DiggingStatus
      ,''Int32 --  X
      ,''Int8  --  Y
      ,''Int32 --  Z
      ,''Face
      ]
  , Member { memberTag = Just 0x0f
           , memberName = mkName "PlayerBlockPlacement"
           , memberFields =
               [standardField [t|Int32|] --  X
               ,standardField [t|Int8 |] --  Y
               ,standardField [t|Int32|] --  Z
               ,standardField [t|Face |]
               ,Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
                      , fieldGet  = [|getMaybe16 getJ|]
                      , fieldPut  = \n -> [|putMaybe16 putJ $(n)|]
                      } --  Optional block, count, and use
               ]
           }
  , con' 0x10 "HoldingChange"
      [''SlotId
      ]
  , con' 0x12 "Animation"
      [''EntityId --  Player ID
      ,''Animate
      ]
  , con' 0x13 "EntityAction"
      [''EntityId --  Player ID
      ,''Action
      ]
  , con' 0x14 "NamedEntitySpawn"
      [''EntityId --  Player ID
      ,''String --  Player Name
      ,''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ,''Int8 --  Rotation
      ,''Int8 --  Pitch
      ,''ItemId --  Current Item
      ]
  , con' 0x15 "PickupSpawn"
      [''EntityId --  Player ID
      ,''ItemId
      ,''Int8  --  Count
      ,''Int16  --  Damage?
      ,''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ,''Int8 --  Rotation
      ,''Int8 --  Pitch
      ,''Int8 --  Roll
      ]
  , con' 0x16 "CollectItem"
      [''EntityId --  Collected
      ,''EntityId --  Collector
      ]
  , con' 0x17 "AddObject"
      [''EntityId
      ,''Int8  --  Type ID
      ,''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ]
  , con' 0x18 "MobSpawn"
      [''EntityId
      ,''MobId --  Mob ID
      ,''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ,''Int8  --  Yaw
      ,''Int8  --  Pitch
      ,''Metadata
      ]
  , con' 0x19 "Painting"
      [''EntityId
      ,''String --  Name of painting
      ,''Int32 --  X
      ,''Int32 --  Y
      ,''Int32 --  Z
      ,''GraphicId
      ]
  , con' 0x1c "EntityVelocity"
      [''EntityId
      ,''Int16 --  X Velocity
      ,''Int16 --  Y Velocity
      ,''Int16 --  Z Velocity
      ]
  , con' 0x1d "DestroyEntity"
     [''EntityId
     ]
  , con' 0x1e "Entity"
     [''EntityId
     ]
  , con' 0x1f "EntityRelativeMove"
     [''EntityId
     ,''Int8 --  Change in X
     ,''Int8 --  Change in Y
     ,''Int8 --  Change in Z
     ]
  , con' 0x20 "EntityLook"
     [''EntityId
     ,''Int8 --  Yaw
     ,''Int8 --  Pitch
     ]
  , con' 0x21 "EntityLookMove"
     [''EntityId
     ,''Int8  --  dX
     ,''Int8  --  dY
     ,''Int8  --  dZ
     ,''Int8  --  Yaw
     ,''Int8  --  Pitch
     ]
  , con' 0x22 "EntityTeleport"
     [''EntityId
     ,''Int32 --  X
     ,''Int32 --  Y
     ,''Int32 --  Z
     ,''Int8  --  Yaw
     ,''Int8  --  Pitch
     ]
  , con' 0x26 "EntityStatus"
     [''EntityId
     ,''EntityStatus
     ]
  , con' 0x27 "AttachEntity"
     [''EntityId --  Entity ID
     ,''EntityId --  Vehicle ID
     ]
  , con' 0x28 "EntityMetadata"
     [''EntityId
     ,''Metadata
     ]
  , con' 0x32 "Prechunk"
     [''ChunkLoc
     ,''PrechunkStatus
     ]
  , Member
     { memberName = mkName "Mapchunk"
     , memberTag  = Just 0x33
     , memberFields = map (standardField . conT)
             [''Int32 --  X
             ,''Int16 --  Y
             ,''Int32 --  Z
             ] ++ [
               Field { fieldType = strictType isStrict [t|( Int8, Int8, Int8, [BlockId]
                                      , ByteString
                                      , ByteString
                                      , ByteString)|]
                     , fieldGet = [| mapchunkDataGet |]
                     , fieldPut = \n -> [| mapchunkDataPut $(n) |]
                     }
             ] }

  , Member
      { memberName = mkName "MultiblockChange"
      , memberTag  = Just 0x34
      , memberFields = [standardField [t|ChunkLoc|]
                       , Field { fieldType = strictType isStrict [t|[(BlockLoc, BlockId, Int8)]|] --  Coordinate, Block type, Meta
                       , fieldGet = [| getChanges |]
                       , fieldPut = \n -> [| putChanges $(n)  |] } ] }

  , con' 0x35 "BlockChange"
      [''Int32 --  Block X
      ,''Int8  --  Block Y
      ,''Int32 --  Block Z
      ,''BlockId
      ,''Int8  --  Block metadata
      ]
  , con' 0x36 "PlayNote"
      [''Int32 --  Block X
      ,''Int16 --  Block Y
      ,''Int32 --  Block Z
      ,''InstrumentType
      ,''Int8  --  Pitch
      ]
  , Member { memberName = mkName "Explosion"
           , memberTag  = Just 0x3c
           , memberFields = map (standardField . conT)
                            [''Double --  X
                            ,''Double --  Y
                            ,''Double --  Z
                            ,''Float  --  Radius?
                            ] ++ [
             Field { fieldType = strictType isStrict [t|[(Int8,Int8,Int8)]|] --  Relative X,Y,Z of affected blocks
                   , fieldGet = [| getCoords |]
                   , fieldPut = \n -> [| putCoords $(n) |]
                   } ] }

  , con' 0x64 "OpenWindow"
      [''WindowId
      ,''InventoryType
      ,''String --  Title
      ,''Int8 --  Number of slots
      ]
  , con' 0x65 "CloseWindow"
      [''WindowId
      ]
  , Member { memberTag = Just 0x66
           , memberName = mkName "WindowClick"
           , memberFields = map (standardField . conT)
                            [''WindowId
                            ,''SlotId
                            ,''Bool --  Right-click
                            ,''TransactionId
                            ] ++ [
                Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
                      , fieldGet  = [|getMaybe16 getJ|]
                      , fieldPut  = \n -> [|putMaybe16 putJ $(n)|]
                      } --  Optional block, count, and use
                ]}
  , Member { memberTag = Just 0x67
           , memberName = mkName "SetSlot"
           , memberFields = map (standardField . conT)
                            [''WindowId
                            ,''SlotId
                            ] ++ [
                Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
                      , fieldGet  = [|getMaybe16 getJ|]
                      , fieldPut  = \n -> [|putMaybe16 putJ $(n)|]
                      } --  Optional block, count, and use
                ]}
  , Member { memberTag = Just 0x68
           , memberName = mkName "WindowItems"
           , memberFields = map (standardField . conT)
                            [''WindowId
                            ] ++ [
                Field { fieldType = strictType isStrict [t|[Maybe (ItemId, Int8, Int16)]|]
                      , fieldGet  = [|do n <- getJ :: Get Int16
                                         replicateM (fromIntegral n) (getMaybe16 getJ)|]
                      , fieldPut  = \n -> [| do let xs = $(n)
                                                putJ (fromIntegral (length xs) :: Int16)
                                                traverse_ (putMaybe16 putJ) xs|]
                      } --  Optional block, count, and use
                ]}

  , con' 0x69 "UpdateProgressBar"
      [''WindowId
      ,''ProgressBarId
      ,''Int16 --  Value
      ]
  , con' 0x6a "Transaction"
      [''WindowId
      ,''TransactionId
      ,''Bool --  Success
      ]
  , con' 0x82 "UpdateSign"
      [''Int32  --  X
      ,''Int16  --  Y
      ,''Int32  --  Z
      ,''String --  Text on line 1
      ,''String --  Text on line 2
      ,''String --  Text on line 3
      ,''String --  Text on line 4
      ]
  , con' 0xff "Disconnect"
      [''String
      ] --  Reason
  ])

toMessages :: ByteString -> [Message]
toMessages bs = msg : toMessages rest
  where
  (msg, rest) = runGet (liftA2 (,) getJ getRemainingLazyByteString) bs

proxyChat :: String -> Message
proxyChat text = Chat $ "\194\167\&6" ++ text

