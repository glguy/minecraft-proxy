{-# LANGUAGE TemplateHaskell #-}
module Protocol where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.Int
import Network.Socket (ServiceName)

import JavaBinary
import Generator
import ProtocolHelper
import Language.Haskell.TH

defaultMinecraftPort :: ServiceName
defaultMinecraftPort = "25565"

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
    where putTag ty ix = putJ (ty `shiftL` 5 .|. ix .&. 0x1f)
          aux (ix, MetadataByte   x) = putTag 0 ix *> putJ x
          aux (ix, MetadataShort  x) = putTag 1 ix *> putJ x
          aux (ix, MetadataInt    x) = putTag 2 ix *> putJ x
          aux (ix, MetadataFloat  x) = putTag 3 ix *> putJ x
          aux (ix, MetadataString x) = putTag 4 ix *> putJ x
          aux (ix, MetadataTriple x) = putTag 5 ix *> putJ x


highlight :: String -> String
highlight text = "\194\167\&b" ++ text

packetData "Message"
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
  , con' 0x0f "PlayerBlockPlacement"
      [''Int32 --  X
      ,''Int8  --  Y
      ,''Int32 --  Z
      ,''Face 
      ] `addField`
      Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
            , fieldGet  = [|getMaybe16 getJ|]
            , fieldPut  = [|putMaybe16 putJ|]
            } --  Optional block, count, and use
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
  , con' 0x33 "Mapchunk"
     [] `addField`
     Field { fieldType = strictType isStrict [t|(ChunkLoc,Maybe (Array (Int8, Int8, Int8) BlockId
                                      , ByteString
                                      , ByteString
                                      , ByteString))|]
           , fieldGet = [| mapchunkDataGet |]
           , fieldPut = [| mapchunkDataPut |]
           }
  , con' 0x34 "MultiblockChange"
     [''ChunkLoc
     ] `addField`
     Field { fieldType = strictType isStrict [t|[(BlockLoc, BlockId, Int8)]|]
           , fieldGet = [| getChanges |]
           , fieldPut = [| putChanges |]
           } --  Coordinate, Block type, Meta
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
  , con' 0x3c "Explosion"
     [''Double --  X
     ,''Double --  Y
     ,''Double --  Z
     ,''Float  --  Radius?
     ] `addField`
     Field { fieldType = strictType isStrict [t|[(Int8,Int8,Int8)]|]
           , fieldGet = [| getCoords |]
           , fieldPut = [| putCoords |]
           } --  Relative X,Y,Z of affected blocks
  , con' 0x64 "OpenWindow"
      [''WindowId
      ,''InventoryType
      ,''String --  Title
      ,''Int8 --  Number of slots
      ]
  , con' 0x65 "CloseWindow"
      [''WindowId
      ]
  , con' 0x66 "WindowClick"
      [''WindowId
      ,''SlotId
      ,''Bool --  Right-click
      ,''TransactionId
      ] `addField`
      Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
            , fieldGet  = [|getMaybe16 getJ|]
            , fieldPut  = [|putMaybe16 putJ|]
            } --  Optional block, count, and use
  , con' 0x67 "SetSlot"
      [''WindowId
      ,''SlotId
      ] `addField`
      Field { fieldType = strictType isStrict [t|Maybe (ItemId, Int8, Int16)|]
            , fieldGet  = [|getMaybe16 getJ|]
            , fieldPut  = [|putMaybe16 putJ|]
            } --  Optional block, count, and use
  , con' 0x68 "WindowItems"
      [''WindowId
      ] `addField`
      Field { fieldType = strictType isStrict [t|[Maybe (ItemId, Int8, Int16)]|]
            , fieldGet  = [|do n <- getJ :: Get Int16
                               replicateM (fromIntegral n) (getMaybe16 getJ)|]
            , fieldPut  = [| \ xs -> do putJ (fromIntegral (length xs) :: Int16)
                                        traverse_ (putMaybe16 putJ) xs|]
            } --  Optional block, count, and use
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
  ]

toMessages :: ByteString -> [Message]
toMessages bs = msg : toMessages rest
  where
  (msg, rest) = runGet (liftA2 (,) getJ getRemainingLazyByteString) bs

proxyChat :: String -> Message
proxyChat text = Chat $ "\194\167\&6" ++ text

