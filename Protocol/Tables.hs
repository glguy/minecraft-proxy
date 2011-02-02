module Protocol.Tables where

-- | 'blocks' defines the mapping between tag and block constructor name.
blocks :: [(Integer, String)]
blocks =
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

-- | 'items' defines the mapping between tag and item constructor name.
-- All blocks are included in the list of items by including "AsItem"
-- on the end of the constructor name.
items :: [(Integer, String)]
items =
  [(tag,name ++ "AsItem") | (tag,name) <- blocks]
  ++
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