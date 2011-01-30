{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array.IO
import Data.Binary.Put (runPut)
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.IORef
import Data.Int
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Set (Set)
import Data.Traversable (for)
import Data.Word
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import System.Console.GetOpt
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network

import GameState
import JavaBinary
import Protocol

data ProxyState = PS
  { gameState :: MVar GameState
  , glassVar  :: IORef Bool
  , digThrough :: IORef Int
  , restoreVar :: MVar (Map (Int32,Int32) (Set (Int8, Int8, Int8)))
  , lineVar   :: IORef (Bool, [Message])
  , digVar    :: IORef Int
  , followVar :: MVar (Maybe (String, EntityId))
  }

newProxyState :: IO ProxyState
newProxyState = do
  gameState  <- newMVar newGameState
  glassVar   <- newIORef False
  digThrough <- newIORef 0
  restoreVar <- newMVar Map.empty
  lineVar    <- newIORef (False, [])
  digVar     <- newIORef 1
  followVar  <- newMVar Nothing
  return PS {..}

data Configuration = Config
  { listenHost :: HostName
  , listenPort :: ServiceName
  , configHelp :: Bool }

defaultConfig = Config
  { listenHost = "localhost"
  , listenPort = "25565"
  , configHelp = False
  }

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option ['l'] ["listen-host"]
     (ReqArg (\ str c -> c { listenHost = str }) "HOSTNAME")
     "Hostname to bind to"
  , Option ['p'] ["listen-port"]
     (ReqArg (\ str c -> c { listenPort = str }) "SERVICENAME")
     "Port to bind to"
  , Option ['h'] ["help"]
     (NoArg (\ c -> c { configHelp = True }))
     "Print this list"
  ]

getOptions = do
  (fs, args, errs) <- getOpt Permute options <$> getArgs
  let config = foldl' (\ c f -> f c) defaultConfig fs
  unless (null errs) $ do
    traverse_ putStrLn errs
    exitFailure
  let usageText = "minecraft-proxy <FLAGS> SERVER-HOSTNAME SERVER-PORT"
  when (configHelp config) $ do
    putStrLn $ usageInfo usageText options
    exitSuccess
  case args of
    [host,port] -> return (host,port,config)
    _ -> do putStrLn $ usageInfo usageText options
            exitFailure

addrInfoHints =  defaultHints { addrFamily = AF_INET
                              , addrSocketType = Stream
                              , addrFlags = [AI_PASSIVE] }

main :: IO ()
main = do
  (host,port,config) <- getOptions

  ais <- getAddrInfo (Just addrInfoHints)
           (Just (listenHost config))
           (Just (listenPort config))
  addr <- case ais of
    (ai : _ ) -> return $ addrAddress ai
    [] -> fail "Unable to resolve bind address"

  l <- socket AF_INET Stream defaultProtocol
  setSocketOption l ReuseAddr 1
  bindSocket l addr
  listen l 5

  putStrLn "Ready to accept connections"

  forever $ do res <- accept l
               _ <- forkIO (handleClient host port res)
               return ()

handleClient :: HostName -> ServiceName -> (Socket, SockAddr) -> IO ()
handleClient host port (c,csa) = do
  putStr "Got connection from "
  print csa

  ais <- getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  addr <- case ais of
    (ai : _ ) -> return $ addrAddress ai
    _ -> fail "Unable to resolve server address"

  s <- socket AF_INET Stream defaultProtocol
  connect s addr
  proxy c s

 `Control.Exception.catch` \ (SomeException e) -> do
      sendAll c $ encode $ Disconnect (show e)
      fail (show e)

-- | 'proxy' creates the threads necessary to proxy a Minecraft
--   connection between a client and a server socket.
proxy ::
  Socket {- ^ client socket -} ->
  Socket {- ^ server socket -} ->
  IO ()
proxy c s = do
  sbs <- toMessages <$> getContents s
  cbs <- toMessages <$> getContents c

  var <- newChan
  state <- newProxyState
  clientChan <- newChan
  serverChan <- newChan
  serverToProxy <- forkIO $ do
          traverse_ (inboundLogic clientChan state) sbs
         `bad` writeChan var "inbound"
  proxyToClient <- forkIO $ forever (sendAll c =<< readChan clientChan)
                  `bad` writeChan var "inbound network" 
  proxyToServer <- forkIO $ forever (sendAll s =<< readChan serverChan)
                 `bad` writeChan var "outbound network" 
  clientToProxy <- forkIO $ do
          traverse_ (outboundLogic clientChan serverChan state) cbs
         `bad` writeChan var "outbound"
  who <- readChan var
  putStr who
  putStrLn " died"
  traverse_ killThread [serverToProxy, proxyToClient, proxyToServer, clientToProxy]
  where
  bad m n = m `Control.Exception.catch` \ (SomeException e) -> print e >> n

makeGlass :: BlockId -> BlockId
makeGlass Dirt  = Glass
makeGlass Stone = Glass
makeGlass Grass = Glass
makeGlass other = other

inboundLogic ::
  Chan ByteString {- ^ client channel -} ->
  ProxyState                             ->
  Message                                ->
  IO ()
inboundLogic clientChan state msg = do

  -- Track entities
  changedEid <- modifyMVar (gameState state) $ \ gs -> do
    (change, gs') <- updateGameState msg gs
    gs' `seq` return (gs', change)

  -- Global glass modifications
  glass <- readIORef (glassVar state)
  let msg' = case msg of
               Mapchunk x y z sx sy sz bs a b c
                 | glass -> Mapchunk x y z sx sy sz (map makeGlass bs) a b c
               _ -> msg

  -- Update compass
  followMsgs <- withMVar (followVar state) $ \ interested ->
    case interested of
      Just (_,ieid) | fmap snd interested == changedEid -> do
       e <- entityMap <$> readMVar (gameState state)
       return $ case Map.lookup ieid e of
         Just (_ty, x, y, z) ->
           [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32)]
         _ -> []
      _ -> return []

  sendMessages clientChan (msg' : followMsgs)


processCommand ::
  Chan ByteString {- ^ client channel -} ->
  ProxyState                             ->
  String          {- ^ chat command   -} ->
  IO ()

processCommand clientChan _ "help"
  = traverse_ (tellPlayer clientChan) helpMessage

processCommand clientChan state "status"
  = traverse_ (tellPlayer clientChan) =<< statusMessages state

processCommand clientChan state text
  | "echo " `isPrefixOf` text
  = case reads (drop 5 text) of
      [(msg,_)] -> sendMessages clientChan [msg]
      _ -> tellPlayer clientChan "Unable to parse message"

processCommand clientChan state "glass on"
  =  writeIORef (glassVar state) True
  *> tellPlayer clientChan "Glass On"

processCommand clientChan state "glass off"
  =  writeIORef (glassVar state) False
  *> tellPlayer clientChan "Glass Off"

processCommand clientChan state text | "through " `isPrefixOf` text
  =  case reads $ drop 8 text of
       [(n,_)] -> writeIORef (digThrough state) n
               *> tellPlayer clientChan "Through Set"
       _ -> tellPlayer clientChan "Bad through value"

processCommand clientChan state text | "dig " `isPrefixOf` text
  =  case reads $ drop 4 text of
       [(n,_)] -> writeIORef (digVar state) n
               *> tellPlayer clientChan "Dig Set"

       _       -> tellPlayer clientChan "Bad dig number"

processCommand clientChan state "follow off" = do
  modifyMVar_ (followVar state) $ \ _ -> do
    mb <- spawnLocation <$> readMVar (gameState state)
    case mb of
      Nothing      -> tellPlayer clientChan "Follow disabled - spawn point unknown"
      Just (x,y,z) -> do sendMessages clientChan [SpawnPosition x y z]
                         tellPlayer clientChan "Follow disabled - compass restored"
    return Nothing

processCommand clientChan state text | "follow " `isPrefixOf` text
  = do e <- entityMap <$> readMVar (gameState state)
       case find (\ (_,(x,_,_,_)) -> x == Left key) (Map.assocs e) of
         Just (k,_) -> swapMVar (followVar state) (Just (key,k))
                    *>  tellPlayer clientChan "Follow registered"
         Nothing -> tellPlayer clientChan "Player not found"
  where key = drop 7 text

processCommand clientChan state "lines on"
  =  writeIORef (lineVar state) (True, [])
  *> tellPlayer clientChan "Lines On"

processCommand clientChan state "lines off"
  =  modifyIORef (lineVar state) (\ (_ , xs) -> (False, xs))
  *> tellPlayer clientChan "Lines Off"

processCommand clientChan _ _
  =  tellPlayer clientChan "Command not understood"


sendMessages :: Chan L.ByteString -> [Message] -> IO ()
sendMessages _    [] = return ()
sendMessages chan xs = writeChan chan . runPut . traverse_ putJ $ xs

tellPlayer :: Chan L.ByteString -> String -> IO ()
tellPlayer chan text = sendMessages chan [proxyChat text]
     
outboundLogic :: Chan ByteString {- ^ client channel -} ->
                 Chan ByteString {- ^ server channel -} ->
                 ProxyState                             ->
                 Message                                ->
                 IO ()
outboundLogic clientChan serverChan state msg = do

  (recording, macros) <- readIORef $ lineVar state
  shiftCount <- readIORef (digThrough state)

  msgs <- case msg of
    PlayerPosition {} -> return [msg]
    PlayerPositionLook {} -> return [msg]
    PlayerLook {} -> return [msg]
    Player {} -> return [msg]
    KeepAliv -> return [msg]

    PlayerDigging Digging x y z face -> do
     let (x',y',z') = digShift x y z face shiftCount
     n <- readIORef $ digVar state
     return $ replicate n $ PlayerDigging Digging x' y' z' face

    PlayerDigging action x y z face -> do
     let (x',y',z') = digShift x y z face shiftCount
     return [PlayerDigging action x' y' z' face]

    PlayerBlockPlacement x y z _ (Just (IID 0x15B, _, _)) -> do
      attacked <- glassAttack clientChan state x y z
      return $ if attacked then [] else [msg]

    PlayerBlockPlacement x y z None (Just (IID 0x159, _, _)) -> do
       restoreMap <- swapMVar (restoreVar state) Map.empty
       bm <- blockMap <$> readMVar (gameState state)
       msgs <- makeRestore bm restoreMap
       if null msgs
        then tellPlayer clientChan "Nothing to restore"
        else tellPlayer clientChan "Restoring!"
          *> sendMessages clientChan msgs
       return []


    PlayerBlockPlacement x1 y1 z1 f o | recording -> case macros of
      [PlayerBlockPlacement x y z _ _] ->
        do writeIORef (lineVar state) (recording, [msg])
           return $ drawLine msg x y z x1 y1 z1 f o

      _ -> [msg] <$ writeIORef (lineVar state) (recording, [msg])

    Chat ('\\':xs) -> [] <$ processCommand clientChan state xs
                
    _ -> [msg] <$ putStrLn ("outbound: " ++ show msg)
  sendMessages serverChan msgs

glassAttack ::
  Chan ByteString {- ^ client channel -} ->
  ProxyState                             ->
  Int32 {- ^ X block coordinate -}       ->
  Int8  {- ^ Y block coordinate -}       ->
  Int32 {- ^ Z block coordinate -}       ->
  IO Bool
glassAttack clientChan state x y z = do
   bm <- blockMap <$> readMVar (gameState state)
   let (chunkC,blockC) = decomposeCoords x y z
   case Map.lookup chunkC bm of
        Just (arr,_) -> do
          blockId <- readArray arr blockC
          if (blockId /= Air) then do
            tellPlayer clientChan "Glass attack!"

            let coords = chunkedCoords $ nearby x y z
            coords' <- mapM (filterGlassUpdate bm blockId) coords
            let glassMsgs = makeGlassUpdate =<< coords'
            sendMessages clientChan glassMsgs

            modifyMVar_ (restoreVar state) $ \ m ->
              return $! mergeCoords m coords'

            return True
           else return False
        _ -> return False

mergeCoords :: Map ChunkLoc (Set BlockLoc) ->
               [(ChunkLoc, [BlockLoc])]    -> 
               Map ChunkLoc (Set BlockLoc)
mergeCoords = foldl' $ \ m (chunk,blocks) ->
  let aux = Just
          . Set.union (Set.fromList blocks)
          . fromMaybe Set.empty
  in if null blocks then m else Map.alter aux chunk m

drawLine :: Message ->
  Int32 {- ^ First X -} ->
  Int8  {- ^ First Y -} ->
  Int32 {- ^ First Z -} ->
  Int32 {- ^ Second X -} ->
  Int8  {- ^ Second Y -} ->
  Int32 {- ^ Second Z -} ->
  Face ->
  Maybe (ItemId, Int8, Int16) {- ^ Hand contents -} ->
  [Message]
drawLine msg x y z x1 y1 z1 f o
  | x == x1 && y == y1 = [PlayerBlockPlacement x y z2 f o | z2 <- [min z z1 .. max z z1]]
  | x == x1 && z == z1 = [PlayerBlockPlacement x y2 z f o | y2 <- [min y y1 .. max y y1]]
  | z == z1 && y == y1 = [PlayerBlockPlacement x2 y z f o | x2 <- [min x x1 .. max x x1]]
  | otherwise          = [msg]

lookupBlock :: BlockMap -> ChunkLoc -> BlockLoc -> IO (Maybe BlockId)
lookupBlock bm chunkC blockC = do
  for (Map.lookup chunkC bm) $ \ (blockArray, _) ->
    readArray blockArray blockC

filterGlassUpdate :: BlockMap -> BlockId -> (ChunkLoc, [BlockLoc]) -> IO (ChunkLoc, [BlockLoc])
filterGlassUpdate bm victim (chunk, blocks) = do
  let isVictim x = x == Just victim
  xs <- filterM (\ c -> isVictim <$> lookupBlock bm chunk c) blocks
  return (chunk,xs)

makeGlassUpdate :: (ChunkLoc,[BlockLoc]) -> [Message]
makeGlassUpdate (_, []) = []
makeGlassUpdate (chunk, coords)
  = [MultiblockChange chunk [(c, Glass, 0) | c <- coords]]

makeRestore :: BlockMap -> Map ChunkLoc (Set BlockLoc) -> IO [Message]
makeRestore bm = sequence . mapMaybe toMessage . Map.toList
  where
  toMessage :: (ChunkLoc, Set BlockLoc) -> Maybe (IO Message)
  toMessage (chunk, blocks)
    = do guard (not (Set.null blocks))
         (blockArray, metaArray) <- Map.lookup chunk bm
         return $ fmap (MultiblockChange chunk)
                $ for (Set.toList blocks) $ \ b ->
                    do blockType <- readArray blockArray b
                       blockMeta <- readArray metaArray  b
                       return (b, blockType, fromIntegral blockMeta)
                     
 
 

chunkedCoords :: [(Int32,Int8,Int32)] -> [(ChunkLoc, [BlockLoc])]
chunkedCoords = collect . map categorize
  where categorize (x,y,z) = decomposeCoords x y z

collect :: Ord a => [(a,b)] -> [(a, [b])]
collect = Map.toList . Map.fromListWith (++) . map (\ (a,b) -> (a,[b]))

-- | 'nearby' computes the coordinates of blocks that are within 10
-- blocks of the given coordinate.
nearby ::
  Int32 {- ^ X coordinate -} ->
  Int8  {- ^ Y coordinate -} ->
  Int32 {- ^ Z coordinate -} ->
  [(Int32,Int8,Int32)]
nearby x y z = filter inSphere box
  where radius :: Num a => a
        radius = 10
        inSphere (x1,y1,z1) = squared radius > (squared (x1-x) + fromIntegral (squared (y1-y)) + squared (z1-z))
        squared i = i * i
        box = (,,) <$> [x-radius .. x+radius] <*> [y-radius .. y+radius] <*> [z-radius .. z+radius]

digShift :: Int32 -> Int8 -> Int32 -> Face -> Int -> (Int32, Int8, Int32)
digShift x y z face i = case face of
  X1 -> (x+i',y,z)
  X2 -> (x-i',y,z)
  Y1 -> (x,y+i',z)
  Y2 -> (x,y-i',z)
  Z1 -> (x,y,z+i')
  Z2 -> (x,y,z-i')
  None -> (x,y,z)

 where i' :: Num a => a
       i' = fromIntegral i

helpMessage :: [String]
helpMessage =
  ["Commands:"
  ,"\\status               - List proxy status"
  ,"\\follow <player name> - Compass points to player"
  ,"\\follow off           - Compass points to spawnpoint"
  ,"\\dig <n>              - Multiplies dig speed by 'n'"
  ,"\\glass {on,off}       - Glassify new chunks"
  ,"\\through <n>          - Dig 'n' blocks behind"
  ,"\\lines {on,off}       - Place blocks in lines on an axis"
  ,"Right-click with clock - Glassify local sphere"
  ,"Right-click with compass - Revert glass spheres"
  ]

statusMessages :: ProxyState -> IO [String]
statusMessages state = sequence
  [ return "Proxy Status ------------------------"
  , followStatus  <$> readMVar (followVar state)
  , digStatus     <$> readIORef (digVar state)
  , glassStatus   <$> readIORef (glassVar state)
  , throughStatus <$> readIORef (digThrough state)
  , lineStatus    <$> readIORef (lineVar state)
  , restoreStatus <$> readMVar (restoreVar state) 
  ]
  where
  onOff True = highlight "on"
  onOff False = highlight "off"
  followStatus x = "Following " ++
    case x of
      Nothing -> "spawn point"
      (Just (name,_)) -> " player: " ++ highlight name
  digStatus x = "Dig speed: " ++ highlight (show x)
  glassStatus b = "Glass chunks: "++ onOff b
  throughStatus n = "Dig through: " ++ highlight (show n)
  lineStatus (b,_) = "Line mode: " ++ onOff b
  restoreStatus m = "Blocks to restore: " ++
    highlight (show (Data.Foldable.sum (fmap Set.size (Map.elems m))))
