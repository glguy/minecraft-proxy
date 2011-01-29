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
  , followVar :: MVar (Maybe EntityId)
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

main :: IO ()
main = do
  (host,port) <- do
    args <- getArgs
    case args of
      [host, port] -> return (host, port)
      _ -> do putStrLn "Usage: minecraft-proxy host port"
              exitFailure

  l <- Network.listenOn (Network.PortNumber (fromInteger 25565))
  putStrLn "Ready"
  forever $ do res <- accept l
               _ <- forkIO (handleClient host port res)
               return ()
handleClient :: HostName -> ServiceName -> (Socket, SockAddr) -> IO ()
handleClient host port (c,csa) = do
  putStr "Got connection from "
  print csa

  ais <- getAddrInfo (Just defaultHints { addrFamily = AF_INET
                                        , addrSocketType = Stream })
                     (Just host) (Just port)
  case ais of
    (ai : _ ) -> do s <- socket AF_INET Stream defaultProtocol
                    connect s $ addrAddress ai
                    proxy c s
    _ -> fail "Unable to resolve server address"
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
  var <- newChan
  state <- newProxyState
  clientChan <- newChan
  serverChan <- newChan
  serverToProxy <- forkIO $ do
          sbs <- getContents s
          traverse_ (inboundLogic clientChan state) (toMessages sbs)
         `bad` writeChan var "inbound"
  proxyToClient <- forkIO $ forever (sendAll c =<< readChan clientChan)
                  `bad` writeChan var "inbound network" 
  proxyToServer <- forkIO $ forever (sendAll s =<< readChan serverChan)
                 `bad` writeChan var "outbound network" 
  clientToProxy <- forkIO $ do
          cbs <- getContents c
          traverse_ (outboundLogic clientChan serverChan state)
                    (toMessages cbs)
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
      Just ieid | interested == changedEid -> do
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

processCommand clientChan state "restore"
  = do restoreMap <- swapMVar (restoreVar state) Map.empty
       bm <- blockMap <$> readMVar (gameState state)
       msgs <- makeRestore bm restoreMap
       if null msgs then
         tellPlayer clientChan "Nothing to restore"
        else do 
         tellPlayer clientChan "Restoring!"
         sendMessages clientChan msgs

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
         Just (k,_) -> swapMVar (followVar state) (Just k)
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

            let coords = chunkedCoords $ nearby (x, y, z)
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
  Int8 {- ^ First Y -} ->
  Int32 {- ^ First Z -} ->
  Int32 {- ^ Second X -} ->
  Int8 {- ^ Second Y -} ->
  Int32 {- ^ Second Z -} ->
  Face ->
  Maybe (ItemId, Int8, Int16) {- ^ Hand contents -} ->
  [Message]
drawLine msg x y z x1 y1 z1 f o
  | x == x1 && y == y1 = [PlayerBlockPlacement x y z2 f o | z2 <- [min z z1 .. max z z1]]
  | x == x1 && z == z1 = [PlayerBlockPlacement x y2 z f o | y2 <- [min y y1 .. max y y1]]
  | z == z1 && y == y1 = [PlayerBlockPlacement x2 y z f o | x2 <- [min x x1 .. max x x1]]
  | otherwise          = [msg]

lookupBlock ::
  BlockMap ->
  ChunkLoc ->
  BlockLoc ->
  IO (Maybe BlockId)
lookupBlock bm chunkC blockC = do
  for (Map.lookup chunkC bm) $ \ (blockArray, _) ->
    readArray blockArray blockC

filterGlassUpdate :: BlockMap -> BlockId -> (ChunkLoc, [BlockLoc]) -> IO (ChunkLoc, [BlockLoc])
filterGlassUpdate bm victim ((cx,cz), blocks) = do
  let isVictim x = x == Just victim
  xs <- filterM (\ c -> isVictim <$> lookupBlock bm (cx,cz) c) blocks
  return ((cx,cz),xs)

makeGlassUpdate :: (ChunkLoc,[BlockLoc]) -> [Message]
makeGlassUpdate (_, []) = []
makeGlassUpdate ((cx,cz), coords)
  = [MultiblockChange cx cz [(packCoords c, Glass, 0) | c <- coords]]

makeRestore :: BlockMap -> Map ChunkLoc (Set BlockLoc) -> IO [Message]
makeRestore bm = sequence . mapMaybe toMessage . Map.toList
  where
  toMessage :: (ChunkLoc, Set BlockLoc) -> Maybe (IO Message)
  toMessage (chunk@(cx,cz), blocks)
    = do guard (not (Set.null blocks))
         (blockArray, metaArray) <- Map.lookup chunk bm
         return $ fmap (MultiblockChange cx cz)
                $ for (Set.toList blocks) $ \ b ->
                    do blockType <- readArray blockArray b
                       blockMeta <- readArray metaArray  b
                       return (packCoords b, blockType, fromIntegral blockMeta)
                     
 
-- | 'packCoords' packs coordinates local to a chunk into a single
-- 'Int16'.
packCoords :: BlockLoc -> Int16
packCoords (x,y,z) = fromIntegral (fromIntegral x `shiftL` 12
                               .|. fromIntegral z `shiftL` 8
                               .|. fromIntegral y :: Word16)
 

chunkedCoords :: [(Int32,Int8,Int32)] -> [(ChunkLoc, [BlockLoc])]
chunkedCoords = collect . map categorize
  where categorize (x,y,z) = decomposeCoords x y z

collect :: Ord a => [(a,b)] -> [(a, [b])]
collect = Map.toList . Map.fromListWith (++) . map (\ (a,b) -> (a,[b]))

nearby :: (Int32,Int8,Int32) -> [(Int32,Int8,Int32)]
nearby (x,y,z) = filter inSphere box
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
