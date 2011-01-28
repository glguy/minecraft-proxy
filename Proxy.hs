{-# LANGUAGE RecordWildCards #-}
module Main where

import Protocol

import Codec.Compression.Zlib
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)
import Data.Bits
import Data.Foldable
import Data.Traversable (for)
import Data.IORef
import Data.Array.IO
import Data.Int
import Data.Word
import Data.Map (Map)
import Data.Maybe (fromMaybe,catMaybes)
import Data.Set (Set)
import Data.List (groupBy,sort,isPrefixOf)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import System.Environment
import System.Exit
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network
import JavaBinary
import GameState

data ProxyState = PS
  { gameState :: MVar GameState
  , glassVar  :: IORef Bool
  , restoreVar :: MVar (Map (Int32,Int32) (Set (Int8, Int8, Int8)))
  , lineVar   :: IORef (Bool, [Message])
  , digVar    :: IORef Int
  , followVar :: MVar (Maybe EntityId)
  }

newProxyState = do
  gameState  <- newMVar newGameState
  glassVar   <- newIORef False
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

handleClient host port (c,csa) = do
  putStr "Got connection from "
  print csa

  s <- socket AF_INET Stream defaultProtocol
  ais <- getAddrInfo (Just defaultHints { addrFamily = AF_INET
                                         , addrSocketType = Stream })
                       (Just host) (Just port)
  case ais of
    (ai : _ ) -> do let sa = addrAddress ai
                    print sa
                    connect s sa
                    proxy c s
    _ -> fail "Unable to resolve server address"
 `Control.Exception.catch` \ (SomeException e) -> do
      sendAll c $ encode $ Disconnect (show e)
      fail (show e)

proxy :: Socket -> Socket -> IO a
proxy c s = do
  var <- newEmptyMVar
  state <- newProxyState
  clientChan <- newChan
  serverChan <- newChan
  _ <- forkIO $ do
          sbs <- getContents s
          traverse_ (proxy1 clientChan (inboundLogic state))
                    (toMessages sbs)
         `bad` putMVar var "inbound"
  _ <- forkIO $ forever (sendAll c =<< readChan clientChan)
                  `bad` putMVar var "inbound network" 
  _ <- forkIO $ forever (sendAll s =<< readChan serverChan)
                 `bad` putMVar var "outbound network" 
  _ <- forkIO $ do
          cbs <- getContents c
          traverse_ (proxy1 serverChan (outboundLogic clientChan state))
                    (toMessages cbs)
         `bad` putMVar var "outbound"
  who <- takeMVar var
  putStr who
  putStrLn " died"
  exitFailure
  where
  bad m n = m `Control.Exception.catch` \ (SomeException e) -> print e >> n

makeGlass Dirt = Glass
makeGlass Stone = Glass
makeGlass Grass = Glass
makeGlass block = block

inboundLogic :: ProxyState -> Message -> IO [Message]
inboundLogic state msg = do
  case msg of
    Entity {} -> return ()
    EntityLook {} -> return ()
    EntityVelocity {} -> return ()
    EntityRelativeMove {} -> return ()
    EntityLookMove {} -> return ()
    DestroyEntity {} -> return ()
    KeepAliv -> return ()
    Prechunk{} -> return ()
    TimeUpdate {} -> return ()
    Mapchunk {} -> return ()
    BlockChange {} -> return ()
    MultiblockChange {} -> return ()
    _ -> putStrLn $ "inbound: " ++ show msg

  changedEid <- modifyMVar (gameState state) $ \ gs -> do
                       (change, gs') <- updateGameState msg gs
                       gs' `seq` return (gs', change)

  glass <- readIORef (glassVar state)
  let msg' = case msg of
               Mapchunk x y z sx sy sz bs a b c
                 | glass -> Mapchunk x y z sx sy sz (map makeGlass bs) a b c
               _ -> msg

  withMVar (followVar state) $ \ interested ->
    case interested of
      Just ieid | interested == changedEid -> do
       e <- entityMap <$> readMVar (gameState state)
       case Map.lookup ieid e of
         Just (_ty, x, y, z) ->
           return [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32),msg']
         _ -> return [msg']
      _ -> return [msg']


processCommand :: Chan L.ByteString -> ProxyState -> String -> IO [Message]

processCommand clientChan state "restore glass"
  = do restoreMap <- swapMVar (restoreVar state) Map.empty
       bm <- blockMap <$> readMVar (gameState state)
       tellPlayer clientChan "Restoring!"
       writeChan clientChan . runPut . traverse_ putJ =<< makeRestore bm restoreMap
       return []

processCommand clientChan state "glass on"
  =  writeIORef (glassVar state) True
  *> tellPlayer clientChan "Glass On"
  *> return []

processCommand clientChan state "glass off"
  =  writeIORef (glassVar state) False
  *> tellPlayer clientChan "Glass Off"
  *> return []

processCommand clientChan state text | "dig " `isPrefixOf` text
  =  case reads $ drop 4 text of
       [(n,_)] -> writeIORef (digVar state) n
               *> tellPlayer clientChan "Dig Set"
               *> return []

       _       -> tellPlayer clientChan "Bad dig number"
               *> return []

processCommand clientChan state "follow off" = do
  modifyMVar_ (followVar state) $ \ _ -> do
    mb <- spawnLocation <$> readMVar (gameState state)
    case mb of
      Nothing      -> tellPlayer clientChan "Follow disabled - spawn point unknown"
      Just (x,y,z) -> do writeChan clientChan $ encode $ SpawnPosition x y z
                         tellPlayer clientChan "Follow disabled - compass restored"
    return Nothing
  return []

processCommand clientChan state text | "follow " `isPrefixOf` text
  = case reads key of
      [(eid, _)] -> do swapMVar (followVar state) $ Just $ EID eid
                       tellPlayer clientChan "Follow registered"
      _ -> do e <- entityMap <$> readMVar (gameState state)
              case find (\ (_,(x,_,_,_)) -> x == Left key) (Map.assocs e) of
                Just (k,_) -> swapMVar (followVar state) (Just k)
                 *>  tellPlayer clientChan "Follow registered"
                Nothing -> tellPlayer clientChan "Player not found"
  *> return []
  where key = drop 7 text

processCommand clientChan state "lines on"
  =  writeIORef (lineVar state) (True, [])
  *> tellPlayer clientChan "Lines On"
  *> return []

processCommand clientChan state "lines off"
  =  modifyIORef (lineVar state) (\ (_ , xs) -> (False, xs))
  *> tellPlayer clientChan "Lines Off"
  *> return []



processCommand clientChan _ _
  =  tellPlayer clientChan "Command not understood"
  *> return []

tellPlayer chan text = writeChan chan $ encode $ Chat $ "\194\167\&6" ++ text
     
outboundLogic :: Chan ByteString ->
                 ProxyState ->
                 Message ->
                 IO [Message]
outboundLogic clientChan state msg = do

  (recording, macros) <- readIORef $ lineVar state

  case msg of
    PlayerPosition {} -> return [msg]
    PlayerPositionLook {} -> return [msg]
    PlayerLook {} -> return [msg]
    Player {} -> return [msg]
    KeepAliv -> return [msg]
    PlayerDigging Digging x y z face -> do
     n <- readIORef $ digVar state
     return $ replicate n msg
    PlayerBlockPlacement x y z _ (Just (IID 0x15B, _, _)) -> do
      bm <- blockMap <$> readMVar (gameState state)
      let (chunkC,blockC) = decomposeCoords x (fromIntegral y) z
      case Map.lookup chunkC bm of
        Just (arr,_) -> do
          blockId <- readArray arr blockC
          if (blockId /= Air) then do
            tellPlayer clientChan "Glass attack!"
            let coords = chunkedNearby (x, fromIntegral y, z)
            coords' <- filter (not . null . snd) <$> mapM (filterGlassUpdate bm blockId) coords
            glassMsgs <- mapM (makeGlassUpdate bm blockId) coords'
            print glassMsgs
            writeChan clientChan $ runPut $ traverse_ putJ $ Prelude.concat glassMsgs
            modifyMVar_ (restoreVar state) $ \ m -> 
              let m' = foldl' (\ m (chunk,blocks) -> Map.alter (Just . Set.union (Set.fromList blocks) . fromMaybe Set.empty) chunk m) m coords'
              in return $! m'
            return []
           else return [msg]
         where
        _ -> return [msg]
    PlayerBlockPlacement x1 y1 z1 f o | recording -> case macros of
      [PlayerBlockPlacement x y z f o] -> do
        writeIORef (lineVar state) (recording, [msg])
        if x == x1 && y == y1 then return [PlayerBlockPlacement x y z2 f o | z2 <- [min z z1 .. max z z1]]
         else if x == x1 && z == z1 then return [PlayerBlockPlacement x y2 z f o | y2 <- [min y y1 .. max y y1]]
         else if z == z1 && y == y1 then return [PlayerBlockPlacement x2 y z f o | x2 <- [min x x1 .. max x x1]]
         else return [msg]
               
      _ -> do writeIORef (lineVar state) (recording, [msg]) *> return [msg]
    Chat ('\\':xs) -> processCommand clientChan state xs
                
    _ -> do putStrLn $ "outbound: " ++ show msg
            return [msg]

lookupBlock bm chunkC blockC = do
  case Map.lookup chunkC bm of
    Nothing -> return Nothing
    Just (arr,_) -> Just <$> readArray arr blockC

lookupBlock' bm chunkC blockC = do
  case Map.lookup chunkC bm of
    Nothing -> return Nothing
    Just (blockArray,metaArray) -> do
      x <- readArray blockArray blockC
      y <- readArray metaArray blockC
      return (Just (x,y))

filterGlassUpdate bm victim ((cx,cz), blocks) = do
  xs <- filterM ( \ c -> (== Just victim) <$> lookupBlock bm (cx,cz) c  ) blocks
  return ((cx,cz),xs)

makeGlassUpdate :: BlockMap -> BlockId -> ((Int32,Int32),[(Int8, Int8,Int8)]) -> IO [Message]
makeGlassUpdate bm victim ((cx,cz), coords) =
  if null coords then return []
   else return [MultiblockChange cx cz [(packCoords' c, Glass, 0) | c <- coords]]

makeRestore :: BlockMap -> Map (Int32,Int32) (Set (Int8,Int8,Int8)) -> IO [Message]
makeRestore bm restoreMap = do
 catMaybes <$> for (Map.toList restoreMap) (\ (chunk, blocks) ->
                     for (Map.lookup chunk bm)   (\ (blockArray, metaArray) ->
                     fmap ( \ xs -> MultiblockChange (fst chunk) (snd chunk) [ (packCoords' c, ty, fromIntegral met) | (c,ty,met) <- xs])   (
                     for (Set.toList blocks)     (\ block -> do
                       x <- readArray blockArray block
                       y <- readArray metaArray block
                       return (block,x,y)))))
 
                     
 

packCoords' (x,y,z) = fromIntegral (fromIntegral x `shiftL` 12
                                      .|. fromIntegral z `shiftL` 8
                                      .|. fromIntegral y :: Word16)
 

chunkedNearby coord = Map.toList $ Map.fromListWith (++) $ map (\ (x,y) -> (x,[y])) $ map (\ (x,y,z) -> decomposeCoords x y z) $ nearby coord

nearby (x,y,z) = sphere
  where radius = 10
        inRange (x1,y1,z1) = squared radius > (squared (x1-x) + squared (y1-y) + squared (z1-z))
        squared x = x * x
        box = (,,) <$> [x-radius .. x+radius] <*> [y-radius .. y+radius] <*> [z-radius .. z+radius]
        sphere = filter inRange box

-- | Read a message from the ByteString, process it with the given
-- continuation, and serialize all resulting mesages to the given channel.
proxy1 :: Chan ByteString -> (Message -> IO [Message]) -> Message
       -> IO ()
proxy1 sock f msg = do
  msgs <- f msg
  unless (null msgs) $ writeChan sock $ runPut $ traverse_ putMessage msgs

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ v f = atomicModifyIORef v $ \ x -> (f x, ())

