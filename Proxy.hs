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
import Data.IORef
import Data.Int
import Data.Map (Map)
import Data.List (groupBy,sort,isPrefixOf)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import System.Environment
import System.Exit
import qualified Data.Map as Map
import qualified Network
import JavaBinary
import GameState

main :: IO ()
main = do
  (host,port) <- do
    args <- getArgs
    case args of
      [host, port] -> return (host, port)
      _ -> do putStrLn "Usage: minecraft-proxy host port"
              exitFailure

  l <- Network.listenOn (Network.PortNumber (fromInteger 25564))
  putStrLn "Ready"
  (c, csa) <- accept l
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

proxy :: Socket -> Socket -> IO a
proxy c s = do
  var <- newEmptyMVar
  emap <- newIORef newGameState
  glassvar <- newIORef False
  follow <- newIORef Nothing
  chan <- newChan
  schan <- newChan
  _ <- forkIO $ do
          sbs <- getContents s
          traverse_ (proxy1 chan (inboundLogic follow emap glassvar))
                    (toMessages sbs)
         `finally` putMVar var "inbound"
  _ <- forkIO $ forever (sendAll c =<< readChan chan )
                  `finally` putMVar var "inbound network" 
  _ <- forkIO $ forever (sendAll s =<< readChan schan )
                 `finally` putMVar var "outbound network" 
  _ <- forkIO $ do
          cbs <- getContents c
          traverse_ (proxy1 schan (outboundLogic chan follow emap glassvar))
                    (toMessages cbs)
         `finally` putMVar var "outbound"
  who <- takeMVar var
  putStr who
  putStrLn " died"
  exitFailure

inboundLogic :: IORef (Maybe EntityId) ->
                IORef GameState ->
                IORef Bool ->
                Message ->
                IO [Message]
inboundLogic follow emap glassvar msg = do
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
    _ -> putStrLn $ "inbound: " ++ show msg

  changedEid <- atomicModifyIORef emap $ \ gs ->
                       let (change, gs') = updateGameState msg gs
                       in (gs', gs' `seq` change)
  changedEid `seq` return ()

  glass <- readIORef glassvar
  let msg' = case msg of
               Mapchunk x y z sx sy sz bs a b c
                 | glass -> Mapchunk x y z sx sy sz (map makeGlass bs) a b c
               _ -> msg
      makeGlass Dirt = Glass
      makeGlass Stone = Glass
      makeGlass Grass = Glass
      makeGlass block = block

  interested <- readIORef follow
  case interested of
    Just ieid | interested == changedEid -> do
     e <- entityMap <$> readIORef emap
     case Map.lookup ieid e of
       Just (_ty, x, y, z) ->
         return [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32),msg']
       _ -> return [msg']
    _ -> return [msg']

processCommand cchan follow emap glassvar "glass on"
  =  writeIORef glassvar True
  *> tellPlayer cchan "Glass On"
  *> return []

processCommand cchan follow emap glassvar "glass off"
  =  writeIORef glassvar False
  *> tellPlayer cchan "Glass Off"
  *> return []

processCommand cchan follow emap glassvar text
  | "follow " `isPrefixOf` text
  = case reads key of
      [(eid, _)] -> do writeIORef follow $ Just $ EID eid
                       tellPlayer cchan "Follow registered"
      _ -> do e <- entityMap <$> readIORef emap
              case find (\ (_,(x,_,_,_)) -> x == Left key) (Map.assocs e) of
                Just (k,_) -> writeIORef follow (Just k)
                 *>  tellPlayer cchan "Follow registered"
                Nothing -> tellPlayer cchan "Player not found"
  *> return []
  where key = drop 7 text

processCommand cchan follow emap glassvar _ = do
  tellPlayer cchan "Command not understood"
  return []

tellPlayer cchan text = writeChan cchan $ encode $ Chat $ "\194\167\&6" ++ text
     
outboundLogic :: Chan ByteString ->
                 IORef (Maybe EntityId) ->
                 IORef GameState ->
                 IORef Bool ->
                 Message ->
                 IO [Message]
outboundLogic cchan follow emap glassvar msg = do
  case msg of
    PlayerPosition {} -> return [msg]
    PlayerPositionLook {} -> return [msg]
    PlayerLook {} -> return [msg]
    Player {} -> return [msg]
    KeepAliv -> return [msg]
    PlayerBlockPlacement x y z _ (Just (IID 0x15B, _, _)) -> do
      tellPlayer cchan "Glass attack!"
      bm <- blockMap <$> readIORef emap
      let (chunkC,blockC) = decomposeCoords x (fromIntegral y) z
      case Map.lookup blockC =<< Map.lookup chunkC bm of
        Nothing -> return [msg]
        Just (blockId, meta) -> do
         print glassMsgs
         writeChan cchan $ runPut $ traverse_ putJ $ Prelude.concat glassMsgs
         return []
         where
         glassMsgs = map (makeGlassUpdate bm blockId) $ chunkedNearby (x, fromIntegral y, z)
    Chat ('\\':xs) -> processCommand cchan follow emap glassvar xs
                
    _ -> do putStrLn $ "outbound: " ++ show msg
            return [msg]

lookupBlock bm chunkC blockC = fmap fst $ Map.lookup blockC =<< Map.lookup chunkC bm

makeGlassUpdate :: BlockMap -> BlockId -> ((Int32,Int32),[(Int8, Int8,Int8)]) -> [Message]
makeGlassUpdate bm victim ((cx,cz), blocks)
  | null coords = []
  | otherwise   = [MultiblockChange cx cz [(packCoord c, Glass, 0) | c <- coords]]
  where
  coords = filter ( \ c -> lookupBlock bm (cx,cz) c == Just victim ) blocks
  packCoord (x,y,z) = fromIntegral x `shiftL` 12 .|. fromIntegral z `shiftL` 8 .|. fromIntegral y

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

