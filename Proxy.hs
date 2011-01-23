module Main where

import Protocol

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.IORef
import Data.Int
import Data.Map (Map)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import System.Environment
import System.Exit
import qualified Data.Map as Map
import qualified Network

type EntityMap = Map Int32 (Either String MobId, Int32, Int32, Int32)

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
  emap <- newIORef Map.empty
  follow <- newIORef Nothing
  chan <- newChan
  schan <- newChan
  _ <- forkIO $ do
          sbs <- getContents s
          proxy1 sbs chan (inboundLogic follow emap)
         `finally` putMVar var "inbound"
  _ <- forkIO $ forever (sendAll c =<< readChan chan )
                  `finally` putMVar var "inbound network" 
  _ <- forkIO $ forever (sendAll s =<< readChan schan )
                 `finally` putMVar var "outbound network" 
  _ <- forkIO $ do
          cbs <- getContents c
          proxy1 cbs schan (outboundLogic follow emap)
         `finally` putMVar var "outbound"
  who <- takeMVar var
  putStr who
  putStrLn " died"
  exitFailure

-- | Update the state of the entity map and return the Entity ID
-- of the entity that changed, if any.
updateEntityMap :: IORef EntityMap -> IO (Maybe Int32)
updateEntityMap emap (NamedEntitySpawn eid name x y z _ _ _) = do
  atomicModifyIORef_ emap $ Map.insert eid (Left name, x, y, z)
  return (Just eid)

updateEntityMap emap (MobSpawn eid ty x y z _ _ _) = do
  atomicModifyIORef_ emap $ Map.insert eid (Right ty, x, y, z)
  return (Just eid)

updateEntityMap emap (EntityTeleport eid x y z _ _) = do
  atomicModifyIORef_ emap $ Map.update (\ (ty,_,_,_) -> Just (ty, x, y, z)) eid
  return (Just eid)

updateEntityMap emap (EntityRelativeMove eid dX dY dZ) = do
  atomicModifyIORef_ emap
         $ Map.update (\ (ty,x,y,z) -> Just (ty, x + fromIntegral dX,
                                                 y + fromIntegral dY,
                                                 z + fromIntegral dZ)) eid
  return (Just eid)

updateEntityMap emap (EntityLookMove eid dX dY dZ _ _) = do
  atomicModifyIORef_ emap
         $ Map.update (\ (ty,x,y,z) -> Just (ty, x + fromIntegral dX,
                                                 y + fromIntegral dY,
                                                 z + fromIntegral dZ)) eid
  return (Just eid)

updateEntityMap emap (DestroyEntity eid) = do
  atomicModifyIORef_ emap $ Map.delete eid
  return (Just eid)

updateEntityMap _ _ = return Nothing


inboundLogic :: IORef (Maybe Int32) ->
                IORef  ->
                Message ->
                IO [Message]
inboundLogic follow emap msg = do
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

  changedEid <- updateEntityMap emap msg

  interested <- readIORef follow
  case interested of
    Just ieid | interested == changedEid -> do
     e <- readIORef emap
     case Map.lookup ieid e of
       Just (_ty, x, y, z) ->
         return [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32),msg]
       _ -> return [msg]
    _ -> return [msg]
     
outboundLogic :: IORef (Maybe Int32) ->
                 IORef EntityMap ->
                 Message ->
                 IO [Message]
outboundLogic follow emap msg = do
  case msg of
    PlayerPosition {} -> return [msg]
    PlayerPositionLook {} -> return [msg]
    PlayerLook {} -> return [msg]
    Player {} -> return [msg]
    KeepAliv -> return [msg]
    Chat ('F':xs) -> do case reads xs of
                          [(eid,_)] -> writeIORef follow eid
                          _ -> return ()
                        return []
    Chat "E" -> do
       e <- readIORef emap
       print $ Map.map (\ (ty,x,y,z) -> (ty, x`div`32, y`div`32,z`div`32)) e
       return []
                
    _ -> do putStrLn $ "outbound: " ++ show msg
            return [msg]

-- | Read a message from the ByteString, process it with the given
-- continuation, and serialize all resulting mesages to the given channel.
proxy1 :: ByteString -> Chan ByteString -> (Message -> IO [Message])
       -> IO a
proxy1 bs sock f = do
  let (msg, bs') = splitMessage bs
  msgs <- f msg
  unless (null msgs) $ writeChan sock $ runPut $ traverse_ putMessage msgs
  proxy1 bs' sock f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ v f = atomicModifyIORef v $ \ x -> (f x, ())

