module Main where

import Protocol

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Put (runPut)
import Data.Foldable
import Data.IORef
import Data.Int
import Data.Map (Map)
import qualified Network
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import System.Environment
import System.Exit
import qualified Data.Map as Map

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
          proxy1 "inbound" sbs chan (inboundLogic follow emap)
         `finally` putMVar var "inbound"
  _ <- forkIO $ forever (sendAll c =<< readChan chan )
                  `finally` putMVar var "inbound network" 
  _ <- forkIO $ forever (sendAll s =<< readChan schan )
                 `finally` putMVar var "outbound network" 
  _ <- forkIO $ do
          cbs <- getContents c
          proxy1 "outbound" cbs schan (outboundLogic follow emap)
         `finally` putMVar var "outbound"
  who <- takeMVar var
  putStr who
  putStrLn " died"
  exitFailure

inboundLogic :: IORef (Maybe Int32) ->
                IORef (Map Int32 (Either String MobId, Int32, Int32, Int32)) ->
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
  changedEid <- case msg of
    NamedEntitySpawn eid name x y z _ _ _ -> do
      atomicModifyIORef_ emap $ Map.insert eid (Left name,x :: Int32 ,y :: Int32,z :: Int32)
      return (Just eid)
    MobSpawn eid ty x y z _ _ _ -> do
      atomicModifyIORef_ emap $ Map.insert eid (Right ty, x :: Int32 ,y :: Int32,z :: Int32)
      return (Just eid)
    EntityTeleport eid x y z _ _ -> do
      atomicModifyIORef_ emap
         $ Map.update (\ (ty,_,_,_) -> Just (ty, x, y, z)) eid
      return (Just eid)
    EntityRelativeMove eid dX dY dZ -> do
      atomicModifyIORef_ emap
         $ Map.update (\ (ty,x,y,z) -> Just (ty, x + fromIntegral dX,
                                                 y + fromIntegral dY,
                                                 z + fromIntegral dZ)) eid
      return (Just eid)
    EntityLookMove eid dX dY dZ _ _ -> do
      atomicModifyIORef_ emap
         $ Map.update (\ (ty,x,y,z) -> Just (ty, x + fromIntegral dX,
                                                 y + fromIntegral dY,
                                                 z + fromIntegral dZ)) eid
      return (Just eid)
    DestroyEntity eid -> do
      atomicModifyIORef_ emap $ Map.delete eid
      return Nothing
    _ -> return Nothing
  
  interested <- readIORef follow
  case interested of
    Just ieid | interested == changedEid -> do
     e <- readIORef emap
     case Map.lookup ieid e of
       Just (_ty, x, y, z) ->
         return [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32),msg]
       _ -> return [msg]
    _ -> return [msg]
     
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ v f = atomicModifyIORef v $ \ x -> (f x, ())

outboundLogic :: IORef (Maybe Int32) ->
                 IORef (Map Int32 (Either String MobId, Int32, Int32, Int32)) ->
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

proxy1 :: String -> ByteString -> Chan ByteString -> (Message -> IO [Message])
       -> IO a
proxy1 label bs sock f = do
  let (msg, bs') = splitMessage bs
  msgs <- f msg
  unless (null msgs) $ writeChan sock $ runPut $ traverse_ putMessage msgs
  proxy1 label bs' sock f
