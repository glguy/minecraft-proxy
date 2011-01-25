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
import Data.List (isPrefixOf)
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
          proxy1 sbs chan (inboundLogic follow emap glassvar)
         `finally` putMVar var "inbound"
  _ <- forkIO $ forever (sendAll c =<< readChan chan )
                  `finally` putMVar var "inbound network" 
  _ <- forkIO $ forever (sendAll s =<< readChan schan )
                 `finally` putMVar var "outbound network" 
  _ <- forkIO $ do
          cbs <- getContents c
          proxy1 cbs schan (outboundLogic chan follow emap glassvar)
         `finally` putMVar var "outbound"
  who <- takeMVar var
  putStr who
  putStrLn " died"
  exitFailure

processChunk sx sy sz bs = compress bigbs'
  where
  bigbs = decompress bs
  block_count = (fromIntegral sx + 1)
              * (fromIntegral sy + 1)
              * (fromIntegral sz + 1)

  (blocks,bigbs1) = L.splitAt block_count bigbs
  blocks' = L.map makeClear blocks
  bigbs' = L.append blocks' bigbs1

  makeClear 0x01 = 0x14
  makeClear 0x02 = 0x14
  makeClear 0x03 = 0x14
  makeClear 0x38 = 0x59
  makeClear x    = x

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
    Mapchunk x y z sx sy sz bs -> return ()
    _ -> putStrLn $ "inbound: " ++ show msg

{-
  changedEid <- atomicModifyIORef emap $ \ gs ->
                       let (change, gs') = updateGameState msg gs
                       in (gs', gs' `seq` change)
  changedEid `seq` return ()

  glass <- readIORef glassvar
  let msg' = case msg of
               Mapchunk x y z sx sy sz bs
                 | glass -> Mapchunk x y z sx sy sz $ processChunk sx sy sz bs
               _ -> msg

  interested <- readIORef follow
  case interested of
    Just ieid | interested == changedEid -> do
     e <- entityMap <$> readIORef emap
     case Map.lookup ieid e of
       Just (_ty, x, y, z) ->
         return [SpawnPosition (x `div` 32) (y `div` 32) (z `div` 32),msg']
       _ -> return [msg']
    _ -> return [msg']
  -}
  return [msg]

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
    Chat ('\\':xs) -> processCommand cchan follow emap glassvar xs
                
    _ -> do putStrLn $ "outbound: " ++ show msg
            return [msg]

-- | Read a message from the ByteString, process it with the given
-- continuation, and serialize all resulting mesages to the given channel.
proxy1 :: ByteString -> Chan ByteString -> (Message -> IO [Message])
       -> IO a
proxy1 bs sock f = do
  let (msg, bs') = splitMessage bs
  print msg
  msgs <- f msg
  unless (null msgs) $ writeChan sock $ runPut $ traverse_ putMessage msgs
  proxy1 bs' sock f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ v f = atomicModifyIORef v $ \ x -> (f x, ())

