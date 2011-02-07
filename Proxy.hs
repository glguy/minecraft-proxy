{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array.IO
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable
import Data.IORef
import Data.Int
import Data.List (isPrefixOf, intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Set (Set)
import Data.Traversable (for)
import Network.Socket hiding (send)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents, catch)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO hiding (getContents)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import GameState
import JavaBinary
import Protocol
import ProtocolHelper

data ProxyState = PS
  { gameState :: MVar GameState
  , glassVar  :: IORef Bool
  , digThrough :: IORef Int
  , timeVar    :: IORef (Maybe Int64)
  , restoreVar :: MVar (Map (Int32,Int32) (Set (Int8, Int8, Int8)))
  , lineVar   :: IORef (Bool, [Message])
  , digVar    :: IORef Int
  , followVar :: MVar (Maybe (String, EntityId))
  , consoleFile :: Maybe String
  }

newProxyState :: Maybe String -> IO ProxyState
newProxyState consoleFile = do
  gameState  <- newMVar newGameState
  glassVar   <- newIORef False
  digThrough <- newIORef 0
  timeVar    <- newIORef Nothing
  restoreVar <- newMVar Map.empty
  lineVar    <- newIORef (False, [])
  digVar     <- newIORef 1
  followVar  <- newMVar Nothing
  return PS {..}

data Configuration = Config
  { listenHost :: Maybe HostName
  , listenPort :: ServiceName
  , configConsoleFile :: Maybe String
  , configHelp :: Bool }

defaultConfig :: Configuration
defaultConfig = Config
  { listenHost = Nothing
  , listenPort = "25565"
  , configHelp = False
  , configConsoleFile = Nothing
  }



main :: IO ()
main = withSocketsDo $ do
  (host,port,config) <- getOptions

  let passiveHints = defaultHints { addrSocketType = Stream
                                  , addrFlags = [AI_ADDRCONFIG,AI_PASSIVE] }
  proxyAIs <- getAddrInfo (Just passiveHints)
                          (listenHost config)
                          (Just (listenPort config))

  let activeHints = defaultHints { addrSocketType = Stream
                                 , addrFlags      = [AI_ADDRCONFIG] }
  serverAI <- head <$> getAddrInfo (Just activeHints) (Just host) (Just port)

  waitThreadGroup $
     map (makeListenerThread (configConsoleFile config) serverAI) proxyAIs


-- | 'makeListenerThread' binds to the specified address on the proxy
-- and makes connections to the specified server address.
makeListenerThread ::
  Maybe FilePath {- ^ console file path -} ->
  AddrInfo {- ^ Server's address information -} ->
  AddrInfo {- ^ Proxy's address information  -} ->
  IO ()
makeListenerThread consoleFile serverAI proxyAI = do
  l <- addrInfoToSocket proxyAI
  setSocketOption l ReuseAddr 1
  bindSocketToAddrInfo l proxyAI
  listen l 5

  putStr $ "Ready to accept connections on "
        ++ show (addrAddress proxyAI) ++ "\n"

  forever $
    do (clientSock, clientAddr) <- accept l
       _ <- forkIO (handleClient consoleFile serverAI clientSock clientAddr)
       return ()

handleClient ::
  Maybe FilePath {- ^ console file path -} ->
  AddrInfo    {- ^ Server's information  -} ->
  Socket      {- ^ Client's socket -}       ->
  SockAddr    {- ^ Client's address -}      ->
  IO ()
handleClient consoleFile serverAI c csa = do
  putStr "Got connection from "
  print csa

  s <- addrInfoToSocket serverAI
  connectToAddrInfo s serverAI
  proxy consoleFile c s

 `Control.Exception.catch` \ (SomeException e) -> do
      sendAll c $ encode $ Disconnect (show e)
      fail (show e)

-- | 'proxy' creates the threads necessary to proxy a Minecraft
--   connection between a client and a server socket.
proxy ::
  Maybe FilePath {- ^ console file path -} ->
  Socket {- ^ client socket -} ->
  Socket {- ^ server socket -} ->
  IO ()
proxy consoleFile c s = do

  var <- newChan
  state <- newProxyState consoleFile
  clientChan <- newChan
  serverChan <- newChan

  let bad who (SomeException e) = print e >> writeChan var who
      start who f xsm = forkIO . handle (bad who) . traverse_ f =<< xsm


  serverToProxy <- start "from server"
                     (inboundLogic clientChan state)
                     (getMessages s)
  clientToProxy <- start "from client"
                     (outboundLogic clientChan serverChan state)
                     (getMessages c)
  proxyToClient <- start "to client"  (sendAll c) (getChanContents clientChan)
  proxyToServer <- start "to server" (sendAll s) (getChanContents serverChan)

  who <- readChan var
  putStr who
  putStrLn " died"
  traverse_ killThread [serverToProxy, clientToProxy]

getMessages :: Socket -> IO [Message]
getMessages s = toMessages <$> getContents s


makeGlass :: BlockId -> BlockId
makeGlass Dirt  = Glass
makeGlass Stone = Glass
makeGlass Grass = Glass
makeGlass Snow  = Glass
makeGlass other = other

inboundLogic ::
  Chan ByteString {- ^ client channel -} ->
  ProxyState                             ->
  Message                                ->
  IO ()
inboundLogic clientChan state msg = do

  case msg of
    NamedEntitySpawn _ name _ _ _ _ _ _ ->
      tellPlayer clientChan $ name ++ " in range"
    _ -> return ()
  -- Track entities
  changedEid <- modifyMVar (gameState state) $ \ gs -> do
    (change, gs') <- updateGameState msg gs
    gs' `seq` return (gs', change)

  -- Global glass modifications
  glass <- readIORef (glassVar state)
  time  <- readIORef (timeVar state)
  let msg' = case msg of
        Mapchunk (chunk, Just (bs, a, b, c))
          | glass -> Mapchunk (chunk, Just (fmap makeGlass bs, a, b, c))
        Mapchunk (chunk, Nothing) -> Chat $ "Bad map chunk " ++ show chunk
        TimeUpdate {} -> case time of
          Nothing -> msg
          Just t -> TimeUpdate t
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

processCommand clientChan state "console-echo"
  = case consoleFile state of
      Nothing -> tellPlayer clientChan "Console not enabled"
      Just fp -> makePipeListener clientChan fp

processCommand clientChan _ "help"
  = traverse_ (tellPlayer clientChan) helpMessage

processCommand clientChan state "list"
  = do players <- listPlayers state
       tellPlayer clientChan $ "Players: " ++ intercalate ", " players

processCommand clientChan state "status"
  = traverse_ (tellPlayer clientChan) =<< statusMessages state

processCommand clientChan state "time off"
  = writeIORef (timeVar state) Nothing
  *> tellPlayer clientChan "Time passing"

processCommand clientChan state text | "time " `isPrefixOf` text
  = case reads (drop 5 text) of

      [(n,_)] | 0 <= n && n <= 24000
        -> writeIORef (timeVar state) (Just n)
        *> tellPlayer clientChan "Time fixed"

      _ -> tellPlayer clientChan "Unable to parse time"

processCommand clientChan _ text | "echo " `isPrefixOf` text
  = case reads (drop 5 text) of
      [(msg,_)] -> sendMessages clientChan [msg]
      _         -> tellPlayer clientChan "Unable to parse message"

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
      Nothing -> tellPlayer clientChan "Follow disabled - spawn point unknown"
      Just (x,y,z) -> sendMessages clientChan [SpawnPosition x y z]
                   *> tellPlayer clientChan "Follow disabled - compass restored"
    return Nothing

processCommand clientChan state text | "follow " `isPrefixOf` text
  = do e <- entityMap <$> readMVar (gameState state)
       let key = drop 7 text
       case find (\ (_,(x,_,_,_)) -> x == Left key) (Map.assocs e) of
         Just (k,_) -> swapMVar (followVar state) (Just (key,k))
                    *> tellPlayer clientChan "Follow registered"
         Nothing    -> tellPlayer clientChan "Player not found"

processCommand clientChan state "lines on"
  =  writeIORef (lineVar state) (True, [])
  *> tellPlayer clientChan "Lines On"

processCommand clientChan state "lines off"
  =  modifyIORef (lineVar state) (\ (_ , xs) -> (False, xs))
  *> tellPlayer clientChan "Lines Off"

processCommand clientChan _ _
  =  tellPlayer clientChan "Command not understood"

-- | Serialize a list of messages and write them as one unit to
-- the specified channel.
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

  (recording, macros) <- readIORef (lineVar state)
  shiftCount          <- readIORef (digThrough state)

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

    PlayerBlockPlacement x y z _ (Just (Clock, _, _)) -> do
      attacked <- glassAttack clientChan state x y z
      return $ if attacked then [] else [msg]

    PlayerBlockPlacement _ _ _ None (Just (Compass, _, _)) -> do
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
  | x == x1 && y == y1 = [PlayerBlockPlacement x y z2 f o
                             | z2 <- [min z z1 .. max z z1]]
  | x == x1 && z == z1 = [PlayerBlockPlacement x y2 z f o
                             | y2 <- [min y y1 .. max y y1]]
  | z == z1 && y == y1 = [PlayerBlockPlacement x2 y z f o
                             | x2 <- [min x x1 .. max x x1]]
  | otherwise          = [msg]

lookupBlock :: BlockMap -> ChunkLoc -> BlockLoc -> IO (Maybe BlockId)
lookupBlock bm chunkC blockC = do
  for (Map.lookup chunkC bm) $ \ (blockArray, _) ->
    readArray blockArray blockC

filterGlassUpdate ::
  BlockMap ->
  BlockId ->
  (ChunkLoc, [BlockLoc]) ->
  IO (ChunkLoc, [BlockLoc])
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
        inSphere (x1,y1,z1)
          = squared radius > ( squared (x1-x)
                             + fromIntegral (squared (y1-y))
                             + squared (z1-z))
        squared i = i * i
        box = (,,) <$> [x-radius .. x+radius]
                   <*> [y-radius .. y+radius]
                   <*> [z-radius .. z+radius]

listPlayers :: ProxyState -> IO [String]
listPlayers state = do
  e <- entityMap <$> readMVar (gameState state)
  return [ name | (Left name,_,_,_) <- Map.elems e]

-- | 'digShift' alters a set of coordinates given a depth and face.
digShift ::
  Int32 {- ^ X coordinate -} ->
  Int8  {- ^ Y coordinate -} ->
  Int32 {- ^ Z coordinate -} ->
  Face ->
  Int   {- ^ depth        -} ->
  (Int32, Int8, Int32)
digShift x y z face i = case face of
  X1 -> (x+i',y,z)
  X2 -> (x-i',y,z)
  Y1 -> (x,y+i',z)
  Y2 -> (x,y-i',z)
  Z1 -> (x,y,z+i')
  Z2 -> (x,y,z-i')
  _  -> (x,y,z)

 where i' :: Num a => a
       i' = fromIntegral i

-- | 'helpMessage' is sent to the client in response to "\help"
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
  ,"\\time <n>             - Set time to value 0--24000"
  ,"\\time off             - Allow time to pass"
  ,"\\list                 - List nearby players"
  ,"Right-click with clock - Glassify local sphere"
  ,"Right-click with compass - Revert glass spheres"
  ]

-- | 'statusMessage' computes a list of messages to send to the client
-- based on the current proxy state.
statusMessages :: ProxyState -> IO [String]
statusMessages state = sequence
  [ return "Proxy Status ------------------------"
  , followStatus  <$> readMVar (followVar state)
  , digStatus     <$> readIORef (digVar state)
  , glassStatus   <$> readIORef (glassVar state)
  , throughStatus <$> readIORef (digThrough state)
  , lineStatus    <$> readIORef (lineVar state)
  , timeStatus    <$> readIORef (timeVar state)
  , restoreStatus <$> readMVar (restoreVar state) 
  ]
  where
  onOff True  = highlight "on"
  onOff False = highlight "off"

  followStatus Nothing = "Following spawn point"
  followStatus (Just (name,_)) = "Following player: " ++ highlight name

  digStatus x = "Dig speed: " ++ highlight (show x)

  glassStatus b = "Glass chunks: "++ onOff b

  throughStatus n = "Dig through: " ++ highlight (show n)

  lineStatus (b,_) = "Line mode: " ++ onOff b

  timeStatus Nothing = "Time mode normal"
  timeStatus (Just x) = "Time fixed at: " ++ highlight (show x)

  restoreStatus m = "Blocks to restore: " ++
    highlight (show (Data.Foldable.sum (fmap Set.size (Map.elems m))))

-- Threading helpers

-- | 'waitThreadGroup' runs multiple computations in parallel
-- and kills the group if one of those computations terminates
waitThreadGroup ::
  [IO ()] {- ^ Operations to run in parallel -} ->
  IO ()
waitThreadGroup xs = do
  var <- newEmptyMVar
  threadIds <- for xs $ \ x -> forkIO $ x
                   `catch` \ (SomeException e) -> 
                     do print e
                        putMVar var ()
  takeMVar var
  traverse_ killThread threadIds


-- Networking Helpers

addrInfoToSocket :: AddrInfo -> IO Socket
addrInfoToSocket AddrInfo {..} = socket addrFamily addrSocketType addrProtocol

bindSocketToAddrInfo :: Socket -> AddrInfo -> IO ()
bindSocketToAddrInfo sock AddrInfo {..} = bindSocket sock addrAddress

connectToAddrInfo :: Socket -> AddrInfo -> IO ()
connectToAddrInfo sock AddrInfo {..} = connect sock addrAddress


-- Command-line option processing

options :: [OptDescr (Configuration -> Configuration)]
options =
  [ Option ['l'] ["listen-host"]
     (ReqArg (\ str c -> c { listenHost = Just str }) "PROXY-HOSTNAME")
     "Optional hostname to bind to"
  , Option ['p'] ["listen-port"]
     (ReqArg (\ str c -> c { listenPort = str }) "PROXY-SERVICENAME")
     "Optional port to bind to"
  , Option ['h'] ["help"]
     (NoArg (\ c -> c { configHelp = True }))
     "Print this list"
  , Option ['c'] ["console"]
     (ReqArg (\ str c -> c { configConsoleFile = Just str }) "PATH")
     "Optional console file or named-pipe name"
  ]

usageText :: String
usageText =
  "minecraft-proxy <FLAGS> SERVER-HOSTNAME [SERVER-PORT]\n\
  \\n\
  \example: minecraft-proxy -l localhost -p 2000 example.com\n"

getOptions :: IO (String, String, Configuration)
getOptions = do
  (fs, args, errs) <- getOpt Permute options <$> getArgs
  let config = foldl' (\ c f -> f c) defaultConfig fs
  unless (null errs) $ do
    traverse_ (hPutStrLn stderr) errs
    hPutStrLn stderr $ usageInfo usageText options
    exitFailure
  when (configHelp config) $ do
    hPutStrLn stderr $ usageInfo usageText options
    exitSuccess
  case args of
    [host,port] -> return (host,port,config)
    [host] -> return (host,defaultMinecraftPort,config)
    (_:_:_:_) ->
         do hPutStrLn stderr "Too many arguments\n"
            hPutStrLn stderr $ usageInfo usageText options
            exitFailure
    _ -> do hPutStrLn stderr "Required server-host missing\n"
            hPutStrLn stderr $ usageInfo usageText options
            exitFailure

-- External command echo support

makePipeListener :: Chan ByteString -> FilePath -> IO ()
makePipeListener clientChan consoleFile =
  do _ <- forkIO $ do
       xs <- lines <$> readFile consoleFile
       tellPlayer clientChan "Console opened"
       traverse_ process xs
       tellPlayer clientChan "Console closed"
      `Control.Exception.catch` \ (SomeException e) ->
        tellPlayer clientChan $ "Console failed: " ++ show e
     return ()
 where
  process x = case reads x of
    [(y,_)] -> sendMessages clientChan [y]
    _ -> hPutStrLn stderr $ "Failed to parse: " ++ show x
