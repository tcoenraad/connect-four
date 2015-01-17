{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module ConnectFour.Server where

  import Debug.Trace

  import Data.Map.Strict hiding (map)
  import Data.List.Split

  import Control.Applicative
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM
  import Control.Monad.IO.Class (MonadIO, liftIO)
  import Control.Monad (forever)

  import Network (accept, Socket)
  import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode (..), Handle)

  import qualified Data.Map as Map
  import qualified Data.Text as Text
  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID
  import qualified Network.EngineIO as EIO

  import qualified ConnectFour.Protocol as Protocol
  import ConnectFour.Game

  type ID = String
  data TCPClient = TCPClient {
    tcpHandle :: Handle,
    tcpChat :: Bool,
    tcpChallenge :: Bool,
    tcpLeaderboard :: Bool
  } deriving Show
  data WSClient = WSClient {
    wsSocket :: EIO.Socket,
    wsChat :: Bool,
    wsChallenge :: Bool,
    wsLeaderboard :: Bool
  }

  data ServerState = ServerState {
    queue :: TVar (Maybe TCPClient),
    tcpClients :: TVar (Map ID TCPClient),
    wsClients :: TVar (Map ID WSClient),
    games :: TVar (Map Int Game)
  }

  nameInUse :: String -> ServerState -> IO (Bool)
  nameInUse n ServerState{tcpClients=tcp, wsClients=ws} =
    atomically $ do
      tcpClientMap <- readTVar tcp
      wsClientMap <- readTVar ws
      return $ or $ map (\name -> name == n) (Map.keys tcpClientMap ++ Map.keys wsClientMap)

  handleSocketWS :: MonadIO m => ServerState -> EIO.Socket -> m EIO.SocketApp
  handleSocketWS state socket = do
    liftIO $ do
      uuid <- UUID.toString <$> UUID.nextRandom
      handshakeWS uuid socket state

    return EIO.SocketApp {
      EIO.saApp = processCommandWS socket state
    , EIO.saOnDisconnect = return ()
    }

  processCommandWS :: EIO.Socket -> ServerState -> IO ()
  processCommandWS socket state = forever $ do 
    (EIO.TextPacket packet) <- liftIO $ atomically $ EIO.receive socket
    broadcastTCP state (Text.unpack packet)
    broadcastWS state (Text.unpack packet)

  handleSocketTCP :: ServerState -> Socket -> IO ()
  handleSocketTCP state socket = forever $ do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ processCommandTCP handle state

  startGame :: [TCPClient] -> IO ()
  startGame clients = do
    mapM_ (\client -> sendMessageTCP client Protocol.gameStarted) clients

  processCommandTCP :: Handle -> ServerState -> IO ()
  processCommandTCP handle state@ServerState{queue=q} = do
    line <- hGetLine handle
    args <- return $ splitOn " " line

    case args of
      (Protocol.handshake -> True) -> do
        maybeClient <- handshakeTCP args handle state
        case maybeClient of
          Just client -> do
            forever $ do
              line <- hGetLine handle
              args <- return $ splitOn " " line
              case args of
                (Protocol.play -> True) -> do
                  maybeQueuedClient <- readTVarIO q
                  case maybeQueuedClient of
                    Just queuedClient -> do
                      atomically $ writeTVar q Nothing
                      startGame [queuedClient, client]
                    Nothing -> atomically $ writeTVar q (Just client)
                _ ->
                  sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- unknown command
          Nothing -> sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorNameInUse -- handshake failed
      _ -> sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- no initial handshake

  handshake :: forall a. ID -> a -> TVar (Map ID a) -> IO ()
  handshake name client clients = do
    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert name client clientMap

  handshakeWS :: String -> EIO.Socket -> ServerState -> IO ()
  handshakeWS name socket ServerState{wsClients=clients} = do
    handshake name client clients where
      client = WSClient { wsSocket = socket }

  handshakeTCP :: [String] -> Handle -> ServerState -> IO (Maybe TCPClient)
  handshakeTCP args handle s@ServerState{tcpClients=clients} = do
    name <- return $ args !! 1
    inUse <- nameInUse name s
    if inUse then do
      return Nothing
    else do
      handshake (args !! 1) client clients
      return $ Just client
        where
          client = TCPClient { tcpHandle = handle,
                               tcpChat = (args !! 2) !! 0 == Protocol.true,
                               tcpChallenge = (args !! 2) !! 1 == Protocol.true,
                               tcpLeaderboard = (args !! 2) !! 2 == Protocol.true }

  sendMessageWS :: WSClient -> String -> IO ()
  sendMessageWS WSClient{wsSocket=socket} msg = atomically $ do
    EIO.send socket (EIO.TextPacket $ Text.pack msg)

  sendMessageTCP :: TCPClient -> String -> IO ()
  sendMessageTCP TCPClient{tcpHandle=handle} msg = hPutStrLn handle msg

  broadcast :: forall a. (a -> String -> IO ()) -> TVar (Map ID a) -> String -> IO ()
  broadcast sendMessage clients msg = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessage socket msg) (Map.elems clientMap)

  broadcastWS :: ServerState -> String -> IO ()
  broadcastWS ServerState{wsClients=clients} msg = broadcast sendMessageWS clients msg

  broadcastTCP :: ServerState -> String -> IO ()
  broadcastTCP ServerState{tcpClients=clients} msg = broadcast sendMessageTCP clients msg
