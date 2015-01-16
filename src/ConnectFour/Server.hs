{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module ConnectFour.Server where

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

  type ID = String
  data TCPClient = TCPClient {
    tcpHandle :: Handle,
    tcpChat :: Bool,
    tcpChallenge :: Bool,
    tcpLeaderboard :: Bool
  }
  data WSClient = WSClient {
    wsSocket :: EIO.Socket,
    wsChat :: Bool,
    wsChallenge :: Bool,
    wsLeaderboard :: Bool
  }

  data Server = Server {
    tcpClients :: TVar (Map ID TCPClient),
    wsClients :: TVar (Map ID WSClient)
  }

  nameInUse :: String -> Server -> IO (Bool)
  nameInUse n Server{tcpClients=tcp, wsClients=ws} =
    atomically $ do
      tcpClientMap <- readTVar tcp
      wsClientMap <- readTVar ws
      return $ or $ map (\name -> name == n) (Map.keys tcpClientMap ++ Map.keys wsClientMap)

  handleSocketWS :: MonadIO m => Server -> EIO.Socket -> m EIO.SocketApp
  handleSocketWS state socket = do
    liftIO $ do
      uuid <- UUID.toString <$> UUID.nextRandom
      handshakeWS uuid socket state

    return EIO.SocketApp {
      EIO.saApp = processCommandWS socket state
    , EIO.saOnDisconnect = return ()
    }

  processCommandWS :: EIO.Socket -> Server -> IO ()
  processCommandWS socket state = forever $ do 
    (EIO.TextPacket packet) <- liftIO $ atomically $ EIO.receive socket
    broadcastTCP state (Text.unpack packet)
    broadcastWS state (Text.unpack packet)

  handleSocketTCP :: Server -> Socket -> IO ()
  handleSocketTCP state socket = forever $ do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering

    line <- hGetLine handle
    args <- return $ splitOn " " line
    case args of
      (Protocol.handshake -> True) ->
        do
          handshake <- handshakeTCP args handle state
          if handshake then do
            _ <- forkIO $ processCommandTCP handle state
            return ()
          else
            sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorNameInUse
      _ -> sendMessageTCP TCPClient{tcpHandle=handle} "First identify yourself!"

  processCommandTCP :: Handle -> Server -> IO ()
  processCommandTCP handle state = forever $ do
    line <- hGetLine handle
    args <- return $ splitOn " " line
    broadcastTCP state line
    broadcastWS state line

  handshake :: forall a. ID -> a -> TVar (Map ID a) -> IO ()
  handshake name client clients = do
    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert name client clientMap

  handshakeWS :: String -> EIO.Socket -> Server -> IO ()
  handshakeWS name socket Server{wsClients=clients} = do
    handshake name client clients where
      client = WSClient { wsSocket = socket }

  handshakeTCP :: [String] -> Handle -> Server -> IO (Bool)
  handshakeTCP args handle s@Server{tcpClients=clients} = do
    name <- return $ args !! 1
    inUse <- nameInUse name s
    if inUse then do
      return False
    else do
      handshake (args !! 1) client clients
      return True
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

  broadcastWS :: Server -> String -> IO ()
  broadcastWS Server{wsClients=clients} msg = broadcast sendMessageWS clients msg

  broadcastTCP :: Server -> String -> IO ()
  broadcastTCP Server{tcpClients=clients} msg = broadcast sendMessageTCP clients msg
