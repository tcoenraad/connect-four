{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ConnectFour.Server where

  import Data.Map.Strict

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

  type ID = String
  type TCPClient = Handle
  type WSClient = EIO.Socket

  data Server = Server {
    tcpClients :: TVar (Map ID TCPClient),
    wsClients :: TVar (Map ID WSClient)
  }

  handleSocketWS :: MonadIO m => Server -> EIO.Socket -> m EIO.SocketApp
  handleSocketWS state socket = do
    liftIO $ handshakeWS socket state

    return EIO.SocketApp {
      EIO.saApp = processCommandWS
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
    handshakeTCP handle state
    forkIO $ processCommandTCP handle state

  processCommandTCP :: Handle -> Server -> IO ()
  processCommandTCP handle state = forever $ do
    line <- hGetLine handle
    broadcastTCP state line
    broadcastWS state line

  handshake :: forall a. a -> TVar (Map String a) -> IO ()
  handshake handle clients = do
    uuid <- UUID.toString <$> UUID.nextRandom

    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert uuid handle clientMap

  handshakeWS :: EIO.Socket -> Server -> IO ()
  handshakeWS socket Server{wsClients=clients} = do
    handshake socket clients

  handshakeTCP :: Handle -> Server -> IO ()
  handshakeTCP handle Server{tcpClients=clients} = do
    handshake handle clients

  sendMessageWS :: EIO.Socket -> String -> IO ()
  sendMessageWS socket msg = atomically $ do
    EIO.send socket (EIO.TextPacket $ Text.pack msg)

  sendMessageTCP :: Handle -> String -> IO ()
  sendMessageTCP handle msg = hPutStrLn handle msg

  broadcast :: forall a. (a -> String -> IO ()) -> TVar (Map ID a) -> String -> IO ()
  broadcast sendMessage clients msg = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessage socket msg) (Map.elems clientMap)

  broadcastWS :: Server -> String -> IO ()
  broadcastWS Server{wsClients=clients} msg = broadcast sendMessageWS clients msg

  broadcastTCP :: Server -> String -> IO ()
  broadcastTCP Server{tcpClients=clients} msg = broadcast sendMessageTCP clients msg