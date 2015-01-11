{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ConnectFour.Server where

  import Debug.Trace

  import Data.Map.Strict

  import Control.Applicative
  import GHC.Conc (unsafeIOToSTM)
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM
  import Control.Monad.IO.Class (MonadIO, liftIO)
  import Control.Monad (forever)
  import Control.Monad.State.Strict (StateT)
  import Control.Monad.Trans.Reader (ReaderT)

  import Network (accept, Socket)
  import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode (..),Handle)

  import qualified Data.Map as Map
  import qualified Data.Text as Text
  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID
  import qualified Network.EngineIO as EIO

  type Name = String
  type TCPClient = Handle
  type WSClient = EIO.Socket

  data Server = Server {
    tcpClients :: TVar (Map Name TCPClient),
    wsClients :: TVar (Map Name WSClient)
  }

  socketHandlerWS :: MonadIO m => Server -> EIO.Socket -> m EIO.SocketApp
  socketHandlerWS state socket = do
    liftIO $ handshakeWS socket state

    return EIO.SocketApp {
      EIO.saApp = forever $ do
        (EIO.TextPacket packet) <- liftIO $ atomically $ EIO.receive socket
        broadcastTCP state (Text.unpack packet)
        broadcastWS state (Text.unpack packet)
    , EIO.saOnDisconnect = return ()
    }

  socketHandlerTCP :: Server -> Socket -> IO ()
  socketHandlerTCP state socket = forever $ do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering
    handshakeTCP handle state
    forkIO $ commandTCP handle state

  commandTCP :: Handle -> Server -> IO ()
  commandTCP handle state = forever $ do
    line <- hGetLine handle
    broadcastTCP state line
    broadcastWS state line

  handshakeWS :: EIO.Socket -> Server -> IO ()
  handshakeWS socket Server{wsClients=clients} = do
    uuid <- UUID.toString <$> UUID.nextRandom

    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert uuid socket clientMap

  handshakeTCP :: Handle -> Server -> IO ()
  handshakeTCP handle Server{tcpClients=clients} = do
    uuid <- UUID.toString <$> UUID.nextRandom
    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert uuid handle clientMap

  sendMessageWS :: EIO.Socket -> String -> IO ()
  sendMessageWS socket msg = atomically $ do
    EIO.send socket (EIO.TextPacket $ Text.pack msg)

  broadcastWS :: Server -> String -> IO ()
  broadcastWS Server{wsClients=clients} msg = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessageWS socket msg) (Map.elems clientMap)

  sendMessageTCP :: Handle -> String -> IO ()
  sendMessageTCP handle msg = hPutStrLn handle msg

  broadcastTCP :: Server -> String -> IO ()
  broadcastTCP Server{tcpClients=clients} msg = do
    clientMap <- readTVarIO clients
    mapM_ (\handle -> sendMessageTCP handle msg) (Map.elems clientMap)
