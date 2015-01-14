{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module ConnectFour.Server where

  import Data.Map.Strict
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
  type TCPClient = Handle
  type WSClient = EIO.Socket

  data Server = Server {
    tcpClients :: TVar (Map ID TCPClient),
    wsClients :: TVar (Map ID WSClient)
  }

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
    if length args < 2 then
      sendMessageTCP handle "First identify yourself!"
    else
      case (head args) of
        (Protocol.handshake -> True) ->
          do
            handshakeTCP (args !! 1) handle state
            _ <- forkIO $ processCommandTCP handle state
            return ()
        _ -> sendMessageTCP handle "First identify yourself!"

  processCommandTCP :: Handle -> Server -> IO ()
  processCommandTCP handle state = forever $ do
    line <- hGetLine handle
    broadcastTCP state line
    broadcastWS state line

  handshake :: forall a. ID -> a -> TVar (Map ID a) -> IO ()
  handshake name handle clients = do
    atomically $ do
      clientMap <- readTVar clients
      writeTVar clients $ Map.insert name handle clientMap

  handshakeWS :: ID -> EIO.Socket -> Server -> IO ()
  handshakeWS name socket Server{wsClients=clients} = do
    handshake name socket clients

  handshakeTCP :: ID -> Handle -> Server -> IO ()
  handshakeTCP name handle Server{tcpClients=clients} = do
    handshake name handle clients

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