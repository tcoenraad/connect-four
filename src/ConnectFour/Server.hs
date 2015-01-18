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
  import ConnectFour.Game as Game

  type Name = String
  data TCPClient = TCPClient {
    tcpName :: Name,
    tcpHandle :: Handle,
    tcpChat :: Bool,
    tcpChallenge :: Bool,
    tcpLeaderboard :: Bool
  } deriving (Eq, Show)

  data WSClient = WSClient {
    wsSocket :: EIO.Socket,
    wsChat :: Bool,
    wsChallenge :: Bool,
    wsLeaderboard :: Bool
  }

  data ServerState = ServerState {
    queue :: TVar (Maybe TCPClient),
    tcpClients :: TVar (Map Name TCPClient),
    wsClients :: TVar (Map Name WSClient),
    games :: TVar [ServerGame]
  }

  data ServerGame = ServerGame {
    game :: TVar Game,
    players :: [TCPClient]
  }

  findServerGame :: String -> [ServerGame] -> Maybe ServerGame
  findServerGame name (sg@ServerGame{players=ps}:gs) | name `elem` (map playerToName ps) = Just sg
                                               | otherwise = findServerGame name gs
  findServerGame name [] = Nothing

  clientsToString :: [TCPClient] -> String
  clientsToString [] = ""
  clientsToString (TCPClient{tcpName=name}:cs) = name ++ " " ++ clientsToString cs

  serverGameToClients :: ServerGame -> [TCPClient]
  serverGameToClients ServerGame{players=ps} = ps

  getCurrentPlayer :: Game -> Int
  getCurrentPlayer Game{currentPlayer=cp} = cp

  playerToName :: TCPClient -> String
  playerToName TCPClient{tcpName=name} = name

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

  playCommand :: TCPClient -> ServerState -> IO ()
  playCommand client ServerState{queue=q, games=gs, tcpClients=tcpCs} = do
    maybeQueuedClient <- readTVarIO q
    case maybeQueuedClient of
      Just queuedClient -> do
        atomically $ writeTVar q Nothing
        clients <- return [queuedClient, client]
        newGame <- newTVarIO initalizeGame
        game <- return ServerGame{ players = clients, game = newGame }
        atomically $ do
          games <- readTVar gs
          writeTVar gs $ games ++ [game]
        mapM_ (\client -> sendMessageTCP client (Protocol.gameStarted ++ " " ++ clientsToString clients)) clients
      Nothing -> atomically $ writeTVar q (Just client)

  moveCommand :: TCPClient -> ServerState -> [String] -> IO ()
  moveCommand client@TCPClient{tcpName=name} ServerState{games=gs} args = do
    games <- readTVarIO gs
    maybeServerGame <- return $ findServerGame name games
    case maybeServerGame of
      Just serverGame@ServerGame{players=ps, game=g} -> do
        game <- readTVarIO g
        if (ps !! getCurrentPlayer game) == client then do
          row <- return $ read (args !! 1)
          maybeGame <- return $ Game.dropCoin game row
          case maybeGame of
            Just game -> do
              atomically $ writeTVar g game
              clients <- return $ serverGameToClients serverGame
              mapM_ (\client -> sendMessageTCP client (Protocol.moveDone ++ " " ++ show row)) clients
            Nothing ->
              sendMessageTCP client Protocol.errorInvalidMove -- move not allowed
        else
          sendMessageTCP client Protocol.errorInvalidMove -- not clients' turn
      Nothing -> do
        sendMessageTCP client Protocol.errorInvalidMove -- no game found

  processCommandTCP :: Handle -> ServerState -> IO ()
  processCommandTCP handle state = do
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
                  playCommand client state
                (Protocol.move -> True) -> do
                  moveCommand client state args
                _ ->
                  sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- unknown command
          Nothing -> sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorNameInUse -- handshake failed
      _ -> sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- no initial handshake

  handshake :: forall a. Name -> a -> TVar (Map Name a) -> IO ()
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
          client = TCPClient { tcpName = args !! 1,
                               tcpHandle = handle,
                               tcpChat = (args !! 2) !! 0 == Protocol.true,
                               tcpChallenge = (args !! 2) !! 1 == Protocol.true,
                               tcpLeaderboard = (args !! 2) !! 2 == Protocol.true }

  sendMessageWS :: WSClient -> String -> IO ()
  sendMessageWS WSClient{wsSocket=socket} msg = atomically $ do
    EIO.send socket (EIO.TextPacket $ Text.pack msg)

  sendMessageTCP :: TCPClient -> String -> IO ()
  sendMessageTCP TCPClient{tcpHandle=handle} msg = hPutStrLn handle msg

  broadcast :: forall a. (a -> String -> IO ()) -> TVar (Map Name a) -> String -> IO ()
  broadcast sendMessage clients msg = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessage socket msg) (Map.elems clientMap)

  broadcastWS :: ServerState -> String -> IO ()
  broadcastWS ServerState{wsClients=clients} msg = broadcast sendMessageWS clients msg

  broadcastTCP :: ServerState -> String -> IO ()
  broadcastTCP ServerState{tcpClients=clients} msg = broadcast sendMessageTCP clients msg
