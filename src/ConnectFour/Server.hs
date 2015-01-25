{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module ConnectFour.Server where

  -- import Debug.Trace

  import Data.Aeson ((.=))
  import Data.Map.Strict hiding (map, (\\))
  import Data.List.Split
  import Data.List

  import Control.Applicative ((<$>))
  import Control.Exception (try)
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (newTVarIO, readTVar, readTVarIO, writeTVar, atomically, TVar)
  import Control.Monad (forever, when)
  import Control.Monad.IO.Class (MonadIO, liftIO)

  import Network (accept, Socket)
  import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode (..), Handle)
  import System.IO.Error (isEOFError)

  import qualified Data.Aeson as Aeson
  import qualified Data.ByteString.Lazy as BSL
  import qualified Data.Map as Map
  import qualified Data.Text as Text
  import qualified Data.Text.Encoding as Text
  import qualified Data.UUID as UUID
  import qualified Data.UUID.V4 as UUID
  import qualified Network.EngineIO as EIO

  import qualified ConnectFour.Protocol as Protocol
  import ConnectFour.Game as Game

  data TCPClient = TCPClient {
    tcpName :: String,
    tcpHandle :: Handle,
    tcpChat :: Bool,
    tcpChallenge :: Bool,
    tcpLeaderboard :: Bool
  } deriving (Eq, Show)

  instance Aeson.ToJSON TCPClient where
    toJSON (TCPClient{tcpName=name}) =
      Aeson.object [ "name" .= name ]

  data WSClient = WSClient {
    wsSocket :: EIO.Socket,
    wsChat :: Bool,
    wsChallenge :: Bool,
    wsLeaderboard :: Bool
  }

  data ServerState = ServerState {
    queue :: TVar (Maybe TCPClient),
    tcpClients :: TVar (Map String TCPClient),
    wsClients :: TVar (Map String WSClient),
    games :: TVar [ServerGame]
  }

  data ServerGame = ServerGame {
    game :: TVar Game,
    players :: [TCPClient]
  } deriving (Eq)

  whileM :: (Monad m) => m Bool -> m a -> m ()
  whileM cond body = do c <- cond
                        when c (body >> whileM cond body)

  findServerGame :: String -> [ServerGame] -> Maybe ServerGame
  findServerGame name (sg@ServerGame{players=ps}:gs) | name `elem` (map playerToName ps) = Just sg
                                                     | otherwise = findServerGame name gs
  findServerGame _ [] = Nothing

  clientsToString :: [TCPClient] -> String
  clientsToString [] = ""
  clientsToString (TCPClient{tcpName=name}:cs) = name ++ " " ++ clientsToString cs

  serverGameToClients :: ServerGame -> [TCPClient]
  serverGameToClients ServerGame{players=ps} = ps

  getCurrentPlayer :: Game -> Int
  getCurrentPlayer Game{currentPlayer=cp} = cp

  playerToName :: TCPClient -> String
  playerToName TCPClient{tcpName=name} = name

  pushUpdate :: ServerState -> Aeson.Value -> IO ()
  pushUpdate state jsonObject =
    broadcastWS (Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode $ jsonObject) state

  pushUpdateAll :: ServerState -> IO ()
  pushUpdateAll state = do
    pushUpdateTCPClients state
    pushUpdateGames state

  pushUpdateLog :: String -> String -> ServerState -> IO ()
  pushUpdateLog name msg state =
    pushUpdate state $ Aeson.object ["log" .= Aeson.object [(Text.pack name) .= msg]]

  pushUpdateTCPClients :: ServerState -> IO ()
  pushUpdateTCPClients state@ServerState{tcpClients=tcpCs} = do
    tcpClients <- Map.elems <$> readTVarIO tcpCs
    pushUpdate state $ Aeson.object ["clients" .= (Aeson.toJSON tcpClients)]

  pushUpdateGames :: ServerState -> IO ()
  pushUpdateGames state@ServerState{games=gs} = do
    serverGames <- readTVarIO gs
    games <- mapM (\ServerGame{players=tcpCs, game=g} -> do
      game <- readTVarIO g
      return $ Aeson.toJSON (tcpCs, game)
      ) serverGames
    pushUpdate state $ Aeson.object ["serverGames" .= (Aeson.toJSON games)]

  nameInUse :: String -> ServerState -> IO (Bool)
  nameInUse n ServerState{tcpClients=tcp, wsClients=ws} =
    atomically $ do
      tcpClientMap <- readTVar tcp
      wsClientMap <- readTVar ws
      return $ any (\name -> name == n) (Map.keys tcpClientMap ++ Map.keys wsClientMap)

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
    (EIO.TextPacket packet) <- atomically $ EIO.receive socket
    arg <- return $ Text.unpack packet
    case arg of
      "connected" -> pushUpdateAll state
      _ -> return ()

  handleSocketTCP :: ServerState -> Socket -> IO ()
  handleSocketTCP state socket = forever $ do
    (handle, _, _) <- accept socket
    hSetBuffering handle NoBuffering
    forkIO $ processCommandTCP handle state

  processCommandTCP :: Handle -> ServerState -> IO ()
  processCommandTCP handle state = do
    line <- hGetLine handle
    pushUpdateLog "<unknown>" line state 
    args <- return $ splitOn " " line

    case args of
      (Protocol.handshake -> True) -> do
        maybeClient <- handshakeTCP args handle state
        case maybeClient of
          Just client@TCPClient{tcpName=name} -> do
            sendMessageTCP TCPClient{tcpHandle=handle} $ Protocol.ack ++ " 000"
            forever $ do
              input <- try $ hGetLine handle
              case input of
                Left e ->
                  if isEOFError e then cleanup client state
                  else ioError e
                Right line -> do
                  pushUpdateLog name line state

                  args <- return $ splitOn " " line
                  case args of
                    (Protocol.play -> True) -> do
                      playCommand client state
                    (Protocol.move -> True) -> do
                      moveCommand client args state
                    _ -> do
                      sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- unknown command
                      cleanup client state
          Nothing -> do
            sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorNameInUse -- handshake failed
      _ -> do
        sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- no initial handshake

  playCommand :: TCPClient -> ServerState -> IO ()
  playCommand client state@ServerState{queue=q, games=gs} = do
    maybeQueuedClient <- readTVarIO q
    case maybeQueuedClient of
      Just queuedClient -> do
        if queuedClient == client then
          return ()
        else do
          atomically $ writeTVar q Nothing
          clients <- return [queuedClient, client]
          newGame <- newTVarIO initalizeGame
          game <- return ServerGame{ players = clients, game = newGame }
          atomically $ do
            games <- readTVar gs
            writeTVar gs $ games ++ [game]
          mapM_ (\client -> sendMessageTCP client (Protocol.gameStarted ++ " " ++ clientsToString clients)) clients
          pushUpdateGames state
      Nothing -> do
        atomically $ writeTVar q (Just client)
        return ()

  moveCommand :: TCPClient -> [String] -> ServerState -> IO ()
  moveCommand client@TCPClient{tcpName=name} args state@ServerState{games=gs} = do
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
              clients <- return $ serverGameToClients serverGame
              atomically $ writeTVar g game
              mapM_ (\client -> sendMessageTCP client (Protocol.moveDone ++ " " ++ show row)) clients
              pushUpdateGames state
              if Game.winningColumn game row then do
                mapM_ (\client -> sendMessageTCP client (Protocol.gameOver ++ " " ++ Protocol.false ++ " " ++ name)) clients
                shutdownServerGame serverGame state
              else if Game.fullBoard game then do
                mapM_ (\client -> sendMessageTCP client (Protocol.gameOver ++ " " ++ Protocol.true)) clients
                shutdownServerGame serverGame state
              else do
                return ()
            Nothing -> do
              sendMessageTCP client Protocol.errorInvalidMove -- move not allowed
              cleanup client state
        else do
          sendMessageTCP client Protocol.errorInvalidMove -- not clients' turn
          cleanup client state
      Nothing -> do
        sendMessageTCP client Protocol.errorInvalidMove -- no game found
        cleanup client state

  shutdownServerGame :: ServerGame -> ServerState -> IO ()
  shutdownServerGame serverGame ServerState{games=gs} = do
    serverGames <- readTVarIO gs
    atomically $ writeTVar gs (serverGames \\ [serverGame])

  cleanup :: TCPClient -> ServerState -> IO ()
  cleanup client@TCPClient{tcpName=name, tcpHandle=handle} state@ServerState{tcpClients=tcpCs, queue=q, games=gs} = do
    -- clean tcp clients
    tcpClients <- readTVarIO tcpCs
    atomically $ writeTVar tcpCs $ Map.delete name tcpClients

    -- clean queue
    queue <- readTVarIO q
    case queue of
      Just queuedClient -> do
        if queuedClient == client then
          atomically $ writeTVar q Nothing
        else return ()
      _ -> return ()

    -- clean server game
    serverGames <- readTVarIO gs
    maybeServerGame <- return $ findServerGame name serverGames
    case maybeServerGame of
      Just serverGame@ServerGame{players=ps} -> do
        mapM_ (\client -> sendMessageTCP client (Protocol.errorInvalidMove)) ps
        shutdownServerGame serverGame state
      _ -> return ()
    hClose handle
    pushUpdateAll state

  handshake :: forall a. String -> a -> TVar (Map String a) -> IO ()
  handshake name client cs = do
    atomically $ do
      clients <- readTVar cs
      writeTVar cs $ Map.insert name client clients

  handshakeWS :: String -> EIO.Socket -> ServerState -> IO ()
  handshakeWS name socket ServerState{wsClients=clients} = do
    handshake name client clients where
      client = WSClient { wsSocket = socket }

  handshakeTCP :: [String] -> Handle -> ServerState -> IO (Maybe TCPClient)
  handshakeTCP args handle state@ServerState{tcpClients=clients} = do
    name <- return $ args !! 1
    inUse <- nameInUse name state
    if inUse then do
      return Nothing
    else do
      handshake (args !! 1) client clients
      pushUpdateTCPClients state
      return $ Just client
        where
          client = TCPClient { tcpName = args !! 1,
                               tcpHandle = handle,
                               tcpChat = (args !! 2) !! 0 == Protocol.boolTrue,
                               tcpChallenge = (args !! 2) !! 1 == Protocol.boolTrue,
                               tcpLeaderboard = (args !! 2) !! 2 == Protocol.boolTrue }

  sendMessageWS :: WSClient -> Text.Text -> IO ()
  sendMessageWS WSClient{wsSocket=socket} packet = atomically $ do
    EIO.send socket (EIO.TextPacket $ packet)

  sendMessageTCP :: TCPClient -> String -> IO ()
  sendMessageTCP TCPClient{tcpHandle=handle} msg = hPutStrLn handle msg

  broadcastWS :: Text.Text -> ServerState -> IO ()
  broadcastWS packet ServerState{wsClients=clients} = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessageWS socket packet) (Map.elems clientMap)
