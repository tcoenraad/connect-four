{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module ConnectFour.Server where

  -- import Debug.Trace

  import Prelude hiding (lookup)
  import Data.Aeson ((.=))
  import Data.Map.Strict hiding (map, filter, (\\))
  import Data.List.Split
  import Data.List hiding (lookup)
  import Data.String.Utils

  import Control.Applicative ((<$>))
  import Control.Concurrent (forkIO)
  import Control.Concurrent.STM (newTVarIO, readTVar, readTVarIO, writeTVar, atomically, TVar)
  import Control.Monad (forever, when, filterM)
  import Control.Monad.IO.Class (MonadIO, liftIO)

  import Network (accept, Socket)
  import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode (..), Handle)
  import System.IO.Error (catchIOError, isEOFError)

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
    serverGames :: TVar [ServerGame],
    challengedTCPClients :: TVar (Map String TCPClient)
  }

  data ServerGame = ServerGame {
    game :: TVar Game,
    players :: [TCPClient]
  } deriving (Eq)

  whileM :: (Monad m) => m Bool -> m a -> m ()
  whileM cond body = do c <- cond
                        when c (body >> whileM cond body)

  boolToString :: Bool -> String
  boolToString b = if b then [Protocol.boolTrue] else [Protocol.boolFalse]

  findServerGame :: String -> ServerState -> IO (Maybe ServerGame)
  findServerGame name ServerState{serverGames=sg} = do
    serverGames <- readTVarIO sg
    return $ findServerGame' name serverGames

  findServerGame' :: String -> [ServerGame] -> Maybe ServerGame
  findServerGame' name (sg@ServerGame{players=ps}:gs) | name `elem` (map playerToName ps) = Just sg
                                                      | otherwise = findServerGame' name gs
  findServerGame' _ [] = Nothing

  clientsToString :: [TCPClient] -> String
  clientsToString [] = ""
  clientsToString [TCPClient{tcpName=name}] = name
  clientsToString (TCPClient{tcpName=name}:cs) = name ++ " " ++ clientsToString cs

  clientsToStringWithOptions :: [TCPClient] -> String
  clientsToStringWithOptions [] = ""
  clientsToStringWithOptions (TCPClient{tcpName=name, tcpChat=chat, tcpChallenge=challenge, tcpLeaderboard=leaderboard}:cs) =
    name ++ " "++ (boolToString chat) ++ (boolToString challenge) ++ (boolToString leaderboard) ++ ", " ++ clientsToStringWithOptions cs

  serverGameToClients :: ServerGame -> [TCPClient]
  serverGameToClients ServerGame{players=ps} = ps

  getCurrentPlayer :: Game -> Int
  getCurrentPlayer Game{currentPlayer=cp} = cp

  playerToName :: TCPClient -> String
  playerToName TCPClient{tcpName=name} = name

  tcpClientsInLobby :: ServerState -> IO ([TCPClient])
  tcpClientsInLobby state@ServerState{tcpClients=tcpCs} = do
    tcpClients <- Map.elems <$> readTVarIO tcpCs
    filterM (\TCPClient{tcpName=name} -> do
      serverGame <- findServerGame name state
      case serverGame of
        Just _ -> return False
        Nothing -> return True) tcpClients

  pushUpdate :: ServerState -> Aeson.Value -> IO ()
  pushUpdate state jsonObject =
    broadcastWS (Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode $ jsonObject) state

  pushUpdateAll :: ServerState -> IO ()
  pushUpdateAll state = do
    pushUpdateTCPClients state
    pushUpdateGames state

  pushUpdateLog :: String -> String -> ServerState -> IO ()
  pushUpdateLog name msg state = do
    putStrLn $ name ++ ": `" ++ msg ++ "`"
    pushUpdate state $ Aeson.object ["log" .= Aeson.object [(Text.pack name) .= msg]]

  pushUpdateTCPClients :: ServerState -> IO ()
  pushUpdateTCPClients state@ServerState{tcpClients=tcpCs} = do
    tcpLobbyClients <- tcpClientsInLobby state
    let lobbyPlayerList = clientsToStringWithOptions tcpLobbyClients
    let msg = Protocol.sendPlayers ++ " " ++ (take ((length lobbyPlayerList) - 2) lobbyPlayerList)
    mapM_ ((flip sendMessageTCP) msg) tcpLobbyClients

    tcpClients <- readTVarIO tcpCs
    pushUpdate state $ Aeson.object ["clients" .= (Aeson.toJSON tcpClients)]

  pushUpdateGames :: ServerState -> IO ()
  pushUpdateGames state@ServerState{serverGames=sg} = do
    serverGames <- readTVarIO sg
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
  handleSocketWS state@ServerState{wsClients=wsCs} socket = do
    uuid <- liftIO $ UUID.toString <$> UUID.nextRandom
    liftIO $ handshakeWS uuid socket state

    wsClients <- liftIO $ readTVarIO wsCs
    liftIO $ putStrLn $ show $ length $ Map.keys wsClients

    return EIO.SocketApp {
      EIO.saApp = processCommandWS socket state
    , EIO.saOnDisconnect = cleanupWS uuid state
    }

  processCommandWS :: EIO.Socket -> ServerState -> IO ()
  processCommandWS socket state = forever $ do 
    (EIO.TextPacket packet) <- atomically $ EIO.receive socket
    let arg = Text.unpack packet
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
    line <- strip <$> hGetLine handle
    pushUpdateLog "<unknown>" line state 
    let args = splitOn " " line

    case args of
      (Protocol.handshake -> True) -> do
        maybeClient <- handshakeTCP args handle state
        case maybeClient of
          Just client@TCPClient{tcpName=name} -> do
            sendMessageTCP TCPClient{tcpHandle=handle} $ Protocol.ack ++ " " ++ Protocol.supported
            pushUpdateTCPClients state
            forever $ do
              catchIOError (do
                input <- hGetLine handle
                let line = strip input

                pushUpdateLog name line state

                let args = splitOn " " line
                case args of
                  (Protocol.play -> True) -> do
                    playCommand client state
                  (Protocol.move -> True) -> do
                    moveCommand client args state
                  (Protocol.chat -> True) -> do
                    chatCommand client args state
                  (Protocol.challenge -> True) -> do
                    challengeCommand client args state
                  _ -> do
                    sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- unknown command
                    hClose handle)
                (\e -> do
                  cleanup client state
                  ioError e
                )
          Nothing -> do
            sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorNameInUse -- handshake failed
      _ -> do
        sendMessageTCP TCPClient{tcpHandle=handle} Protocol.errorUnknownCommand -- no initial handshake

  playCommand :: TCPClient -> ServerState -> IO ()
  playCommand client state@ServerState{queue=q, serverGames=sg} = do
    maybeQueuedClient <- readTVarIO q
    case maybeQueuedClient of
      Just queuedClient -> do
        if queuedClient == client then
          return ()
        else do
          atomically $ writeTVar q Nothing
          newGame <- newTVarIO initalizeGame
          let clients = [queuedClient, client]
          let game = ServerGame{ players = clients, game = newGame }
          atomically $ do
            games <- readTVar sg
            writeTVar sg $ games ++ [game]
          mapM_ (\client -> sendMessageTCP client (Protocol.gameStarted ++ " " ++ clientsToString clients)) clients

          pushUpdateLog "<server>" ("<game started> " ++ (clientsToString clients)) state
          pushUpdateAll state
      Nothing -> do
        atomically $ writeTVar q (Just client)
        return ()

  moveCommand :: TCPClient -> [String] -> ServerState -> IO ()
  moveCommand client@TCPClient{tcpName=name, tcpHandle=handle} args state = do
    maybeServerGame <- findServerGame name state
    case maybeServerGame of
      Just serverGame@ServerGame{players=ps, game=g} -> do
        game <- readTVarIO g
        if (ps !! getCurrentPlayer game) == client then do
          let row = read (args !! 1)
          let maybeGame = Game.dropCoin game row
          case maybeGame of
            Just game -> do
              let clients = serverGameToClients serverGame
              atomically $ writeTVar g game
              mapM_ (\client -> sendMessageTCP client (Protocol.moveDone ++ " " ++ show row)) clients
              if Game.winningColumn game row then do
                mapM_ (\client -> sendMessageTCP client (Protocol.gameOver ++ " " ++ Protocol.false ++ " " ++ name)) clients
                shutdownServerGame serverGame state
                pushUpdateLog "<server>" ("<game won> " ++ name) state
              else if Game.fullBoard game then do
                mapM_ (\client -> sendMessageTCP client (Protocol.gameOver ++ " " ++ Protocol.true)) clients
                shutdownServerGame serverGame state
                pushUpdateLog "<server>" ("<game draw> " ++ (clientsToString clients)) state
              else do
                return ()
              pushUpdateAll state
            Nothing -> do
              hClose handle
        else do
          hClose handle
      Nothing -> do
        hClose handle

  chatCommand :: TCPClient -> [String] -> ServerState -> IO ()
  chatCommand client@TCPClient{tcpName=name, tcpChat=challenge, tcpHandle=handle} args state = do
    if (not challenge) then do
      sendMessageTCP client $ Protocol.errorInvalidClient
      hClose handle
    else do
      maybeServerGame <- findServerGame name state
      let msg = Protocol.sendChat ++ " " ++ name ++ " " ++ intercalate " " (tail args)
      case maybeServerGame of
        Just ServerGame{players=ps} -> do
          let chatClients = filter (\TCPClient{tcpChat=chat} -> chat) ps
          mapM_ (\client -> sendMessageTCP client msg) chatClients
        Nothing -> do
          clients <- tcpClientsInLobby state
          let chatClients = filter (\TCPClient{tcpChat=chat} -> chat) clients
          mapM_ (\client -> sendMessageTCP client msg) chatClients

  challengeCommand :: TCPClient -> [String] -> ServerState -> IO ()
  challengeCommand client@TCPClient{tcpName=name, tcpChat=chat, tcpHandle=handle} args state@ServerState{tcpClients=tcpCs,challengedTCPClients=chTcpCs} = do
    let challengeeName = args !! 1
    if (not chat) || (name == challengeeName) then do
      sendMessageTCP client $ Protocol.errorInvalidClient
      hClose handle
    else do
      tcpClients <- readTVarIO tcpCs
      let maybeChallengee = lookup challengeeName tcpClients
      case maybeChallengee of
        Just challengee@TCPClient{tcpName = challengeeName} -> do
          challengedTCPClients <- readTVarIO chTcpCs

          -- challengee is already challanger or challengee
          if challengeeName `elem` ((Map.keys challengedTCPClients) ++ map (\TCPClient{tcpName=name} -> name) (Map.elems challengedTCPClients)) then do
            sendMessageTCP client Protocol.errorInvalidClient
            -- do not close handle, this could just happen without any notice towards client
          else do
            atomically $ writeTVar chTcpCs (Map.insert name challengee challengedTCPClients)
            sendMessageTCP challengee $ Protocol.challenged ++ " " ++ name
        Nothing -> do
          sendMessageTCP client Protocol.errorInvalidClient
          hClose handle

  shutdownServerGame :: ServerGame -> ServerState -> IO ()
  shutdownServerGame serverGame ServerState{serverGames=sg} = do
    serverGames <- readTVarIO sg
    atomically $ writeTVar sg (serverGames \\ [serverGame])

  cleanup :: TCPClient -> ServerState -> IO ()
  cleanup client@TCPClient{tcpName=name} state@ServerState{tcpClients=tcpCs, queue=q} = do
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
    maybeServerGame <- findServerGame name state
    case maybeServerGame of
      Just serverGame@ServerGame{players=ps} -> do
        mapM_ (\client -> sendMessageTCP client (Protocol.errorInvalidMove)) (ps \\ [client])
        shutdownServerGame serverGame state
      _ -> return ()
    pushUpdateAll state
    pushUpdateLog "<server>" ("<client disconnected> " ++ name) state

  cleanupWS :: String -> ServerState -> IO ()
  cleanupWS uuid ServerState{wsClients=wsCs} = do
    wsClients <- readTVarIO wsCs
    atomically $ writeTVar wsCs $ Map.delete uuid wsClients

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
    let name = args !! 2

    inUse <- nameInUse name state
    if inUse then do
      return Nothing
    else do
      let opts = args !! 1
      let client = TCPClient { tcpName = name,
                               tcpHandle = handle,
                               tcpChat = opts !! 0 == Protocol.boolTrue,
                               tcpChallenge = opts !! 1 == Protocol.boolTrue,
                               tcpLeaderboard = opts !! 2 == Protocol.boolTrue }

      handshake name client clients
      return $ Just client
       
  sendMessageWS :: WSClient -> Text.Text -> IO ()
  sendMessageWS WSClient{wsSocket=socket} packet = atomically $ do
    EIO.send socket (EIO.TextPacket $ packet)

  sendMessageTCP :: TCPClient -> String -> IO ()
  sendMessageTCP TCPClient{tcpHandle=handle} msg = hPutStrLn handle msg

  broadcastWS :: Text.Text -> ServerState -> IO ()
  broadcastWS packet ServerState{wsClients=clients} = do
    clientMap <- readTVarIO clients
    mapM_ (\socket -> sendMessageWS socket packet) (Map.elems clientMap)
