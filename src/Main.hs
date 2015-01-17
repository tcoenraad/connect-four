{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Network (listenOn, PortID (..))
  import Control.Concurrent.Async (async, waitAny)

  import qualified Control.Concurrent.STM as STM
  import qualified Data.Map.Strict as Map
  import qualified Snap.Core as Snap
  import qualified Snap.Util.FileServe as Snap
  import qualified Snap.Http.Server as Snap
  import qualified Network.EngineIO as EIO
  import qualified Network.EngineIO.Snap as EIOSnap

  import ConnectFour.Server

  import Paths_connect_four (getDataDir)

  splitIO :: [IO ()] -> IO ()
  splitIO procs = do
    asyncs <- mapM async procs
    _ <- waitAny asyncs
    return ()

  main :: IO ()
  main = do
    state <- newServer
    splitIO [mainWS state, mainTCP state]

  newServer :: IO (ServerState)
  newServer = do
    q <- STM.newTVarIO Nothing
    g <- STM.newTVarIO Map.empty
    tcp <- STM.newTVarIO Map.empty
    ws <- STM.newTVarIO Map.empty
    return ServerState { queue = q, games = g, tcpClients = tcp, wsClients = ws }

  mainWS :: ServerState -> IO ()
  mainWS state = do
    eio <- EIO.initialize
    dataDir <- getDataDir
    Snap.quickHttpServe $
      Snap.route [ ("/engine.io", EIO.handler eio (handleSocketWS state) EIOSnap.snapAPI)
                 , ("/", Snap.serveDirectory dataDir)
                 ]

  mainTCP :: ServerState -> IO ()
  mainTCP state = do
    sock <- listenOn $ PortNumber 8001
    handleSocketTCP state sock
