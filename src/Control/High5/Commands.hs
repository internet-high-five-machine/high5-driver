{-# LANGUAGE LambdaCase        #-}

module Control.High5.Commands where

import           High5.Prelude
import           Control.High5.Core
import           Pipes
import           Pipes.Concurrent
import           Pipes.WebSockets
import           Data.Aeson                    (decode)
import           Network.Socket                (withSocketsDo)
import           Control.Lens                  ( (^.))
import           Control.High5.Machine         (doHigh5
                                               , MachineResponse(..)
                                               )
import qualified Wuss                          as WS



ioOperations :: [Op]
ioOperations = [ ((== "ping"),      ping       )
               , ((== "configure"), configure  )
               ]


wsOperations :: [Op]
wsOperations = [ ((== "ping"),      ping       )
               , ((== "high5"),     high5      )
               ]

-- |
ping :: Command
ping = const $ return "pong"


-- | 
high5 :: Command
high5 _ = do
    appData <- ask
    r <- lift $ doHigh5 appData
    case r of
         High5Success -> return "high5-success"
         High5Failure -> return "high5-failure"


configure :: Command
configure raw = do
    --
    let mcp :: Maybe ConfigureParameters
        mcp = decode raw
    --
    case mcp of
        Nothing -> return "error decoding json"
        Just cp -> doConfigure cp


capture :: MonadIO m 
      => Text 
      -> Pipe Text Text m ()
capture prefix = do
    x <- await
    putStrLn (prefix ++ x)
    yield x


-- | We need to kill the thread that was sitting and reading from the WS
--   mailbox. We then relaunch it.
doConfigure :: ConfigureParameters -> DriverT IO Text
doConfigure cp = do
    --
    appData <- ask
    mps     <- lift $ readIORef (appData ^. webSocketProcess)
    --
    case mps of
         -- Cancel an existing process if it exists
         Just x  -> lift $ cancel x
         --
         -- Otherwise do nothing
         Nothing -> return ()

    -- Create a mailbox to write to and read from
    (output, input) <- lift $ spawn unbounded
    

    let wsPipe = runReaderT (operatingPipe wsOperations) appData


        -- Read from the socket, process it, and put it in the mailbox
    let toMb :: WebSocketsT IO ()
        toMb   = runEffect $ wsInOnce >∞> wsPipe >∞> toOutput output
        --
        -- Read from the mailbox, send it to the socket.
        fromMb :: WebSocketsT IO ()
        fromMb = runEffect $ fromInput input >-> (capture "ws out: ") >∞> wsOut

    -- Start the socket
    liftIO $ do
            let clientProc :: IO ()
                clientProc = withSocketsDo $ WS.runSecureClient h p u (\c -> do
                                    void $ concurrently (runReaderT toMb c) (runReaderT fromMb c)
                             ) `catchAny` \_ -> do
                                 let m = appData ^. ioConsumer
                                 runEffect $ yield "websocket connection dropped" >-> m

            aproc <- async clientProc
            writeIORef (appData ^. webSocketProcess) (Just aproc)
    --
    -- Send our opening "connect" message up the socket.
    runEffect $ (yield ("connect " ++ (cp ^. email) ++ " " ++ (cp ^. token)))
                >-> toOutput output
    --
    return "websocket connected"
  where
      h = cp ^. server
      p = 443 -- Insanity but necessary.
      u = cp ^. url
