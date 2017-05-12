module Control.High5.App where

import           High5.Prelude
import           Control.High5.Core            (operatingPipe
                                               , AppData(..)
                                               , ioConsumer
                                               , ioProducer
                                               , Settings(..)
                                               )
import           Control.High5.Commands        (ioOperations)
import           Pipes
import qualified Pipes.Prelude                 as P
import qualified Data.Yaml                     as Y
import           Control.Lens                  ((^.))
import           System.IO                     (hSetBuffering
                                               , BufferMode(..))

go :: IO ()
go = do
    -- Globally disable buffering; buffering is the devil and prevents
    -- us from high5-ing. In particular, at least on Windows, if buffering
    -- isn't turned off here, it's not possible to obtain lines in Real Time
    -- from this process while it is executing.
    hSetBuffering stdout NoBuffering
    --
    ref <- newIORef Nothing

    -- Load in the yaml config
    fileBytes <- readFile "settings.yaml"
    let msettings = Y.decode fileBytes :: Maybe Settings
    case msettings of
      Just settings -> do
            let pipeline = mkApp $ AppData ref inP outP settings
            runEffect pipeline
                where
                    inP  = P.stdinLn >-> forever (await >>= yield . pack)
                    outP = forever (await >>= yield . unpack) >-> P.stdoutLn
      Nothing -> do
            error "`settings.yaml` invalid."


mkApp :: AppData -> Effect IO ()
mkApp appData = do
    let pipe = runReaderT (operatingPipe ioOperations) appData
    ioIn >-> forever pipe >-> ioOut
      where
          ioIn  = appData ^. ioProducer
          ioOut = appData ^. ioConsumer

