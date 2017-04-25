module Control.High5.App where

import           High5.Prelude
import           Control.High5.Core            ( operatingPipe
                                               , AppData(..)
                                               , ioConsumer
                                               , ioProducer
                                               , (>∞>)
                                               )
import           Control.High5.Commands        (ioOperations)
import           Pipes
import qualified Pipes.Prelude                 as P
import           Control.Lens                  ( (^.))
import           System.IO                     ( hSetBuffering
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
    runEffect (mkApp (AppData ref inP outP))
      where
          inP  = P.stdinLn >-> forever (await >>= yield . pack)
          outP = forever (await >>= yield . unpack) >-> P.stdoutLn


mkApp :: AppData -> Effect IO ()
mkApp appData = do
    let pipe = runReaderT (operatingPipe ioOperations) appData
    --
    -- Build the pipe
    ioIn >-> forever pipe >-> ioOut
      where
          ioIn  = appData ^. ioProducer
          ioOut = appData ^. ioConsumer

