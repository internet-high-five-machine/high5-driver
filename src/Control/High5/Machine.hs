module Control.High5.Machine (
      doHigh5
    , MachineResponse(..)
  )
  where

import High5.Prelude
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.High5.Core
import Control.Lens ((^.))

data MachineResponse = 
      High5Success
    | High5Failure


-- | TODO: Communicate with the machine.
doHigh5 ::AppData -> IO MachineResponse
doHigh5 appData = do
    -- TODO: Add exception handling
    let cmd:args = appData ^. settings ^. exec ^. machineCommand
    (exitCode, _, _stderr) <-  readProcessWithExitCode cmd args ""
    case exitCode of
      ExitSuccess   -> return High5Success
      ExitFailure _ -> return High5Failure
