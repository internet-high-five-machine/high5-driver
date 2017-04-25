module Control.High5.Machine (
      doHigh5
    , MachineResponse(..)
  )
  where

import High5.Prelude

data MachineResponse = 
      High5Success
    | High5Failure


-- | TODO: Communicate with the machine.
doHigh5 :: IO MachineResponse
doHigh5 = do
    putStrLn "High 5 !"
    return High5Success
