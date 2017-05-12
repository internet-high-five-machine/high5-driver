{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}

module Control.High5.Core where

import           High5.Prelude
import           Pipes
import           Control.Concurrent.Async      (Async)
import           Control.Lens                  (makeFields)
import           Data.Aeson                    (ToJSON(..)
                                               , FromJSON(..)
                                               , Value(..)
                                               , object
                                               , (.=)
                                               , (.:)
                                               )
import qualified Data.ByteString.Lazy          as LB
import qualified Data.Text                     as T
import qualified Data.List                     as L
import qualified Data.Yaml                     as Y

-- | Forever fish.
--
-- TODO: Clean up this type signature.
(>∞>) :: forall a' a b (m :: * -> *) r c' c a1 a2.
          Monad m
      => Proxy a' a () b m a1
      -> Proxy () b c' c m a2
      -> Proxy a' a c' c m r
(>∞>) a b = forever a >-> forever b




data ConfigureParameters = CP {
      _configureParametersServer :: String
    , _configureParametersUrl    :: String
    , _configureParametersEmail  :: Text
    , _configureParametersToken  :: Text
    } deriving Show



data Exec = Exec {
    _execMachineCommand :: [String]
    }

data Settings = Settings {
    _settingsExec :: Exec
    }
--
-- | Data that we need to do application things.  
data AppData = AppData {
    -- | The reference to the socket-listening process
    --   that we may like to cancel occasionally.
      _appDataWebSocketProcess :: IORef (Maybe (Async ()))
    --
    -- TODO: Make this `MonadIO`?
    , _appDataIoProducer       :: Producer Text IO () 
    , _appDataIoConsumer       :: Consumer Text IO ()
    , _appDataSettings         :: Settings
    }


instance FromJSON Settings where
  parseJSON (Y.Object v) = Settings <$>
    v .:   "exec"
  parseJSON _ = fail "Expected Object for Settings value"

instance FromJSON Exec where
  parseJSON (Y.Object v) = Exec <$>
    v .:   "machine-command"
  parseJSON _ = fail "Expected Object for Settings value"



$(makeFields ''Exec)
$(makeFields ''AppData)
$(makeFields ''Settings)
$(makeFields ''ConfigureParameters)


-- | Some monad transformer so that we can pass around data to ourself.
type DriverT = ReaderT AppData


-- | A command takes a Lazy ByteString (which it should
--   interpret as a JSON object) and gives back some text
--   which indicates whether or not it was successful.
--
--   Example:
--
--   {"thing": 1} -> <stuff happens> -> "Ok"
type Command = LB.ByteString -> DriverT IO Text


-- | An "Op" is tuple of a function that determins if the command
--   should be evaluated, and the command to evaluate if this is 
--   the case.
type Op = (Text -> Bool, Command)



instance FromJSON ConfigureParameters where
 parseJSON (Object o) =
    CP <$> o .: "server"
       <*> o .: "url"
       <*> o .: "email"
       <*> o .: "token"
 parseJSON _ = mzero

instance ToJSON ConfigureParameters where
 toJSON (CP s u e t) =
    object [ "server" .= s
           , "url"    .= u
           , "email"  .= e
           , "token"  .= t
           ]


-- | Our internal json format is "command jsonObject".
--
--   NOTE: This is probably crazy. What I'd really like to do this decode some
--          object like:
--
--          Command a = ConfigureCommand ConfigParams | TestCommand TestParams
--
--          with "appropriate json". The problem is I'm not sure what the JSON
--          looks like; nor how to decode the command first so we can decode
--          the appropriate params.
--
parseRawJson :: Text -> (Text, LB.ByteString)
parseRawJson msg = (cmd, LB.fromStrict $ encodeUtf8 $ json)
  where
      (cmd, j) = T.breakOn " " msg
      json     = T.drop 1 j


-- | Our internal message format is "command arg1 arg2 ...".
parseRawSpaces :: Text -> (Text, [Text])
parseRawSpaces msg = (cmd, args)
  where
      raw  = T.splitOn " " msg
      cmd  = L.head raw
      args = L.tail raw


-- | A pipe that enacts the operations on an incoming string
--   and outputs a resulting string,
operatingPipe :: MonadIO m 
              => [Op]                -- A list of commands for us to process.
              -> DriverT (Pipe Text Text m) ()
operatingPipe ops = do
    app <- ask
    m   <- lift await
    --
    x   <- liftIO $ runReaderT (dispatch m ops) app
    --
    lift $ yield x


-- | Evaluates commands. If more than one operation in "Op" was
--   able to process the command, we consider this to be an error.
dispatch :: Text
         -> [Op]
         -> DriverT IO Text
dispatch s ops = do
    let -- Obtain those commands which satisfy the predicate
        trueOps = filter (\(f, _) -> f cmd) ops
    --
    case length trueOps of
         1 -> do
                let (_, op) = unsafeHead trueOps
                op json
         _ -> error $ "Can't process command: `" ++ (T.unpack cmd) ++ "`"
  where
      (cmd, json) = parseRawJson s
