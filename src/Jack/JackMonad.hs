{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jack.JackMonad where

import qualified Jack.Bindings as Jack 
import Control.Monad.Trans
import Control.Monad.Reader 
import Control.Monad
import Control.Exception

ignore :: Monad m => m a -> m ()
ignore x = x >> return ()

-- | Open a Jack client and run the supplied action 
withOpenClient
  :: String -- ^ The client name 
  -> [Jack.Options] -- ^ client options
  -> String -- ^ server name
  -> (Jack.Client -> [Jack.Status] -> IO a) 
    -- ^ action to be run with the open client
  -> IO () 
withOpenClient serverName options clientName action = bracket 
  (Jack.clientOpenWithServerName clientName options serverName )
  (\(client,flags) -> 
       unless (Jack.Failure `elem` flags) (ignore $ Jack.clientClose client )) 
  (\(client, flags) -> unless (Jack.Failure `elem` flags) . ignore $ action client flags)

newtype JackAction a = JA {unJA :: (ReaderT Jack.Client IO a) }
  deriving (Monad, MonadIO)

runJackAction
  :: String
     -> [Jack.Options]
     -> String
     -> ([Jack.Status] -> JackAction a)
     -> IO ()
runJackAction serverName options clientName action = 
    withOpenClient serverName options clientName 
      (\client flags ->  flip runReaderT client . unJA $ action flags )
