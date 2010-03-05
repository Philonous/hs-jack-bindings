{-# LANGUAGE ForeignFunctionInterface#-}


module Jack.Jack where 

import qualified Jack.Bindings as Jack
import Foreign
import Control.Monad
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Reader 

ignore :: Monad m => m a -> m ()
ignore x = x >> return ()

foreign import ccall "wrapper" mkFreeFunPtr 
  :: (FunPtr a -> IO ()) -> IO (FunPtr (FunPtr a -> IO ()))


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
       

