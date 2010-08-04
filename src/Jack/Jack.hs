{-# LANGUAGE ForeignFunctionInterface#-}


module Jack.Jack where 

import qualified Jack.Bindings as Jack
import Foreign
import Control.Monad
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Reader 

-- | discard the result of a monad action and return ()
ignore :: Monad m => m a -> m ()
ignore x = x >> return ()


-- Number of Frames to Process -> user Data -> IO return Value
type ProcessCallback = CUInt -> F.Ptr () -> IO CInt

-- | Convert a processing function to a function pointer
foreign import ccall safe "wrapper" mkProcessCallback
  :: ProcessCallback -> IO Jack.ProcessCallback
     
--  SampleRate -> user Data -> IO returnvalue
type SampleRateCallback = CUInt -> F.Ptr () -> IO CInt

-- | Convert a Sample rate callback function to a function pointer
foreign import ccall safe "wrapper" mkSampleRateCallback
  :: SampleRateCallback -> IO Jack.SampleRateCallback


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
       

