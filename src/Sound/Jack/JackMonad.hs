-- Haskell bindings to the JACK Audio Connection Kit 
--
-- Copyright (c) 2010 Philipp Balzarek (p.balzarek@googlemail.com)
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use,
-- copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following
-- conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
-- HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
-- OTHER DEALINGS IN THE SOFTWARE.
--
-- Description
--
-- Language : GHC Haskell
--
-- This module provides a monad interface to the JACK bindings.
-- The client is handled by a Reader Monad to prevent leaking it


{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}


module Sound.Jack.JackMonad 
  ( runWithNewClient
  , JackAction
  , getClientName
  , activate
  , deactivate
  , clientThreadId
  , isRealtime
  , cycleWait
  , cycleSignal
  , setProcessThread
  , setThreadInitCallback
  , setProcessCallback
  , setFreewheelCallback
  , setBufferSizeCallback
  , setSampleRateCallback
  , setClientRegistrationCallback
  , setPortRegistrationCallback
  , setPortConnectCallback
  , setGraphOrderCallback
  , setXrunCallback
  , setFreewheel
  , setBufferSize
  , getSampleRate
  , getBufferSize
  , engineTakeoverTimebase
  , cpuLoad
  , portRegister
  , portUnregister
  , portIsMine
  , portGetAllConnections
  , jackPortGetTotalLatency
  , recomputeTotalLatencies
  , portRequestMonitorByName
  , connect
  , disconnect
  , portDisconnect
  , getPorts
  , portByName
  , portById
  , framesSinceCycleStart
  , frameTime
  , framesToTime
  , timeToFrames
    
  , unsafeAskClient
  , unsafeClientClose
    
  -- reexports from Bindings
  , FFI.Port
  , FFI.PortFlags(..)
  , FFI.AudioSample
  , FFI.BufferSizeCallbackPtr
  , FFI.ClientRegistrationCallbackPtr
  , FFI.FreewheelCallbackPtr
  , FFI.GraphOrderCallbackPtr
  , FFI.PortConnectCallbackPtr
  , FFI.PortRegistrationCallbackPtr
  , FFI.ProcessCallbackPtr
  , FFI.SampleRateCallbackPtr
  , FFI.ThreadCallbackPtr
  , FFI.ThreadInitCallbackPtr
  , FFI.XRunCallbackPtr
  , FFI.Options(..)
  , FFI.Client                   -- opaque
  , FFI.Status(..)
  , FFI.PortID                   -- opaque
  , FFI.defaultAudioType
  , FFI.defaultMidiType
  , FFI.fromPThread
  , FFI.NFrames
  , FFI.PThread
  , FFI.Time
  ) where

import qualified Sound.Jack as Jack 
import qualified Sound.Jack.Bindings as FFI

import Data.Word

import Foreign.Ptr
import Foreign.C

import Control.Monad.Trans
import Control.Monad.Reader 
import Control.Monad
import Control.Exception
import Control.Applicative

newtype JackAction a = JA {unJA :: (ReaderT Jack.Client IO a) }
  deriving (Monad, Functor, MonadIO)


runWithClient :: FFI.Client -> JackAction a -> IO a
runWithClient client action = runReaderT (unJA action) client

runWithNewClient
  :: String
     -> [Jack.Options]
     -> String
     -> ([Jack.Status] -> JackAction a)
     -> IO ()
runWithNewClient serverName options clientName action = 
    Jack.withOpenClient serverName options clientName 
      (\client flags ->  runWithClient client (action flags) )

-- brings the first Argument into the last position
flip1 :: (t -> t1) -> t -> t1
flip1 f z         = f z
flip2 :: (t1 -> t -> t2) -> t -> t1 -> t2
flip2 f y z       = f z y
flip3 :: (t2 -> t -> t1 -> t3) -> t -> t1 -> t2 -> t3
flip3 f x y z     = f z x y
flip4 :: (t3 -> t -> t1 -> t2 -> t4) -> t -> t1 -> t2 -> t3 -> t4
flip4 f w x y z   = f z w x y
flip5  :: (t4 -> t -> t1 -> t2 -> t3 -> t5) -> t -> t1 -> t2 -> t3 -> t4 -> t5
flip5 f v w x y z = f z v w x y

readerT1 :: (FFI.Client -> IO a) -> JackAction a
readerT1 f        = JA . ReaderT $ flip1 f
readerT2 :: (FFI.Client -> t -> IO a) -> t -> JackAction a
readerT2 f x      = JA . ReaderT $ flip2 f x
readerT3
  :: (FFI.Client -> t -> t1 -> IO a) -> t -> t1 -> JackAction a
readerT3 f x y    = JA . ReaderT $ flip3 f x y
readerT4
  :: (FFI.Client -> t -> t1 -> t2 -> IO a)
     -> t -> t1 -> t2 -> JackAction a
readerT4 f x y z  = JA . ReaderT $ flip4 f x y z
readerT5
  :: (FFI.Client -> t -> t1 -> t2 -> t3 -> IO a)
     -> t -> t1 -> t2 -> t3 -> JackAction a
readerT5 f x y z u= JA . ReaderT $ flip5 f x y z u

-- semi-automatically generated from the types
-----------------------------------------------

-- | Read the client handle from the reader.
-- You shouldn't need that. 
unsafeAskClient :: JackAction FFI.Client
unsafeAskClient = JA ask

-- | close the Client. RunWithNewClient should normally do that for you. 
unsafeClientClose             = readerT1 FFI.clientClose
getClientName                 = readerT1 FFI.getClientName                
activate                      = readerT1 FFI.activate                     
deactivate                    = readerT1 FFI.deactivate                   
clientThreadId                = readerT1 FFI.clientThreadId               
isRealtime                    = readerT1 FFI.isRealtime                   
cycleWait                     = readerT1 FFI.cycleWait                    
cycleSignal                   = readerT2 FFI.cycleSignal                  
setProcessThread              = readerT3 FFI.setProcessThread             
setThreadInitCallback         = readerT3 FFI.setThreadInitCallback        
setProcessCallback            = readerT3 FFI.setProcessCallback           
setFreewheelCallback          = readerT3 FFI.setFreewheelCallback         
setBufferSizeCallback         = readerT3 FFI.setBufferSizeCallback        
setSampleRateCallback         = readerT3 FFI.setSampleRateCallback        
setClientRegistrationCallback = readerT3 FFI.setClientRegistrationCallback
setPortRegistrationCallback   = readerT3 FFI.setPortRegistrationCallback  
setPortConnectCallback        = readerT3 FFI.setPortConnectCallback       
setGraphOrderCallback         = readerT3 FFI.setGraphOrderCallback        
setXrunCallback               = readerT3 FFI.setXrunCallback              
setFreewheel                  = readerT2 FFI.setFreewheel                 
setBufferSize                 = readerT2 FFI.setBufferSize                
getSampleRate                 = readerT1 FFI.getSampleRate                
getBufferSize                 = readerT1 FFI.getBufferSize                
engineTakeoverTimebase        = readerT1 FFI.engineTakeoverTimebase       
cpuLoad                       = readerT1 FFI.cpuLoad                      
portRegister                  = readerT5 FFI.portRegister                 
portUnregister                = readerT2 FFI.portUnregister               
portIsMine                    = readerT2 FFI.portIsMine                   
portGetAllConnections         = readerT2 FFI.portGetAllConnections        
jackPortGetTotalLatency       = readerT2 FFI.jackPortGetTotalLatency      
recomputeTotalLatencies       = readerT1 FFI.recomputeTotalLatencies      
portRequestMonitorByName      = readerT3 FFI.portRequestMonitorByName     
connect                       = readerT3 FFI.connect                      
disconnect                    = readerT3 FFI.disconnect                   
portDisconnect                = readerT2 FFI.portDisconnect               
getPorts                      = readerT4 FFI.getPorts                     
portByName                    = readerT2 FFI.portByName                   
portById                      = readerT2 FFI.portById                     
framesSinceCycleStart         = readerT1 FFI.framesSinceCycleStart        
frameTime                     = readerT1 FFI.frameTime                    
framesToTime                  = readerT2 FFI.framesToTime                 
timeToFrames                  = readerT2 FFI.timeToFrames                       
