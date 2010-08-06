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
-- Language : Haskell 98
--
-- This module provides low level language bindings to the 
-- Jack Audio Connection Kit (http://jackaudio.org ). 
-- It is intended to be a faithfull representation of the C include files.
-- Parameters are converted only where the meaning is obvious. 

{-# LANGUAGE ForeignFunctionInterface #-}


#include <jack/jack.h>
#include "wrapper.h"


module Sound.Jack.Bindings 
  ( clientOpenWithDefaultserver
  , clientOpenWithServerName
  , clientClose
  , clientNameSize
  , getClientName
  , internalClientNew
  , internalClientClose
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
  , portGetBuffer
  , portName
  , portShortName
  , portFlags
  , portType
  , portIsMine
  , portConnected
  , portGetConnections
  , portGetAllConnections
  , jackPortGetLatency
  , jackPortGetTotalLatency
  , portSetLatency
  , recomputeTotalLatencies
  , portSetName
  , portSetAlias
  , portUnsetAlias
  , portRequestMonitor
  , portRequestMonitorByName
  , portEnsureMonitor
  , portMonitoringInput
  , connect
  , disconnect
  , portDisconnect
  , portNameSize
  , portTypeSize
  , getPorts
  , portByName
  , portById
  , framesSinceCycleStart
  , frameTime
  , framesToTime
  , timeToFrames
  , getTime

  , Port
  , PortFlags(..)
  , AudioSample
  , BufferSizeCallbackPtr
  , ClientRegistrationCallbackPtr
  , FreewheelCallbackPtr
  , GraphOrderCallbackPtr
  , PortConnectCallbackPtr
  , PortRegistrationCallbackPtr
  , ProcessCallbackPtr
  , SampleRateCallbackPtr
  , ThreadCallbackPtr
  , ThreadInitCallbackPtr
  , XRunCallbackPtr
  , Options(..)
  , Client                   -- opaque
  , Status(..)
  , PortID                   -- opaque
  , defaultAudioType
  , defaultMidiType
  , fromPThread
  , NFrames
  , PThread
  , Time
  ) where

import C2HS
import Control.Monad

{#context lib="jack" prefix = "jack" #}

defaultAudioType :: String
defaultAudioType = "32 bit float mono audio"

defaultMidiType :: String
defaultMidiType = "8 bit raw midi"

-- | jack_client_t is opaque. We only toss it around, never touch it directly
newtype Client = Client { fromClient :: (Ptr ()) } 

-- deprecated:
-- client_new 

type AudioSample = {#type jack_default_audio_sample_t #}

{# enum Options {upcaseFirstLetter} deriving (Eq,Read,Show)  #}
{# enum Status {upcaseFirstLetter} deriving (Eq,Read,Show) #}

statusList :: [Status]
statusList = [ Failure, InvalidOption, NameNotUnique, ServerStarted
              , ServerFailed, ServerError, NoSuchClient, LoadFailure
              , InitFailure, ShmFailure, VersionError]

extractStatusMasks :: (Bits a, Storable a) => Ptr a -> IO [Status]
extractStatusMasks = peek >=> \bits ->
  return [bm | bm <- statusList, bits `containsBitMask` bm]

-------------------------

-- #c
-- // Wrapper for jack_client_open which uses varargs aka "The horror from below"
-- // Someone should tell those chaps that cool kids don't use vararg 
-- jack_client_t *jack_client_open_with_defaultserver(const char *client_name, 
--   jack_options_t options, jack_status_t *status) {
--    jack_client_open(client_name, options, status);
--   }
-- #endc 

-- | open an external client with the defailt server
{#fun client_open_with_defaultserver as ^ 
  {`String', combineBitMasks `[Options]' , alloca- `[Status]' extractStatusMasks* }
  -> `Client' Client #}

------------------------


-- #c
-- // Let's just hope no one ever needs more than one server... 
-- jack_client_t *jack_client_open_with_server_name(const char *client_name, 
--   jack_options_t options, jack_status_t *status, const char* server) {
--    jack_client_open(client_name, options, status, server);
--   }
-- #endc 


-- | open an external client with the specified server
{#fun client_open_with_server_name as ^ 
  {`String', combineBitMasks `[Options]' -- | foooblarg
  , alloca- `[Status]' extractStatusMasks*
  , `String' }
  -> `Client' Client #}

-------------------------

-- | disconnect a client from the server
{#fun client_close as ^ 
 {fromClient `Client'} -> `()' #}


-- | the maximal length of the client name in bytes
{#fun client_name_size as ^ 
  {} -> `Int' #}

-- | The name of the client. This may differ from the one specified in 
-- createClient when the requested name whas not unique
{#fun get_client_name as ^ 
 {fromClient `Client'} -> `String' #}

-- | DEPRECATED. Use internalClientOpen
{#fun internal_client_new  as ^
 {`String',`String',`String' } -> `Int' #}

-- | DEPRECATED. use internalClientLoad
{#fun internal_client_close as ^ 
 {`String'} -> `()' #}

-- | Tell the server to start processing audio
{#fun activate as ^ 
 {fromClient `Client'} -> `Int' #}

{#fun deactivate as ^ 
 {fromClient `Client'} -> `Int' #}

newtype PThread = PThread { fromPThread :: {#type pthread_t #} }

{#fun client_thread_id as ^ 
 {fromClient `Client'} -> `PThread' PThread #}

{#fun is_realtime as ^
 {fromClient `Client'} -> `Bool' #}

-- deprecated:
-- thread_wait 

type NFrames =  {#type nframes_t #} 

{#fun cycle_wait as ^
 {fromClient `Client'} -> `Word32' #}

{#fun cycle_signal as ^
 {fromClient `Client', `Int'} -> `()' #}

firstly action = (>>=) . action

type ThreadCallbackPtr = {# type ThreadCallback #}
type ThreadCallback = Ptr () -> IO (Ptr ())

foreign import ccall safe "wrapper" mkThreadCallback
  :: ThreadCallback            -> IO ThreadCallbackPtr            

toThreadCallback = firstly mkThreadCallback

{#fun set_process_thread as ^
 { fromClient  `Client'
 , toThreadCallback* `ThreadCallback'
 , id  `Ptr ()' 
 } 
  -> `Int' #} 

type ThreadInitCallbackPtr =  {#type ThreadInitCallback#} 

{#fun set_thread_init_callback as ^
 { fromClient `Client'
 , id `ThreadInitCallbackPtr'
 , id `Ptr ()' } 
  -> `Int' #} 

-- TODO:
-- thread_on_shutdown

--type ProcessCallbackPtr = FunPtr(CUInt -> Ptr () -> IO CInt)
type ProcessCallbackPtr = {# type ProcessCallback #}

{#fun set_process_callback as ^
 { fromClient `Client'
 , id `ProcessCallbackPtr'
 , id `Ptr ()' } 
  -> `Int' #} 


type FreewheelCallbackPtr = {#type FreewheelCallback#} 

{#fun set_freewheel_callback as ^
 { fromClient `Client'
 , id `FreewheelCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 

type BufferSizeCallbackPtr = {#type BufferSizeCallback#} 

{#fun set_buffer_size_callback as ^
 { fromClient `Client'
 , id `BufferSizeCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 


type SampleRateCallbackPtr = {#type SampleRateCallback#} 

{#fun set_sample_rate_callback as ^
 { fromClient `Client'
 , id `SampleRateCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 


type ClientRegistrationCallbackPtr = {#type ClientRegistrationCallback#} 

{#fun set_client_registration_callback as ^
 { fromClient `Client'
 , id `ClientRegistrationCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 

type PortRegistrationCallbackPtr = {#type PortRegistrationCallback#} 

{#fun set_port_registration_callback as ^
 { fromClient `Client'
 , id `PortRegistrationCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 

type PortConnectCallbackPtr = {#type PortConnectCallback#}

{#fun set_port_connect_callback as ^
 { fromClient `Client'
 , id `PortConnectCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 

type GraphOrderCallbackPtr = {#type GraphOrderCallback#} 

{#fun set_graph_order_callback as ^
 { fromClient `Client'
 , id `GraphOrderCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 


type XRunCallbackPtr = {#type XRunCallback#} 

{#fun set_xrun_callback as ^
 { fromClient `Client'
 , id `XRunCallbackPtr'
 , id `(Ptr ())' } 
  -> `Int' #} 

{#fun set_freewheel as ^
 {fromClient `Client', `Bool'} -> `Int' #}

{#fun set_buffer_size as ^
 {fromClient `Client', `Word32'} -> `Int' #}

{#fun get_sample_rate as ^
 {fromClient `Client'} -> `CUInt' fromIntegral #}

{#fun get_buffer_size as ^
 {fromClient `Client'} -> `Word32' #}

{#fun engine_takeover_timebase as ^ 
 {fromClient `Client'} -> `Int' #}

{#fun cpu_load as ^
 {fromClient `Client'} -> `Float' #}

newtype Port = Port { fromPort :: Ptr () } -- opaque

{#enum PortFlags {upcaseFirstLetter} deriving (Eq,Read,Show) #}

-- We need to walk all the constructors to extract a list of flags from a bitmask
-- Let's hope those don't change any time soon ;)
portFlagList :: [PortFlags]
portFlagList = [PortIsInput , PortIsOutput , PortIsPhysical , PortCanMonitor , PortIsTerminal]

{#fun port_register as ^ 
 {fromClient `Client', `String', `String',  combineBitMasks `[PortFlags]', `Int'} 
 -> `Port' Port #}

{#fun port_unregister as ^ 
 {fromClient `Client', fromPort `Port'} -> `Int' #}

{#fun port_get_buffer as ^
 {fromPort `Port', `Word32'} -> `Ptr AudioSample' castPtr #}

{#fun port_name as ^
 {fromPort `Port'} -> `String' #}

{#fun port_short_name as ^
 {fromPort `Port'} -> `String' #}

extractPortFlagMasks :: Bits a => a -> [PortFlags]
extractPortFlagMasks bits =
  [bm | bm <- portFlagList, bits `containsBitMask` bm]

{#fun port_flags as ^
 {fromPort `Port'} -> `[PortFlags]' extractPortFlagMasks #}

{#fun port_type as ^
 {fromPort `Port'} -> `String' #}

{#fun port_is_mine as ^
 {fromClient `Client', fromPort `Port'} -> `Bool' #}

{#fun port_connected as ^
 {fromPort `Port'} -> `Int' #}

peekCStringArray0 :: (Ptr CString) -> IO [String]
peekCStringArray0 p = do 
  ptrs <- peekArray0 nullPtr p
  mapM peekCString ptrs

{#fun port_get_connections as ^
 {fromPort `Port'} -> `[String]' peekCStringArray0* #}

{#fun port_get_all_connections as ^
 {fromClient `Client', fromPort `Port'} -> `[String]' peekCStringArray0* #}

-- deprecated:
-- port_tie

-- deprecated:
-- port_untie

{#fun jack_port_get_latency as ^
 {fromPort `Port'} -> `Word32' #}


{#fun jack_port_get_total_latency as ^
 {fromClient `Client', fromPort `Port'} -> `Word32' #}

{#fun port_set_latency as ^
 {fromPort `Port', `Word32'} -> `()' #}

{#fun recompute_total_latencies as ^
 {fromClient `Client'} -> `Int' #}

{#fun port_set_name as ^
 {fromPort `Port', `String'} -> `Int' #}

{#fun port_set_alias as ^
 {fromPort `Port', `String'} -> `Int' #}

{#fun port_unset_alias as ^
 {fromPort `Port', `String'} -> `Int' #}

-- TODO:
-- port_get_aliases 

{#fun port_request_monitor as ^
 {fromPort `Port', `Bool'} -> `Int' #}

{#fun port_request_monitor_by_name as ^
 {fromClient `Client', `String', `Bool'} -> `Int' #}

{#fun port_ensure_monitor as ^
 {fromPort `Port', `Bool'} -> `Int' #}

{#fun port_monitoring_input as ^
 {fromPort `Port' } -> `Int' #}

{#fun connect as ^
 {fromClient `Client', `String', `String'} -> `Int' #}

{#fun disconnect as ^
 {fromClient `Client', `String', `String'} -> `Int' #}

{#fun port_disconnect as ^
 {fromClient `Client', fromPort `Port' } -> `Int' #}

{#fun port_name_size as ^
 {} -> `Int' #}

{#fun port_type_size as ^
 {} -> `Int' #}

{#fun get_ports as ^
 {fromClient `Client', `String', `String', combineBitMasks `[PortFlags]'}
 -> `[String]' peekCStringArray0* #}

{#fun port_by_name as ^
 {fromClient `Client', `String'} -> `Port' Port #}

newtype PortID = PortID { fromPortID :: {#type port_id_t#} }

{#fun port_by_id as ^
 {fromClient `Client', fromPortID `PortID'} -> `Port' Port #}

{#fun frames_since_cycle_start as ^
 {fromClient `Client'} -> `Word32'  #}

{#fun frame_time as ^
 {fromClient `Client'} -> `Word32' #}

type Time = {#type jack_time_t#}

{#fun frames_to_time as ^
 {fromClient `Client', `Word32'} -> `Time' id #}

{#fun time_to_frames as ^
 {fromClient `Client', id `Time'} -> `Word32' #}

{#fun get_time as ^
 {} -> `Time' id #}

-- TODO:
-- add ErrorOutput
