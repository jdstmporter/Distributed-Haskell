
-- | Module for simple encapsulated distributed storage service, using an in-memory store.  It implements
--   the class 'Distributed.Storage.DataStore' which provides basic methods for discovering datastores
--   and interacting with them.
module Distributed.Storage.InMemory.Client where

import Distributed.Storage

import Remote (getPeers,getSelfPid,findPeerByRole,spawn,ProcessId,send,expect)
import Data.Binary (Binary,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))
import Distributed.Storage.Common
import Distributed.Storage.InMemory.Server(storageServer__closure)

-- | The datastore type - just the 'Remote.ProcessId' for the client and the server.
data InMemory = In ProcessId ProcessId

-- | The instance of 'Distributed.Storage.DataStore'.  
--   Some stuff from @Remote@ to find the client and server 'Remote.ProcessId', and then
--   the methods themselves, sending the messages from 'Distributed.Storage.Common.DataMessage' to the server and
--   serialising / deserialising data.
instance DataStore InMemory where
        store role = do
                mypid <- getSelfPid                                                     -- gets my unique identifier
                spid <- getStoragePID role storageServer__closure
                return $ In mypid spid
        push (In mypid spid) x = do
                send spid (mypid,Push (encode <$> x))                                   -- send a message
        pull (In mypid spid) = do
                send spid (mypid,Pull::DataMessage)                                     -- send a message
                x <- expect                                                             -- wait for response
                return $ decode <$> x
        exchange (In mypid spid) = do
                send spid (mypid,Exchange::DataMessage)                                 -- send message
                





        
        
