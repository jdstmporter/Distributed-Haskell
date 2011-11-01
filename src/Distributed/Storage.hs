{-# LANGUAGE TemplateHaskell #-}

-- | Defines the typeclass for a storage server's client API, providing the ability to 'push', 'pull' and 'exchange' data,
--   as well as a means to discover datastores.
module Distributed.Storage (store,push,pull,exchange,makeServer,metadata,DataStore,DataStoreServer) where


import Remote (ProcessId,ProcessM,getPeers,getSelfPid,findPeerByRole,spawn,send,expect) 
import Remote.Closure (Closure)
import Remote.Reg (Lookup)
import Data.Binary (Binary,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))
import Distributed.Storage.Common


-- | The datastore type - just the 'Remote.ProcessId' for the client and the server.
data DataStore = In ProcessId ProcessId

-- | The datastore server type.  Represents information needed to access a server instance.
data DataStoreServer = Se { closure :: Closure (ProcessM ()), metadata :: Lookup -> Lookup }

-- | Construct a 'DataStoreServer' from a CloudHaskell closure and metadata.  Allows one to 
--   hide the CloudHaskell generated functions from applications
makeServer :: Closure (ProcessM ())                                             -- ^ The closure
        -> (Lookup -> Lookup)                                                   -- ^ The metadata
        -> DataStoreServer                                                      -- ^ The representative of the server
makeServer = Se


-- | method to find the 'Remote.ProcessId' of a server in a given role
getStoragePID :: DataStoreServer                                                -- ^ The server object
        -> String                                                               -- ^ The CloudHaskell role
        -> ProcessM ProcessId                                                   -- ^ The 'Remote.ProcessId' wrapped in 'Remote.ProcessM'
getStoragePID server role = do
        peers <- getPeers                                                       -- CloudHaskell function; gets list of machines from config file
        mypid <- getSelfPid                                                     -- gets my unique identifier
        let storage = findPeerByRole peers $ role                               -- returns those machines with the role "STORAGE"
        -- mapM_ (\ nid  -> say("STORAGE : "++show nid)) storage
        case storage of
                [] -> error "No storage server is running"
                _  -> do
                        pid <- spawn (head storage) (closure server)            -- get a unique identifier for the storage server 
                        return pid



-- | The instance of 'Distributed.Storage.DataStore'.  
--   Some stuff from @Remote@ to find the client and server 'Remote.ProcessId', and then
--   the methods themselves, sending the messages from 'Distributed.Storage.Common.DataMessage' to the server and
--   serialising / deserialising data.

-- | Simple method to discover a store
store :: DataStoreServer                         -- ^ the identifier of the server to connect to
        -> String                                -- ^ the CloudHaskell role of the server
        -> ProcessM DataStore                    -- ^ the datastore wrapped in 'Remote.ProcessM'
store server role = do
        mypid <- getSelfPid                                                     -- gets my unique identifier
        spid <- getStoragePID server role
        return $ In mypid spid
        
-- | Client API function to append a list of type @['Data.ByteString.Lazy.ByteString']@ to the outputs list in the store.
push :: (Binary a) => DataStore                  -- ^ the datastore
        -> [a]                                   -- ^ the data to send
        -> ProcessM()                            -- ^ null marker         
push (In mypid spid) x = do
        send spid (mypid,Push (encode <$> x))                                   -- send a message

-- | Client API function to pull the inputs list of type @['Data.ByteString.Lazy.ByteString']@ from the store.        
pull :: (Binary a) => DataStore                  -- ^ the datastore
        -> ProcessM [a]                          -- ^ data from server wrapped in 'Remote.ProcessM'
pull (In mypid spid) = do
        send spid (mypid,Pull::DataMessage)                                     -- send a message
        x <- expect                                                             -- wait for response
        return $ decode <$> x

-- | Client API function to do exchange: replaces the inputs list with the outputs list and
--   initialises the inputs list to @[]@ 
exchange :: DataStore                           -- ^ the datastore 
        -> ProcessM()                           -- ^ null marker        
exchange (In mypid spid) = do
        send spid (mypid,Exchange::DataMessage)                                 -- send message



