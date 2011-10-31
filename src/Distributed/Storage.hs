-- | Defines the typeclass for a storage server's client API, providing the ability to 'push', 'pull' and 'exchange' data,
--   as well as a means to discover datastores.
module Distributed.Storage where


import Remote (ProcessM)
import Remote.Closure(Closure)
import Data.Binary(Binary)


-- | The client API for a datastore
class DataStore d where
        -- | Simple method to discover a store
        store :: String                                  -- ^ the CloudHaskell role identifier for the server
                -> ProcessM d                            -- ^ the datastore wrapped in 'Remote.ProcessM'
        
        -- | Client API function to append a list of type @['Data.ByteString.Lazy.ByteString']@ to the outputs list in the store.
        push :: (Binary a) => d                          -- ^ the datastore
                -> [a]                                   -- ^ the data to send
                -> ProcessM()                            -- ^ null marker 
                
        -- | Client API function to pull the inputs list of type @['Data.ByteString.Lazy.ByteString']@ from the store.        
        pull :: (Binary a) => d                          -- ^ the datastore
                -> ProcessM [a]                          -- ^ data from server wrapped in 'Remote.ProcessM'
                
        -- | Client API function to do exchange: replaces the inputs list with the outputs list and
        --   initialises the inputs list to @[]@ 
        exchange :: d                                   -- ^ the datastore 
                -> ProcessM()                           -- ^ null marker

-- | The management API for a datastore
class DataStoreServer d where
        -- | start the server
        start :: d                                      -- ^ the server
        
        -- | stop the server
        stop :: d                                       -- ^ the server
                -> ProcessM ()                          -- ^ nil marker
        
        -- | return the 'Remote.Closure.Closure' that CloudHaskell uses to identify the server        
        closure :: d                                    -- ^ the server
                -> Closure (ProcessM ())                -- ^ its server