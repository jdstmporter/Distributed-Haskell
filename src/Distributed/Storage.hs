-- | Defines the typeclass for a storage server's client API, providing the ability to 'push', 'pull' and 'exchange' data,
--   as well as a means to discover datastores.
module Distributed.Storage where


import Remote (ProcessM)
import Data.Binary(Binary)


-- | The API for a datastore
class DataStore d where
        -- | Simple method to discover a store
        store :: ProcessM d                              -- ^ the datastore wrapped in 'Remote.ProcessM'
        
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
