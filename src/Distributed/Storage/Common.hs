{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

-- | Common types for message passing by distributed stores, and a generic API to be provided by a future version
--   of the service.
module Distributed.Storage.Common where

import Data.ByteString.Lazy (ByteString)
import Data.Binary (Binary,Get,Put,get,put)
import Data.Typeable
import Remote (ProcessId,ProcessM,Serializable)


-- | Basic type that is used to pass messages between client and server.  Messages
--   either push some data, request a pull, or request an exchange.  Derives 'Data.Typeable.Typeable'
--   as part of making it 'Remote.Serializable'.
data DataMessage = Push [ByteString] |  Pull | Exchange
        deriving(Typeable) 

-- | Make it 'Data.Binary.Binary' to enable 'Remote.Serializable'.  Standard boilerplate.        
instance Binary DataMessage  where
        put (Push kv) = put 'P' >> put kv
        put Exchange = put 'U'
        put Pull = put 'G'
        get = do
                h <- get :: Get Char
                case h of
                        'P' -> do
                                kv <- get
                                return (Push kv)
                        'G' -> return Pull
                        'U' -> return Exchange
                        
-- | The client API for a datastore
class DataStore d where
        -- | Client API function to append a list of type @['Data.ByteString.Lazy.ByteString']@ to the outputs list in the store.
        push :: (Binary a) => d                          -- ^ the datastore
                -> ProcessId                             -- ^ client's unique identifier
                -> ProcessId                             -- ^ server's unique identifier
                -> [a]                                   -- ^ the data to send
                -> ProcessM()                            -- ^ null marker 
                
        -- | Client API function to pull the inputs list of type @['Data.ByteString.Lazy.ByteString']@ from the store.        
        pull :: (Binary a) => d                          -- ^ the datastore
                -> ProcessId                             -- ^ client's unique identifier
                -> ProcessId                             -- ^ server's unique identifier
                -> ProcessM [a]                          -- ^ data from server wrapped in 'ProcessM'
                
        -- | Client API function to do exchange: replaces the inputs list with the outputs list and
        --   initialises the inputs list to @[]@ 
        exchange :: d                                   -- ^ the datastore 
                -> ProcessId                            -- ^ client's unique identifier
                -> ProcessId                            -- ^ server's unique identifier 
                -> ProcessM()                           -- ^ null marker