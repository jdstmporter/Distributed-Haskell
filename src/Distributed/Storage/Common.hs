{-# LANGUAGE DeriveDataTypeable #-}

-- | Common type for message passing by distributed stores.  Used only by store implementations.
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
                        
