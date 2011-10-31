{-# LANGUAGE DeriveDataTypeable #-}

-- | Common type for message passing by distributed stores.  Used only by store implementations.
module Distributed.Storage.Common where

import Data.ByteString.Lazy (ByteString)
import Data.Binary (Binary,Get,Put,get,put)
import Data.Typeable
import Remote (ProcessId,ProcessM,Serializable,getPeers,getSelfPid,findPeerByRole,spawn)
import Remote.Closure


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
                        
getStoragePID :: String -> Closure (ProcessM ()) -> ProcessM ProcessId
getStoragePID role closure = do
        peers <- getPeers                                                       -- CloudHaskell function; gets list of machines from config file
        mypid <- getSelfPid                                                     -- gets my unique identifier
        let storage = findPeerByRole peers role                                 -- returns those machines with the role "STORAGE"
        -- mapM_ (\ nid  -> say("STORAGE : "++show nid)) storage
        case storage of
                [] -> error "No storage server is running"
                _  -> do
                        pid <- spawn (head storage) (closure)    -- get a unique identifier for the storage server 
                        return pid