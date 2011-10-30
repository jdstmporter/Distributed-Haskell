{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module Storage (
        pushToStore,storageServer__closure,__remoteCallMetaData,
        pullFromStore,
        exchangeStore) where 

import Remote
import Data.Binary (Binary,Get,Put,get,put,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))

-- | Module that defines the a simple distributed storage service.
--
--   The server runs as a CloudHaskell service, and the API consists of three
--   functions to push, pull and exchange data.  All client interaction takes 
-- place within the @ProcessM@ monad.


-- | Basic type that is used to pass messages between client and server.  Messages
--   either push some data, request a pull, or request an exchange.  Derives @Typeable@
--   as part of making it @Serializable@.
data DataMessage = Push [ByteString] |  Pull | Exchange
        deriving(Typeable) 

-- | Make it @Binary@ to enable @Serializable@.  Standard boilerplate.        
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


-- | The server code.  Executes within the @ProcessM@ monad, as it involves message passing.
getData :: [ByteString]                         -- ^ data from clients
        -> [ByteString]                         -- ^ data to send to clients
        -> ProcessM ()                          -- ^ null marker; the function never returns
getData ins outs = do
        (pid,m) <- expect
        case m of
                Push x -> do
                        say $ "PID " ++ show pid ++ " adding data"
                        getData (ins ++ x) outs
                Pull -> do
                        say $ "PID " ++ show pid ++ " getting data"
                        send pid outs
                        getData ins outs
                Exchange -> do
                        say $ "PID " ++ show pid ++ " exchanging"
                        getData [] ins


-- | The server itself.  Simply invokes @getData@
storageServer :: ProcessM ()
storageServer = getData [] []
        

$( remotable ['storageServer] )


-- | Client API function to push a list of items to the store.
pushToStore :: (Binary a) => ProcessId           -- ^ client's unique identifier
        -> ProcessId                             -- ^ server's unique identifier
        -> [a]                                   -- ^ the data to send
        -> ProcessM()                            -- ^ null marker
pushToStore myPid slavePid xs = do
        send slavePid (myPid,Push (encode <$> xs))

-- | Client API function to pull a list of items from the store.        
pullFromStore :: (Binary a) => ProcessId         -- ^ client's unique identifier
        -> ProcessId                             -- ^ server's unique identifier
        -> ProcessM [a]                          -- ^ data from server wrapped in @ProcessM@            
pullFromStore myPid slavePid = do
        send slavePid (myPid,Pull::DataMessage)
        kv <- expect
        return $ decode <$> kv

-- | Client API function to do exchange.
exchangeStore :: ProcessId                      -- ^ client's unique identifier
        -> ProcessId                            -- ^ server's unique identifier 
        -> ProcessM()                           -- ^ null marker
exchangeStore myPid slavePid = do
        send slavePid (myPid,Exchange::DataMessage)