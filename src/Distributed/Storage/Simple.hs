{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

-- | Module that defines the a simple distributed storage service.
--
--   The server runs as a CloudHaskell service, and the API consists of three
--   functions to push, pull and exchange data.  All client interaction takes 
--   place within the @ProcessM@ monad.
--
--   The basic model is this: the data stored is in the form of @['Data.ByteString.Lazy.ByteString']@.
--
--   (1) The server holds an /inputs list/, which clients can request a copy of with the
--   function 'pullFromStore'.  
--
--   (2) It holds an /outputs list/, to which instances of @['Data.ByteString.Lazy.ByteString']@ that
--   clients send to the server with 'pushToStore' are appended.
--
--   (3) There is an operation 'exchangeStore' that replaces the inputs list with 
--   the contents of the outputs list, and initialises the outputs list to @[]@.
--
--   The idea is that it be used in a distributed round-based computation, where
--   the inputs list holds the input data for the processing nodes, which use
--  'pullFromStore' to get it.  They then process it and each pushes its output
--   back to the server with 'pushToStore', resulting in the server appending it
--   to the outputs list.  Then, when a round is completed a
--   master invokes 'exchangeStore' to turn the concatenated outputs into the
--   inputs for the next round.
module Distributed.Storage.Simple (
        pushToStore,storageServer__closure,__remoteCallMetaData,
        pullFromStore,
        exchangeStore) where 

import Remote
import Data.Binary (Binary,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))
import Distributed.Storage.Common






-- | The server code.  Executes within the 'Remote.ProcessM' monad, as it involves message passing.
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


-- | The server itself.  Simply invokes 'getData'
storageServer :: ProcessM ()
storageServer = getData [] []
        

$( remotable ['storageServer] )


-- | Client API function to append a list of type @['Data.ByteString.Lazy.ByteString']@ to the outputs list in the store.
pushToStore :: (Binary a) => ProcessId           -- ^ client's unique identifier
        -> ProcessId                             -- ^ server's unique identifier
        -> [a]                                   -- ^ the data to send
        -> ProcessM()                            -- ^ null marker
pushToStore myPid slavePid xs = do
        send slavePid (myPid,Push (encode <$> xs))

-- | Client API function to pull the inputs list of type @['Data.ByteString.Lazy.ByteString']@ from the store.        
pullFromStore :: (Binary a) => ProcessId         -- ^ client's unique identifier
        -> ProcessId                             -- ^ server's unique identifier
        -> ProcessM [a]                          -- ^ data from server wrapped in 'ProcessM'            
pullFromStore myPid slavePid = do
        send slavePid (myPid,Pull::DataMessage)
        kv <- expect
        return $ decode <$> kv

-- | Client API function to do exchange: replaces the inputs list with the outputs list and
--   initialises the inputs list to @[]@ 
exchangeStore :: ProcessId                      -- ^ client's unique identifier
        -> ProcessId                            -- ^ server's unique identifier 
        -> ProcessM()                           -- ^ null marker
exchangeStore myPid slavePid = do
        send slavePid (myPid,Exchange::DataMessage)