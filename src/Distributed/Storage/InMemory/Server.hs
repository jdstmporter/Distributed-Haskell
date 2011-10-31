{-# LANGUAGE TemplateHaskell #-}

-- | The InMemory storage server.  A simple loop that sits waiting for messages from clients and responds
--   appropriately.  Stores data in a @['Data.ByteString.Lazy']@.
module Distributed.Storage.InMemory.Server where

import Remote
import Data.Binary (Binary,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))
import Distributed.Storage.Common
import Distributed.Storage

data Server = Se (ProcessM ())

-- | The server code.  Executes within the 'Remote.ProcessM' monad, as it involves message passing.
--   It waits for messages with 'Remote.expect' and then process the data accordingly (as well as
--   producing log messages.
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


-- | The server itself.  Simply invokes 'getData' with empty initial stores.
storageServer :: ProcessM ()
storageServer = getData [] []
        
-- | CloudHaskell boilerplate
$( remotable ['storageServer] )

instance DataStoreServer Server where
        start = Se storageServer
        stop d = return ()  -- find out how to kill it
        closure d = storageServer__closure -- find out how to derive the closure from the function
        
