{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}
module Storage.Simple(
        storageServer__closure,
        __remoteCallMetaData,
        putToStore,
        getFromStore,
        updateStore
        ) where

import Remote
import Data.Binary
import Data.Typeable
import Types.Generic
import Storage.Messages
                     
getData :: KVPair -> ProcessM KVPair -> ProcessM ()
getData outs ins = do
        inp <- ins
        (pid,m) <- expect
        case m of
                Put kv -> do
                        getData outs $ return (inp <++> kv)
                Get -> do
                        send pid outs
                        getData outs $ return inp
                Update -> do
                        getData inp $ return nullKV
               


storageServer :: ProcessM ()
storageServer = getData nullKV $ return nullKV

$( remotable ['storageServer] )


        
putToStore :: (Generic a,Generic b) => ProcessId -> ProcessId -> [(a,b)] -> ProcessM()
putToStore myPid slavePid xs = do
        send slavePid (myPid,Put (toKV xs))
        
getFromStore :: (Generic a,Generic b) => ProcessId -> ProcessId -> ProcessM [(a,b)]
getFromStore myPid slavePid = do
        send slavePid (myPid,Get::DataMessage KVPair)
        kv <- expect
        return $ fromKV kv

updateStore :: ProcessId -> ProcessId -> ProcessM()
updateStore myPid slavePid = do
        send slavePid (myPid,Update::DataMessage KVPair)

