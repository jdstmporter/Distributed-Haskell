{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module Storage (
        putToStore,storageServer__closure,__remoteCallMetaData,
        getFromStore,
        updateStore) where 

import Remote
import Data.Binary (Binary,Get,Put,get,put,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))


data DataMessage = Push [ByteString] |  Pull | Exchange
        deriving(Typeable) 
        
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


getData :: [ByteString] -> [ByteString] -> ProcessM ()
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



storageServer :: ProcessM ()
storageServer = getData [] []
        

$( remotable ['storageServer] )



putToStore :: (Binary a) => ProcessId -> ProcessId -> [a] -> ProcessM()
putToStore myPid slavePid xs = do
        send slavePid (myPid,Push (encode <$> xs))
        
getFromStore :: (Binary a) => ProcessId -> ProcessId -> ProcessM [a]
getFromStore myPid slavePid = do
        send slavePid (myPid,Pull::DataMessage)
        kv <- expect
        return $ decode <$> kv

updateStore :: ProcessId -> ProcessId -> ProcessM()
updateStore myPid slavePid = do
        send slavePid (myPid,Exchange::DataMessage)