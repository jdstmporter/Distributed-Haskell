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


data DataMessage = Put [ByteString] |  Get | Update | New [ByteString]
        deriving(Typeable) 
        
instance Binary DataMessage  where
        put (Put kv) = put 'P' >> put kv
        put Update = put 'U'
        put Get = put 'G'
        get = do
                h <- get :: Get Char
                case h of
                        'P' -> do
                                kv <- get
                                return (Put kv)
                        'G' -> return Get
                        'U' -> return Update


getData :: [ByteString] -> [ByteString] -> ProcessM ()
getData ins outs = do
        (pid,m) <- expect
        case m of
                Put x -> do
                        getData outs $ ins ++ x
                Get -> do
                        send pid outs
                        getData ins outs
                Update -> do
                        getData [] ins



storageServer :: ProcessM ()
storageServer = getData [] []
        

$( remotable ['storageServer] )



putToStore :: (Binary a) => ProcessId -> ProcessId -> [a] -> ProcessM()
putToStore myPid slavePid xs = do
        send slavePid (myPid,Put (encode <$> xs))
        
getFromStore :: (Binary a) => ProcessId -> ProcessId -> ProcessM [a]
getFromStore myPid slavePid = do
        send slavePid (myPid,Get::DataMessage)
        kv <- expect
        return $ decode <$> kv

updateStore :: ProcessId -> ProcessId -> ProcessM()
updateStore myPid slavePid = do
        send slavePid (myPid,Update::DataMessage)