{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module StoragePoly where

 

import Remote
import Data.Binary (Binary,Get,Put,get,put,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))


data (Serializable a) => DataMessage a = Put a |  Get | Update a
        deriving(Typeable) 
        
instance (Serializable a) => Binary (DataMessage a)  where
        put (Put kv) = put 'P' >> put kv
        put (Update kv) = put 'U'>> put kv
        put Get = put 'G'
        get = do
                h <- get :: Get Char
                case h of
                        'P' -> do
                                kv <- get
                                return (Put kv)
                        'G' -> return Get
                        'U' -> do
                                kv <- get
                                return (Update kv)


getData :: [ByteString] -> [ByteString] -> ProcessM ()
getData ins outs = do
        (pid,m) <- expect
        case m of
                Put x -> do
                        getData outs $ ins ++ x
                Get -> do
                        send pid outs
                        getData ins outs
                Update x -> do
                        getData (tail x) ins

putToStore :: (Serializable a) => ProcessId -> ProcessId -> [a] -> ProcessM()
putToStore myPid slavePid xs = do
        send slavePid (myPid,Put xs)
        
getFromStore :: (Serializable a) => ProcessId -> ProcessId -> ProcessM [a]
getFromStore myPid slavePid= do
        send slavePid (myPid,Get::DataMessage ())
        kv <- expect
        return kv

empty :: a -> [a]
empty x = []

updateStore :: (Serializable a) => ProcessId -> ProcessId -> a -> ProcessM()
updateStore myPid slavePid x = do
        send slavePid (myPid,Update [x])
        
        
storageServer :: ProcessM ()
storageServer = getData [] []
        

$( remotable ['storageServer] )