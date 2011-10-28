{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module StoragePoly where

 

import Remote
import Data.Binary (Binary,Get,Put,get,put,encode,decode)
import Data.Typeable
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))


data (Serializable a) => DataMessage a = Put a |  Get a | Update a
        deriving(Typeable) 
        
instance (Serializable a) => Binary (DataMessage a)  where
        put (Put kv) = put 'P' >> put kv
        put (Update kv) = put 'U'>> put kv
        put (Get kv) = put 'G' >> put kv
        get = do
                h <- get :: Get Char
                case h of
                        'P' -> do
                                kv <- get
                                return (Put kv)
                        'G' -> do
                                kv <- get
                                return (Get kv)
                        'U' -> do
                                kv <- get
                                return (Update kv)


getData :: (Serializable a,Serializable b) => [a] -> [b] -> ProcessM ()
getData ins outs = do
        (pid,m) <- expect
        case m of
                Put x -> do
                        say ("Doing Put from " ++ show pid)
                        getData outs $ ins ++ x
                Get x -> do
                        say ("Doing Get to " ++ show pid)
                        send pid outs
                        getData ins outs
                Update x -> do
                        say ("Doing Update from " ++ show pid)
                        getData (tail x) ins

putToStore :: (Serializable a) => ProcessId -> ProcessId -> [a] -> ProcessM()
putToStore myPid slavePid xs = do
        send slavePid (myPid,Put xs)
        
getFromStore :: (Serializable a) => ProcessId -> ProcessId -> a -> ProcessM [a]
getFromStore myPid slavePid x = do
        send slavePid (myPid,Get x)
        kv <- expect
        return kv

empty :: a -> [a]
empty x = []

updateStore :: (Serializable a) => ProcessId -> ProcessId -> a -> ProcessM()
updateStore myPid slavePid x = do
        send slavePid (myPid,Update [x])
        
        
storageServer :: ProcessM ()
storageServer = getData (empty (1::Int,"x")) (empty (1::Int,"x"))
        

$( remotable ['storageServer] )