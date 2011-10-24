{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module Storage.Polymorphic where

import Remote
import Data.Binary
import Data.Typeable
import Data.Maybe (fromJust)
import Storage.Messages

data Stored a = Empty | Data a



(<++>) :: Stored [a] -> [a] -> Stored [a]
(<++>) Empty x = Data x
(<++>) (Data x) y = Data (x ++ y)

extract :: Stored [a] -> [a]
extract Empty = error "No data"
extract (Data x) = x

nil :: [a] -> Stored [a]
nil x = Empty




getData :: (Serializable a) => Stored [a] -> ProcessM (Stored [a]) -> ProcessM ()
getData outs ins = do
        inp <- ins
        (pid,m) <- expect
        case m of
                Put xs -> do
                        getData outs $ return (inp <++> xs)
                Get -> do
                        send pid (extract outs)
                        getData outs $ return inp
                New xs -> do
                        getData (Data xs) $ return (nil xs)
               


storageServer :: ProcessM ()
storageServer = getData n $ return n
        where
                n = nil ([]::[()])

$( remotable ['storageServer] )