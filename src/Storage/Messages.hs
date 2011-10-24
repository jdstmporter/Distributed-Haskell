{-# LANGUAGE TemplateHaskell,DeriveDataTypeable #-}

module Storage.Messages where


import Data.Binary
import Data.Typeable


data DataMessage a = Put a |  Get | Update | New a
        deriving(Typeable) 
        
instance (Binary a) => Binary (DataMessage a)  where
        put (Put kv) = put 'P' >> put kv
        put Update = put 'U'
        put Get = put 'G'
        put (New kv) = put 'N' >> put kv
        get = do
                h <- get :: Get Char
                case h of
                        'P' -> do
                                kv <- get
                                return (Put kv)
                        'G' -> return Get
                        'U' -> return Update
                        'N' -> do
                                kv <- get
                                return (New kv)