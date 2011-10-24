{-# LANGUAGE TemplateHaskell,DeriveDataTypeable,TypeSynonymInstances #-}

module Types.Generic(
        KVPair,
        (<++>),
        nullKV,
        toKV,
        fromKV,
        Generic
        ) where

import Data.Typeable
import Data.Binary
import Control.Applicative ((<$>))

data GenericData = S [String] | I [Int] | B [Bool] | N
        deriving(Typeable,Eq,Show)
        
instance Binary (GenericData) where
        put (S s) = put 'S' >> put s
        put (I i) = put 'I' >> put i
        put (B b) = put 'B' >> put b
        put (N)   = put 'N'
        get = do
                h <- get :: Get Char
                case h of
                        'S' -> do
                                s <- get :: Get [String]
                                return (S s)
                        'I' -> do
                                i <- get :: Get [Int]
                                return (I i)
                        'B' -> do
                                b <- get :: Get [Bool]
                                return (B b)
                        'N' -> return N  

(<+>) :: GenericData -> GenericData -> GenericData
(<+>) N x = x
(<+>) x N = x
(<+>) (S x) (S y) = S (x++y)   
(<+>) (I x) (I y) = I (x++y)
(<+>) (B x) (B y) = B (x++y) 
(<+>) _ _ = error "Cannot combine types"

                       
class Generic a where
        putG :: [a] -> GenericData
        getG :: GenericData -> [a]
               
instance Generic Int where
        putG xs = I xs
        getG (I xs) = xs
        getG _ = error "Type conversion error"
       
        
instance Generic String where
        putG xs = S xs
        getG (S xs) = xs
        getG _ = error "Type conversion error"

instance Generic Bool where
        putG xs = B xs
        getG (B xs) = xs
        getG _ = error "Type conversion error"               
        
data KVPair = KV GenericData GenericData
        deriving(Typeable)

instance Binary KVPair where
        put (KV k v) = put k >> put v
        get = do
                k <- get :: Get GenericData
                v <- get :: Get GenericData
                return $ KV k v        


(<++>) :: KVPair -> KVPair -> KVPair
(<++>) (KV k1 v1) (KV k2 v2) = KV (k1 <+> k2) (v1 <+> v2)

nullKV :: KVPair
nullKV = KV N N
  
fromKV :: (Generic a,Generic b) => KVPair -> [(a,b)]
fromKV (KV k v) = zip (getG k) (getG v)

toKV :: (Generic a,Generic b) => [(a,b)] -> KVPair
toKV xs = KV (putG $ fst <$> xs) (putG $ snd <$> xs)