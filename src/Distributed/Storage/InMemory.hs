{-# LANGUAGE TemplateHaskell #-}

module Distributed.Storage.InMemory where

import Distributed.Storage
import Distributed.Storage.InMemory.Server

-- | Method to get the various bits of CloudHaskell machinery associated to a server.  Allows
--   other parts of 'Distributed.Storage' to abstract away from the actual implementation.
getInMemoryServer :: DataStoreServer    -- ^ The 'Distributed.Storage.Common.DataStoreServer' object.
getInMemoryServer = makeServer storageServer__closure Distributed.Storage.InMemory.Server.__remoteCallMetaData
        
