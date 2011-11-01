{-# LANGUAGE TemplateHaskell #-}

-- | The wrapper module for the in memory service.  Provides a single method that gives a 
--   handle on the server instance and then re-exports methods required from other packages.
module Distributed.Storage.InMemory (Service,service,metadata,push,pull,exchange,getInMemoryServer) where

import Distributed
import Distributed.Storage
import Distributed.Storage.InMemory.Server

-- | Method to get the various bits of CloudHaskell machinery associated to a server.  Allows
--   other parts of 'Distributed.Storage' to abstract away from the actual implementation.
getInMemoryServer :: ServerHandle    -- ^ The 'Distributed.ServerHandle' object.
getInMemoryServer = makeServer storageServer__closure Distributed.Storage.InMemory.Server.__remoteCallMetaData
        
