
-- | Defines the storage server's client API, providing the ability to 'push', 'pull' and 'exchange' data.
module Distributed.Storage (push,pull,exchange) where


import Remote (ProcessM,expect) 
import Data.Binary (Binary,encode,decode)
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<$>))
import Distributed (Service,sendToService)
import Distributed.Storage.Common

-- | Client API function to append a list of type @['Data.ByteString.Lazy.ByteString']@ to the outputs list in the store.
push :: (Binary a) => Service                    -- ^ the datastore
        -> [a]                                   -- ^ the data to send
        -> ProcessM()                            -- ^ null marker         
push s x = do
        sendToService s $ Push (encode <$> x)                                 -- send a message

-- | Client API function to pull the inputs list of type @['Data.ByteString.Lazy.ByteString']@ from the store.        
pull :: (Binary a) => Service                    -- ^ the datastore
        -> ProcessM [a]                          -- ^ data from server wrapped in 'Remote.ProcessM'
pull s = do
        sendToService s (Pull::DataMessage)                                   -- send a message
        x <- expect                                                           -- wait for response
        return $ decode <$> x

-- | Client API function to do exchange: replaces the inputs list with the outputs list and
--   initialises the inputs list to @[]@ 
exchange :: Service                             -- ^ the datastore 
        -> ProcessM()                           -- ^ null marker        
exchange s = do
        sendToService s (Exchange::DataMessage)                               -- send message



