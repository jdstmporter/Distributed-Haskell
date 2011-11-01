-- | Module providing basic machinery for distributed services.  This includes a server instance handle, 
--   a client handle
module Distributed (getServerPID,makeServer,closure,metadata,service,sendToService,ServerHandle,Service) where


import Remote (ProcessId,ProcessM,Serializable,getPeers,getSelfPid,findPeerByRole,spawn,send,expect)
import Remote.Closure (Closure)
import Remote.Reg (Lookup) 



-- | private method to find the 'Remote.ProcessId' of a server in a given role
getServerPID :: ServerHandle                                                    -- ^ The server object
        -> String                                                               -- ^ The CloudHaskell role
        -> ProcessM ProcessId                                                   -- ^ The 'Remote.ProcessId' wrapped in 'Remote.ProcessM'
getServerPID server role = do
        peers <- getPeers                                                       -- CloudHaskell function; gets list of machines from config file
        mypid <- getSelfPid                                                     -- gets my unique identifier
        let storage = findPeerByRole peers $ role                               -- returns those machines with the role "STORAGE"
        -- mapM_ (\ nid  -> say("STORAGE : "++show nid)) storage
        case storage of
                [] -> error $ "No "  ++ role ++ " server is running"
                _  -> do
                        pid <- spawn (head storage) (closure server)            -- get a unique identifier for the storage server 
                        return pid


-- | The service handle - used by the client API to talk to distributed services.
data Service = In { mypid :: ProcessId, spid :: ProcessId }

-- | Simple method to discover a service
service :: ServerHandle                            -- ^ the identifier of the server to connect to
        -> String                                -- ^ the CloudHaskell role of the server
        -> ProcessM Service                      -- ^ the service wrapped in 'Remote.ProcessM'
service server role = do
        mypid <- getSelfPid                                                     -- gets my unique identifier
        spid <- getServerPID server role
        return $ In mypid spid

-- | Method to send a message of type @('Remote.ProcessId',a)@ to the service identifier by the
--   'Service' argument.  Wrapper round 'Remote.send'
sendToService :: (Serializable a) => Service            -- ^ The service to send to
        -> a                                            -- ^ The message
        -> ProcessM ()
sendToService s m = send (spid s) (mypid s,m)

-- | The server handle.  Represents information needed to access a server instance.
data ServerHandle = Se { closure :: Closure (ProcessM ()), metadata :: Lookup -> Lookup }

-- | Construct a 'ServerHandle' for a CloudHaskell server instance.  Allows one to 
--   hide the CloudHaskell generated functions from applications
makeServer :: Closure (ProcessM ())                                             -- ^ The closure
        -> (Lookup -> Lookup)                                                   -- ^ The metadata
        -> ServerHandle                                                         -- ^ The representative of the server¤
makeServer = Se