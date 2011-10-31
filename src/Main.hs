module Main where

import System.Environment (getArgs)
import System.Directory (copyFile)
import Paths_MR
--import Distributed.Storage.Simple
import Distributed.Storage.InMemory.Client
import Distributed.Storage.InMemory.Server
import Distributed.Storage
import Remote


{-- | Simple interaction with server; put two slugs of data then get them back
doStuff' :: ProcessId ->                 -- ^ client's unique identifier 
        ProcessId ->                    -- ^ server's unique identifier
        ProcessM()                      -- ^ null marker
doStuff' myPid slavePid = do
        say $ "Putting data to PID " ++ show slavePid
        pushToStore myPid slavePid ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]::[(Int,String)])
        say $ "Putting data to PID " ++ show slavePid
        pushToStore myPid slavePid ([(6,"a"),(7,"b"),(8,"c"),(9,"d"),(0,"e")]::[(Int,String)])
        say $ "Exchanging on PID " ++ show slavePid
        exchangeStore myPid slavePid 
        say $ "Pulling from PID " ++ show slavePid
        xs <- (pullFromStore myPid slavePid)::ProcessM [(Int,String)]
        say $ "Got "++show xs
        return ()


-- | run the CloudHaskell process
initialProcess' :: String ->     -- ^ the mode in which it runs, taken from config file 
        ProcessM ()
initialProcess' "STORAGE" =
  receiveWait []

initialProcess' "MASTER" = do
        peers <- getPeers                                               -- CloudHaskell function; gets list of machines from config file
        mypid <- getSelfPid                                             -- gets my unique identifier
        let storage = findPeerByRole peers "STORAGE"                    -- returns those machines with the role "STORAGE"
        mapM_ (\ nid  -> say("STORAGE : "++show nid)) storage
        case storage of
                [] -> error "No storage server is running"
                _  -> do
                pid <- spawn (head storage) (storageServer__closure)    -- get a unique identifier for the storage server 
                doStuff mypid pid                                       -- interact    
        return ()
     
initialProcess' _ = error "Role must be STORAGE or MASTER" --}

-- | Simple interaction with server; put two slugs of data then get them back
doStuff :: InMemory ->                  -- ^ the datastore
        ProcessM()                      -- ^ null marker
doStuff d = do
        say $ "Putting data to PID " -- ++ show slavePid
        push d ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]::[(Int,String)])
        say $ "Putting data to PID " -- ++ show slavePid
        push d ([(6,"a"),(7,"b"),(8,"c"),(9,"d"),(0,"e")]::[(Int,String)])
        say $ "Exchanging on PID " -- ++ show slavePid
        exchange d 
        say $ "Pulling from PID " -- ++ show slavePid
        xs <- (pull d)::ProcessM [(Int,String)]
        say $ "Got "++show xs
        return ()

-- | run the CloudHaskell process
initialProcess :: String ->     -- ^ the mode in which it runs, taken from config file 
        ProcessM ()
initialProcess "STORAGE" =
  receiveWait []

initialProcess "MASTER" = do
        d <- store
        doStuff d
        return ()
     
initialProcess _ = error "Role must be STORAGE or MASTER"



usage :: IO()
usage = putStrLn "Usage:\n\tMR -c : run as client\n\tMR -s : run as server"
        
main::IO()
main = do
        args <- getArgs
        case args of
                [] -> usage
                _ -> do
                case (head args) of
                        "-c" -> init "config_master"
                        "-s" -> init "config_storage"
                        _    -> usage 
                where
                        init confFile = do
                                conf <- getDataFileName confFile
                                putStrLn $ "Using config file " ++ show conf
                                copyFile conf ".config"                         -- get the right config file in the right place
                                remoteInit (Just ".config") [Distributed.Storage.InMemory.Server.__remoteCallMetaData] initialProcess