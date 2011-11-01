module Main where

import System.Environment (getArgs)
import System.Directory (copyFile)
import Paths_MR
import Distributed.Storage.InMemory
import Remote



-- | Simple interaction with server; put two slugs of data then get them back
doStuff :: Service ->                 -- ^ the datastore
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
initialProcess :: String ->     -- ^ The CloudHaskell role I am running in
        ProcessM ()             -- ^ Nill return
initialProcess "STORAGE" = receiveWait []
initialProcess "MASTER" = do
        let s = getInMemoryServer 
        d <- service s "STORAGE"
        doStuff d
        return ()
initialProcess _ = error "Role must be STORAGE or MASTER"


-- | Print usage string
usage :: IO()
usage = putStrLn "Usage:\n\tMR -c : run as client\n\tMR -s : run as server"

-- | Main method.  Accepts command-line argument "-s" for server and "-c" for client.         
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
                                let s = getInMemoryServer
                                remoteInit (Just ".config") [metadata s] initialProcess