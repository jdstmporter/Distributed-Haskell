module Main where

import System.Environment (getArgs)
import System.Directory (copyFile)
import Paths_MR
import Storage
import Remote

initialProcess :: String -> ProcessM ()
initialProcess "STORAGE" =
  receiveWait []

initialProcess "MASTER" = do
        peers <- getPeers
        mypid <- getSelfPid
        let storage = findPeerByRole peers "STORAGE"
        mapM_ (\ nid  -> say("STORAGE : "++show nid)) storage
        pid <- spawn (head storage) (storageServer__closure) 
        doStuff mypid pid       
        return ()
     

initialProcess _ = error "Role must be STORAGE or MASTER"


        


doStuff :: ProcessId -> ProcessId -> ProcessM()
doStuff myPid slavePid = do
        say $ "Putting data to PID " ++ show slavePid
        putToStore myPid slavePid ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]::[(Int,String)])
        say $ "Putting data to PID " ++ show slavePid
        putToStore myPid slavePid ([(6,"a"),(7,"b"),(8,"c"),(9,"d"),(0,"e")]::[(Int,String)])
        say $ "Exchanging on PID " ++ show slavePid
        updateStore myPid slavePid 
        say $ "Pulling from PID " ++ show slavePid
        xs <- (getFromStore myPid slavePid)::ProcessM [(Int,String)]
        say $ "Got "++show xs
        return ()
        


main::IO()
main = do
        args <- getArgs
        case (head args) of
                "-m" -> init "config_master"
                "-s" -> init "config_storage"
                _    -> putStrLn "Option must be -m or -s"
        where
                init confFile = do
                        conf <- getDataFileName confFile
                        putStrLn (show conf)
                        copyFile conf ".config"
                        remoteInit (Just ".config") [Storage.__remoteCallMetaData] initialProcess