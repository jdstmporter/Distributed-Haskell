module Main where

import StoragePoly
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
        say("Putting stuff")
        putToStore myPid slavePid ([(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]::[(Int,String)])
        say("Putting more stuff")
        putToStore myPid slavePid ([(6,"a"),(7,"b"),(8,"c"),(9,"d"),(0,"e")]::[(Int,String)])
        say("Swapping stuff")
        updateStore myPid slavePid (1::Int,"x")
        say("Getting stuff")
        xs <- (getFromStore myPid slavePid (1::Int,"x"))::ProcessM [(Int,String)]
        say("Got "++show xs)
        return ()
        


main::IO()
main = remoteInit (Just "config") [StoragePoly.__remoteCallMetaData] initialProcess