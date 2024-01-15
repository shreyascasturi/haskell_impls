-- try and open a file
oFile :: String -> IO Handle
oFile fileName = openFile 
