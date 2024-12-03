module MongoFFI where

import Foreign
import Foreign.C

data MongoClient
type MongoClientPtr = Ptr MongoClient

foreign import ccall unsafe "lib/mongoffi.dll initialize_mongo"
    c_initialize_mongo :: CString -> Ptr CChar -> IO MongoClientPtr

foreign import ccall unsafe "lib/mongoffi.dll destroy_mongo"
    c_destroy_mongo :: MongoClientPtr -> IO ()

