module LibCurl
  ( module LibCurl
  , module LibCurl.Constants
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BS
import Data.IORef
import Data.List qualified as List
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

import LibCurl.Constants

foreign import ccall "curl_global_init"
  curl_global_init :: CURLGlobalFlags -> IO ()

foreign import ccall "curl_global_cleanup"
  curl_global_cleanup :: IO ()

data CurlSlist

foreign import ccall "curl_slist_append"
  curl_slist_append :: Ptr CurlSlist -> Ptr a -> IO (Ptr CurlSlist)

foreign import ccall "curl_slist_free_all"
  curl_slist_free_all :: Ptr CurlSlist -> IO ()

withCurlSlist :: [String] -> (Ptr CurlSlist -> IO a) -> IO a
withCurlSlist strs = bracket aquire release . (. snd) where
  aquire = do
    cstrs <- mapM newCString strs
    slist <- foldM curl_slist_append nullPtr cstrs
    return (cstrs, slist)
  release (cstrs, slist) = do
    curl_slist_free_all slist
    mapM_ free cstrs

data Curl

foreign import ccall "curl_easy_init"
  curl_easy_init :: IO (Ptr Curl)

foreign import ccall "curl_easy_cleanup"
  curl_easy_cleanup :: Ptr Curl -> IO ()

foreign import ccall "curl_easy_perform"
  curl_easy_perform :: Ptr Curl -> IO CURLcode

data CurlMethod = CurlGET | CurlPOST ByteString deriving Show

data CurlRequest = CurlRequest
  { url :: ByteString
  , method :: CurlMethod
  , headers :: [String]
  , verbose :: Bool
  } deriving (Show, Generic)

foreign import ccall "curl_easy_setopt"
  curl_easy_setopt_slist :: Ptr Curl -> CURLoption -> Ptr CurlSlist -> IO ()

foreign import ccall "curl_easy_setopt"
  curl_easy_setopt_long :: Ptr Curl -> CURLoption -> CLong -> IO ()

foreign import ccall "curl_easy_setopt"
  curl_easy_setopt_str :: Ptr Curl -> CURLoption -> CString -> IO ()

foreign import ccall "curl_easy_setopt"
  curl_easy_setopt_fun :: Ptr Curl -> CURLoption -> FunPtr a -> IO ()

data CurlResponse = CurlResponse
  { code :: CURLcode
  , status :: CLong
  , headers :: [ByteString]
  , body :: BSL.ByteString
  } deriving (Show, Generic)

curl :: CurlRequest -> IO CurlResponse
curl o = do
  bodyChunksRef <- newIORef []
  headerChunksRef <- newIORef []
  withCurlHandle \hdl ->
    withCurlSlist o.headers \hdrs ->
    BS.useAsCString o.url \urlz ->
    withCURLCallback (collectChunksCallback bodyChunksRef) \writeBodyCb ->
    withCURLCallback (collectChunksCallback headerChunksRef) \writeHeaderCb ->
    do
      case o.method of
        CurlGET -> pure ()
        CurlPOST body -> BS.unsafeUseAsCStringLen body \(ptr, len) -> do
          curl_easy_setopt_long hdl cURLOPT_POSTFIELDSIZE $ fromIntegral len
          curl_easy_setopt_str hdl cURLOPT_POSTFIELDS ptr
      curl_easy_setopt_str hdl cURLOPT_URL urlz
      curl_easy_setopt_slist hdl cURLOPT_HTTPHEADER hdrs
      curl_easy_setopt_fun hdl cURLOPT_WRITEFUNCTION writeBodyCb
      curl_easy_setopt_fun hdl cURLOPT_HEADERFUNCTION writeHeaderCb
      when o.verbose $ curl_easy_setopt_long hdl cURLOPT_VERBOSE 1
      code <- curl_easy_perform hdl
      status <- curlEasyGetinfoLong hdl cURLINFO_RESPONSE_CODE
      bodyChunks <- readIORef bodyChunksRef
      headerChunks <- readIORef headerChunksRef
      return CurlResponse
        { code
        , body = BSL.fromChunks $ List.reverse bodyChunks
        , headers = List.reverse headerChunks
        , status
        }

withCurlHandle :: (Ptr Curl -> IO a) -> IO a
withCurlHandle = bracket curl_easy_init curl_easy_cleanup

collectChunksCallback :: IORef [ByteString] -> CURLCallback
collectChunksCallback ref contents size nmemb _udata = do
  let realsize = size * nmemb
  chunk <- BS.packCStringLen (castPtr contents, fromIntegral realsize)
  modifyIORef ref (chunk:)
  return realsize

type CURLCallback = Ptr Word8 -> CSize -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall "wrapper" wrapCURLCallback :: CURLCallback -> IO (FunPtr CURLCallback)

withCURLCallback :: CURLCallback -> (FunPtr CURLCallback -> IO a) -> IO a
withCURLCallback cb = bracket aquire release where
  aquire = wrapCURLCallback cb
  release = freeHaskellFunPtr

foreign import ccall "curl_easy_getinfo"
  curl_easy_getinfo_long :: Ptr Curl -> CURLINFO -> Ptr CLong -> IO CInt

curlEasyGetinfoLong :: Ptr Curl -> CURLINFO -> IO CLong
curlEasyGetinfoLong hdl cinfo = alloca \ptr -> do
  curl_easy_getinfo_long hdl cinfo ptr
  peek ptr
