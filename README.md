Wrote a wrapper for libcurl for a specific use case, the bindings are
incomplete. Needed libcurl because http-client couldn't send HTTP2
requests

```hs
import LibCurl

main = do
  curl_global_init cURL_GLOBAL_DEFAULT
  resp <- curl CurlRequest
    { url = "https://freetestapi.com/api/v1/countries"
    , method = CurlGET
    , headers = ["Accept: application/json"]
    }
  print resp
  curl_global_cleanup
```
