module Main where

import LibCurl

main :: IO ()
main = do
  curl_global_init cURL_GLOBAL_DEFAULT
  print cURLOPT_PROXY_TLS13_CIPHERS.unCURLoption
  code <- curl CurlRequest
    { url = "https://futures.mexc.com/api/v1/private/order/create?mhash=62b4ca09202889a3b6c4cf1b2d2688f8"
    , method = CurlPOST "{\"symbol\":\"ADA_USDT\",\"side\":1,\"openType\":1,\"type\":\"1\",\"vol\":100,\"leverage\":20,\"marketCeiling\":false,\"price\":\"0.31\",\"priceProtect\":\"1\",\"p0\":\"KxQA800cWRuIcEK/c6SlmbutfisBx2iXtfPdgraeNZEmfhn0M5yIB0/Y7bkdTERkp2ZhgKwNcvvo0DoNEo4xW/GdEL2IotNmr0gtGknAI86/Ii14eD9U6WuKFucDTeZ0uQZwwb/E3pke4IMarkeGXI/H5EjFtx3OB+/gh0jnKoMAyyDJhkWYvpB4SlU5FLUDmhhgZrFks/a1fWwGBTIUeFPRIomfDzsYDFecHYagYqujFIc7RPmyoU74fw9zVk/xjoHfTRzOV9Wu4902omSs37dBwGFMpJB0XqMcBUpbA43TTwvdGxl47BCoE1zwmzld+U9a\",\"k0\":\"FmevCaL4nMgdQqk2pUaQPzVPY3Rufl8ZT5q09JB0Xmp0voqJe5z00s6YV0xQFEB+d3uciXTaHrn5W7t7x70tCRdB+XyFcddPW2GxGs7Bu6pBE/Y17tl767oEEhOB7QhA84+Cc1UuwG/IU3rtM4rikZMlXwiYzBAjeGCbVbeFV0xxxIwsXLYZXsY45YSA0JvsTun+kjX2QSjexZ1JeLLmN045K/PNWtDf2cZS6sxo3B1AsO+QYApdB07DA+0lKzY8Lpi4xui3ZfH5Ien/LqgKnKTLUSa2xfA6+wTZyJZ8m36ZPXoNuKNdA1HkcXNEXro2+TGNGNOFB3JyLtV74ZYsVw==\",\"chash\":\"d6c64d28e362f314071b3f9d78ff7494d9cd7177ae0465e772d1840e9f7905d8\",\"mtoken\":\"NB0hQoT1S9jCP0ZqWKqi\",\"ts\":1720251866200,\"mhash\":\"62b4ca09202889a3b6c4cf1b2d2688f8\"}"
    , headers =
      [ "accept: */*"
      , "accept-language: en-US,en;q=0.9"
      , "authorization: WEB9872d55c51d665c0603d920d1fad89157688522698084c49c5d9648e7f118787"
      , "content-type: application/json"
      , "cookie: _ga=GA1.1.835132953.1710066822; _fbp=fb.1.1710066822085.1761640300; mxc_reset_tradingview_key=true; x-mxc-fingerprint=NB0hQoT1S9jCP0ZqWKqi; mc_exchange_kline_price_setting=LAST_CLOSE; mxc_theme_main=light; sensorsdata2015jssdkcross=%7B%22distinct_id%22%3A%222159bf13a5ad454b92ba33ea6ae17eac%22%2C%22first_id%22%3A%2218e27ecdaa7147f-053171b521a4468-16462c6f-1838736-18e27ecdaa81249%22%2C%22props%22%3A%7B%22%24latest_traffic_source_type%22%3A%22%E7%9B%B4%E6%8E%A5%E6%B5%81%E9%87%8F%22%2C%22%24latest_search_keyword%22%3A%22%E6%9C%AA%E5%8F%96%E5%88%B0%E5%80%BC_%E7%9B%B4%E6%8E%A5%E6%89%93%E5%BC%80%22%2C%22%24latest_referrer%22%3A%22%22%2C%22%24latest_utm_source%22%3A%22mexc%22%2C%22%24latest_utm_medium%22%3A%22internalmsg%22%2C%22%24latest_utm_campaign%22%3A%22trading-activity202405%22%7D%2C%22identities%22%3A%22eyIkaWRlbnRpdHlfY29va2llX2lkIjoiMThlMjdlY2RhYTcxNDdmLTA1MzE3MWI1MjFhNDQ2OC0xNjQ2MmM2Zi0xODM4NzM2LTE4ZTI3ZWNkYWE4MTI0OSIsIiRpZGVudGl0eV9sb2dpbl9pZCI6IjIxNTliZjEzYTVhZDQ1NGI5MmJhMzNlYTZhZTE3ZWFjIn0%3D%22%2C%22history_login_id%22%3A%7B%22name%22%3A%22%24identity_login_id%22%2C%22value%22%3A%222159bf13a5ad454b92ba33ea6ae17eac%22%7D%2C%22%24device_id%22%3A%2218e27ecdaa7147f-053171b521a4468-16462c6f-1838736-18e27ecdaa81249%22%7D; NEXT_LOCALE=en-US; _vid_t=HspSv6e3LUJGk9RpYO5YBNGDcHzvb62AvD0SaF2Z3GpwpHWJIQTFentbarrxkumupSBQ/HWFTPcvts8PzLmNJIJzfDVgkT2CMfLKADo=; mexc_fingerprint_visitorId=NB0hQoT1S9jCP0ZqWKqi; mexc_fingerprint_requestId=1719852043374.O7Kin0; AKA_A2=A; bm_mi=D9DC36BD1171745943AA5135128F5274~YAAQDknqvHv76VyQAQAAIOf+hhgQ/LFu/hcH/JPc+gQZc6kh14rl7kef+9dveuPz9/BHFg1DEqj1D1mSYjtXQeZpqk4BP/dQyL+qHe7CTb6CMhpoRypoHGdsjNTG4TSQxJSuHBm4CwLNue0g/I4yV3jmrMDmwMM5oniz2liSoqQmEtnmG4VIUloSZ1zMX7m5UpkrPJtIrHZrNV3/i2b1zip9XOtCXqZmAjUHzhytz5lvzOFAHyngE7xmnBp/+ZBQemI+pz5RwEe8IKwOIygRC/ZJUYmyN2Y6FtqvVo/IFRTuDarvO5dAzrm5REqwBHoxG1iI8br2pfouBpY=~1; ak_bmsc=D4EF8A45C06A495AE3946D4D1D534F3F~000000000000000000000000000000~YAAQDknqvIP76VyQAQAAce/+hhiCvt3NBF+DsaN4V5Z7XJm/oAs/DgKQF8UgNGcJMjWRmNE6gkcRY6na94Y3YVxQ02ufvt2Ih7CGWDftwPJlh2Fbcob6dZqHvDD43uHdO+MUrSqOWweGRTP5PDYpkJ1BzF/RBi8tU3uUjvQa0e8N3KSKEL+R1qEtAG9yx3uTKIxEjdcQqNS2Z/bMH1CxinrVFpSrPMhiosLGhUxyu8AinvPizhOC2ABN2pYYVc+H3TzFXNlK51S/ZIATh7KufnOk7NIaSBrQ7V3cCFI/sWvLMsbvBAXqFY5h21ggVsz3vTIV56rUQJaECk2fpgdAq8BUizwp6YF4q34NS7Io4y74z+Y+G2Vtse5SsdoZJw+qpJXUneDymAtPisK6YuNWA60bqOXwDLbuWL5ZfRmbO4MUpMNU1g02JVN+QiQkGc+SxrO4Y6dPO1ld/WDewvWHc9gPFPJ9N/1xD84vD/GjlRw/MBldBR5eO79cRI/h; _rdt_uuid=1710066822069.8fb0dca1-af0e-4429-befb-062ff23137dd; uc_token=WEB9872d55c51d665c0603d920d1fad89157688522698084c49c5d9648e7f118787; u_id=WEB9872d55c51d665c0603d920d1fad89157688522698084c49c5d9648e7f118787; _ga_L6XJCQTK75=GS1.1.1720251823.267.1.1720251845.38.0.0; RT=\"z=1&dm=futures.mexc.com&si=c598cbc0-f5a4-47c9-be8e-63a1463b601b&ss=ly9thje2&sl=3&tt=5pu&bcn=%2F%2F684dd313.akstat.io%2F&ld=1oyq\"; bm_sv=21471782FC1DC1FF512BCDBA23830D95~YAAQBknqvEEcg0mQAQAAPjcAhxh2sOlAcBulkL3mVFksiTuhwZ1Tz8MtKZrqXNfUWfgjHH1f4H7DESnxYkNDMZ59j+1mCTskU7p3r6paqTlhcPjo52q5mFGMfSrsvm5K65oLSDJViZNYNjD/t4JD4ICUBsVBxC/LqlCPzCY3G2yWhzVwk71cbHJD0dXuoj955BHTRwMqO3Ftouc9moG2TvDb5f8g4yRKIdiU+C3v5dcHqxz5t8EUlFzJHFPOY/Y=~1; _lr_hb_-skyeye%2Fprod={%22heartbeat%22:1720251861035}; _lr_uf_-skyeye=91362d96-0fc3-4ca0-bee2-960e146bea0d; _lr_tabs_-skyeye%2Fprod={%22sessionID%22:0%2C%22recordingID%22:%225-0d36a05d-53b6-4e85-9ad4-09afa8f8133a%22%2C%22webViewID%22:null%2C%22lastActivity%22:1720251866117}"
      , "language: English"
      , "origin: https://futures.mexc.com"
      , "pragma: akamai-x-cache-on"
      , "priority: u=1, i"
      , "referer: https://futures.mexc.com/exchange/ADA_USDT?type=linear_swap"
      , "sec-ch-ua: \"Not/A)Brand\";v=\"8\", \"Chromium\";v=\"126\", \"Google Chrome\";v=\"126\""
      , "sec-ch-ua-mobile: ?0"
      , "sec-ch-ua-platform: \"Linux\""
      , "sec-fetch-dest: empty"
      , "sec-fetch-mode: cors"
      , "sec-fetch-site: same-origin"
      , "trochilus-trace-id: 92e0df02-b3c7-4099-b97d-44ea354d05b8-0129"
      , "trochilus-uid: 00095577"
      , "x-mxc-nonce: 1720251866037"
      , "x-mxc-sign: ec97a0f48a1fa4387cad0ffc393e1f84"
      ]
    }
  -- code2 <- curl CurlRequest
  --   { url = "https://futures.mexc.com/api/v1/private/order/create?mhash=62b4ca09202889a3b6c4cf1b2d2688f8"
  --   , method = CurlPOST "{\"chash\":\"d6c64d28e362f314071b3f9d78ff7494d9cd7177ae0465e772d1840e9f7905d8\",\"k0\":\"iFWBXo+YDydLlTMXtrFTYjtK/+YZt+z60/BuBF3Z/ciQamLX6L+eOr+y0gFh3PBqyuI3X1QDA/+pmDu55yWQZ1+O4jOmBvZUOVp41RU2LGGb48rK/BAZ9MPA4t7kBDwyqFB0opIj7SkUH5BlmdxskGjW+qEnguQvsfjA4jVdO/4BT8ASI0hSBbWRKe9iQ43r1V6jUjcABIkjXFEH2xY/KMKv+42hSEb+fY6hvBAm2E2QGg9ARPbkdGmC1wKDkDkF/dQAxp7dXVY/oR3EsRZMKnad7VXlJGnE8KcTX/dqFG9dGOQynNhy2uGj2ugYxoQN20LCEUKV1dK0TvfXIvCUXw==\",\"leverage\":20,\"marketCeiling\":false,\"mhash\":\"62b4ca09202889a3b6c4cf1b2d2688f8\",\"mtoken\":\"NB0hQoT1S9jCP0ZqWKqi\",\"openType\":1,\"price\":\"0.39\",\"priceProtect\":\"0\",\"side\":1,\"symbol\":\"ADA_USDT\",\"ts\":1720286978888,\"type\":1,\"vol\":100}"
  --   , headers =
  --     [ "accept: */*"
  --     ,"accept-language: en-US,en;q=0.9"
  --     ,"authorization: WEBb96cb220113be4db75feabaa199cb4372cdb5b204f1eb2dd4a80d93926966e37"
  --     ,"content-type: application/json"
  --     ,"language: English"
  --     ,"origin: https://futures.mexc.com"
  --     ,"pragma: akamai-x-cache-on"
  --     ,"referrer: https://futures.mexc.com/exchange/ADA_USDT?type=linear_swap"
  --     ,"sec-ch-ua: Google Chrome\";v=\"123\", \"Not:A-Brand\";v=\"8\", \"Chromium\";v=\"123\""
  --     ,"sec-ch-ua-mobile: ?0"
  --     ,"sec-ch-ua-platform: \"Linux\""
  --     ,"sec-fetch-dest: empty"
  --     ,"sec-fetch-mode: cors"
  --     ,"sec-fetch-site: same-origin"
  --     ,"trochilus-trace-id: 4e2ce7db-f96d-4726-984c-05a9791ac9ec-0342"
  --     ,"trochilus-uid: 00095577"
  --     ,"user-agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36"
  --     ,"x-mxc-nonce: 1720286978888"
  --     ,"x-mxc-sign: e8522860c13d93b220c76d3ea4599b2c"
  --     ]
  --   }

  -- -- code <- curl CurlOptions
  -- --   { url = "https://google-translate1.p.rapidapi.com/language/translate/v2/detect"
  -- --   , headers =
  -- --     [ "Accept: application/json"
  -- --     , "Content-Type: multipart/form-data"
  -- --     , "x-rapidapi-host: google-translate1.p.rapidapi.com"
  -- --     , "x-rapidapi-key: 8c9817f030msh53e4d9cbe9d2c5dp102120jsn3e270601b739"
  -- --     ]
  -- --   , method = CurlPOST "q=English is hard, but detectably so"
  -- --   }
  print code
  curl_global_cleanup
