module LibCurl.Constants where

import Foreign.C.Types
import Data.Bits

#include <curl/curl.h>

newtype CURLGlobalFlags = CURLGlobalFlag {unCURLGlobalFlag :: CInt}
  deriving newtype (Eq, Bits)

{- no purpose since 7.57.0 -}
cURL_GLOBAL_SSL = CURLGlobalFlag #{const CURL_GLOBAL_SSL}
cURL_GLOBAL_WIN32 = CURLGlobalFlag #{const CURL_GLOBAL_WIN32}
cURL_GLOBAL_ALL = CURLGlobalFlag #{const CURL_GLOBAL_ALL}
cURL_GLOBAL_NOTHING = CURLGlobalFlag #{const CURL_GLOBAL_NOTHING}
cURL_GLOBAL_DEFAULT = CURLGlobalFlag #{const CURL_GLOBAL_DEFAULT}
cURL_GLOBAL_ACK_EINTR = CURLGlobalFlag #{const CURL_GLOBAL_ACK_EINTR}

newtype CURLoption = CURLoption {unCURLoption :: Int}

{- This is the FILE * or void * the regular output should be written to. -}
cURLOPT_WRITEDATA = CURLoption #{const CURLOPT_WRITEDATA}

{- The full URL to get/put -}
cURLOPT_URL = CURLoption #{const CURLOPT_URL}

{- Port number to connect to, if other than default. -}
cURLOPT_PORT = CURLoption #{const CURLOPT_PORT}

{- Name of proxy to use. -}
cURLOPT_PROXY = CURLoption #{const CURLOPT_PROXY}

{- "user:password;options" to use when fetching. -}
cURLOPT_USERPWD = CURLoption #{const CURLOPT_USERPWD}

{- "user:password" to use with proxy. -}
cURLOPT_PROXYUSERPWD = CURLoption #{const CURLOPT_PROXYUSERPWD}

{- Range to get, specified as an ASCII string. -}
cURLOPT_RANGE = CURLoption #{const CURLOPT_RANGE}

{- Specified file stream to upload from (use as input): -}
cURLOPT_READDATA = CURLoption #{const CURLOPT_READDATA}

{- Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
 * bytes big. -}
cURLOPT_ERRORBUFFER = CURLoption #{const CURLOPT_ERRORBUFFER}

{- Function that will be called to store the output (instead of fwrite). The
 * parameters will use fwrite() syntax, make sure to follow them. -}
cURLOPT_WRITEFUNCTION = CURLoption #{const CURLOPT_WRITEFUNCTION}

{- Function that will be called to read the input (instead of fread). The
 * parameters will use fread() syntax, make sure to follow them. -}
cURLOPT_READFUNCTION = CURLoption #{const CURLOPT_READFUNCTION}

{- Time-out the read operation after this amount of seconds -}
cURLOPT_TIMEOUT = CURLoption #{const CURLOPT_TIMEOUT}

{- If CURLOPT_READDATA is used, this can be used to inform libcurl about
 * how large the file being sent really is. That allows better error
 * checking and better verifies that the upload was successful. -1 means
 * unknown size.
 *
 * For large file support, there is also a _LARGE version of the key
 * which takes an off_t type, allowing platforms with larger off_t
 * sizes to handle larger files.  See below for INFILESIZE_LARGE.
 -}
cURLOPT_INFILESIZE = CURLoption #{const CURLOPT_INFILESIZE}

{- POST static input fields. -}
cURLOPT_POSTFIELDS = CURLoption #{const CURLOPT_POSTFIELDS}

{- Set the referrer page (needed by some CGIs) -}
cURLOPT_REFERER = CURLoption #{const CURLOPT_REFERER}

{- Set the FTP PORT string (interface name, named or numerical IP address)
   Use i.e '-' to use default address. -}
cURLOPT_FTPPORT = CURLoption #{const CURLOPT_FTPPORT}

{- Set the User-Agent string (examined by some CGIs) -}
cURLOPT_USERAGENT = CURLoption #{const CURLOPT_USERAGENT}

{- If the download receives less than "low speed limit" bytes/second
 * during "low speed time" seconds, the operations is aborted.
 * You could i.e if you have a pretty high speed connection, abort if
 * it is less than 2000 bytes/sec during 20 seconds.
 -}

{- Set the "low speed limit" -}
cURLOPT_LOW_SPEED_LIMIT = CURLoption #{const CURLOPT_LOW_SPEED_LIMIT}

{- Set the "low speed time" -}
cURLOPT_LOW_SPEED_TIME = CURLoption #{const CURLOPT_LOW_SPEED_TIME}

{- Set the continuation offset.
 *
 * Note there is also a _LARGE version of this key which uses
 * off_t types, allowing for large file offsets on platforms which
 * use larger-than-32-bit off_t's.  Look below for RESUME_FROM_LARGE.
 -}
cURLOPT_RESUME_FROM = CURLoption #{const CURLOPT_RESUME_FROM}

{- Set cookie in request: -}
cURLOPT_COOKIE = CURLoption #{const CURLOPT_COOKIE}

{- This points to a linked list of headers, struct curl_slist kind. This
   list is also used for RTSP (in spite of its name) -}
cURLOPT_HTTPHEADER = CURLoption #{const CURLOPT_HTTPHEADER}

{- name of the file keeping your private SSL-certificate -}
cURLOPT_SSLCERT = CURLoption #{const CURLOPT_SSLCERT}

{- password for the SSL or SSH private key -}
cURLOPT_KEYPASSWD = CURLoption #{const CURLOPT_KEYPASSWD}

{- send TYPE parameter? -}
cURLOPT_CRLF = CURLoption #{const CURLOPT_CRLF}

{- send linked-list of QUOTE commands -}
cURLOPT_QUOTE = CURLoption #{const CURLOPT_QUOTE}

{- send FILE * or void * to store headers to, if you use a callback it
   is simply passed to the callback unmodified -}
cURLOPT_HEADERDATA = CURLoption #{const CURLOPT_HEADERDATA}

{- point to a file to read the initial cookies from, also enables
   "cookie awareness" -}
cURLOPT_COOKIEFILE = CURLoption #{const CURLOPT_COOKIEFILE}

{- What version to specifically try to use.
   See CURL_SSLVERSION defines below. -}
cURLOPT_SSLVERSION = CURLoption #{const CURLOPT_SSLVERSION}

{- What kind of HTTP time condition to use, see defines -}
cURLOPT_TIMECONDITION = CURLoption #{const CURLOPT_TIMECONDITION}

{- Time to use with the above condition. Specified in number of seconds
   since 1 Jan 1970 -}
cURLOPT_TIMEVALUE = CURLoption #{const CURLOPT_TIMEVALUE}

{- 35 = OBSOLETE -}

{- Custom request, for customizing the get command like
   HTTP: DELETE, TRACE and others
   FTP: to use a different list command
   -}
cURLOPT_CUSTOMREQUEST = CURLoption #{const CURLOPT_CUSTOMREQUEST}

{- FILE handle to use instead of stderr -}
cURLOPT_STDERR = CURLoption #{const CURLOPT_STDERR}

{- 38 is not used -}

{- send linked-list of post-transfer QUOTE commands -}
cURLOPT_POSTQUOTE = CURLoption #{const CURLOPT_POSTQUOTE}

 {- OBSOLETE, do not use! -}
cURLOPT_OBSOLETE40 = CURLoption #{const CURLOPT_OBSOLETE40}

{- talk a lot -}
cURLOPT_VERBOSE = CURLoption #{const CURLOPT_VERBOSE}

{- throw the header out too -}
cURLOPT_HEADER = CURLoption #{const CURLOPT_HEADER}

{- shut off the progress meter -}
cURLOPT_NOPROGRESS = CURLoption #{const CURLOPT_NOPROGRESS}

{- use HEAD to get http document -}
cURLOPT_NOBODY = CURLoption #{const CURLOPT_NOBODY}

{- no output on http error codes >= 400 -}
cURLOPT_FAILONERROR = CURLoption #{const CURLOPT_FAILONERROR}

{- this is an upload -}
cURLOPT_UPLOAD = CURLoption #{const CURLOPT_UPLOAD}

{- HTTP POST method -}
cURLOPT_POST = CURLoption #{const CURLOPT_POST}

{- bare names when listing directories -}
cURLOPT_DIRLISTONLY = CURLoption #{const CURLOPT_DIRLISTONLY}

{- Append instead of overwrite on upload! -}
cURLOPT_APPEND = CURLoption #{const CURLOPT_APPEND}

{- Specify whether to read the user+password from the .netrc or the URL.
 * This must be one of the CURL_NETRC_* enums below. -}
cURLOPT_NETRC = CURLoption #{const CURLOPT_NETRC}

{- use Location: Luke! -}
cURLOPT_FOLLOWLOCATION = CURLoption #{const CURLOPT_FOLLOWLOCATION}

 {- transfer data in text/ASCII format -}
cURLOPT_TRANSFERTEXT = CURLoption #{const CURLOPT_TRANSFERTEXT}

{- Data passed to the CURLOPT_PROGRESSFUNCTION and CURLOPT_XFERINFOFUNCTION
   callbacks -}
cURLOPT_XFERINFODATA = CURLoption #{const CURLOPT_XFERINFODATA}

{- We want the referrer field set automatically when following locations -}
cURLOPT_AUTOREFERER = CURLoption #{const CURLOPT_AUTOREFERER}

{- Port of the proxy, can be set in the proxy string as well with:
   "[host]:[port]" -}
cURLOPT_PROXYPORT = CURLoption #{const CURLOPT_PROXYPORT}

{- size of the POST input data, if strlen() is not good to use -}
cURLOPT_POSTFIELDSIZE = CURLoption #{const CURLOPT_POSTFIELDSIZE}

{- tunnel non-http operations through an HTTP proxy -}
cURLOPT_HTTPPROXYTUNNEL = CURLoption #{const CURLOPT_HTTPPROXYTUNNEL}

{- Set the interface string to use as outgoing network interface -}
cURLOPT_INTERFACE = CURLoption #{const CURLOPT_INTERFACE}

{- Set the krb4/5 security level, this also enables krb4/5 awareness.  This
 * is a string, 'clear', 'safe', 'confidential' or 'private'.  If the string
 * is set but doesn't match one of these, 'private' will be used.  -}
cURLOPT_KRBLEVEL = CURLoption #{const CURLOPT_KRBLEVEL}

{- Set if we should verify the peer in ssl handshake, set 1 to verify. -}
cURLOPT_SSL_VERIFYPEER = CURLoption #{const CURLOPT_SSL_VERIFYPEER}

{- The CApath or CAfile used to validate the peer certificate
   this option is used only if SSL_VERIFYPEER is true -}
cURLOPT_CAINFO = CURLoption #{const CURLOPT_CAINFO}

{- 66 = OBSOLETE -}
{- 67 = OBSOLETE -}

{- Maximum number of http redirects to follow -}
cURLOPT_MAXREDIRS = CURLoption #{const CURLOPT_MAXREDIRS}

{- Pass a long set to 1 to get the date of the requested document (if
   possible)! Pass a zero to shut it off. -}
cURLOPT_FILETIME = CURLoption #{const CURLOPT_FILETIME}

{- This points to a linked list of telnet options -}
cURLOPT_TELNETOPTIONS = CURLoption #{const CURLOPT_TELNETOPTIONS}

{- Max amount of cached alive connections -}
cURLOPT_MAXCONNECTS = CURLoption #{const CURLOPT_MAXCONNECTS}

{- OBSOLETE, do not use! -}
cURLOPT_OBSOLETE72 = CURLoption #{const CURLOPT_OBSOLETE72}

{- 73 = OBSOLETE -}

{- Set to explicitly use a new connection for the upcoming transfer.
   Do not use this unless you're absolutely sure of this, as it makes the
   operation slower and is less friendly for the network. -}
cURLOPT_FRESH_CONNECT = CURLoption #{const CURLOPT_FRESH_CONNECT}

{- Set to explicitly forbid the upcoming transfer's connection to be reused
   when done. Do not use this unless you're absolutely sure of this, as it
   makes the operation slower and is less friendly for the network. -}
cURLOPT_FORBID_REUSE = CURLoption #{const CURLOPT_FORBID_REUSE}

{- Time-out connect operations after this amount of seconds, if connects are
   OK within this time, then fine... This only aborts the connect phase. -}
cURLOPT_CONNECTTIMEOUT = CURLoption #{const CURLOPT_CONNECTTIMEOUT}

{- Function that will be called to store headers (instead of fwrite). The
 * parameters will use fwrite() syntax, make sure to follow them. -}
cURLOPT_HEADERFUNCTION = CURLoption #{const CURLOPT_HEADERFUNCTION}

{- Set this to force the HTTP request to get back to GET. Only really usable
   if POST, PUT or a custom request have been used first.
 -}
cURLOPT_HTTPGET = CURLoption #{const CURLOPT_HTTPGET}

{- Set if we should verify the Common name from the peer certificate in ssl
 * handshake, set 1 to check existence, 2 to ensure that it matches the
 * provided hostname. -}
cURLOPT_SSL_VERIFYHOST = CURLoption #{const CURLOPT_SSL_VERIFYHOST}

{- Specify which file name to write all known cookies in after completed
   operation. Set file name to "-" (dash) to make it go to stdout. -}
cURLOPT_COOKIEJAR = CURLoption #{const CURLOPT_COOKIEJAR}

{- Specify which SSL ciphers to use -}
cURLOPT_SSL_CIPHER_LIST = CURLoption #{const CURLOPT_SSL_CIPHER_LIST}

{- Specify which HTTP version to use! This must be set to one of the
   CURL_HTTP_VERSION* enums set below. -}
cURLOPT_HTTP_VERSION = CURLoption #{const CURLOPT_HTTP_VERSION}

{- Specifically switch on or off the FTP engine's use of the EPSV command. By
   default, that one will always be attempted before the more traditional
   PASV command. -}
cURLOPT_FTP_USE_EPSV = CURLoption #{const CURLOPT_FTP_USE_EPSV}

{- type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") -}
cURLOPT_SSLCERTTYPE = CURLoption #{const CURLOPT_SSLCERTTYPE}

{- name of the file keeping your private SSL-key -}
cURLOPT_SSLKEY = CURLoption #{const CURLOPT_SSLKEY}

{- type of the file keeping your private SSL-key ("DER", "PEM", "ENG") -}
cURLOPT_SSLKEYTYPE = CURLoption #{const CURLOPT_SSLKEYTYPE}

{- crypto engine for the SSL-sub system -}
cURLOPT_SSLENGINE = CURLoption #{const CURLOPT_SSLENGINE}

{- set the crypto engine for the SSL-sub system as default
   the param has no meaning...
 -}
cURLOPT_SSLENGINE_DEFAULT = CURLoption #{const CURLOPT_SSLENGINE_DEFAULT}

{- DNS cache timeout -}
cURLOPT_DNS_CACHE_TIMEOUT = CURLoption #{const CURLOPT_DNS_CACHE_TIMEOUT}

{- send linked-list of pre-transfer QUOTE commands -}
cURLOPT_PREQUOTE = CURLoption #{const CURLOPT_PREQUOTE}

{- set the debug function -}
cURLOPT_DEBUGFUNCTION = CURLoption #{const CURLOPT_DEBUGFUNCTION}

{- set the data for the debug function -}
cURLOPT_DEBUGDATA = CURLoption #{const CURLOPT_DEBUGDATA}

{- mark this as start of a cookie session -}
cURLOPT_COOKIESESSION = CURLoption #{const CURLOPT_COOKIESESSION}

{- The CApath directory used to validate the peer certificate
   this option is used only if SSL_VERIFYPEER is true -}
cURLOPT_CAPATH = CURLoption #{const CURLOPT_CAPATH}

{- Instruct libcurl to use a smaller receive buffer -}
cURLOPT_BUFFERSIZE = CURLoption #{const CURLOPT_BUFFERSIZE}

{- Instruct libcurl to not use any signal/alarm handlers, even when using
   timeouts. This option is useful for multi-threaded applications.
   See libcurl-the-guide for more background information. -}
cURLOPT_NOSIGNAL = CURLoption #{const CURLOPT_NOSIGNAL}

{- Provide a CURLShare for mutexing non-ts data -}
cURLOPT_SHARE = CURLoption #{const CURLOPT_SHARE}

{- indicates type of proxy. accepted values are CURLPROXY_HTTP (default),
   CURLPROXY_HTTPS, CURLPROXY_SOCKS4, CURLPROXY_SOCKS4A and
   CURLPROXY_SOCKS5. -}
cURLOPT_PROXYTYPE = CURLoption #{const CURLOPT_PROXYTYPE}

{- Set the Accept-Encoding string. Use this to tell a server you would like
   the response to be compressed. Before 7.21.6, this was known as
   CURLOPT_ENCODING -}
cURLOPT_ACCEPT_ENCODING = CURLoption #{const CURLOPT_ACCEPT_ENCODING}

{- Set pointer to private data -}
cURLOPT_PRIVATE = CURLoption #{const CURLOPT_PRIVATE}

{- Set aliases for HTTP 200 in the HTTP Response header -}
cURLOPT_HTTP200ALIASES = CURLoption #{const CURLOPT_HTTP200ALIASES}

{- Continue to send authentication (user+password) when following locations,
   even when hostname changed. This can potentially send off the name
   and password to whatever host the server decides. -}
cURLOPT_UNRESTRICTED_AUTH = CURLoption #{const CURLOPT_UNRESTRICTED_AUTH}

{- Specifically switch on or off the FTP engine's use of the EPRT command (
   it also disables the LPRT attempt). By default, those ones will always be
   attempted before the good old traditional PORT command. -}
cURLOPT_FTP_USE_EPRT = CURLoption #{const CURLOPT_FTP_USE_EPRT}

{- Set this to a bitmask value to enable the particular authentications
   methods you like. Use this in combination with CURLOPT_USERPWD.
   Note that setting multiple bits may cause extra network round-trips. -}
cURLOPT_HTTPAUTH = CURLoption #{const CURLOPT_HTTPAUTH}

{- Set the ssl context callback function, currently only for OpenSSL or
   WolfSSL ssl_ctx, or mbedTLS mbedtls_ssl_config in the second argument.
   The function must match the curl_ssl_ctx_callback prototype. -}
cURLOPT_SSL_CTX_FUNCTION = CURLoption #{const CURLOPT_SSL_CTX_FUNCTION}

{- Set the userdata for the ssl context callback function's third
   argument -}
cURLOPT_SSL_CTX_DATA = CURLoption #{const CURLOPT_SSL_CTX_DATA}

{- FTP Option that causes missing dirs to be created on the remote server.
   In 7.19.4 we introduced the convenience enums for this option using the
   CURLFTP_CREATE_DIR prefix.
-}
cURLOPT_FTP_CREATE_MISSING_DIRS = CURLoption #{const CURLOPT_FTP_CREATE_MISSING_DIRS}

{- Set this to a bitmask value to enable the particular authentications
   methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
   Note that setting multiple bits may cause extra network round-trips. -}
cURLOPT_PROXYAUTH = CURLoption #{const CURLOPT_PROXYAUTH}

{- Option that changes the timeout, in seconds, associated with getting a
   response.  This is different from transfer timeout time and essentially
   places a demand on the server to acknowledge commands in a timely
   manner. For FTP, SMTP, IMAP and POP3. -}
cURLOPT_SERVER_RESPONSE_TIMEOUT = CURLoption #{const CURLOPT_SERVER_RESPONSE_TIMEOUT}

{- Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
   tell libcurl to use those IP versions only. This only has effect on
   systems with support for more than one, i.e IPv4 _and_ IPv6. -}
cURLOPT_IPRESOLVE = CURLoption #{const CURLOPT_IPRESOLVE}

{- Set this option to limit the size of a file that will be downloaded from
   an HTTP or FTP server.

   Note there is also _LARGE version which adds large file support for
   platforms which have larger off_t sizes.  See MAXFILESIZE_LARGE below. -}
cURLOPT_MAXFILESIZE = CURLoption #{const CURLOPT_MAXFILESIZE}

{- See the comment for INFILESIZE above, but in short, specifies
 * the size of the file being uploaded.  -1 means unknown.
 -}
cURLOPT_INFILESIZE_LARGE = CURLoption #{const CURLOPT_INFILESIZE_LARGE}

{- Sets the continuation offset.  There is also a CURLOPTTYPE_LONG version
 * of this; look above for RESUME_FROM.
 -}
cURLOPT_RESUME_FROM_LARGE = CURLoption #{const CURLOPT_RESUME_FROM_LARGE}

{- Sets the maximum size of data that will be downloaded from
 * an HTTP or FTP server.  See MAXFILESIZE above for the LONG version.
 -}
cURLOPT_MAXFILESIZE_LARGE = CURLoption #{const CURLOPT_MAXFILESIZE_LARGE}

{- Set this option to the file name of your .netrc file you want libcurl
   to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
   a poor attempt to find the user's home directory and check for a .netrc
   file in there. -}
cURLOPT_NETRC_FILE = CURLoption #{const CURLOPT_NETRC_FILE}

{- Enable SSL/TLS for FTP, pick one of:
   CURLUSESSL_TRY     - try using SSL, proceed anyway otherwise
   CURLUSESSL_CONTROL - SSL for the control connection or fail
   CURLUSESSL_ALL     - SSL for all communication or fail
-}
cURLOPT_USE_SSL = CURLoption #{const CURLOPT_USE_SSL}

{- The _LARGE version of the standard POSTFIELDSIZE option -}
cURLOPT_POSTFIELDSIZE_LARGE = CURLoption #{const CURLOPT_POSTFIELDSIZE_LARGE}

{- Enable/disable the TCP Nagle algorithm -}
cURLOPT_TCP_NODELAY = CURLoption #{const CURLOPT_TCP_NODELAY}

{- When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL), this option
   can be used to change libcurl's default action which is to first try
   "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
   response has been received.

   Available parameters are:
   CURLFTPAUTH_DEFAULT - let libcurl decide
   CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
   CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL
-}
cURLOPT_FTPSSLAUTH = CURLoption #{const CURLOPT_FTPSSLAUTH}

{- null-terminated string for pass on to the FTP server when asked for
   "account" info -}
cURLOPT_FTP_ACCOUNT = CURLoption #{const CURLOPT_FTP_ACCOUNT}

{- feed cookie into cookie engine -}
cURLOPT_COOKIELIST = CURLoption #{const CURLOPT_COOKIELIST}

{- ignore Content-Length -}
cURLOPT_IGNORE_CONTENT_LENGTH = CURLoption #{const CURLOPT_IGNORE_CONTENT_LENGTH}

{- Set to non-zero to skip the IP address received in a 227 PASV FTP server
   response. Typically used for FTP-SSL purposes but is not restricted to
   that. libcurl will then instead use the same IP address it used for the
   control connection. -}
cURLOPT_FTP_SKIP_PASV_IP = CURLoption #{const CURLOPT_FTP_SKIP_PASV_IP}

{- Select "file method" to use when doing FTP, see the curl_ftpmethod
   above. -}
cURLOPT_FTP_FILEMETHOD = CURLoption #{const CURLOPT_FTP_FILEMETHOD}

{- Local port number to bind the socket to -}
cURLOPT_LOCALPORT = CURLoption #{const CURLOPT_LOCALPORT}

{- Number of ports to try, including the first one set with LOCALPORT.
   Thus, setting it to 1 will make no additional attempts but the first.
-}
cURLOPT_LOCALPORTRANGE = CURLoption #{const CURLOPT_LOCALPORTRANGE}

{- no transfer, set up connection and let application use the socket by
   extracting it with CURLINFO_LASTSOCKET -}
cURLOPT_CONNECT_ONLY = CURLoption #{const CURLOPT_CONNECT_ONLY}

{- if the connection proceeds too quickly then need to slow it down -}
{- limit-rate: maximum number of bytes per second to send or receive -}
cURLOPT_MAX_SEND_SPEED_LARGE = CURLoption #{const CURLOPT_MAX_SEND_SPEED_LARGE}
cURLOPT_MAX_RECV_SPEED_LARGE = CURLoption #{const CURLOPT_MAX_RECV_SPEED_LARGE}

{- Pointer to command string to send if USER/PASS fails. -}
cURLOPT_FTP_ALTERNATIVE_TO_USER = CURLoption #{const CURLOPT_FTP_ALTERNATIVE_TO_USER}

{- callback function for setting socket options -}
cURLOPT_SOCKOPTFUNCTION = CURLoption #{const CURLOPT_SOCKOPTFUNCTION}
cURLOPT_SOCKOPTDATA = CURLoption #{const CURLOPT_SOCKOPTDATA}

{- set to 0 to disable session ID reuse for this transfer, default is
   enabled (== 1) -}
cURLOPT_SSL_SESSIONID_CACHE = CURLoption #{const CURLOPT_SSL_SESSIONID_CACHE}

{- allowed SSH authentication methods -}
cURLOPT_SSH_AUTH_TYPES = CURLoption #{const CURLOPT_SSH_AUTH_TYPES}

{- Used by scp/sftp to do public/private key authentication -}
cURLOPT_SSH_PUBLIC_KEYFILE = CURLoption #{const CURLOPT_SSH_PUBLIC_KEYFILE}
cURLOPT_SSH_PRIVATE_KEYFILE = CURLoption #{const CURLOPT_SSH_PRIVATE_KEYFILE}

{- Send CCC (Clear Command Channel) after authentication -}
cURLOPT_FTP_SSL_CCC = CURLoption #{const CURLOPT_FTP_SSL_CCC}

{- Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution -}
cURLOPT_TIMEOUT_MS = CURLoption #{const CURLOPT_TIMEOUT_MS}
cURLOPT_CONNECTTIMEOUT_MS = CURLoption #{const CURLOPT_CONNECTTIMEOUT_MS}

{- set to zero to disable the libcurl's decoding and thus pass the raw body
   data to the application even when it is encoded/compressed -}
cURLOPT_HTTP_TRANSFER_DECODING = CURLoption #{const CURLOPT_HTTP_TRANSFER_DECODING}
cURLOPT_HTTP_CONTENT_DECODING = CURLoption #{const CURLOPT_HTTP_CONTENT_DECODING}

{- Permission used when creating new files and directories on the remote
   server for protocols that support it, SFTP/SCP/FILE -}
cURLOPT_NEW_FILE_PERMS = CURLoption #{const CURLOPT_NEW_FILE_PERMS}
cURLOPT_NEW_DIRECTORY_PERMS = CURLoption #{const CURLOPT_NEW_DIRECTORY_PERMS}

{- Set the behavior of POST when redirecting. Values must be set to one
   of CURL_REDIR* defines below. This used to be called CURLOPT_POST301 -}
cURLOPT_POSTREDIR = CURLoption #{const CURLOPT_POSTREDIR}

{- used by scp/sftp to verify the host's public key -}
cURLOPT_SSH_HOST_PUBLIC_KEY_MD5 = CURLoption #{const CURLOPT_SSH_HOST_PUBLIC_KEY_MD5}

{- Callback function for opening socket (instead of socket(2)). Optionally,
   callback is able change the address or refuse to connect returning
   CURL_SOCKET_BAD.  The callback should have type
   curl_opensocket_callback -}
cURLOPT_OPENSOCKETFUNCTION = CURLoption #{const CURLOPT_OPENSOCKETFUNCTION}
cURLOPT_OPENSOCKETDATA = CURLoption #{const CURLOPT_OPENSOCKETDATA}

{- POST volatile input fields. -}
cURLOPT_COPYPOSTFIELDS = CURLoption #{const CURLOPT_COPYPOSTFIELDS}

{- set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy -}
cURLOPT_PROXY_TRANSFER_MODE = CURLoption #{const CURLOPT_PROXY_TRANSFER_MODE}

{- Callback function for seeking in the input stream -}
cURLOPT_SEEKFUNCTION = CURLoption #{const CURLOPT_SEEKFUNCTION}
cURLOPT_SEEKDATA = CURLoption #{const CURLOPT_SEEKDATA}

{- CRL file -}
cURLOPT_CRLFILE = CURLoption #{const CURLOPT_CRLFILE}

{- Issuer certificate -}
cURLOPT_ISSUERCERT = CURLoption #{const CURLOPT_ISSUERCERT}

{- (IPv6) Address scope -}
cURLOPT_ADDRESS_SCOPE = CURLoption #{const CURLOPT_ADDRESS_SCOPE}

{- Collect certificate chain info and allow it to get retrievable with
   CURLINFO_CERTINFO after the transfer is complete. -}
cURLOPT_CERTINFO = CURLoption #{const CURLOPT_CERTINFO}

{- "name" and "pwd" to use when fetching. -}
cURLOPT_USERNAME = CURLoption #{const CURLOPT_USERNAME}
cURLOPT_PASSWORD = CURLoption #{const CURLOPT_PASSWORD}

  {- "name" and "pwd" to use with Proxy when fetching. -}
cURLOPT_PROXYUSERNAME = CURLoption #{const CURLOPT_PROXYUSERNAME}
cURLOPT_PROXYPASSWORD = CURLoption #{const CURLOPT_PROXYPASSWORD}

{- Comma separated list of hostnames defining no-proxy zones. These should
   match both hostnames directly, and hostnames within a domain. For
   example, local.com will match local.com and www.local.com, but NOT
   notlocal.com or www.notlocal.com. For compatibility with other
   implementations of this, .local.com will be considered to be the same as
   local.com. A single * is the only valid wildcard, and effectively
   disables the use of proxy. -}
cURLOPT_NOPROXY = CURLoption #{const CURLOPT_NOPROXY}

{- block size for TFTP transfers -}
cURLOPT_TFTP_BLKSIZE = CURLoption #{const CURLOPT_TFTP_BLKSIZE}

{- Socks Service -}
cURLOPT_SOCKS5_GSSAPI_NEC = CURLoption #{const CURLOPT_SOCKS5_GSSAPI_NEC}

{- set the SSH knownhost file name to use -}
cURLOPT_SSH_KNOWNHOSTS = CURLoption #{const CURLOPT_SSH_KNOWNHOSTS}

{- set the SSH host key callback, must point to a curl_sshkeycallback
   function -}
cURLOPT_SSH_KEYFUNCTION = CURLoption #{const CURLOPT_SSH_KEYFUNCTION}

{- set the SSH host key callback custom pointer -}
cURLOPT_SSH_KEYDATA = CURLoption #{const CURLOPT_SSH_KEYDATA}

{- set the SMTP mail originator -}
cURLOPT_MAIL_FROM = CURLoption #{const CURLOPT_MAIL_FROM}

{- set the list of SMTP mail receiver(s) -}
cURLOPT_MAIL_RCPT = CURLoption #{const CURLOPT_MAIL_RCPT}

{- FTP: send PRET before PASV -}
cURLOPT_FTP_USE_PRET = CURLoption #{const CURLOPT_FTP_USE_PRET}

{- RTSP request method (OPTIONS, SETUP, PLAY, etc...) -}
cURLOPT_RTSP_REQUEST = CURLoption #{const CURLOPT_RTSP_REQUEST}

{- The RTSP session identifier -}
cURLOPT_RTSP_SESSION_ID = CURLoption #{const CURLOPT_RTSP_SESSION_ID}

{- The RTSP stream URI -}
cURLOPT_RTSP_STREAM_URI = CURLoption #{const CURLOPT_RTSP_STREAM_URI}

{- The Transport: header to use in RTSP requests -}
cURLOPT_RTSP_TRANSPORT = CURLoption #{const CURLOPT_RTSP_TRANSPORT}

{- Manually initialize the client RTSP CSeq for this handle -}
cURLOPT_RTSP_CLIENT_CSEQ = CURLoption #{const CURLOPT_RTSP_CLIENT_CSEQ}

{- Manually initialize the server RTSP CSeq for this handle -}
cURLOPT_RTSP_SERVER_CSEQ = CURLoption #{const CURLOPT_RTSP_SERVER_CSEQ}

{- The stream to pass to INTERLEAVEFUNCTION. -}
cURLOPT_INTERLEAVEDATA = CURLoption #{const CURLOPT_INTERLEAVEDATA}

{- Let the application define a custom write method for RTP data -}
cURLOPT_INTERLEAVEFUNCTION = CURLoption #{const CURLOPT_INTERLEAVEFUNCTION}

{- Turn on wildcard matching -}
cURLOPT_WILDCARDMATCH = CURLoption #{const CURLOPT_WILDCARDMATCH}

{- Directory matching callback called before downloading of an
   individual file (chunk) started -}
cURLOPT_CHUNK_BGN_FUNCTION = CURLoption #{const CURLOPT_CHUNK_BGN_FUNCTION}

{- Directory matching callback called after the file (chunk)
   was downloaded, or skipped -}
cURLOPT_CHUNK_END_FUNCTION = CURLoption #{const CURLOPT_CHUNK_END_FUNCTION}

{- Change match (fnmatch-like) callback for wildcard matching -}
cURLOPT_FNMATCH_FUNCTION = CURLoption #{const CURLOPT_FNMATCH_FUNCTION}

{- Let the application define custom chunk data pointer -}
cURLOPT_CHUNK_DATA = CURLoption #{const CURLOPT_CHUNK_DATA}

{- FNMATCH_FUNCTION user pointer -}
cURLOPT_FNMATCH_DATA = CURLoption #{const CURLOPT_FNMATCH_DATA}

{- send linked-list of name:port:address sets -}
cURLOPT_RESOLVE = CURLoption #{const CURLOPT_RESOLVE}

{- Set a username for authenticated TLS -}
cURLOPT_TLSAUTH_USERNAME = CURLoption #{const CURLOPT_TLSAUTH_USERNAME}

{- Set a password for authenticated TLS -}
cURLOPT_TLSAUTH_PASSWORD = CURLoption #{const CURLOPT_TLSAUTH_PASSWORD}

{- Set authentication type for authenticated TLS -}
cURLOPT_TLSAUTH_TYPE = CURLoption #{const CURLOPT_TLSAUTH_TYPE}

{- Set to 1 to enable the "TE:" header in HTTP requests to ask for
   compressed transfer-encoded responses. Set to 0 to disable the use of TE:
   in outgoing requests. The current default is 0, but it might change in a
   future libcurl release.

   libcurl will ask for the compressed methods it knows of, and if that
   isn't any, it will not ask for transfer-encoding at all even if this
   option is set to 1.

-}
cURLOPT_TRANSFER_ENCODING = CURLoption #{const CURLOPT_TRANSFER_ENCODING}

{- Callback function for closing socket (instead of close(2)). The callback
   should have type curl_closesocket_callback -}
cURLOPT_CLOSESOCKETFUNCTION = CURLoption #{const CURLOPT_CLOSESOCKETFUNCTION}
cURLOPT_CLOSESOCKETDATA = CURLoption #{const CURLOPT_CLOSESOCKETDATA}

{- allow GSSAPI credential delegation -}
cURLOPT_GSSAPI_DELEGATION = CURLoption #{const CURLOPT_GSSAPI_DELEGATION}

{- Set the name servers to use for DNS resolution.
 * Only supported by the c-ares DNS backend -}
cURLOPT_DNS_SERVERS = CURLoption #{const CURLOPT_DNS_SERVERS}

{- Time-out accept operations (currently for FTP only) after this amount
   of milliseconds. -}
cURLOPT_ACCEPTTIMEOUT_MS = CURLoption #{const CURLOPT_ACCEPTTIMEOUT_MS}

{- Set TCP keepalive -}
cURLOPT_TCP_KEEPALIVE = CURLoption #{const CURLOPT_TCP_KEEPALIVE}

{- non-universal keepalive knobs (Linux, AIX, HP-UX, more) -}
cURLOPT_TCP_KEEPIDLE = CURLoption #{const CURLOPT_TCP_KEEPIDLE}
cURLOPT_TCP_KEEPINTVL = CURLoption #{const CURLOPT_TCP_KEEPINTVL}

{- Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_* -}
cURLOPT_SSL_OPTIONS = CURLoption #{const CURLOPT_SSL_OPTIONS}

{- Set the SMTP auth originator -}
cURLOPT_MAIL_AUTH = CURLoption #{const CURLOPT_MAIL_AUTH}

{- Enable/disable SASL initial response -}
cURLOPT_SASL_IR = CURLoption #{const CURLOPT_SASL_IR}

{- Function that will be called instead of the internal progress display
 * function. This function should be defined as the curl_xferinfo_callback
 * prototype defines. (Deprecates CURLOPT_PROGRESSFUNCTION) -}
cURLOPT_XFERINFOFUNCTION = CURLoption #{const CURLOPT_XFERINFOFUNCTION}

{- The XOAUTH2 bearer token -}
cURLOPT_XOAUTH2_BEARER = CURLoption #{const CURLOPT_XOAUTH2_BEARER}

{- Set the interface string to use as outgoing network
 * interface for DNS requests.
 * Only supported by the c-ares DNS backend -}
cURLOPT_DNS_INTERFACE = CURLoption #{const CURLOPT_DNS_INTERFACE}

{- Set the local IPv4 address to use for outgoing DNS requests.
 * Only supported by the c-ares DNS backend -}
cURLOPT_DNS_LOCAL_IP4 = CURLoption #{const CURLOPT_DNS_LOCAL_IP4}

{- Set the local IPv6 address to use for outgoing DNS requests.
 * Only supported by the c-ares DNS backend -}
cURLOPT_DNS_LOCAL_IP6 = CURLoption #{const CURLOPT_DNS_LOCAL_IP6}

{- Set authentication options directly -}
cURLOPT_LOGIN_OPTIONS = CURLoption #{const CURLOPT_LOGIN_OPTIONS}

{- Enable/disable TLS ALPN extension (http2 over ssl might fail without) -}
cURLOPT_SSL_ENABLE_ALPN = CURLoption #{const CURLOPT_SSL_ENABLE_ALPN}

{- Time to wait for a response to an HTTP request containing an
 * Expect: 100-continue header before sending the data anyway. -}
cURLOPT_EXPECT_100_TIMEOUT_MS = CURLoption #{const CURLOPT_EXPECT_100_TIMEOUT_MS}

{- This points to a linked list of headers used for proxy requests only,
   struct curl_slist kind -}
cURLOPT_PROXYHEADER = CURLoption #{const CURLOPT_PROXYHEADER}

{- Pass in a bitmask of "header options" -}
cURLOPT_HEADEROPT = CURLoption #{const CURLOPT_HEADEROPT}

{- The public key in DER form used to validate the peer public key
   this option is used only if SSL_VERIFYPEER is true -}
cURLOPT_PINNEDPUBLICKEY = CURLoption #{const CURLOPT_PINNEDPUBLICKEY}

{- Path to Unix domain socket -}
cURLOPT_UNIX_SOCKET_PATH = CURLoption #{const CURLOPT_UNIX_SOCKET_PATH}

{- Set if we should verify the certificate status. -}
cURLOPT_SSL_VERIFYSTATUS = CURLoption #{const CURLOPT_SSL_VERIFYSTATUS}

{- Set if we should enable TLS false start. -}
cURLOPT_SSL_FALSESTART = CURLoption #{const CURLOPT_SSL_FALSESTART}

{- Do not squash dot-dot sequences -}
cURLOPT_PATH_AS_IS = CURLoption #{const CURLOPT_PATH_AS_IS}

{- Proxy Service Name -}
cURLOPT_PROXY_SERVICE_NAME = CURLoption #{const CURLOPT_PROXY_SERVICE_NAME}

{- Service Name -}
cURLOPT_SERVICE_NAME = CURLoption #{const CURLOPT_SERVICE_NAME}

{- Wait/don't wait for pipe/mutex to clarify -}
cURLOPT_PIPEWAIT = CURLoption #{const CURLOPT_PIPEWAIT}

{- Set the protocol used when curl is given a URL without a protocol -}
cURLOPT_DEFAULT_PROTOCOL = CURLoption #{const CURLOPT_DEFAULT_PROTOCOL}

{- Set stream weight, 1 - 256 (default is 16) -}
cURLOPT_STREAM_WEIGHT = CURLoption #{const CURLOPT_STREAM_WEIGHT}

{- Set stream dependency on another CURL handle -}
cURLOPT_STREAM_DEPENDS = CURLoption #{const CURLOPT_STREAM_DEPENDS}

{- Set E-xclusive stream dependency on another CURL handle -}
cURLOPT_STREAM_DEPENDS_E = CURLoption #{const CURLOPT_STREAM_DEPENDS_E}

{- Do not send any tftp option requests to the server -}
cURLOPT_TFTP_NO_OPTIONS = CURLoption #{const CURLOPT_TFTP_NO_OPTIONS}

{- Linked-list of host:port:connect-to-host:connect-to-port,
   overrides the URL's host:port (only for the network layer) -}
cURLOPT_CONNECT_TO = CURLoption #{const CURLOPT_CONNECT_TO}

{- Set TCP Fast Open -}
cURLOPT_TCP_FASTOPEN = CURLoption #{const CURLOPT_TCP_FASTOPEN}

{- Continue to send data if the server responds early with an
 * HTTP status code >= 300 -}
cURLOPT_KEEP_SENDING_ON_ERROR = CURLoption #{const CURLOPT_KEEP_SENDING_ON_ERROR}

{- The CApath or CAfile used to validate the proxy certificate
   this option is used only if PROXY_SSL_VERIFYPEER is true -}
cURLOPT_PROXY_CAINFO = CURLoption #{const CURLOPT_PROXY_CAINFO}

{- The CApath directory used to validate the proxy certificate
   this option is used only if PROXY_SSL_VERIFYPEER is true -}
cURLOPT_PROXY_CAPATH = CURLoption #{const CURLOPT_PROXY_CAPATH}

{- Set if we should verify the proxy in ssl handshake,
   set 1 to verify. -}
cURLOPT_PROXY_SSL_VERIFYPEER = CURLoption #{const CURLOPT_PROXY_SSL_VERIFYPEER}

{- Set if we should verify the Common name from the proxy certificate in ssl
 * handshake, set 1 to check existence, 2 to ensure that it matches
 * the provided hostname. -}
cURLOPT_PROXY_SSL_VERIFYHOST = CURLoption #{const CURLOPT_PROXY_SSL_VERIFYHOST}

{- What version to specifically try to use for proxy.
   See CURL_SSLVERSION defines below. -}
cURLOPT_PROXY_SSLVERSION = CURLoption #{const CURLOPT_PROXY_SSLVERSION}

{- Set a username for authenticated TLS for proxy -}
cURLOPT_PROXY_TLSAUTH_USERNAME = CURLoption #{const CURLOPT_PROXY_TLSAUTH_USERNAME}

{- Set a password for authenticated TLS for proxy -}
cURLOPT_PROXY_TLSAUTH_PASSWORD = CURLoption #{const CURLOPT_PROXY_TLSAUTH_PASSWORD}

{- Set authentication type for authenticated TLS for proxy -}
cURLOPT_PROXY_TLSAUTH_TYPE = CURLoption #{const CURLOPT_PROXY_TLSAUTH_TYPE}

{- name of the file keeping your private SSL-certificate for proxy -}
cURLOPT_PROXY_SSLCERT = CURLoption #{const CURLOPT_PROXY_SSLCERT}

{- type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") for
   proxy -}
cURLOPT_PROXY_SSLCERTTYPE = CURLoption #{const CURLOPT_PROXY_SSLCERTTYPE}

{- name of the file keeping your private SSL-key for proxy -}
cURLOPT_PROXY_SSLKEY = CURLoption #{const CURLOPT_PROXY_SSLKEY}

{- type of the file keeping your private SSL-key ("DER", "PEM", "ENG") for
   proxy -}
cURLOPT_PROXY_SSLKEYTYPE = CURLoption #{const CURLOPT_PROXY_SSLKEYTYPE}

{- password for the SSL private key for proxy -}
cURLOPT_PROXY_KEYPASSWD = CURLoption #{const CURLOPT_PROXY_KEYPASSWD}

{- Specify which SSL ciphers to use for proxy -}
cURLOPT_PROXY_SSL_CIPHER_LIST = CURLoption #{const CURLOPT_PROXY_SSL_CIPHER_LIST}

{- CRL file for proxy -}
cURLOPT_PROXY_CRLFILE = CURLoption #{const CURLOPT_PROXY_CRLFILE}

{- Enable/disable specific SSL features with a bitmask for proxy, see
   CURLSSLOPT_* -}
cURLOPT_PROXY_SSL_OPTIONS = CURLoption #{const CURLOPT_PROXY_SSL_OPTIONS}

{- Name of pre proxy to use. -}
cURLOPT_PRE_PROXY = CURLoption #{const CURLOPT_PRE_PROXY}

{- The public key in DER form used to validate the proxy public key
   this option is used only if PROXY_SSL_VERIFYPEER is true -}
cURLOPT_PROXY_PINNEDPUBLICKEY = CURLoption #{const CURLOPT_PROXY_PINNEDPUBLICKEY}

{- Path to an abstract Unix domain socket -}
cURLOPT_ABSTRACT_UNIX_SOCKET = CURLoption #{const CURLOPT_ABSTRACT_UNIX_SOCKET}

{- Suppress proxy CONNECT response headers from user callbacks -}
cURLOPT_SUPPRESS_CONNECT_HEADERS = CURLoption #{const CURLOPT_SUPPRESS_CONNECT_HEADERS}

{- The request target, instead of extracted from the URL -}
cURLOPT_REQUEST_TARGET = CURLoption #{const CURLOPT_REQUEST_TARGET}

{- bitmask of allowed auth methods for connections to SOCKS5 proxies -}
cURLOPT_SOCKS5_AUTH = CURLoption #{const CURLOPT_SOCKS5_AUTH}

{- Enable/disable SSH compression -}
cURLOPT_SSH_COMPRESSION = CURLoption #{const CURLOPT_SSH_COMPRESSION}

{- Post MIME data. -}
cURLOPT_MIMEPOST = CURLoption #{const CURLOPT_MIMEPOST}

{- Time to use with the CURLOPT_TIMECONDITION. Specified in number of
   seconds since 1 Jan 1970. -}
cURLOPT_TIMEVALUE_LARGE = CURLoption #{const CURLOPT_TIMEVALUE_LARGE}

{- Head start in milliseconds to give happy eyeballs. -}
cURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS = CURLoption #{const CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS}

{- Function that will be called before a resolver request is made -}
cURLOPT_RESOLVER_START_FUNCTION = CURLoption #{const CURLOPT_RESOLVER_START_FUNCTION}

{- User data to pass to the resolver start callback. -}
cURLOPT_RESOLVER_START_DATA = CURLoption #{const CURLOPT_RESOLVER_START_DATA}

{- send HAProxy PROXY protocol header? -}
cURLOPT_HAPROXYPROTOCOL = CURLoption #{const CURLOPT_HAPROXYPROTOCOL}

{- shuffle addresses before use when DNS returns multiple -}
cURLOPT_DNS_SHUFFLE_ADDRESSES = CURLoption #{const CURLOPT_DNS_SHUFFLE_ADDRESSES}

{- Specify which TLS 1.3 ciphers suites to use -}
cURLOPT_TLS13_CIPHERS = CURLoption #{const CURLOPT_TLS13_CIPHERS}
cURLOPT_PROXY_TLS13_CIPHERS = CURLoption #{const CURLOPT_PROXY_TLS13_CIPHERS}

{- Disallow specifying username/login in URL. -}
cURLOPT_DISALLOW_USERNAME_IN_URL = CURLoption #{const CURLOPT_DISALLOW_USERNAME_IN_URL}

{- DNS-over-HTTPS URL -}
cURLOPT_DOH_URL = CURLoption #{const CURLOPT_DOH_URL}

{- Preferred buffer size to use for uploads -}
cURLOPT_UPLOAD_BUFFERSIZE = CURLoption #{const CURLOPT_UPLOAD_BUFFERSIZE}

{- Time in ms between connection upkeep calls for long-lived connections. -}
cURLOPT_UPKEEP_INTERVAL_MS = CURLoption #{const CURLOPT_UPKEEP_INTERVAL_MS}

{- Specify URL using CURL URL API. -}
cURLOPT_CURLU = CURLoption #{const CURLOPT_CURLU}

{- add trailing data just after no more data is available -}
cURLOPT_TRAILERFUNCTION = CURLoption #{const CURLOPT_TRAILERFUNCTION}

{- pointer to be passed to HTTP_TRAILER_FUNCTION -}
cURLOPT_TRAILERDATA = CURLoption #{const CURLOPT_TRAILERDATA}

{- set this to 1L to allow HTTP/0.9 responses or 0L to disallow -}
cURLOPT_HTTP09_ALLOWED = CURLoption #{const CURLOPT_HTTP09_ALLOWED}

{- alt-svc control bitmask -}
cURLOPT_ALTSVC_CTRL = CURLoption #{const CURLOPT_ALTSVC_CTRL}

{- alt-svc cache file name to possibly read from/write to -}
cURLOPT_ALTSVC = CURLoption #{const CURLOPT_ALTSVC}

{- maximum age (idle time) of a connection to consider it for reuse
 * (in seconds) -}
cURLOPT_MAXAGE_CONN = CURLoption #{const CURLOPT_MAXAGE_CONN}

{- SASL authorization identity -}
cURLOPT_SASL_AUTHZID = CURLoption #{const CURLOPT_SASL_AUTHZID}

{- allow RCPT TO command to fail for some recipients -}
cURLOPT_MAIL_RCPT_ALLOWFAILS = CURLoption #{const CURLOPT_MAIL_RCPT_ALLOWFAILS}

{- the private SSL-certificate as a "blob" -}
cURLOPT_SSLCERT_BLOB = CURLoption #{const CURLOPT_SSLCERT_BLOB}
cURLOPT_SSLKEY_BLOB = CURLoption #{const CURLOPT_SSLKEY_BLOB}
cURLOPT_PROXY_SSLCERT_BLOB = CURLoption #{const CURLOPT_PROXY_SSLCERT_BLOB}
cURLOPT_PROXY_SSLKEY_BLOB = CURLoption #{const CURLOPT_PROXY_SSLKEY_BLOB}
cURLOPT_ISSUERCERT_BLOB = CURLoption #{const CURLOPT_ISSUERCERT_BLOB}

{- Issuer certificate for proxy -}
cURLOPT_PROXY_ISSUERCERT = CURLoption #{const CURLOPT_PROXY_ISSUERCERT}
cURLOPT_PROXY_ISSUERCERT_BLOB = CURLoption #{const CURLOPT_PROXY_ISSUERCERT_BLOB}

{- the EC curves requested by the TLS client (RFC 8422, 5.1);
 * OpenSSL support via 'set_groups'/'set_curves':
 * https://www.openssl.org/docs/manmaster/man3/SSL_CTX_set1_groups.html
 -}
cURLOPT_SSL_EC_CURVES = CURLoption #{const CURLOPT_SSL_EC_CURVES}

{- HSTS bitmask -}
cURLOPT_HSTS_CTRL = CURLoption #{const CURLOPT_HSTS_CTRL}
{- HSTS file name -}
cURLOPT_HSTS = CURLoption #{const CURLOPT_HSTS}

{- HSTS read callback -}
cURLOPT_HSTSREADFUNCTION = CURLoption #{const CURLOPT_HSTSREADFUNCTION}
cURLOPT_HSTSREADDATA = CURLoption #{const CURLOPT_HSTSREADDATA}

{- HSTS write callback -}
cURLOPT_HSTSWRITEFUNCTION = CURLoption #{const CURLOPT_HSTSWRITEFUNCTION}
cURLOPT_HSTSWRITEDATA = CURLoption #{const CURLOPT_HSTSWRITEDATA}

{- Parameters for V4 signature -}
cURLOPT_AWS_SIGV4 = CURLoption #{const CURLOPT_AWS_SIGV4}

{- Same as CURLOPT_SSL_VERIFYPEER but for DoH (DNS-over-HTTPS) servers. -}
cURLOPT_DOH_SSL_VERIFYPEER = CURLoption #{const CURLOPT_DOH_SSL_VERIFYPEER}

{- Same as CURLOPT_SSL_VERIFYHOST but for DoH (DNS-over-HTTPS) servers. -}
cURLOPT_DOH_SSL_VERIFYHOST = CURLoption #{const CURLOPT_DOH_SSL_VERIFYHOST}

{- Same as CURLOPT_SSL_VERIFYSTATUS but for DoH (DNS-over-HTTPS) servers. -}
cURLOPT_DOH_SSL_VERIFYSTATUS = CURLoption #{const CURLOPT_DOH_SSL_VERIFYSTATUS}

{- The CA certificates as "blob" used to validate the peer certificate
   this option is used only if SSL_VERIFYPEER is true -}
cURLOPT_CAINFO_BLOB = CURLoption #{const CURLOPT_CAINFO_BLOB}

{- The CA certificates as "blob" used to validate the proxy certificate
   this option is used only if PROXY_SSL_VERIFYPEER is true -}
cURLOPT_PROXY_CAINFO_BLOB = CURLoption #{const CURLOPT_PROXY_CAINFO_BLOB}

{- used by scp/sftp to verify the host's public key -}
cURLOPT_SSH_HOST_PUBLIC_KEY_SHA256 = CURLoption #{const CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256}

{- Function that will be called immediately before the initial request
   is made on a connection (after any protocol negotiation step).  -}
cURLOPT_PREREQFUNCTION = CURLoption #{const CURLOPT_PREREQFUNCTION}

{- Data passed to the CURLOPT_PREREQFUNCTION callback -}
cURLOPT_PREREQDATA = CURLoption #{const CURLOPT_PREREQDATA}

{- maximum age (since creation) of a connection to consider it for reuse
 * (in seconds) -}
cURLOPT_MAXLIFETIME_CONN = CURLoption #{const CURLOPT_MAXLIFETIME_CONN}

{- Set MIME option flags. -}
cURLOPT_MIME_OPTIONS = CURLoption #{const CURLOPT_MIME_OPTIONS}

{- set the SSH host key callback, must point to a curl_sshkeycallback
   function -}
cURLOPT_SSH_HOSTKEYFUNCTION = CURLoption #{const CURLOPT_SSH_HOSTKEYFUNCTION}

{- set the SSH host key callback custom pointer -}
cURLOPT_SSH_HOSTKEYDATA = CURLoption #{const CURLOPT_SSH_HOSTKEYDATA}

{- specify which protocols that are allowed to be used for the transfer,
   which thus helps the app which takes URLs from users or other external
   inputs and want to restrict what protocol(s) to deal with. Defaults to
   all built-in protocols. -}
cURLOPT_PROTOCOLS_STR = CURLoption #{const CURLOPT_PROTOCOLS_STR}

{- specify which protocols that libcurl is allowed to follow directs to -}
cURLOPT_REDIR_PROTOCOLS_STR = CURLoption #{const CURLOPT_REDIR_PROTOCOLS_STR}

{- websockets options -}
cURLOPT_WS_OPTIONS = CURLoption #{const CURLOPT_WS_OPTIONS}

{- CA cache timeout -}
cURLOPT_CA_CACHE_TIMEOUT = CURLoption #{const CURLOPT_CA_CACHE_TIMEOUT}

{- Can leak things, gonna exit() soon -}
cURLOPT_QUICK_EXIT = CURLoption #{const CURLOPT_QUICK_EXIT}

{- set a specific client IP for HAProxy PROXY protocol header? -}
cURLOPT_HAPROXY_CLIENT_IP = CURLoption #{const CURLOPT_HAPROXY_CLIENT_IP}

{- millisecond version -}
cURLOPT_SERVER_RESPONSE_TIMEOUT_MS = CURLoption #{const CURLOPT_SERVER_RESPONSE_TIMEOUT_MS}

newtype CURLINFO = CURLINFO {unCURLINFO :: CInt}

cURLINFO_EFFECTIVE_URL = CURLINFO #{const CURLINFO_EFFECTIVE_URL}
cURLINFO_RESPONSE_CODE = CURLINFO #{const CURLINFO_RESPONSE_CODE}
cURLINFO_TOTAL_TIME = CURLINFO #{const CURLINFO_TOTAL_TIME}
cURLINFO_NAMELOOKUP_TIME = CURLINFO #{const CURLINFO_NAMELOOKUP_TIME}
cURLINFO_CONNECT_TIME = CURLINFO #{const CURLINFO_CONNECT_TIME}
cURLINFO_PRETRANSFER_TIME = CURLINFO #{const CURLINFO_PRETRANSFER_TIME}
cURLINFO_SIZE_UPLOAD_T = CURLINFO #{const CURLINFO_SIZE_UPLOAD_T}
cURLINFO_SIZE_DOWNLOAD_T = CURLINFO #{const CURLINFO_SIZE_DOWNLOAD_T}
cURLINFO_SPEED_DOWNLOAD_T = CURLINFO #{const CURLINFO_SPEED_DOWNLOAD_T}
cURLINFO_SPEED_UPLOAD_T = CURLINFO #{const CURLINFO_SPEED_UPLOAD_T}
cURLINFO_HEADER_SIZE = CURLINFO #{const CURLINFO_HEADER_SIZE}
cURLINFO_REQUEST_SIZE = CURLINFO #{const CURLINFO_REQUEST_SIZE}
cURLINFO_SSL_VERIFYRESULT = CURLINFO #{const CURLINFO_SSL_VERIFYRESULT}
cURLINFO_FILETIME = CURLINFO #{const CURLINFO_FILETIME}
cURLINFO_FILETIME_T = CURLINFO #{const CURLINFO_FILETIME_T}
cURLINFO_CONTENT_LENGTH_DOWNLOAD_T = CURLINFO #{const CURLINFO_CONTENT_LENGTH_DOWNLOAD_T}
cURLINFO_CONTENT_LENGTH_UPLOAD_T = CURLINFO #{const CURLINFO_CONTENT_LENGTH_UPLOAD_T}
cURLINFO_STARTTRANSFER_TIME = CURLINFO #{const CURLINFO_STARTTRANSFER_TIME}
cURLINFO_CONTENT_TYPE = CURLINFO #{const CURLINFO_CONTENT_TYPE}
cURLINFO_REDIRECT_TIME = CURLINFO #{const CURLINFO_REDIRECT_TIME}
cURLINFO_REDIRECT_COUNT = CURLINFO #{const CURLINFO_REDIRECT_COUNT}
cURLINFO_PRIVATE = CURLINFO #{const CURLINFO_PRIVATE}
cURLINFO_HTTP_CONNECTCODE = CURLINFO #{const CURLINFO_HTTP_CONNECTCODE}
cURLINFO_HTTPAUTH_AVAIL = CURLINFO #{const CURLINFO_HTTPAUTH_AVAIL}
cURLINFO_PROXYAUTH_AVAIL = CURLINFO #{const CURLINFO_PROXYAUTH_AVAIL}
cURLINFO_OS_ERRNO = CURLINFO #{const CURLINFO_OS_ERRNO}
cURLINFO_NUM_CONNECTS = CURLINFO #{const CURLINFO_NUM_CONNECTS}
cURLINFO_SSL_ENGINES = CURLINFO #{const CURLINFO_SSL_ENGINES}
cURLINFO_COOKIELIST = CURLINFO #{const CURLINFO_COOKIELIST}
cURLINFO_FTP_ENTRY_PATH = CURLINFO #{const CURLINFO_FTP_ENTRY_PATH}
cURLINFO_REDIRECT_URL = CURLINFO #{const CURLINFO_REDIRECT_URL}
cURLINFO_PRIMARY_IP = CURLINFO #{const CURLINFO_PRIMARY_IP}
cURLINFO_APPCONNECT_TIME = CURLINFO #{const CURLINFO_APPCONNECT_TIME}
cURLINFO_CERTINFO = CURLINFO #{const CURLINFO_CERTINFO}
cURLINFO_CONDITION_UNMET = CURLINFO #{const CURLINFO_CONDITION_UNMET}
cURLINFO_RTSP_SESSION_ID = CURLINFO #{const CURLINFO_RTSP_SESSION_ID}
cURLINFO_RTSP_CLIENT_CSEQ = CURLINFO #{const CURLINFO_RTSP_CLIENT_CSEQ}
cURLINFO_RTSP_SERVER_CSEQ = CURLINFO #{const CURLINFO_RTSP_SERVER_CSEQ}
cURLINFO_RTSP_CSEQ_RECV = CURLINFO #{const CURLINFO_RTSP_CSEQ_RECV}
cURLINFO_PRIMARY_PORT = CURLINFO #{const CURLINFO_PRIMARY_PORT}
cURLINFO_LOCAL_IP = CURLINFO #{const CURLINFO_LOCAL_IP}
cURLINFO_LOCAL_PORT = CURLINFO #{const CURLINFO_LOCAL_PORT}
cURLINFO_ACTIVESOCKET = CURLINFO #{const CURLINFO_ACTIVESOCKET}
cURLINFO_TLS_SSL_PTR = CURLINFO #{const CURLINFO_TLS_SSL_PTR}
cURLINFO_HTTP_VERSION = CURLINFO #{const CURLINFO_HTTP_VERSION}
cURLINFO_PROXY_SSL_VERIFYRESULT = CURLINFO #{const CURLINFO_PROXY_SSL_VERIFYRESULT}
cURLINFO_SCHEME = CURLINFO #{const CURLINFO_SCHEME}
cURLINFO_TOTAL_TIME_T = CURLINFO #{const CURLINFO_TOTAL_TIME_T}
cURLINFO_NAMELOOKUP_TIME_T = CURLINFO #{const CURLINFO_NAMELOOKUP_TIME_T}
cURLINFO_CONNECT_TIME_T = CURLINFO #{const CURLINFO_CONNECT_TIME_T}
cURLINFO_PRETRANSFER_TIME_T = CURLINFO #{const CURLINFO_PRETRANSFER_TIME_T}
cURLINFO_STARTTRANSFER_TIME_T = CURLINFO #{const CURLINFO_STARTTRANSFER_TIME_T}
cURLINFO_REDIRECT_TIME_T = CURLINFO #{const CURLINFO_REDIRECT_TIME_T}
cURLINFO_APPCONNECT_TIME_T = CURLINFO #{const CURLINFO_APPCONNECT_TIME_T}
cURLINFO_RETRY_AFTER = CURLINFO #{const CURLINFO_RETRY_AFTER}
cURLINFO_EFFECTIVE_METHOD = CURLINFO #{const CURLINFO_EFFECTIVE_METHOD}
cURLINFO_PROXY_ERROR = CURLINFO #{const CURLINFO_PROXY_ERROR}
cURLINFO_REFERER = CURLINFO #{const CURLINFO_REFERER}
cURLINFO_CAINFO = CURLINFO #{const CURLINFO_CAINFO}
cURLINFO_CAPATH = CURLINFO #{const CURLINFO_CAPATH}
cURLINFO_XFER_ID = CURLINFO #{const CURLINFO_XFER_ID}
cURLINFO_CONN_ID = CURLINFO #{const CURLINFO_CONN_ID}
cURLINFO_QUEUE_TIME_T = CURLINFO #{const CURLINFO_QUEUE_TIME_T}
cURLINFO_USED_PROXY = CURLINFO #{const CURLINFO_USED_PROXY}
