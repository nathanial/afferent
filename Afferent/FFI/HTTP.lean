/-
  Afferent FFI HTTP
  HTTP client bindings using libcurl.
-/

namespace Afferent.FFI.HTTP

/-- Initialize libcurl globally. Call once at application startup. -/
@[extern "lean_curl_global_init"]
opaque globalInit : IO Unit

/-- Cleanup libcurl globally. Call once at application shutdown. -/
@[extern "lean_curl_global_cleanup"]
opaque globalCleanup : IO Unit

/-- HTTP GET request returning binary data.
    Returns `Except.ok` with the response body as a ByteArray on success,
    or `Except.error` with an error message on failure. -/
@[extern "lean_curl_http_get_binary"]
opaque httpGetBinary (url : @& String) : IO (Except String ByteArray)

end Afferent.FFI.HTTP
