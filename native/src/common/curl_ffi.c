/*
 * Afferent CURL FFI
 * HTTP client bindings using libcurl
 */

#include <lean/lean.h>
#include <curl/curl.h>
#include <string.h>
#include <stdlib.h>

// Binary response buffer
typedef struct {
    unsigned char *data;
    size_t size;
} BinaryBuffer;

// Write callback for binary data
static size_t write_binary_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    BinaryBuffer *buf = (BinaryBuffer *)userp;

    unsigned char *ptr = realloc(buf->data, buf->size + realsize);
    if (!ptr) {
        return 0; // Out of memory
    }

    buf->data = ptr;
    memcpy(&(buf->data[buf->size]), contents, realsize);
    buf->size += realsize;

    return realsize;
}

// Initialize libcurl globally
LEAN_EXPORT lean_obj_res lean_curl_global_init(lean_obj_arg world) {
    CURLcode res = curl_global_init(CURL_GLOBAL_DEFAULT);
    if (res != CURLE_OK) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("Failed to initialize libcurl"))
        );
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// Cleanup libcurl globally
LEAN_EXPORT lean_obj_res lean_curl_global_cleanup(lean_obj_arg world) {
    curl_global_cleanup();
    return lean_io_result_mk_ok(lean_box(0));
}

// HTTP GET returning binary data as ByteArray
// Returns: IO (Except String ByteArray)
LEAN_EXPORT lean_obj_res lean_curl_http_get_binary(b_lean_obj_arg url_obj, lean_obj_arg world) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        // Return Except.error
        lean_object* except_error = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(except_error, 0, lean_mk_string("Failed to initialize CURL handle"));
        return lean_io_result_mk_ok(except_error);
    }

    const char *url = lean_string_cstr(url_obj);
    BinaryBuffer response = {NULL, 0};

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_binary_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "Afferent/1.0 (Lean4 Graphics Framework)");

    CURLcode res = curl_easy_perform(curl);
    lean_object *result;

    if (res != CURLE_OK) {
        // Except.error with curl error message
        lean_object* except_error = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(except_error, 0, lean_mk_string(curl_easy_strerror(res)));
        result = except_error;
    } else {
        long http_code;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

        if (http_code == 200 && response.data && response.size > 0) {
            // Except.ok ByteArray
            // lean_alloc_sarray(elem_size, size, capacity)
            lean_object* byte_array = lean_alloc_sarray(1, response.size, response.size);
            memcpy(lean_sarray_cptr(byte_array), response.data, response.size);

            lean_object* except_ok = lean_alloc_ctor(1, 1, 0);
            lean_ctor_set(except_ok, 0, byte_array);
            result = except_ok;
        } else {
            // HTTP error
            char msg[128];
            snprintf(msg, sizeof(msg), "HTTP error: %ld", http_code);
            lean_object* except_error = lean_alloc_ctor(0, 1, 0);
            lean_ctor_set(except_error, 0, lean_mk_string(msg));
            result = except_error;
        }
    }

    if (response.data) free(response.data);
    curl_easy_cleanup(curl);
    return lean_io_result_mk_ok(result);
}
