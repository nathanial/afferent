/*
 * Afferent Disk Cache FFI
 * File I/O operations for tile caching
 */

#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include <utime.h>
#include <time.h>
#include <unistd.h>

// Helper to create parent directories recursively (like mkdir -p)
static int mkpath(const char *path) {
    char tmp[1024];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp), "%s", path);
    len = strlen(tmp);

    // Remove trailing slash if present
    if (len > 0 && tmp[len - 1] == '/') {
        tmp[len - 1] = '\0';
    }

    for (p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';
            if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
                return -1;
            }
            *p = '/';
        }
    }
    // Create the final directory
    if (mkdir(tmp, 0755) != 0 && errno != EEXIST) {
        return -1;
    }
    return 0;
}

// Check if file exists and is a regular file
// Returns: IO Bool
LEAN_EXPORT lean_obj_res lean_disk_cache_exists(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    struct stat st;
    bool exists = (stat(path, &st) == 0 && S_ISREG(st.st_mode));
    return lean_io_result_mk_ok(lean_box(exists ? 1 : 0));
}

// Read file contents as ByteArray
// Returns: IO (Except String ByteArray)
LEAN_EXPORT lean_obj_res lean_disk_cache_read(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    FILE *f = fopen(path, "rb");

    if (!f) {
        // Except.error
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size < 0) {
        fclose(f);
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string("Failed to get file size"));
        return lean_io_result_mk_ok(err);
    }

    // Allocate ByteArray
    lean_object *byte_array = lean_alloc_sarray(1, (size_t)size, (size_t)size);
    size_t bytes_read = fread(lean_sarray_cptr(byte_array), 1, (size_t)size, f);
    fclose(f);

    if (bytes_read != (size_t)size) {
        // Except.error
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string("Short read"));
        return lean_io_result_mk_ok(err);
    }

    // Except.ok
    lean_object *ok = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, byte_array);
    return lean_io_result_mk_ok(ok);
}

// Write ByteArray to file (creating parent directories, atomic via temp+rename)
// Returns: IO (Except String Unit)
LEAN_EXPORT lean_obj_res lean_disk_cache_write(b_lean_obj_arg path_obj,
                                                b_lean_obj_arg data_obj,
                                                lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);

    // Extract parent directory and create it
    char *parent = strdup(path);
    if (!parent) {
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string("Out of memory"));
        return lean_io_result_mk_ok(err);
    }

    char *last_slash = strrchr(parent, '/');
    if (last_slash) {
        *last_slash = '\0';
        if (mkpath(parent) != 0) {
            free(parent);
            lean_object *err = lean_alloc_ctor(0, 1, 0);
            lean_ctor_set(err, 0, lean_mk_string("Failed to create directories"));
            return lean_io_result_mk_ok(err);
        }
    }
    free(parent);

    // Write to temp file first for atomic operation
    char tmp_path[1024];
    snprintf(tmp_path, sizeof(tmp_path), "%s.tmp.%d", path, getpid());

    FILE *f = fopen(tmp_path, "wb");
    if (!f) {
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    size_t size = lean_sarray_size(data_obj);
    const uint8_t *data = lean_sarray_cptr(data_obj);
    size_t written = fwrite(data, 1, size, f);
    fclose(f);

    if (written != size) {
        remove(tmp_path);
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string("Short write"));
        return lean_io_result_mk_ok(err);
    }

    // Atomic rename
    if (rename(tmp_path, path) != 0) {
        remove(tmp_path);
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    // Except.ok Unit
    lean_object *ok = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, lean_box(0));
    return lean_io_result_mk_ok(ok);
}

// Get file size in bytes
// Returns: IO (Except String Nat)
LEAN_EXPORT lean_obj_res lean_disk_cache_file_size(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    struct stat st;

    if (stat(path, &st) != 0) {
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    // Except.ok with size as Nat
    lean_object *ok = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, lean_usize_to_nat((size_t)st.st_size));
    return lean_io_result_mk_ok(ok);
}

// Get file modification time (seconds since epoch)
// Returns: IO (Except String Nat)
LEAN_EXPORT lean_obj_res lean_disk_cache_mod_time(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    struct stat st;

    if (stat(path, &st) != 0) {
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    // Except.ok with mtime as Nat
    lean_object *ok = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, lean_usize_to_nat((size_t)st.st_mtime));
    return lean_io_result_mk_ok(ok);
}

// Touch file (update access/modification time to now)
// Returns: IO Unit
LEAN_EXPORT lean_obj_res lean_disk_cache_touch(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);
    utime(path, NULL);  // Set to current time, ignore errors
    return lean_io_result_mk_ok(lean_box(0));
}

// Delete a file
// Returns: IO (Except String Unit)
LEAN_EXPORT lean_obj_res lean_disk_cache_delete(b_lean_obj_arg path_obj, lean_obj_arg world) {
    const char *path = lean_string_cstr(path_obj);

    if (remove(path) != 0 && errno != ENOENT) {
        lean_object *err = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(err, 0, lean_mk_string(strerror(errno)));
        return lean_io_result_mk_ok(err);
    }

    // Except.ok Unit
    lean_object *ok = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, lean_box(0));
    return lean_io_result_mk_ok(ok);
}

// Get current time in milliseconds (for LRU timestamps)
// Returns: IO Nat
LEAN_EXPORT lean_obj_res lean_disk_cache_now_ms(lean_obj_arg world) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    uint64_t ms = (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
    return lean_io_result_mk_ok(lean_usize_to_nat((size_t)ms));
}
