/*
 * FloatBuffer - High-performance mutable float array
 *
 * This buffer lives in C memory and provides true in-place mutation,
 * avoiding Lean's copy-on-write array semantics which cause O(n) copies
 * on each element update.
 *
 * For 10k particles with 8 floats each, this eliminates 80,000 array
 * allocations per frame.
 */

#include "afferent.h"
#include <stdlib.h>
#include <string.h>

struct AfferentFloatBuffer {
    float* data;
    size_t capacity;
};

AfferentResult afferent_float_buffer_create(size_t capacity, AfferentFloatBufferRef* out) {
    if (!out) return AFFERENT_ERROR_BUFFER_FAILED;

    AfferentFloatBufferRef buf = malloc(sizeof(struct AfferentFloatBuffer));
    if (!buf) return AFFERENT_ERROR_BUFFER_FAILED;

    buf->data = malloc(capacity * sizeof(float));
    if (!buf->data) {
        free(buf);
        return AFFERENT_ERROR_BUFFER_FAILED;
    }

    buf->capacity = capacity;
    // Zero-initialize for safety
    memset(buf->data, 0, capacity * sizeof(float));

    *out = buf;
    return AFFERENT_OK;
}

void afferent_float_buffer_destroy(AfferentFloatBufferRef buf) {
    if (buf) {
        free(buf->data);
        free(buf);
    }
}

void afferent_float_buffer_set(AfferentFloatBufferRef buf, size_t index, float value) {
    // No bounds checking for maximum performance - caller must ensure validity
    buf->data[index] = value;
}

float afferent_float_buffer_get(AfferentFloatBufferRef buf, size_t index) {
    // No bounds checking for maximum performance - caller must ensure validity
    return buf->data[index];
}

size_t afferent_float_buffer_capacity(AfferentFloatBufferRef buf) {
    return buf->capacity;
}

const float* afferent_float_buffer_data(AfferentFloatBufferRef buf) {
    return buf->data;
}
