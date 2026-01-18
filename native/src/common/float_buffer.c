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
    size_t count;
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
    buf->count = capacity;
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

float* afferent_float_buffer_data(AfferentFloatBufferRef buf) {
    return buf->data;
}

size_t afferent_float_buffer_count(AfferentFloatBufferRef buf) {
    return buf->count;
}

void afferent_float_buffer_set_count(AfferentFloatBufferRef buf, size_t count) {
    if (!buf) return;
    if (count > buf->capacity) {
        buf->count = buf->capacity;
    } else {
        buf->count = count;
    }
}

void afferent_float_buffer_set_vec8(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7) {
    // Direct memory writes - 8x less FFI overhead than 8 separate calls
    float* ptr = buf->data + index;
    ptr[0] = v0;
    ptr[1] = v1;
    ptr[2] = v2;
    ptr[3] = v3;
    ptr[4] = v4;
    ptr[5] = v5;
    ptr[6] = v6;
    ptr[7] = v7;
}

void afferent_float_buffer_set_vec9(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7, float v8) {
    // Direct memory writes - 9x less FFI overhead than 9 separate calls
    float* ptr = buf->data + index;
    ptr[0] = v0;
    ptr[1] = v1;
    ptr[2] = v2;
    ptr[3] = v3;
    ptr[4] = v4;
    ptr[5] = v5;
    ptr[6] = v6;
    ptr[7] = v7;
    ptr[8] = v8;
}

void afferent_float_buffer_set_vec5(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4) {
    // 5 floats for sprite data: [x, y, rotation, halfSize, alpha]
    float* ptr = buf->data + index;
    ptr[0] = v0;
    ptr[1] = v1;
    ptr[2] = v2;
    ptr[3] = v3;
    ptr[4] = v4;
}

// ============================================================================
// Sprite System - High-performance bouncing sprites with C-side physics
// Layout: [x, y, vx, vy, rotation] per sprite (5 floats)
// ============================================================================

// Initialize sprites with random positions and velocities
void afferent_float_buffer_init_sprites(AfferentFloatBufferRef buf, uint32_t count,
    float screenWidth, float screenHeight, uint32_t seed) {
    uint32_t s = seed;
    for (uint32_t i = 0; i < count; i++) {
        float* ptr = buf->data + i * 5;
        // Simple LCG random
        s = s * 1103515245 + 12345;
        ptr[0] = ((float)(s & 0x7FFFFFFF) / 2147483648.0f) * screenWidth;  // x
        s = s * 1103515245 + 12345;
        ptr[1] = ((float)(s & 0x7FFFFFFF) / 2147483648.0f) * screenHeight; // y
        s = s * 1103515245 + 12345;
        ptr[2] = (((float)(s & 0x7FFFFFFF) / 2147483648.0f) - 0.5f) * 400.0f; // vx
        s = s * 1103515245 + 12345;
        ptr[3] = (((float)(s & 0x7FFFFFFF) / 2147483648.0f) - 0.5f) * 400.0f; // vy
        ptr[4] = 0.0f; // rotation
    }
}

// Update sprite physics (bouncing) - runs entirely in C, no FFI overhead per sprite
void afferent_float_buffer_update_sprites(AfferentFloatBufferRef buf, uint32_t count,
    float dt, float halfSize, float screenWidth, float screenHeight) {
    for (uint32_t i = 0; i < count; i++) {
        float* ptr = buf->data + i * 5;
        float x = ptr[0];
        float y = ptr[1];
        float vx = ptr[2];
        float vy = ptr[3];

        // Update position
        x += vx * dt;
        y += vy * dt;

        // Bounce off walls
        if (x < halfSize) { x = halfSize; vx = -vx; }
        else if (x > screenWidth - halfSize) { x = screenWidth - halfSize; vx = -vx; }
        if (y < halfSize) { y = halfSize; vy = -vy; }
        else if (y > screenHeight - halfSize) { y = screenHeight - halfSize; vy = -vy; }

        ptr[0] = x;
        ptr[1] = y;
        ptr[2] = vx;
        ptr[3] = vy;
    }
}
