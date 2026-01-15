// buffer_pool.m - Buffer pooling and memory management
#import "render.h"

// Global buffer pool instance
BufferPool g_buffer_pool = {0};

// Staging buffer for text vertex conversion (reused across frames)
TextVertex* g_text_vertex_staging = NULL;
size_t g_text_vertex_staging_capacity = 0;

// Get a wrapper struct from the pool (or allocate if pool is empty)
struct AfferentBuffer* pool_acquire_wrapper(void) {
    if (g_buffer_pool.wrapper_pool_used < g_buffer_pool.wrapper_pool_count) {
        struct AfferentBuffer* wrapper = g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_used++];
        wrapper->pooled = true;
        return wrapper;
    }
    // Pool exhausted, allocate new and try to add to pool
    struct AfferentBuffer* wrapper = malloc(sizeof(struct AfferentBuffer));
    if (g_buffer_pool.wrapper_pool_count < WRAPPER_POOL_SIZE) {
        g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_count++] = wrapper;
        g_buffer_pool.wrapper_pool_used++;
        wrapper->pooled = true;
    } else {
        wrapper->pooled = false;
    }
    return wrapper;
}

// Find or create a buffer of at least the required size
id<MTLBuffer> pool_acquire_buffer(id<MTLDevice> device, PooledBuffer* pool, int* count, size_t required_size) {

    // First, try to find an existing buffer that's large enough and not in use
    for (int i = 0; i < *count; i++) {
        if (!pool[i].in_use && pool[i].capacity >= required_size) {
            pool[i].in_use = true;
            return pool[i].buffer;
        }
    }

    // No suitable buffer found - create a new one
    // Round up to power of 2 for better reuse
    size_t capacity = 4096;  // Minimum 4KB
    while (capacity < required_size && capacity < MAX_BUFFER_SIZE) {
        capacity *= 2;
    }
    if (capacity < required_size) {
        capacity = required_size;  // For very large buffers
    }

    id<MTLBuffer> newBuffer = [device newBufferWithLength:capacity
                                                  options:MTLResourceStorageModeShared];
    if (!newBuffer) {
        return nil;
    }

    // Add to pool if there's room
    if (*count < BUFFER_POOL_SIZE) {
        pool[*count].buffer = newBuffer;
        pool[*count].capacity = capacity;
        pool[*count].in_use = true;
        (*count)++;
    }
    // If pool is full, just return the buffer (it won't be pooled)

    return newBuffer;
}

// Mark all buffers as available for reuse (call at frame start)
void pool_reset_frame(void) {
    for (int i = 0; i < g_buffer_pool.vertex_pool_count; i++) {
        g_buffer_pool.vertex_pool[i].in_use = false;
    }
    for (int i = 0; i < g_buffer_pool.index_pool_count; i++) {
        g_buffer_pool.index_pool[i].in_use = false;
    }
    // Reset text buffer pools
    for (int i = 0; i < g_buffer_pool.text_vertex_pool_count; i++) {
        g_buffer_pool.text_vertex_pool[i].in_use = false;
    }
    for (int i = 0; i < g_buffer_pool.text_index_pool_count; i++) {
        g_buffer_pool.text_index_pool[i].in_use = false;
    }
    // Reset wrapper pool (structs stay allocated, just reset usage counter)
    g_buffer_pool.wrapper_pool_used = 0;
}
