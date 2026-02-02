/*
 * Afferent Text Rendering
 * FreeType integration for font loading and glyph rasterization.
 */

#include <ft2build.h>
#include FT_FREETYPE_H

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "afferent.h"

// External function to release Metal texture (defined in metal_render.m)
extern void afferent_release_metal_texture(void* texture_ptr);

// FreeType library handle (global, initialized once)
static FT_Library g_ft_library = NULL;
static int g_ft_init_count = 0;

// Texture atlas sizing
#define ATLAS_INITIAL_WIDTH 1024
#define ATLAS_INITIAL_HEIGHT 1024
#define ATLAS_MAX_DIM 4096

// Glyph cache sizing
#define GLYPH_TABLE_INITIAL_CAPACITY 1024
#define GLYPH_TABLE_MAX_LOAD_NUM 7
#define GLYPH_TABLE_MAX_LOAD_DEN 10

// Range used for conservative metric scan
#define ASCII_SCAN_START 32
#define ASCII_SCAN_END 256

// Glyph cache entry
typedef struct {
    uint32_t codepoint;
    float advance_x;      // How far to move cursor after this glyph
    float bearing_x;      // Horizontal offset from cursor to glyph
    float bearing_y;      // Vertical offset from baseline to top of glyph
    uint16_t width;       // Glyph bitmap width
    uint16_t height;      // Glyph bitmap height
    uint16_t atlas_x;     // Position in texture atlas
    uint16_t atlas_y;
    uint8_t valid;        // Whether this glyph is cached
} GlyphInfo;

typedef struct {
    GlyphInfo* entries;
    uint32_t capacity;
    uint32_t count;
} GlyphTable;

// Font structure
struct AfferentFont {
    FT_Face face;
    uint32_t size;
    float ascender;
    float descender;
    float line_height;

    // Glyph cache (dynamic hash table)
    GlyphTable glyphs;

    // Texture atlas for glyph bitmaps
    uint8_t* atlas_data;
    uint32_t atlas_width;
    uint32_t atlas_height;
    uint32_t atlas_cursor_x;
    uint32_t atlas_cursor_y;
    uint32_t atlas_row_height;

    // Dirty tracking - only upload when new glyphs are added
    int atlas_dirty;

    // Metal texture handle (set by renderer)
    void* metal_texture;
};

static uint32_t next_pow2(uint32_t v) {
    if (v < 2) return 2;
    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    return v + 1;
}

static uint32_t glyph_hash(uint32_t codepoint) {
    // Knuth multiplicative hash
    return codepoint * 2654435761u;
}

static int glyph_table_init(GlyphTable* table, uint32_t capacity) {
    uint32_t cap = next_pow2(capacity);
    table->entries = calloc(cap, sizeof(GlyphInfo));
    if (!table->entries) {
        table->capacity = 0;
        table->count = 0;
        return 0;
    }
    table->capacity = cap;
    table->count = 0;
    return 1;
}

static void glyph_table_destroy(GlyphTable* table) {
    if (table->entries) {
        free(table->entries);
    }
    table->entries = NULL;
    table->capacity = 0;
    table->count = 0;
}

static int glyph_table_rehash(GlyphTable* table, uint32_t new_capacity) {
    GlyphInfo* old_entries = table->entries;
    uint32_t old_capacity = table->capacity;

    uint32_t cap = next_pow2(new_capacity);
    GlyphInfo* new_entries = calloc(cap, sizeof(GlyphInfo));
    if (!new_entries) {
        return 0;
    }

    table->entries = new_entries;
    table->capacity = cap;
    table->count = 0;

    if (old_entries) {
        uint32_t mask = cap - 1;
        for (uint32_t i = 0; i < old_capacity; i++) {
            GlyphInfo* entry = &old_entries[i];
            if (!entry->valid) continue;
            uint32_t idx = glyph_hash(entry->codepoint) & mask;
            while (new_entries[idx].valid) {
                idx = (idx + 1) & mask;
            }
            new_entries[idx] = *entry;
            table->count++;
        }
        free(old_entries);
    }

    return 1;
}

static GlyphInfo* glyph_table_find(GlyphTable* table, uint32_t codepoint) {
    if (!table->entries || table->capacity == 0) {
        return NULL;
    }
    uint32_t mask = table->capacity - 1;
    uint32_t idx = glyph_hash(codepoint) & mask;
    for (uint32_t i = 0; i < table->capacity; i++) {
        GlyphInfo* entry = &table->entries[idx];
        if (!entry->valid) {
            return NULL;
        }
        if (entry->codepoint == codepoint) {
            return entry;
        }
        idx = (idx + 1) & mask;
    }
    return NULL;
}

static GlyphInfo* glyph_table_find_slot(GlyphTable* table, uint32_t codepoint, int* existed) {
    if (!table->entries || table->capacity == 0) {
        return NULL;
    }

    uint32_t threshold = (table->capacity * GLYPH_TABLE_MAX_LOAD_NUM) / GLYPH_TABLE_MAX_LOAD_DEN;
    if (table->count + 1 > threshold) {
        if (!glyph_table_rehash(table, table->capacity * 2)) {
            return NULL;
        }
    }

    uint32_t mask = table->capacity - 1;
    uint32_t idx = glyph_hash(codepoint) & mask;
    for (uint32_t i = 0; i < table->capacity; i++) {
        GlyphInfo* entry = &table->entries[idx];
        if (!entry->valid) {
            if (existed) *existed = 0;
            return entry;
        }
        if (entry->codepoint == codepoint) {
            if (existed) *existed = 1;
            return entry;
        }
        idx = (idx + 1) & mask;
    }
    return NULL;
}

static uint32_t utf8_next(const char** p) {
    const unsigned char* s = (const unsigned char*)*p;
    unsigned char c = s[0];
    if (c == 0) {
        return 0;
    }
    if (c < 0x80) {
        (*p)++;
        return c;
    }
    if ((c >> 5) == 0x6) {
        if ((s[1] & 0xC0) != 0x80) {
            (*p)++;
            return 0xFFFD;
        }
        uint32_t cp = ((uint32_t)(c & 0x1F) << 6) | (uint32_t)(s[1] & 0x3F);
        if (cp < 0x80) {
            (*p)++;
            return 0xFFFD;
        }
        (*p) += 2;
        return cp;
    }
    if ((c >> 4) == 0xE) {
        if ((s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80) {
            (*p)++;
            return 0xFFFD;
        }
        uint32_t cp = ((uint32_t)(c & 0x0F) << 12) |
                      ((uint32_t)(s[1] & 0x3F) << 6) |
                      (uint32_t)(s[2] & 0x3F);
        if (cp < 0x800 || (cp >= 0xD800 && cp <= 0xDFFF)) {
            (*p)++;
            return 0xFFFD;
        }
        (*p) += 3;
        return cp;
    }
    if ((c >> 3) == 0x1E) {
        if ((s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80 || (s[3] & 0xC0) != 0x80) {
            (*p)++;
            return 0xFFFD;
        }
        uint32_t cp = ((uint32_t)(c & 0x07) << 18) |
                      ((uint32_t)(s[1] & 0x3F) << 12) |
                      ((uint32_t)(s[2] & 0x3F) << 6) |
                      (uint32_t)(s[3] & 0x3F);
        if (cp < 0x10000 || cp > 0x10FFFF) {
            (*p)++;
            return 0xFFFD;
        }
        (*p) += 4;
        return cp;
    }
    (*p)++;
    return 0xFFFD;
}

static int ensure_atlas_capacity(AfferentFontRef font, uint32_t glyph_w, uint32_t glyph_h) {
    if (!font) {
        return 0;
    }

    uint32_t needed_w = font->atlas_cursor_x + glyph_w + 1;
    uint32_t needed_h = font->atlas_cursor_y + glyph_h + 1;

    if (needed_w <= font->atlas_width && needed_h <= font->atlas_height) {
        return 1;
    }

    uint32_t new_w = font->atlas_width;
    uint32_t new_h = font->atlas_height;

    while (new_w < needed_w || new_h < needed_h) {
        if (new_w < needed_w) new_w *= 2;
        if (new_h < needed_h) new_h *= 2;
        if (new_w > ATLAS_MAX_DIM) new_w = ATLAS_MAX_DIM;
        if (new_h > ATLAS_MAX_DIM) new_h = ATLAS_MAX_DIM;
        if (new_w < needed_w || new_h < needed_h) {
            return 0;
        }
    }

    if (new_w == font->atlas_width && new_h == font->atlas_height) {
        return 1;
    }

    uint8_t* new_data = calloc((size_t)new_w * (size_t)new_h, 1);
    if (!new_data) {
        return 0;
    }

    for (uint32_t y = 0; y < font->atlas_height; y++) {
        memcpy(new_data + (size_t)y * new_w,
               font->atlas_data + (size_t)y * font->atlas_width,
               font->atlas_width);
    }

    free(font->atlas_data);
    font->atlas_data = new_data;
    font->atlas_width = new_w;
    font->atlas_height = new_h;

    if (font->metal_texture) {
        afferent_release_metal_texture(font->metal_texture);
        font->metal_texture = NULL;
    }

    font->atlas_dirty = 1;
    return 1;
}

// Initialize FreeType
AfferentResult afferent_text_init(void) {
    if (g_ft_init_count > 0) {
        g_ft_init_count++;
        return AFFERENT_OK;
    }

    FT_Error error = FT_Init_FreeType(&g_ft_library);
    if (error) {
        return AFFERENT_ERROR_FONT_FAILED;
    }

    g_ft_init_count = 1;
    return AFFERENT_OK;
}

// Shutdown FreeType
void afferent_text_shutdown(void) {
    if (g_ft_init_count > 0) {
        g_ft_init_count--;
        if (g_ft_init_count == 0 && g_ft_library) {
            FT_Done_FreeType(g_ft_library);
            g_ft_library = NULL;
        }
    }
}

// Load a font from file
AfferentResult afferent_font_load(
    const char* path,
    uint32_t size,
    AfferentFontRef* out_font
) {
    if (!g_ft_library) {
        // Auto-initialize if not done
        AfferentResult init_result = afferent_text_init();
        if (init_result != AFFERENT_OK) {
            return init_result;
        }
    }

    struct AfferentFont* font = calloc(1, sizeof(struct AfferentFont));
    if (!font) {
        return AFFERENT_ERROR_FONT_FAILED;
    }

    // Load face from file
    FT_Error error = FT_New_Face(g_ft_library, path, 0, &font->face);
    if (error) {
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    (void)FT_Select_Charmap(font->face, FT_ENCODING_UNICODE);

    // Set character size (size in pixels, 72 DPI)
    error = FT_Set_Pixel_Sizes(font->face, 0, size);
    if (error) {
        FT_Done_Face(font->face);
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    font->size = size;

    // Calculate font metrics.
    //
    // FreeType face metrics are often slightly optimistic once hinting/rasterization is applied,
    // which can make layout under-allocate vertical space and cause text overlap. Since we
    // compute conservative ascent/descent from the rasterized glyph bitmaps for a basic
    // ASCII range, and use it to derive a safe line height.
    float ft_asc = font->face->size->metrics.ascender / 64.0f;
    float ft_desc = font->face->size->metrics.descender / 64.0f;
    float ft_line = font->face->size->metrics.height / 64.0f;

    float max_ascent = 0.0f;
    float max_descent = 0.0f;
    for (uint32_t cp = ASCII_SCAN_START; cp < ASCII_SCAN_END; cp++) {
        FT_Error e = FT_Load_Char(font->face, cp, FT_LOAD_RENDER);
        if (e) continue;
        FT_GlyphSlot slot = font->face->glyph;
        float ascent = (float)slot->bitmap_top;  // baseline -> top
        float descent = (float)slot->bitmap.rows - (float)slot->bitmap_top; // baseline -> bottom
        if (ascent > max_ascent) max_ascent = ascent;
        if (descent > max_descent) max_descent = descent;
    }

    float bitmap_line = max_ascent + max_descent;
    if (bitmap_line <= 0.0f) {
        // Fallback to FreeType metrics if raster scan failed.
        font->ascender = ft_asc;
        font->descender = ft_desc;
        font->line_height = ft_line;
    } else {
        font->ascender = max_ascent;
        font->descender = -max_descent;
        // Keep FreeType's line gap if it is larger than bitmap extents.
        font->line_height = (ft_line > bitmap_line) ? ft_line : bitmap_line;
    }

    // Allocate texture atlas
    font->atlas_width = ATLAS_INITIAL_WIDTH;
    font->atlas_height = ATLAS_INITIAL_HEIGHT;
    font->atlas_data = calloc((size_t)ATLAS_INITIAL_WIDTH * (size_t)ATLAS_INITIAL_HEIGHT, 1);
    if (!font->atlas_data) {
        FT_Done_Face(font->face);
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    font->atlas_cursor_x = 1;  // Start at 1 to avoid edge artifacts
    font->atlas_cursor_y = 1;
    font->atlas_row_height = 0;

    // Initialize glyph cache
    if (!glyph_table_init(&font->glyphs, GLYPH_TABLE_INITIAL_CAPACITY)) {
        free(font->atlas_data);
        FT_Done_Face(font->face);
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    *out_font = font;
    return AFFERENT_OK;
}

// Destroy a font
void afferent_font_destroy(AfferentFontRef font) {
    if (font) {
        if (font->face) {
            FT_Done_Face(font->face);
        }
        if (font->atlas_data) {
            free(font->atlas_data);
        }
        glyph_table_destroy(&font->glyphs);
        // Release the Metal texture if one was created
        if (font->metal_texture) {
            afferent_release_metal_texture(font->metal_texture);
        }
        free(font);
    }
}

// Get font metrics
void afferent_font_get_metrics(
    AfferentFontRef font,
    float* ascender,
    float* descender,
    float* line_height
) {
    if (font) {
        if (ascender) *ascender = font->ascender;
        if (descender) *descender = font->descender;
        if (line_height) *line_height = font->line_height;
    }
}

// Cache a glyph (rasterize and add to atlas)
static GlyphInfo* cache_glyph(AfferentFontRef font, uint32_t codepoint) {
    GlyphInfo* glyph = glyph_table_find(&font->glyphs, codepoint);
    if (glyph) {
        return glyph;  // Already cached
    }

    int existed = 0;
    GlyphInfo* glyph_info = glyph_table_find_slot(&font->glyphs, codepoint, &existed);
    if (!glyph_info) {
        return NULL;
    }
    if (existed) {
        return glyph_info;
    }

    // Load glyph
    FT_Error error = FT_Load_Char(font->face, codepoint, FT_LOAD_RENDER);
    if (error) {
        return NULL;
    }

    FT_GlyphSlot ft_slot = font->face->glyph;
    FT_Bitmap* bitmap = &ft_slot->bitmap;

    // Check if we have room in atlas
    if (font->atlas_cursor_x + bitmap->width + 1 > font->atlas_width) {
        // Move to next row
        font->atlas_cursor_x = 1;
        font->atlas_cursor_y += font->atlas_row_height + 1;
        font->atlas_row_height = 0;
    }

    if (!ensure_atlas_capacity(font, bitmap->width, bitmap->rows)) {
        return NULL;
    }

    // Copy bitmap to atlas (handle mono and grayscale pixel modes)
    int pitch = bitmap->pitch;
    for (uint32_t y = 0; y < bitmap->rows; y++) {
        const uint8_t* row = bitmap->buffer +
            (pitch >= 0 ? (int)y * pitch : (int)(bitmap->rows - 1 - y) * -pitch);
        for (uint32_t x = 0; x < bitmap->width; x++) {
            uint8_t value = 0;
            switch (bitmap->pixel_mode) {
                case FT_PIXEL_MODE_GRAY:
                    value = row[x];
                    break;
                case FT_PIXEL_MODE_MONO: {
                    uint8_t byte = row[x >> 3];
                    uint8_t mask = (uint8_t)(0x80 >> (x & 7));
                    value = (byte & mask) ? 0xFF : 0x00;
                    break;
                }
                case FT_PIXEL_MODE_GRAY2: {
                    uint8_t byte = row[x >> 2];
                    uint8_t shift = (uint8_t)(6 - 2 * (x & 3));
                    uint8_t level = (byte >> shift) & 0x3;
                    value = (uint8_t)(level * 85);  // 255 / 3
                    break;
                }
                case FT_PIXEL_MODE_GRAY4: {
                    uint8_t byte = row[x >> 1];
                    uint8_t shift = (uint8_t)((x & 1) ? 0 : 4);
                    uint8_t level = (byte >> shift) & 0xF;
                    value = (uint8_t)(level * 17);  // 255 / 15
                    break;
                }
                case FT_PIXEL_MODE_BGRA:
                    value = row[x * 4 + 3];
                    break;
                default:
                    value = 0;
                    break;
            }
            uint32_t atlas_idx = (font->atlas_cursor_y + y) * font->atlas_width +
                                 (font->atlas_cursor_x + x);
            font->atlas_data[atlas_idx] = value;
        }
    }

    // Store glyph info
    glyph_info->codepoint = codepoint;
    glyph_info->advance_x = ft_slot->advance.x / 64.0f;
    glyph_info->bearing_x = ft_slot->bitmap_left;
    glyph_info->bearing_y = ft_slot->bitmap_top;
    glyph_info->width = bitmap->width;
    glyph_info->height = bitmap->rows;
    glyph_info->atlas_x = font->atlas_cursor_x;
    glyph_info->atlas_y = font->atlas_cursor_y;
    glyph_info->valid = 1;
    font->glyphs.count++;

    // Mark atlas as dirty - needs upload to GPU
    font->atlas_dirty = 1;

    // Update atlas cursor
    font->atlas_cursor_x += bitmap->width + 1;
    if (bitmap->rows > font->atlas_row_height) {
        font->atlas_row_height = bitmap->rows;
    }

    return glyph_info;
}

// Measure text dimensions
void afferent_text_measure(
    AfferentFontRef font,
    const char* text,
    float* width,
    float* height
) {
    if (!font || !text) {
        if (width) *width = 0;
        if (height) *height = 0;
        return;
    }

    float total_width = 0;
    float max_height = font->line_height;

    const char* p = text;
    while (*p) {
        uint32_t codepoint = utf8_next(&p);
        if (codepoint == 0) break;
        GlyphInfo* glyph = cache_glyph(font, codepoint);

        if (glyph) {
            total_width += glyph->advance_x;
        }
    }

    if (width) *width = total_width;
    if (height) *height = max_height;
}

// Get atlas data for creating Metal texture
uint8_t* afferent_font_get_atlas_data(AfferentFontRef font) {
    return font ? font->atlas_data : NULL;
}

uint32_t afferent_font_get_atlas_width(AfferentFontRef font) {
    return font ? font->atlas_width : 0;
}

uint32_t afferent_font_get_atlas_height(AfferentFontRef font) {
    return font ? font->atlas_height : 0;
}

// Set the Metal texture handle (called by renderer after texture creation)
void afferent_font_set_metal_texture(AfferentFontRef font, void* texture) {
    if (font) {
        font->metal_texture = texture;
    }
}

void* afferent_font_get_metal_texture(AfferentFontRef font) {
    return font ? font->metal_texture : NULL;
}

// Check if atlas needs updating (new glyphs were added)
int afferent_font_atlas_dirty(AfferentFontRef font) {
    return font ? font->atlas_dirty : 0;
}

// Clear the dirty flag after uploading atlas to GPU
void afferent_font_atlas_clear_dirty(AfferentFontRef font) {
    if (font) {
        font->atlas_dirty = 0;
    }
}

// Helper to apply 2D affine transform to a point
// Transform is [a, b, c, d, tx, ty] where: x' = a*x + c*y + tx, y' = b*x + d*y + ty
static inline void apply_transform(float px, float py, const float* t,
                                   float* out_x, float* out_y) {
    *out_x = t[0] * px + t[2] * py + t[4];
    *out_y = t[1] * px + t[3] * py + t[5];
}

// Generate vertex data for rendering text with transform support
// Vertex format: pos.x, pos.y, uv.x, uv.y, r, g, b, a (8 floats per vertex)
// Transform is [a, b, c, d, tx, ty] (6 floats), or NULL for identity
// Returns number of vertices generated
int afferent_text_generate_vertices(
    AfferentFontRef font,
    const char* text,
    float x,
    float y,
    float r, float g, float b, float a,
    float screen_width,
    float screen_height,
    const float* transform,
    float** out_vertices,
    uint32_t** out_indices,
    uint32_t* out_vertex_count,
    uint32_t* out_index_count
) {
    if (!font || !text || !out_vertices || !out_indices) {
        return 0;
    }

    // Default to identity transform if none provided
    float identity[6] = {1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f};
    if (!transform) {
        transform = identity;
    }

    size_t text_len = strlen(text);
    if (text_len == 0) {
        *out_vertices = NULL;
        *out_indices = NULL;
        *out_vertex_count = 0;
        *out_index_count = 0;
        return 1;
    }

    // Allocate max possible vertices (4 per byte) and indices (6 per byte)
    float* vertices = malloc(text_len * 4 * 8 * sizeof(float));
    uint32_t* indices = malloc(text_len * 6 * sizeof(uint32_t));

    if (!vertices || !indices) {
        free(vertices);
        free(indices);
        return 0;
    }

    float cursor_x = x;
    float cursor_y = y;
    uint32_t vertex_count = 0;
    uint32_t index_count = 0;

    const char* p = text;
    while (*p) {
        uint32_t codepoint = utf8_next(&p);
        if (codepoint == 0) break;
        GlyphInfo* glyph = cache_glyph(font, codepoint);

        if (glyph && glyph->width > 0 && glyph->height > 0) {
            // Calculate quad corners in pixel coordinates (pre-transform)
            float gx = cursor_x + glyph->bearing_x;
            float gy = cursor_y - glyph->bearing_y;  // FreeType Y is up, screen Y is down
            float gw = glyph->width;
            float gh = glyph->height;

            // The 4 corners in pixel space (before transform)
            float px0 = gx, py0 = gy;           // Top-left
            float px1 = gx + gw, py1 = gy;      // Top-right
            float px2 = gx + gw, py2 = gy + gh; // Bottom-right
            float px3 = gx, py3 = gy + gh;      // Bottom-left

            // Apply transform to get final pixel positions
            float tx0, ty0, tx1, ty1, tx2, ty2, tx3, ty3;
            apply_transform(px0, py0, transform, &tx0, &ty0);
            apply_transform(px1, py1, transform, &tx1, &ty1);
            apply_transform(px2, py2, transform, &tx2, &ty2);
            apply_transform(px3, py3, transform, &tx3, &ty3);

            // Convert transformed positions to NDC
            float x0 = (tx0 / screen_width) * 2.0f - 1.0f;
            float y0 = 1.0f - (ty0 / screen_height) * 2.0f;
            float x1 = (tx1 / screen_width) * 2.0f - 1.0f;
            float y1_ndc = 1.0f - (ty1 / screen_height) * 2.0f;
            float x2 = (tx2 / screen_width) * 2.0f - 1.0f;
            float y2 = 1.0f - (ty2 / screen_height) * 2.0f;
            float x3 = (tx3 / screen_width) * 2.0f - 1.0f;
            float y3 = 1.0f - (ty3 / screen_height) * 2.0f;

            // UV coordinates in atlas (unchanged by transform)
            float u0 = (float)glyph->atlas_x / font->atlas_width;
            float v0 = (float)glyph->atlas_y / font->atlas_height;
            float u1 = (float)(glyph->atlas_x + glyph->width) / font->atlas_width;
            float v1 = (float)(glyph->atlas_y + glyph->height) / font->atlas_height;

            // Add 4 vertices for this glyph's quad
            uint32_t base_vertex = vertex_count;
            uint32_t vi = vertex_count * 8;

            // Top-left
            vertices[vi++] = x0; vertices[vi++] = y0;
            vertices[vi++] = u0; vertices[vi++] = v0;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            // Top-right
            vertices[vi++] = x1; vertices[vi++] = y1_ndc;
            vertices[vi++] = u1; vertices[vi++] = v0;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            // Bottom-right
            vertices[vi++] = x2; vertices[vi++] = y2;
            vertices[vi++] = u1; vertices[vi++] = v1;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            // Bottom-left
            vertices[vi++] = x3; vertices[vi++] = y3;
            vertices[vi++] = u0; vertices[vi++] = v1;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            vertex_count += 4;

            // Add 6 indices for two triangles
            indices[index_count++] = base_vertex + 0;
            indices[index_count++] = base_vertex + 1;
            indices[index_count++] = base_vertex + 2;
            indices[index_count++] = base_vertex + 0;
            indices[index_count++] = base_vertex + 2;
            indices[index_count++] = base_vertex + 3;
        }

        if (glyph) {
            cursor_x += glyph->advance_x;
        }
    }

    *out_vertices = vertices;
    *out_indices = indices;
    *out_vertex_count = vertex_count;
    *out_index_count = index_count;

    return 1;
}

// Generate vertices for multiple text strings into one buffer
// Each string has its own position, color, and transform
int afferent_text_generate_vertices_batch(
    AfferentFontRef font,
    const char** texts,           // Array of strings
    const float* positions,       // [x0, y0, x1, y1, ...]
    const float* colors,          // [r0, g0, b0, a0, ...]
    const float* transforms,      // [a0, b0, c0, d0, tx0, ty0, ...] (6 per entry)
    uint32_t count,
    float screen_width,
    float screen_height,
    float** out_vertices,
    uint32_t** out_indices,
    uint32_t* out_vertex_count,
    uint32_t* out_index_count
) {
    if (!font || !texts || count == 0 || !out_vertices || !out_indices) {
        if (out_vertices) *out_vertices = NULL;
        if (out_indices) *out_indices = NULL;
        if (out_vertex_count) *out_vertex_count = 0;
        if (out_index_count) *out_index_count = 0;
        return count == 0 ? 1 : 0;
    }

    // First pass: count total bytes to allocate buffers
    size_t total_chars = 0;
    for (uint32_t i = 0; i < count; i++) {
        if (texts[i]) {
            total_chars += strlen(texts[i]);
        }
    }

    if (total_chars == 0) {
        *out_vertices = NULL;
        *out_indices = NULL;
        *out_vertex_count = 0;
        *out_index_count = 0;
        return 1;
    }

    // Allocate max possible vertices (4 per byte) and indices (6 per byte)
    float* vertices = malloc(total_chars * 4 * 8 * sizeof(float));
    uint32_t* indices = malloc(total_chars * 6 * sizeof(uint32_t));

    if (!vertices || !indices) {
        free(vertices);
        free(indices);
        return 0;
    }

    uint32_t vertex_count = 0;
    uint32_t index_count = 0;

    // Default identity transform
    float identity[6] = {1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f};

    // Process each text string
    for (uint32_t text_idx = 0; text_idx < count; text_idx++) {
        const char* text = texts[text_idx];
        if (!text || !*text) continue;

        float x = positions ? positions[text_idx * 2] : 0.0f;
        float y = positions ? positions[text_idx * 2 + 1] : 0.0f;
        float r = colors ? colors[text_idx * 4] : 1.0f;
        float g = colors ? colors[text_idx * 4 + 1] : 1.0f;
        float b = colors ? colors[text_idx * 4 + 2] : 1.0f;
        float a = colors ? colors[text_idx * 4 + 3] : 1.0f;
        const float* transform = transforms ? &transforms[text_idx * 6] : identity;

        float cursor_x = x;
        float cursor_y = y;

        const char* p = text;
        while (*p) {
            uint32_t codepoint = utf8_next(&p);
            if (codepoint == 0) break;
            GlyphInfo* glyph = cache_glyph(font, codepoint);

            if (glyph && glyph->width > 0 && glyph->height > 0) {
                // Calculate quad corners in pixel coordinates (pre-transform)
                float gx = cursor_x + glyph->bearing_x;
                float gy = cursor_y - glyph->bearing_y;
                float gw = glyph->width;
                float gh = glyph->height;

                // The 4 corners in pixel space (before transform)
                float px0 = gx, py0 = gy;
                float px1 = gx + gw, py1 = gy;
                float px2 = gx + gw, py2 = gy + gh;
                float px3 = gx, py3 = gy + gh;

                // Apply transform to get final pixel positions
                float tx0, ty0, tx1, ty1, tx2, ty2, tx3, ty3;
                apply_transform(px0, py0, transform, &tx0, &ty0);
                apply_transform(px1, py1, transform, &tx1, &ty1);
                apply_transform(px2, py2, transform, &tx2, &ty2);
                apply_transform(px3, py3, transform, &tx3, &ty3);

                // Convert transformed positions to NDC
                float x0 = (tx0 / screen_width) * 2.0f - 1.0f;
                float y0 = 1.0f - (ty0 / screen_height) * 2.0f;
                float x1 = (tx1 / screen_width) * 2.0f - 1.0f;
                float y1_ndc = 1.0f - (ty1 / screen_height) * 2.0f;
                float x2 = (tx2 / screen_width) * 2.0f - 1.0f;
                float y2 = 1.0f - (ty2 / screen_height) * 2.0f;
                float x3 = (tx3 / screen_width) * 2.0f - 1.0f;
                float y3 = 1.0f - (ty3 / screen_height) * 2.0f;

                // UV coordinates in atlas
                float u0 = (float)glyph->atlas_x / font->atlas_width;
                float v0 = (float)glyph->atlas_y / font->atlas_height;
                float u1 = (float)(glyph->atlas_x + glyph->width) / font->atlas_width;
                float v1 = (float)(glyph->atlas_y + glyph->height) / font->atlas_height;

                // Add 4 vertices for this glyph's quad
                uint32_t base_vertex = vertex_count;
                uint32_t vi = vertex_count * 8;

                // Top-left
                vertices[vi++] = x0; vertices[vi++] = y0;
                vertices[vi++] = u0; vertices[vi++] = v0;
                vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

                // Top-right
                vertices[vi++] = x1; vertices[vi++] = y1_ndc;
                vertices[vi++] = u1; vertices[vi++] = v0;
                vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

                // Bottom-right
                vertices[vi++] = x2; vertices[vi++] = y2;
                vertices[vi++] = u1; vertices[vi++] = v1;
                vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

                // Bottom-left
                vertices[vi++] = x3; vertices[vi++] = y3;
                vertices[vi++] = u0; vertices[vi++] = v1;
                vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

                vertex_count += 4;

                // Add 6 indices for two triangles
                indices[index_count++] = base_vertex + 0;
                indices[index_count++] = base_vertex + 1;
                indices[index_count++] = base_vertex + 2;
                indices[index_count++] = base_vertex + 0;
                indices[index_count++] = base_vertex + 2;
                indices[index_count++] = base_vertex + 3;
            }

            if (glyph) {
                cursor_x += glyph->advance_x;
            }
        }
    }

    *out_vertices = vertices;
    *out_indices = indices;
    *out_vertex_count = vertex_count;
    *out_index_count = index_count;

    return 1;
}
