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

// FreeType library handle (global, initialized once)
static FT_Library g_ft_library = NULL;
static int g_ft_init_count = 0;

// Maximum texture atlas size
#define ATLAS_WIDTH 1024
#define ATLAS_HEIGHT 1024
#define MAX_GLYPHS 256

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

// Font structure
struct AfferentFont {
    FT_Face face;
    uint32_t size;
    float ascender;
    float descender;
    float line_height;

    // Glyph cache (simple direct-mapped for ASCII)
    GlyphInfo glyphs[MAX_GLYPHS];

    // Texture atlas for glyph bitmaps
    uint8_t* atlas_data;
    uint32_t atlas_width;
    uint32_t atlas_height;
    uint32_t atlas_cursor_x;
    uint32_t atlas_cursor_y;
    uint32_t atlas_row_height;

    // Metal texture handle (set by renderer)
    void* metal_texture;
};

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

    // Set character size (size in pixels, 72 DPI)
    error = FT_Set_Pixel_Sizes(font->face, 0, size);
    if (error) {
        FT_Done_Face(font->face);
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    font->size = size;

    // Calculate font metrics (convert from 26.6 fixed point)
    font->ascender = font->face->size->metrics.ascender / 64.0f;
    font->descender = font->face->size->metrics.descender / 64.0f;
    font->line_height = font->face->size->metrics.height / 64.0f;

    // Allocate texture atlas
    font->atlas_width = ATLAS_WIDTH;
    font->atlas_height = ATLAS_HEIGHT;
    font->atlas_data = calloc(ATLAS_WIDTH * ATLAS_HEIGHT, 1);
    if (!font->atlas_data) {
        FT_Done_Face(font->face);
        free(font);
        return AFFERENT_ERROR_FONT_FAILED;
    }

    font->atlas_cursor_x = 1;  // Start at 1 to avoid edge artifacts
    font->atlas_cursor_y = 1;
    font->atlas_row_height = 0;

    // Clear glyph cache
    memset(font->glyphs, 0, sizeof(font->glyphs));

    *out_font = font;
    return AFFERENT_OK;
}

// External function to release Metal texture (defined in metal_render.m)
extern void afferent_release_metal_texture(void* texture_ptr);

// Destroy a font
void afferent_font_destroy(AfferentFontRef font) {
    if (font) {
        if (font->face) {
            FT_Done_Face(font->face);
        }
        if (font->atlas_data) {
            free(font->atlas_data);
        }
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
    if (codepoint >= MAX_GLYPHS) {
        return NULL;  // Only support basic ASCII for now
    }

    GlyphInfo* glyph = &font->glyphs[codepoint];
    if (glyph->valid) {
        return glyph;  // Already cached
    }

    // Load glyph
    FT_Error error = FT_Load_Char(font->face, codepoint, FT_LOAD_RENDER);
    if (error) {
        return NULL;
    }

    FT_GlyphSlot slot = font->face->glyph;
    FT_Bitmap* bitmap = &slot->bitmap;

    // Check if we have room in atlas
    if (font->atlas_cursor_x + bitmap->width + 1 > font->atlas_width) {
        // Move to next row
        font->atlas_cursor_x = 1;
        font->atlas_cursor_y += font->atlas_row_height + 1;
        font->atlas_row_height = 0;
    }

    if (font->atlas_cursor_y + bitmap->rows + 1 > font->atlas_height) {
        // Atlas full - could implement atlas resizing here
        return NULL;
    }

    // Copy bitmap to atlas
    for (uint32_t y = 0; y < bitmap->rows; y++) {
        for (uint32_t x = 0; x < bitmap->width; x++) {
            uint32_t atlas_idx = (font->atlas_cursor_y + y) * font->atlas_width +
                                 (font->atlas_cursor_x + x);
            font->atlas_data[atlas_idx] = bitmap->buffer[y * bitmap->pitch + x];
        }
    }

    // Store glyph info
    glyph->codepoint = codepoint;
    glyph->advance_x = slot->advance.x / 64.0f;
    glyph->bearing_x = slot->bitmap_left;
    glyph->bearing_y = slot->bitmap_top;
    glyph->width = bitmap->width;
    glyph->height = bitmap->rows;
    glyph->atlas_x = font->atlas_cursor_x;
    glyph->atlas_y = font->atlas_cursor_y;
    glyph->valid = 1;

    // Update atlas cursor
    font->atlas_cursor_x += bitmap->width + 1;
    if (bitmap->rows > font->atlas_row_height) {
        font->atlas_row_height = bitmap->rows;
    }

    return glyph;
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
        uint32_t codepoint = (uint8_t)*p;  // Simple ASCII for now
        GlyphInfo* glyph = cache_glyph(font, codepoint);

        if (glyph) {
            total_width += glyph->advance_x;
        }
        p++;
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
    // Simple implementation - always return dirty after caching
    // A more sophisticated implementation would track dirty regions
    return 1;
}

// Generate vertex data for rendering text
// Vertex format: pos.x, pos.y, uv.x, uv.y, r, g, b, a (8 floats per vertex)
// Returns number of vertices generated
int afferent_text_generate_vertices(
    AfferentFontRef font,
    const char* text,
    float x,
    float y,
    float r, float g, float b, float a,
    float screen_width,
    float screen_height,
    float** out_vertices,
    uint32_t** out_indices,
    uint32_t* out_vertex_count,
    uint32_t* out_index_count
) {
    if (!font || !text || !out_vertices || !out_indices) {
        return 0;
    }

    size_t text_len = strlen(text);
    if (text_len == 0) {
        *out_vertices = NULL;
        *out_indices = NULL;
        *out_vertex_count = 0;
        *out_index_count = 0;
        return 1;
    }

    // Allocate max possible vertices (4 per character) and indices (6 per character)
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
        uint32_t codepoint = (uint8_t)*p;
        GlyphInfo* glyph = cache_glyph(font, codepoint);

        if (glyph && glyph->width > 0 && glyph->height > 0) {
            // Calculate quad corners in pixel coordinates
            float gx = cursor_x + glyph->bearing_x;
            float gy = cursor_y - glyph->bearing_y;  // FreeType Y is up, screen Y is down
            float gw = glyph->width;
            float gh = glyph->height;

            // Convert to NDC
            float x0 = (gx / screen_width) * 2.0f - 1.0f;
            float y0 = 1.0f - (gy / screen_height) * 2.0f;
            float x1 = ((gx + gw) / screen_width) * 2.0f - 1.0f;
            float y1 = 1.0f - ((gy + gh) / screen_height) * 2.0f;

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
            vertices[vi++] = x1; vertices[vi++] = y0;
            vertices[vi++] = u1; vertices[vi++] = v0;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            // Bottom-right
            vertices[vi++] = x1; vertices[vi++] = y1;
            vertices[vi++] = u1; vertices[vi++] = v1;
            vertices[vi++] = r; vertices[vi++] = g; vertices[vi++] = b; vertices[vi++] = a;

            // Bottom-left
            vertices[vi++] = x0; vertices[vi++] = y1;
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
        p++;
    }

    *out_vertices = vertices;
    *out_indices = indices;
    *out_vertex_count = vertex_count;
    *out_index_count = index_count;

    return 1;
}
