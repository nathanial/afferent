// shaders.m - Metal shader loading from external files
#import "shaders.h"
#import <Foundation/Foundation.h>

// Global shader source strings (loaded at runtime)
NSString *shaderSource = nil;
NSString *textShaderSource = nil;
NSString *instancedShaderSource = nil;
NSString *animatedShaderSource = nil;
NSString *orbitalShaderSource = nil;
NSString *dynamicCircleShaderSource = nil;
NSString *dynamicRectShaderSource = nil;
NSString *dynamicTriangleShaderSource = nil;
NSString *spriteShaderSource = nil;
NSString *shader3DSource = nil;
NSString *shader3DTexturedSource = nil;
NSString *texturedRectShaderSource = nil;

// Cached shader directory path
static NSString *g_shaderDir = nil;

// Find the shaders directory
// Searches in order:
// 1. AFFERENT_SHADER_DIR environment variable
// 2. ./shaders (relative to current working directory)
// 3. ../shaders (one level up)
// 4. Relative to executable: <exe_dir>/shaders, <exe_dir>/../shaders, etc.
static NSString* find_shader_directory(void) {
    NSFileManager *fm = [NSFileManager defaultManager];

    // Check environment variable first
    const char *envDir = getenv("AFFERENT_SHADER_DIR");
    if (envDir) {
        NSString *path = [NSString stringWithUTF8String:envDir];
        if ([fm fileExistsAtPath:path]) {
            return path;
        }
    }

    // Check relative to current working directory
    NSString *cwd = [fm currentDirectoryPath];
    NSArray *cwdPaths = @[
        [cwd stringByAppendingPathComponent:@"shaders"],
        [cwd stringByAppendingPathComponent:@"native/src/metal/shaders"],
    ];
    for (NSString *path in cwdPaths) {
        if ([fm fileExistsAtPath:path]) {
            return path;
        }
    }

    // Check relative to executable
    NSString *exePath = [[NSBundle mainBundle] executablePath];
    if (exePath) {
        NSString *exeDir = [exePath stringByDeletingLastPathComponent];
        NSArray *exePaths = @[
            [exeDir stringByAppendingPathComponent:@"shaders"],
            [[exeDir stringByDeletingLastPathComponent] stringByAppendingPathComponent:@"shaders"],
            [[[exeDir stringByDeletingLastPathComponent] stringByDeletingLastPathComponent] stringByAppendingPathComponent:@"native/src/metal/shaders"],
        ];
        for (NSString *path in exePaths) {
            if ([fm fileExistsAtPath:path]) {
                return path;
            }
        }
    }

    // Check if we're in a lake build directory structure
    // The executable might be at .lake/build/bin/afferent
    // and shaders at native/src/metal/shaders
    NSArray *lakePaths = @[
        @"native/src/metal/shaders",
        @"../native/src/metal/shaders",
        @"../../native/src/metal/shaders",
        @"../../../native/src/metal/shaders",
    ];
    for (NSString *relPath in lakePaths) {
        NSString *path = [cwd stringByAppendingPathComponent:relPath];
        path = [path stringByStandardizingPath];
        if ([fm fileExistsAtPath:path]) {
            return path;
        }
    }

    return nil;
}

NSString* afferent_load_shader(const char* name) {
    if (!g_shaderDir) {
        g_shaderDir = find_shader_directory();
        if (!g_shaderDir) {
            NSLog(@"Error: Could not find shaders directory");
            NSLog(@"Set AFFERENT_SHADER_DIR environment variable or run from project root");
            return nil;
        }
        NSLog(@"Using shader directory: %@", g_shaderDir);
    }

    NSString *filename = [NSString stringWithFormat:@"%s.metal", name];
    NSString *path = [g_shaderDir stringByAppendingPathComponent:filename];

    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:path
                                                  encoding:NSUTF8StringEncoding
                                                     error:&error];
    if (!content) {
        NSLog(@"Error loading shader '%s' from %@: %@", name, path, error);
        return nil;
    }

    return content;
}

BOOL afferent_init_shaders(void) {
    // Load all shaders
    shaderSource = afferent_load_shader("basic");
    if (!shaderSource) return NO;

    textShaderSource = afferent_load_shader("text");
    if (!textShaderSource) return NO;

    instancedShaderSource = afferent_load_shader("instanced");
    if (!instancedShaderSource) return NO;

    animatedShaderSource = afferent_load_shader("animated");
    if (!animatedShaderSource) return NO;

    orbitalShaderSource = afferent_load_shader("orbital");
    if (!orbitalShaderSource) return NO;

    dynamicCircleShaderSource = afferent_load_shader("dynamic_circle");
    if (!dynamicCircleShaderSource) return NO;

    dynamicRectShaderSource = afferent_load_shader("dynamic_rect");
    if (!dynamicRectShaderSource) return NO;

    dynamicTriangleShaderSource = afferent_load_shader("dynamic_triangle");
    if (!dynamicTriangleShaderSource) return NO;

    spriteShaderSource = afferent_load_shader("sprite");
    if (!spriteShaderSource) return NO;

    shader3DSource = afferent_load_shader("mesh3d");
    if (!shader3DSource) return NO;

    shader3DTexturedSource = afferent_load_shader("mesh3d_textured");
    if (!shader3DTexturedSource) return NO;

    texturedRectShaderSource = afferent_load_shader("textured_rect");
    if (!texturedRectShaderSource) return NO;

    NSLog(@"All shaders loaded successfully");
    return YES;
}
