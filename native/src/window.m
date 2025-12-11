#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"

// Forward declarations
@class AfferentView;
@class AfferentWindowDelegate;

// Metal-backed view
@interface AfferentView : NSView
@property (nonatomic, strong) CAMetalLayer *metalLayer;
@property (nonatomic, strong) id<MTLDevice> device;
@end

@implementation AfferentView

- (instancetype)initWithFrame:(NSRect)frameRect device:(id<MTLDevice>)device {
    self = [super initWithFrame:frameRect];
    if (self) {
        self.device = device;
        self.wantsLayer = YES;

        self.metalLayer = [CAMetalLayer layer];
        self.metalLayer.device = device;
        self.metalLayer.pixelFormat = MTLPixelFormatBGRA8Unorm;
        self.metalLayer.framebufferOnly = YES;
        self.metalLayer.frame = self.bounds;
        // Enable vsync for smooth animation
        self.metalLayer.displaySyncEnabled = YES;
        // Triple buffering for smoother frame pacing
        self.metalLayer.maximumDrawableCount = 3;
        self.layer = self.metalLayer;
    }
    return self;
}

- (void)setFrameSize:(NSSize)newSize {
    [super setFrameSize:newSize];
    CGFloat scale = self.window.backingScaleFactor;
    self.metalLayer.drawableSize = CGSizeMake(newSize.width * scale, newSize.height * scale);
}

- (void)viewDidChangeBackingProperties {
    [super viewDidChangeBackingProperties];
    CGFloat scale = self.window.backingScaleFactor;
    NSSize size = self.bounds.size;
    self.metalLayer.drawableSize = CGSizeMake(size.width * scale, size.height * scale);
}

- (BOOL)acceptsFirstResponder {
    return YES;
}

@end

// Window delegate to track close requests
@interface AfferentWindowDelegate : NSObject <NSWindowDelegate>
@property (nonatomic, assign) bool shouldClose;
@end

@implementation AfferentWindowDelegate

- (instancetype)init {
    self = [super init];
    if (self) {
        _shouldClose = NO;
    }
    return self;
}

- (BOOL)windowShouldClose:(NSWindow *)sender {
    self.shouldClose = YES;
    return NO;  // Don't actually close, let Lean handle it
}

@end

// Internal window structure
struct AfferentWindow {
    NSWindow *nsWindow;
    AfferentView *view;
    AfferentWindowDelegate *delegate;
    id<MTLDevice> device;
};

AfferentResult afferent_window_create(
    uint32_t width,
    uint32_t height,
    const char* title,
    AfferentWindowRef* out_window
) {
    @autoreleasepool {
        // Initialize NSApplication if needed
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

        // Create menu bar (required for proper window behavior)
        if ([NSApp mainMenu] == nil) {
            NSMenu *menuBar = [[NSMenu alloc] init];
            NSMenuItem *appMenuItem = [[NSMenuItem alloc] init];
            [menuBar addItem:appMenuItem];
            [NSApp setMainMenu:menuBar];

            NSMenu *appMenu = [[NSMenu alloc] init];
            NSMenuItem *quitItem = [[NSMenuItem alloc] initWithTitle:@"Quit"
                                                              action:@selector(terminate:)
                                                       keyEquivalent:@"q"];
            [appMenu addItem:quitItem];
            [appMenuItem setSubmenu:appMenu];
        }

        // Get Metal device
        id<MTLDevice> device = MTLCreateSystemDefaultDevice();
        if (!device) {
            NSLog(@"Failed to create Metal device");
            return AFFERENT_ERROR_DEVICE_FAILED;
        }

        // Create window
        NSRect contentRect = NSMakeRect(0, 0, width, height);
        NSWindowStyleMask style = NSWindowStyleMaskTitled |
                                  NSWindowStyleMaskClosable |
                                  NSWindowStyleMaskMiniaturizable |
                                  NSWindowStyleMaskResizable;

        NSWindow *window = [[NSWindow alloc] initWithContentRect:contentRect
                                                       styleMask:style
                                                         backing:NSBackingStoreBuffered
                                                           defer:NO];

        if (!window) {
            NSLog(@"Failed to create NSWindow");
            return AFFERENT_ERROR_WINDOW_FAILED;
        }

        [window setTitle:[NSString stringWithUTF8String:title]];
        [window center];

        // Create Metal view
        AfferentView *view = [[AfferentView alloc] initWithFrame:contentRect device:device];
        [window setContentView:view];

        // Create delegate
        AfferentWindowDelegate *delegate = [[AfferentWindowDelegate alloc] init];
        [window setDelegate:delegate];

        // Show window
        [window makeKeyAndOrderFront:nil];
        [NSApp activateIgnoringOtherApps:YES];

        // Finish launching if not done
        if (![NSApp isRunning]) {
            [NSApp finishLaunching];
        }

        // Create handle
        struct AfferentWindow *handle = malloc(sizeof(struct AfferentWindow));
        handle->nsWindow = window;
        handle->view = view;
        handle->delegate = delegate;
        handle->device = device;

        *out_window = handle;
        return AFFERENT_OK;
    }
}

void afferent_window_destroy(AfferentWindowRef window) {
    if (window) {
        @autoreleasepool {
            [window->nsWindow close];
        }
        free(window);
    }
}

bool afferent_window_should_close(AfferentWindowRef window) {
    return window ? window->delegate.shouldClose : true;
}

void afferent_window_poll_events(AfferentWindowRef window) {
    @autoreleasepool {
        NSEvent *event;
        while ((event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                           untilDate:nil
                                              inMode:NSDefaultRunLoopMode
                                             dequeue:YES])) {
            [NSApp sendEvent:event];
            [NSApp updateWindows];
        }
    }
}

void afferent_window_get_size(AfferentWindowRef window, uint32_t* width, uint32_t* height) {
    if (window) {
        CGSize size = window->view.metalLayer.drawableSize;
        *width = (uint32_t)size.width;
        *height = (uint32_t)size.height;
    }
}

// Expose the Metal device from window (used by renderer)
id<MTLDevice> afferent_window_get_device(AfferentWindowRef window) {
    return window ? window->device : nil;
}

// Expose the Metal layer from window (used by renderer)
CAMetalLayer* afferent_window_get_metal_layer(AfferentWindowRef window) {
    return window ? window->view.metalLayer : nil;
}
