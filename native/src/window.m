#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include <string.h>
#include "afferent.h"

// Forward declarations
@class AfferentView;
@class AfferentWindowDelegate;

// Internal window structure - defined early so view can access it
struct AfferentWindow {
    NSWindow *nsWindow;
    AfferentView *view;
    AfferentWindowDelegate *delegate;
    id<MTLDevice> device;

    // Input state
    float mouseX;
    float mouseY;
    bool mouseDown[3];      // left, right, middle
    bool mousePressed[3];   // pressed this frame (single-frame signal)
    bool mouseReleased[3];  // released this frame (single-frame signal)
    float scrollX;
    float scrollY;
    char textInput[32];     // text input buffer for this frame
    int textInputLen;
};

// Metal-backed view with input handling
@interface AfferentView : NSView
@property (nonatomic, strong) CAMetalLayer *metalLayer;
@property (nonatomic, strong) id<MTLDevice> device;
@property (nonatomic, assign) struct AfferentWindow *windowRef;
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

// Enable mouse tracking for mouseMoved events
- (void)updateTrackingAreas {
    [super updateTrackingAreas];
    for (NSTrackingArea *area in self.trackingAreas) {
        [self removeTrackingArea:area];
    }
    NSTrackingArea *trackingArea = [[NSTrackingArea alloc]
        initWithRect:self.bounds
             options:(NSTrackingMouseMoved | NSTrackingActiveInKeyWindow | NSTrackingInVisibleRect)
               owner:self
            userInfo:nil];
    [self addTrackingArea:trackingArea];
}

// Helper to convert mouse coordinates (flip Y to top-left origin)
- (void)updateMousePosition:(NSEvent *)event {
    if (!self.windowRef) return;
    NSPoint loc = [self convertPoint:event.locationInWindow fromView:nil];
    self.windowRef->mouseX = (float)loc.x;
    self.windowRef->mouseY = (float)(self.bounds.size.height - loc.y);
}

// Mouse button events
- (void)mouseDown:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[0] = true;
    self.windowRef->mousePressed[0] = true;
}

- (void)mouseUp:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[0] = false;
    self.windowRef->mouseReleased[0] = true;
}

- (void)mouseMoved:(NSEvent *)event {
    [self updateMousePosition:event];
}

- (void)mouseDragged:(NSEvent *)event {
    [self updateMousePosition:event];
}

- (void)rightMouseDown:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[1] = true;
    self.windowRef->mousePressed[1] = true;
}

- (void)rightMouseUp:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[1] = false;
    self.windowRef->mouseReleased[1] = true;
}

- (void)rightMouseDragged:(NSEvent *)event {
    [self updateMousePosition:event];
}

- (void)otherMouseDown:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[2] = true;
    self.windowRef->mousePressed[2] = true;
}

- (void)otherMouseUp:(NSEvent *)event {
    [self updateMousePosition:event];
    if (!self.windowRef) return;
    self.windowRef->mouseDown[2] = false;
    self.windowRef->mouseReleased[2] = true;
}

- (void)otherMouseDragged:(NSEvent *)event {
    [self updateMousePosition:event];
}

// Scroll wheel
- (void)scrollWheel:(NSEvent *)event {
    if (!self.windowRef) return;
    self.windowRef->scrollX += (float)event.scrollingDeltaX;
    self.windowRef->scrollY += (float)event.scrollingDeltaY;
}

// Keyboard - capture typed characters for text input
- (void)keyDown:(NSEvent *)event {
    if (!self.windowRef) return;
    NSString *chars = event.characters;
    if (chars.length > 0 && self.windowRef->textInputLen < 31) {
        for (NSUInteger i = 0; i < chars.length && self.windowRef->textInputLen < 31; i++) {
            unichar c = [chars characterAtIndex:i];
            if (c < 128) {  // ASCII only for now
                self.windowRef->textInput[self.windowRef->textInputLen++] = (char)c;
            }
        }
        self.windowRef->textInput[self.windowRef->textInputLen] = '\0';
    }
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

        // Create handle and initialize input state
        struct AfferentWindow *handle = calloc(1, sizeof(struct AfferentWindow));
        handle->nsWindow = window;
        handle->view = view;
        handle->delegate = delegate;
        handle->device = device;

        // Wire up view's reference back to window for input handling
        view.windowRef = handle;

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

// ============================================================================
// Input API
// ============================================================================

// Reset per-frame input state (call at start of each frame)
void afferent_window_new_frame(AfferentWindowRef window) {
    if (!window) return;
    // Clear single-frame signals
    for (int i = 0; i < 3; i++) {
        window->mousePressed[i] = false;
        window->mouseReleased[i] = false;
    }
    window->scrollX = 0.0f;
    window->scrollY = 0.0f;
    window->textInputLen = 0;
    window->textInput[0] = '\0';
}

// Get mouse position
void afferent_window_get_mouse_pos(AfferentWindowRef window, float* x, float* y) {
    if (!window) {
        *x = 0.0f;
        *y = 0.0f;
        return;
    }
    *x = window->mouseX;
    *y = window->mouseY;
}

// Check if mouse button is currently held down
bool afferent_window_mouse_down(AfferentWindowRef window, int button) {
    if (!window || button < 0 || button > 2) return false;
    return window->mouseDown[button];
}

// Check if mouse button was pressed this frame
bool afferent_window_mouse_pressed(AfferentWindowRef window, int button) {
    if (!window || button < 0 || button > 2) return false;
    return window->mousePressed[button];
}

// Check if mouse button was released this frame
bool afferent_window_mouse_released(AfferentWindowRef window, int button) {
    if (!window || button < 0 || button > 2) return false;
    return window->mouseReleased[button];
}

// Get scroll wheel delta
void afferent_window_get_scroll(AfferentWindowRef window, float* x, float* y) {
    if (!window) {
        *x = 0.0f;
        *y = 0.0f;
        return;
    }
    *x = window->scrollX;
    *y = window->scrollY;
}

// Get text input for this frame (returns number of characters)
int afferent_window_get_text_input(AfferentWindowRef window, char* buf, int bufSize) {
    if (!window || !buf || bufSize <= 0) return 0;
    int len = window->textInputLen;
    if (len >= bufSize) len = bufSize - 1;
    memcpy(buf, window->textInput, len);
    buf[len] = '\0';
    return len;
}
