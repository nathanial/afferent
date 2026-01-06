#import <Cocoa/Cocoa.h>
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"

#ifndef AFFERENT_CLICK_CAP
#define AFFERENT_CLICK_CAP 256
#endif

// Forward declarations
@class AfferentView;
@class AfferentWindowDelegate;

// Internal window structure (defined here so the view can access fields)
struct AfferentWindow {
    __unsafe_unretained NSWindow *nsWindow;
    __unsafe_unretained AfferentView *view;
    __unsafe_unretained AfferentWindowDelegate *delegate;
    __unsafe_unretained id<MTLDevice> device;
    bool destroyed;
    // Keyboard state
    uint16_t lastKeyCode;
    bool keyPressed;
    bool keysDown[256];         // Track all key states for continuous input
    // Mouse state
    float mouseX;               // Canvas coordinates (Y-down)
    float mouseY;
    uint8_t mouseButtons;       // Bitmask: left=1, right=2, middle=4
    float scrollDeltaX;
    float scrollDeltaY;
    bool mouseInWindow;
    uint16_t modifiers;         // Bitmask: shift=1, ctrl=2, alt=4, cmd=8
    // Pointer lock state (for FPS camera)
    bool pointerLocked;
    float mouseDeltaX;          // Accumulated delta since last query
    float mouseDeltaY;
    // Click events (poll-based like keyboard). Use a small ring buffer so multiple clicks
    // between frames don't overwrite each other.
    uint16_t clickHead;
    uint16_t clickCount;
    uint8_t clickButton[AFFERENT_CLICK_CAP];    // 0=left, 1=right, 2=middle
    float clickX[AFFERENT_CLICK_CAP];
    float clickY[AFFERENT_CLICK_CAP];
    uint16_t clickModifiers[AFFERENT_CLICK_CAP];
};

static inline void afferent_window_push_click(struct AfferentWindow *w, uint8_t button, float x, float y, uint16_t modifiers) {
    if (!w) return;
    const uint16_t cap = AFFERENT_CLICK_CAP;
    // If full, drop the oldest click.
    if (w->clickCount >= cap) {
        w->clickHead = (uint16_t)((w->clickHead + 1) % cap);
        w->clickCount--;
    }
    uint16_t idx = (uint16_t)((w->clickHead + w->clickCount) % cap);
    w->clickButton[idx] = button;
    w->clickX[idx] = x;
    w->clickY[idx] = y;
    w->clickModifiers[idx] = modifiers;
    w->clickCount++;
}

// Metal-backed view
@interface AfferentView : NSView
@property (nonatomic, strong) CAMetalLayer *metalLayer;
@property (nonatomic, strong) id<MTLDevice> device;
@property (nonatomic, assign) struct AfferentWindow *windowHandle;
@property (nonatomic, assign) CGSize fixedDrawableSize;
@end

@implementation AfferentView

- (BOOL)acceptsFirstMouse:(NSEvent *)event {
    // If the app/window is not active yet, macOS normally uses the first click to activate the window
    // and does not deliver it to the view. This can make UI buttons feel like they need multiple clicks
    // on startup. Accept the first mouse click so widgets receive it reliably.
    (void)event;
    return YES;
}

- (instancetype)initWithFrame:(NSRect)frameRect
                       device:(id<MTLDevice>)device
                 drawableSize:(CGSize)drawableSize
                contentsScale:(CGFloat)contentsScale {
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
        // Avoid blocking the render loop while waiting for a drawable.
        self.metalLayer.allowsNextDrawableTimeout = NO;
        // Triple buffering for smoother frame pacing
        self.metalLayer.maximumDrawableCount = 3;
        // Anchor content to top-left, don't scale when window resizes
        self.metalLayer.contentsGravity = kCAGravityTopLeft;
        // Match the window/screen backing scale so 1 unit in drawableSize == 1 physical pixel.
        if (contentsScale <= 0.0) contentsScale = 1.0;
        self.metalLayer.contentsScale = contentsScale;
        self.layer = self.metalLayer;
        // Ensure layer contents are redrawn during live resize.
        self.layerContentsRedrawPolicy = NSViewLayerContentsRedrawDuringViewResize;

        // Fixed drawable at requested pixel size - Lean code uses pixel coordinates.
        self.fixedDrawableSize = drawableSize;
        self.metalLayer.drawableSize = drawableSize;
    }
    return self;
}

- (void)setFrameSize:(NSSize)newSize {
    [super setFrameSize:newSize];
    // Update drawable to match new window size
    CGFloat scale = self.metalLayer.contentsScale;
    if (scale <= 0.0) scale = 1.0;
    self.metalLayer.drawableSize = CGSizeMake(newSize.width * scale, newSize.height * scale);
}

- (void)viewDidChangeBackingProperties {
    [super viewDidChangeBackingProperties];
    // Update scale and recalculate drawable size
    CGFloat scale = self.window.backingScaleFactor;
    if (scale <= 0.0) scale = 1.0;
    self.metalLayer.contentsScale = scale;
    CGSize bounds = self.bounds.size;
    self.metalLayer.drawableSize = CGSizeMake(bounds.width * scale, bounds.height * scale);
}

- (BOOL)acceptsFirstResponder {
    return YES;
}

- (void)keyDown:(NSEvent *)event {
    if (self.windowHandle) {
        uint16_t keyCode = [event keyCode];
        self.windowHandle->lastKeyCode = keyCode;
        self.windowHandle->keyPressed = true;
        if (keyCode < 256) {
            self.windowHandle->keysDown[keyCode] = true;
        }
    }
}

- (void)keyUp:(NSEvent *)event {
    if (self.windowHandle) {
        uint16_t keyCode = [event keyCode];
        if (keyCode < 256) {
            self.windowHandle->keysDown[keyCode] = false;
        }
    }
}

// Helper: Convert macOS view coordinates (Y-up) to canvas coordinates (Y-down)
- (CGPoint)canvasPointFromEvent:(NSEvent *)event {
    NSPoint viewPoint = [self convertPoint:[event locationInWindow] fromView:nil];
    // Map from view "points" to drawable "pixels" using the current bounds ↔ drawableSize ratio.
    // This stays correct across window resizes, backing-scale changes, and any drawableSize overrides.
    CGSize boundsSize = self.bounds.size;
    CGSize drawableSize = self.metalLayer.drawableSize;

    CGFloat sx = boundsSize.width > 0 ? (drawableSize.width / boundsSize.width) : 1.0;
    CGFloat sy = boundsSize.height > 0 ? (drawableSize.height / boundsSize.height) : 1.0;

    CGFloat canvasX = viewPoint.x * sx;
    CGFloat canvasY;
    if (self.isFlipped) {
        // Flipped views already use a Y-down coordinate system.
        canvasY = viewPoint.y * sy;
    } else {
        // Default NSView is Y-up; convert to Y-down.
        canvasY = drawableSize.height - viewPoint.y * sy;
    }
    return CGPointMake(canvasX, canvasY);
}

// Helper: Update modifier key state from event
- (void)updateModifiers:(NSEvent *)event {
    NSEventModifierFlags flags = [event modifierFlags];
    uint16_t mods = 0;
    if (flags & NSEventModifierFlagShift) mods |= 1;
    if (flags & NSEventModifierFlagControl) mods |= 2;
    if (flags & NSEventModifierFlagOption) mods |= 4;  // Alt/Option
    if (flags & NSEventModifierFlagCommand) mods |= 8;
    self.windowHandle->modifiers = mods;
}

- (void)mouseMoved:(NSEvent *)event {
    if (self.windowHandle) {
        if (self.windowHandle->pointerLocked) {
            // Accumulate delta from event when pointer locked
            self.windowHandle->mouseDeltaX += [event deltaX];
            self.windowHandle->mouseDeltaY += [event deltaY];
        } else {
            // Normal position tracking
            CGPoint p = [self canvasPointFromEvent:event];
            self.windowHandle->mouseX = p.x;
            self.windowHandle->mouseY = p.y;
        }
        [self updateModifiers:event];
    }
}

- (void)mouseDragged:(NSEvent *)event {
    if (self.windowHandle && self.windowHandle->pointerLocked) {
        // Accumulate delta when pointer locked
        self.windowHandle->mouseDeltaX += [event deltaX];
        self.windowHandle->mouseDeltaY += [event deltaY];
        [self updateModifiers:event];
    } else {
        [self mouseMoved:event];
    }
}

- (void)rightMouseDragged:(NSEvent *)event {
    if (self.windowHandle && self.windowHandle->pointerLocked) {
        self.windowHandle->mouseDeltaX += [event deltaX];
        self.windowHandle->mouseDeltaY += [event deltaY];
        [self updateModifiers:event];
    } else {
        [self mouseMoved:event];
    }
}

- (void)otherMouseDragged:(NSEvent *)event {
    if (self.windowHandle && self.windowHandle->pointerLocked) {
        self.windowHandle->mouseDeltaX += [event deltaX];
        self.windowHandle->mouseDeltaY += [event deltaY];
        [self updateModifiers:event];
    } else {
        [self mouseMoved:event];
    }
}

- (void)mouseDown:(NSEvent *)event {
    if (self.windowHandle) {
        CGPoint p = [self canvasPointFromEvent:event];
        self.windowHandle->mouseX = p.x;
        self.windowHandle->mouseY = p.y;
        self.windowHandle->mouseButtons |= 1;  // Left button
        [self updateModifiers:event];
        afferent_window_push_click(self.windowHandle, 0, p.x, p.y, self.windowHandle->modifiers);
    }
}

- (void)mouseUp:(NSEvent *)event {
    if (self.windowHandle) {
        self.windowHandle->mouseButtons &= ~1;  // Clear left button
        [self updateModifiers:event];
    }
}

- (void)rightMouseDown:(NSEvent *)event {
    if (self.windowHandle) {
        CGPoint p = [self canvasPointFromEvent:event];
        self.windowHandle->mouseX = p.x;
        self.windowHandle->mouseY = p.y;
        self.windowHandle->mouseButtons |= 2;  // Right button
        [self updateModifiers:event];
        afferent_window_push_click(self.windowHandle, 1, p.x, p.y, self.windowHandle->modifiers);
    }
}

- (void)rightMouseUp:(NSEvent *)event {
    if (self.windowHandle) {
        self.windowHandle->mouseButtons &= ~2;  // Clear right button
        [self updateModifiers:event];
    }
}

- (void)otherMouseDown:(NSEvent *)event {
    if (self.windowHandle) {
        CGPoint p = [self canvasPointFromEvent:event];
        self.windowHandle->mouseX = p.x;
        self.windowHandle->mouseY = p.y;
        self.windowHandle->mouseButtons |= 4;  // Middle button
        [self updateModifiers:event];
        afferent_window_push_click(self.windowHandle, 2, p.x, p.y, self.windowHandle->modifiers);
    }
}

- (void)otherMouseUp:(NSEvent *)event {
    if (self.windowHandle) {
        self.windowHandle->mouseButtons &= ~4;  // Clear middle button
        [self updateModifiers:event];
    }
}

- (void)scrollWheel:(NSEvent *)event {
    if (self.windowHandle) {
        CGPoint p = [self canvasPointFromEvent:event];
        self.windowHandle->mouseX = p.x;
        self.windowHandle->mouseY = p.y;
        self.windowHandle->scrollDeltaX += [event scrollingDeltaX];
        self.windowHandle->scrollDeltaY += [event scrollingDeltaY];
        [self updateModifiers:event];
    }
}

- (void)mouseEntered:(NSEvent *)event {
    if (self.windowHandle) {
        self.windowHandle->mouseInWindow = true;
    }
}

- (void)mouseExited:(NSEvent *)event {
    if (self.windowHandle) {
        self.windowHandle->mouseInWindow = false;
    }
}

- (void)flagsChanged:(NSEvent *)event {
    [self updateModifiers:event];
}

// Enable mouse tracking for mouseMoved, mouseEntered, mouseExited
- (void)updateTrackingAreas {
    [super updateTrackingAreas];
    // Remove old tracking areas
    for (NSTrackingArea *area in self.trackingAreas) {
        [self removeTrackingArea:area];
    }
    // Add new tracking area covering the entire view
    NSTrackingAreaOptions options = NSTrackingMouseMoved |
                                    NSTrackingMouseEnteredAndExited |
                                    NSTrackingActiveInActiveApp |
                                    NSTrackingInVisibleRect;
    NSTrackingArea *area = [[NSTrackingArea alloc] initWithRect:self.bounds
                                                        options:options
                                                          owner:self
                                                       userInfo:nil];
    [self addTrackingArea:area];
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
    [NSApp stop:nil];
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
        // Treat requested width/height as pixel dimensions. Convert to Cocoa "points" so the
        // on-screen window size matches the requested physical pixel size on Retina displays.
        NSScreen *screen = [NSScreen mainScreen];
        CGFloat scale = screen ? screen.backingScaleFactor : 1.0;
        if (scale <= 0.0) scale = 1.0;
        CGFloat pointsWidth = ((CGFloat)width) / scale;
        CGFloat pointsHeight = ((CGFloat)height) / scale;

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
        NSRect contentRect = NSMakeRect(0, 0, pointsWidth, pointsHeight);
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
        // Required to receive `mouseMoved:` events when no buttons are pressed.
        // Without this, FPS-style mouse look won't work unless the user is dragging.
        [window setAcceptsMouseMovedEvents:YES];

        // Create Metal view
        AfferentView *view = [[AfferentView alloc] initWithFrame:contentRect
                                                          device:device
                                                    drawableSize:CGSizeMake(width, height)
                                                   contentsScale:scale];
        [window setContentView:view];
        // Prefer the Metal view as the initial key-view responder (WASD / Escape) once the window is key.
        [window setInitialFirstResponder:view];

        // Create delegate
        AfferentWindowDelegate *delegate = [[AfferentWindowDelegate alloc] init];
        [window setDelegate:delegate];

        // Show window
        [window makeKeyAndOrderFront:nil];
        [NSApp activateIgnoringOtherApps:YES];
        // Ensure the view receives keyboard events immediately, without requiring a click.
        [window makeFirstResponder:view];

        // Finish launching if not done
        if (![NSApp isRunning]) {
            [NSApp finishLaunching];
        }

        // Create handle
        struct AfferentWindow *handle = malloc(sizeof(struct AfferentWindow));
        handle->nsWindow = (__bridge NSWindow *)CFRetain((__bridge CFTypeRef)window);
        handle->view = (__bridge AfferentView *)CFRetain((__bridge CFTypeRef)view);
        handle->delegate = (__bridge AfferentWindowDelegate *)CFRetain((__bridge CFTypeRef)delegate);
        handle->device = (__bridge id<MTLDevice>)CFRetain((__bridge CFTypeRef)device);
        handle->lastKeyCode = 0;
        handle->keyPressed = false;
        memset(handle->keysDown, 0, sizeof(handle->keysDown));  // All keys up
        // Initialize mouse state
        handle->mouseX = 0;
        handle->mouseY = 0;
        handle->mouseButtons = 0;
        handle->scrollDeltaX = 0;
        handle->scrollDeltaY = 0;
        handle->mouseInWindow = false;
        handle->modifiers = 0;
        // Initialize pointer lock state
        handle->pointerLocked = false;
        handle->mouseDeltaX = 0;
        handle->mouseDeltaY = 0;
        handle->clickHead = 0;
        handle->clickCount = 0;
        handle->destroyed = false;

        // Set back-reference so view can store key events
        view.windowHandle = handle;

        *out_window = handle;
        return AFFERENT_OK;
    }
}

void afferent_window_destroy(AfferentWindowRef window) {
    if (window) {
        if (window->destroyed) {
            return;
        }
        window->destroyed = true;

        AfferentView *view = window->view;
        NSWindow *nsWindow = window->nsWindow;
        AfferentWindowDelegate *delegate = window->delegate;
        id<MTLDevice> device = window->device;

        if (view) {
            view.windowHandle = NULL;
        }
        if (nsWindow) {
            [nsWindow setDelegate:nil];
            [nsWindow setContentView:nil];
            [nsWindow close];
        }

        if (delegate) {
            CFRelease((__bridge CFTypeRef)delegate);
        }
        if (view) {
            CFRelease((__bridge CFTypeRef)view);
        }
        if (nsWindow) {
            CFRelease((__bridge CFTypeRef)nsWindow);
        }
        if (device) {
            CFRelease((__bridge CFTypeRef)device);
        }

        window->delegate = NULL;
        window->view = NULL;
        window->nsWindow = NULL;
        window->device = NULL;
        free(window);
    }
}

bool afferent_window_should_close(AfferentWindowRef window) {
    return window ? window->delegate.shouldClose : true;
}

void afferent_window_poll_events(AfferentWindowRef window) {
    @autoreleasepool {
        (void)window;
        const int maxEvents = 64;
        NSDate *now = [NSDate distantPast];
        for (int i = 0; i < maxEvents; i++) {
            NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                               untilDate:now
                                                  inMode:NSDefaultRunLoopMode
                                                 dequeue:YES];
            if (!event) {
                event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                           untilDate:now
                                              inMode:NSEventTrackingRunLoopMode
                                             dequeue:YES];
            }
            if (!event) {
                break;
            }
            [NSApp sendEvent:event];
            [NSApp updateWindows];
        }
    }
}

void afferent_window_run_event_loop(AfferentWindowRef window) {
    (void)window;
    [NSApp run];
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

// Get the last key code pressed (only valid if hasKeyPressed is true)
uint16_t afferent_window_get_key_code(AfferentWindowRef window) {
    if (window && window->keyPressed) {
        return window->lastKeyCode;
    }
    return 0;
}

// Check if a key is currently pending (use this to distinguish key code 0 from "no key")
bool afferent_window_has_key_pressed(AfferentWindowRef window) {
    return window && window->keyPressed;
}

// Clear the key pressed state (call after handling the key)
void afferent_window_clear_key(AfferentWindowRef window) {
    if (window) {
        window->keyPressed = false;
        window->lastKeyCode = 0;
    }
}

// Get the main screen's backing scale factor
float afferent_get_screen_scale(void) {
    @autoreleasepool {
        return (float)[NSScreen mainScreen].backingScaleFactor;
    }
}

// Mouse input functions
void afferent_window_get_mouse_pos(AfferentWindowRef window, float* x, float* y) {
    if (window) {
        *x = window->mouseX;
        *y = window->mouseY;
    } else {
        *x = 0;
        *y = 0;
    }
}

uint8_t afferent_window_get_mouse_buttons(AfferentWindowRef window) {
    return window ? window->mouseButtons : 0;
}

uint16_t afferent_window_get_modifiers(AfferentWindowRef window) {
    return window ? window->modifiers : 0;
}

void afferent_window_get_scroll_delta(AfferentWindowRef window, float* dx, float* dy) {
    if (window) {
        *dx = window->scrollDeltaX;
        *dy = window->scrollDeltaY;
    } else {
        *dx = 0;
        *dy = 0;
    }
}

void afferent_window_clear_scroll(AfferentWindowRef window) {
    if (window) {
        window->scrollDeltaX = 0;
        window->scrollDeltaY = 0;
    }
}

bool afferent_window_mouse_in_window(AfferentWindowRef window) {
    return window ? window->mouseInWindow : false;
}

// Get pending click event. Returns true if click available.
bool afferent_window_get_click(AfferentWindowRef window, uint8_t* button, float* x, float* y, uint16_t* modifiers) {
    if (window && window->clickCount > 0) {
        uint16_t idx = window->clickHead;
        *button = window->clickButton[idx];
        *x = window->clickX[idx];
        *y = window->clickY[idx];
        *modifiers = window->clickModifiers[idx];
        return true;
    }
    return false;
}

void afferent_window_clear_click(AfferentWindowRef window) {
    if (window) {
        if (window->clickCount > 0) {
            const uint16_t cap = AFFERENT_CLICK_CAP;
            window->clickHead = (uint16_t)((window->clickHead + 1) % cap);
            window->clickCount--;
        }
    }
}

// Pointer lock functions (for FPS camera controls)
void afferent_window_set_pointer_lock(AfferentWindowRef window, bool locked) {
    if (!window) return;

    if (window->pointerLocked == locked) {
        return;
    }
    window->pointerLocked = locked;
    if (locked) {
        // Disable mouse-cursor association (mouse moves don't move cursor)
        CGAssociateMouseAndMouseCursorPosition(false);
        [NSCursor hide];
    } else {
        // Re-enable normal mouse behavior
        CGAssociateMouseAndMouseCursorPosition(true);
        [NSCursor unhide];
    }
    // Reset delta accumulator
    window->mouseDeltaX = 0;
    window->mouseDeltaY = 0;
}

bool afferent_window_get_pointer_lock(AfferentWindowRef window) {
    return window ? window->pointerLocked : false;
}

void afferent_window_get_mouse_delta(AfferentWindowRef window, float* dx, float* dy) {
    if (window) {
        *dx = window->mouseDeltaX;
        *dy = window->mouseDeltaY;
        // Reset after reading
        window->mouseDeltaX = 0;
        window->mouseDeltaY = 0;
    } else {
        *dx = 0;
        *dy = 0;
    }
}

// Key state tracking (for continuous movement input)
bool afferent_window_is_key_down(AfferentWindowRef window, uint16_t keyCode) {
    if (window && keyCode < 256) {
        return window->keysDown[keyCode];
    }
    return false;
}
