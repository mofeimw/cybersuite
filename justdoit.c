// clang -framework Cocoa justdoit.c -o justdoit

#include <objc/objc.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <CoreGraphics/CoreGraphics.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef long NSInteger;
typedef unsigned long NSUInteger;
typedef double CGFloat;
typedef struct CGRect NSRect;

#define MAX_LINE_LENGTH 1024
#define PADDING 2
#define PROGRESS_HEIGHT 11
#define MIN_WINDOW_WIDTH 400
#define MIN_PROGRESS_WIDTH 100

id g_textField = nil;
id g_progressView = nil;
int g_remainingSeconds = 0;
int g_initialSeconds = 0;
char g_activity[MAX_LINE_LENGTH];
CGFloat g_taskWidth = 0;
CGFloat g_windowWidth = 0;
CGFloat g_progressWidth = 0;
CGFloat g_progressStartX = 0;
id g_timer = nil;
id g_timerDelegate = nil;

// Forward declarations
CGSize measure_text(const char* text, id font);
void update_progress_color(float percentage);
void timer_callback(id self, SEL _cmd, id timer);
void quit_callback(id self, SEL _cmd, id timer);

void timer_callback(id self, SEL _cmd, id timer) {
    if (g_remainingSeconds >= 0) {
        g_remainingSeconds--;

        char displayStr[MAX_LINE_LENGTH];
        int minutes = g_remainingSeconds / 60;
        int seconds = g_remainingSeconds % 60;

        // Combine task name and timer
        snprintf(displayStr, sizeof(displayStr), "%s   %2d:%02d", g_activity, minutes, seconds);

        id str = ((id (*)(id, SEL, const char *))objc_msgSend)(
                (id)objc_getClass("NSString"),
                sel_registerName("stringWithUTF8String:"),
                displayStr);

        ((void (*)(id, SEL, id))objc_msgSend)(g_textField,
            sel_registerName("setStringValue:"),
            str);

        CGRect frame = ((CGRect (*)(id, SEL))objc_msgSend)(g_progressView,
                sel_registerName("frame"));
        frame.size.width = (g_remainingSeconds * g_progressWidth) / g_initialSeconds;
        ((void (*)(id, SEL, CGRect))objc_msgSend)(g_progressView,
            sel_registerName("setFrame:"),
            frame);

        float percentage = (g_remainingSeconds * 100.0) / g_initialSeconds;
        update_progress_color(percentage);

        if (g_remainingSeconds == 0) {
            snprintf(displayStr, sizeof(displayStr), "%s   done", g_activity);
            str = ((id (*)(id, SEL, const char *))objc_msgSend)(
                    (id)objc_getClass("NSString"),
                    sel_registerName("stringWithUTF8String:"),
                    displayStr);

            ((void (*)(id, SEL, id))objc_msgSend)(g_textField,
                sel_registerName("setStringValue:"),
                str);

            ((void (*)(id, SEL))objc_msgSend)(timer,
                sel_registerName("invalidate"));

            ((void (*)(id, SEL, double, id, SEL, id, BOOL))objc_msgSend)(
                (id)objc_getClass("NSTimer"),
                sel_registerName("scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"),
                240.0,
                self,
                sel_registerName("quitCallback"),
                nil,
                NO);
        }
    }
}

void update_progress_color(float percentage) {
    id color;
    if (percentage > 50.0) {
        // Green
        color = ((id (*)(id, SEL, CGFloat, CGFloat, CGFloat, CGFloat))objc_msgSend)(
                (id)objc_getClass("NSColor"),
                sel_registerName("colorWithRed:green:blue:alpha:"),
                0.533, 0.82, 0.62, 1.0);
    } else if (percentage > 20.0) {
        // Yellow
        color = ((id (*)(id, SEL, CGFloat, CGFloat, CGFloat, CGFloat))objc_msgSend)(
                (id)objc_getClass("NSColor"),
                sel_registerName("colorWithRed:green:blue:alpha:"),
                1.0, 0.89, 0.569, 1.0);
    } else {
        // Red
        color = ((id (*)(id, SEL, CGFloat, CGFloat, CGFloat, CGFloat))objc_msgSend)(
                (id)objc_getClass("NSColor"),
                sel_registerName("colorWithRed:green:blue:alpha:"),
                0.969, 0.463, 0.557, 1.0);
    }

    id layer = ((id (*)(id, SEL))objc_msgSend)(g_progressView,
            sel_registerName("layer"));
    ((void (*)(id, SEL, CGColorRef))objc_msgSend)(layer,
        sel_registerName("setBackgroundColor:"),
        ((CGColorRef (*)(id, SEL))objc_msgSend)(color,
            sel_registerName("CGColor")));
}

void quit_callback(id self, SEL _cmd, id timer) {
    exit(0);
}

CGSize measure_text(const char* text, id font) {
    id str = ((id (*)(id, SEL, const char *))objc_msgSend)(
            (id)objc_getClass("NSString"),
            sel_registerName("stringWithUTF8String:"),
            text);

    id dict = ((id (*)(id, SEL, id, id))objc_msgSend)((id)objc_getClass("NSDictionary"),
            sel_registerName("dictionaryWithObject:forKey:"),
            font,
            ((id (*)(id, SEL, const char *))objc_msgSend)(
                (id)objc_getClass("NSString"),
                sel_registerName("stringWithUTF8String:"),
                "NSFont"));

    return ((CGSize (*)(id, SEL, id))objc_msgSend)(str,
            sel_registerName("sizeWithAttributes:"),
            dict);
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Usage: %s <task_name> <duration_in_minutes>\n", argv[0]);
        return 1;
    }

    strncpy(g_activity, argv[1], MAX_LINE_LENGTH);
    g_remainingSeconds = atoi(argv[2]) * 60 + 1; // Compensate for losing first tick
    g_initialSeconds = g_remainingSeconds;

    id NSApp = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSApplication"),
            sel_registerName("sharedApplication"));

    Class TimerDelegateClass = objc_allocateClassPair((Class)objc_getClass("NSObject"), "TimerDelegate", 0);
    class_addMethod(TimerDelegateClass, sel_registerName("timerFired:"), (IMP)timer_callback, "v@:@");
    class_addMethod(TimerDelegateClass, sel_registerName("quitCallback"), (IMP)quit_callback, "v@:@");
    objc_registerClassPair(TimerDelegateClass);

    g_timerDelegate = ((id (*)(id, SEL))objc_msgSend)((id)TimerDelegateClass,
            sel_registerName("new"));

    id systemFont = ((id (*)(id, SEL, CGFloat))objc_msgSend)(
            (id)objc_getClass("NSFont"),
            sel_registerName("systemFontOfSize:"),
            13.5);

    // Calculate initial display string to get proper width
    char initialStr[MAX_LINE_LENGTH];
    // Calculate space needed for longest possible string
    snprintf(initialStr, sizeof(initialStr), "%s   00:00", g_activity);
    CGSize timeSize = measure_text(initialStr, systemFont);
    // Also check "done" variant
    snprintf(initialStr, sizeof(initialStr), "%s   done", g_activity);
    CGSize doneSize = measure_text(initialStr, systemFont);
    // Use the larger of the two sizes
    g_taskWidth = (timeSize.width > doneSize.width ? timeSize.width : doneSize.width) + 13;

    g_windowWidth = g_taskWidth + MIN_PROGRESS_WIDTH + PADDING;
    if (g_windowWidth < MIN_WINDOW_WIDTH) g_windowWidth = MIN_WINDOW_WIDTH;

    id window = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSWindow"),
            sel_registerName("alloc"));
    unsigned int styleMask = 0;
    id screen = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSScreen"),
            sel_registerName("mainScreen"));
    CGRect screenFrame = ((CGRect (*)(id, SEL))objc_msgSend)(screen,
            sel_registerName("frame"));
    CGFloat windowHeight = 24;
    CGFloat x = 150;
    CGFloat y = screenFrame.size.height - windowHeight - 8;
    CGRect frame = CGRectMake(x, y, g_windowWidth, windowHeight);

    window = ((id (*)(id, SEL, CGRect, unsigned long, unsigned long, BOOL))objc_msgSend)(
            window,
            sel_registerName("initWithContentRect:styleMask:backing:defer:"),
            frame,
            styleMask,
            2,
            NO);

    ((void (*)(id, SEL, BOOL))objc_msgSend)(window,
        sel_registerName("setOpaque:"),
        NO);
    ((void (*)(id, SEL, NSInteger))objc_msgSend)(window,
        sel_registerName("setLevel:"),
        100);

    // Show in all workspaces
    ((void (*)(id, SEL, BOOL))objc_msgSend)(window,
        sel_registerName("setCollectionBehavior:"),
        0x400);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(window,
        sel_registerName("setHidesOnDeactivate:"),
        NO);

    id blackColor = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSColor"),
            sel_registerName("blackColor"));
    ((void (*)(id, SEL, id))objc_msgSend)(window,
        sel_registerName("setBackgroundColor:"),
        blackColor);

    g_textField = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSTextField"),
            sel_registerName("alloc"));
    CGRect textFrame = CGRectMake(0, (windowHeight - 20) / 2,
            g_taskWidth,
            20);
    g_textField = ((id (*)(id, SEL, CGRect))objc_msgSend)(
            g_textField,
            sel_registerName("initWithFrame:"),
            textFrame);

    ((void (*)(id, SEL, id))objc_msgSend)(g_textField,
        sel_registerName("setFont:"),
        systemFont);
    ((void (*)(id, SEL, id))objc_msgSend)(g_textField,
        sel_registerName("setTextColor:"),
        ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSColor"),
            sel_registerName("whiteColor")));
    ((void (*)(id, SEL, id))objc_msgSend)(g_textField,
        sel_registerName("setBackgroundColor:"),
        blackColor);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(g_textField,
        sel_registerName("setEditable:"),
        NO);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(g_textField,
        sel_registerName("setBezeled:"),
        NO);

    // Calculate exact positions for progress bar
    g_progressStartX = g_taskWidth + PADDING;
    g_progressWidth = g_windowWidth - g_progressStartX;

    // Create progress view (background)
    id bgView = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSView"),
            sel_registerName("alloc"));
    CGRect progressFrame = CGRectMake(g_progressStartX,
            (windowHeight - PROGRESS_HEIGHT) / 2 + 1.5,
            g_progressWidth,
            PROGRESS_HEIGHT);
    bgView = ((id (*)(id, SEL, CGRect))objc_msgSend)(
            bgView,
            sel_registerName("initWithFrame:"),
            progressFrame);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(bgView,
        sel_registerName("setWantsLayer:"),
        (BOOL)YES);

    id whiteColor = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSColor"),
            sel_registerName("whiteColor"));
    id barBgColor = ((id (*)(id, SEL, CGFloat, CGFloat, CGFloat, CGFloat))objc_msgSend)(
            (id)objc_getClass("NSColor"),
            sel_registerName("colorWithRed:green:blue:alpha:"),
            0.125, 0.110, 0.129, 1.0);
    ((void (*)(id, SEL, CGColorRef))objc_msgSend)(((id (*)(id, SEL))objc_msgSend)(bgView,
            sel_registerName("layer")),
        sel_registerName("setBackgroundColor:"),
        ((CGColorRef (*)(id, SEL))objc_msgSend)(barBgColor,
            sel_registerName("CGColor")));

    // Create progress view (colored)
    g_progressView = ((id (*)(id, SEL))objc_msgSend)((id)objc_getClass("NSView"),
            sel_registerName("alloc"));
    g_progressView = ((id (*)(id, SEL, CGRect))objc_msgSend)(
            g_progressView,
            sel_registerName("initWithFrame:"),
            progressFrame);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(g_progressView,
        sel_registerName("setWantsLayer:"),
        (BOOL)YES);

    // Set initial color to green
    update_progress_color(100.0);

    id contentView = ((id (*)(id, SEL))objc_msgSend)(window,
            sel_registerName("contentView"));
    ((void (*)(id, SEL, id))objc_msgSend)(contentView,
        sel_registerName("addSubview:"),
        g_textField);
    ((void (*)(id, SEL, id))objc_msgSend)(contentView,
        sel_registerName("addSubview:"),
        bgView);
    ((void (*)(id, SEL, id))objc_msgSend)(contentView,
        sel_registerName("addSubview:"),
        g_progressView);

    // Fire timer callback immediately to initialize display
    timer_callback(g_timerDelegate, sel_registerName("timerFired:"), nil);

    // Start the repeating timer
    g_timer = ((id (*)(id, SEL, double, id, SEL, id, BOOL))objc_msgSend)(
            (id)objc_getClass("NSTimer"),
            sel_registerName("scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:"),
            1.0,
            g_timerDelegate,
            sel_registerName("timerFired:"),
            nil,
            YES);

    // Show window after timer is set up
    ((void (*)(id, SEL, id))objc_msgSend)(window,
        sel_registerName("makeKeyAndOrderFront:"),
        nil);
    ((void (*)(id, SEL, BOOL))objc_msgSend)(NSApp,
        sel_registerName("activateIgnoringOtherApps:"),
        (BOOL)YES);

    ((void (*)(id, SEL))objc_msgSend)(NSApp, sel_registerName("run"));
    return 0;
}
