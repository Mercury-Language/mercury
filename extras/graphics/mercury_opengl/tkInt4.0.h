/*
 * tkInt.h --
 *
 *	Declarations for things used internally by the Tk
 *	procedures but not exported outside the module.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * @(#) tkInt.h 1.147 95/06/24 17:12:55
 */

#ifndef _TKINT
#define _TKINT

#ifndef _XLIB_H_
#include <X11/Xlib.h>
#endif
#ifndef _XUTIL_H
#include <X11/Xutil.h>
#endif
#ifndef _TK
#include "tk.h"
#endif
#ifndef _TCL
#include "tcl.h"
#endif

/*
 * Opaque type declarations:
 */

typedef struct TkColormap TkColormap;
typedef struct TkGrabEvent TkGrabEvent;
typedef struct Tk_PostscriptInfo Tk_PostscriptInfo;
typedef struct TkStressedCmap TkStressedCmap;

/*
 * One of the following structures is maintained for each display
 * containing a window managed by Tk:
 */

typedef struct TkDisplay {
    Display *display;		/* Xlib's info about display. */
    struct TkDisplay *nextPtr;	/* Next in list of all displays. */
    char *name;			/* Name of display (with any screen
				 * identifier removed).  Malloc-ed. */
    Time lastEventTime;		/* Time of last event received for this
				 * display. */

    /*
     * Information used primarily by tkBind.c:
     */

    int bindInfoStale;		/* Non-zero means the variables in this
				 * part of the structure are potentially
				 * incorrect and should be recomputed. */
    unsigned int modeModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to "mode shift".  If no
				 * such modifier, than this is zero. */
    unsigned int metaModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to the "Meta" key.  If no
				 * such modifier, then this is zero. */
    unsigned int altModMask;	/* Has one bit set to indicate the modifier
				 * corresponding to the "Meta" key.  If no
				 * such modifier, then this is zero. */
    enum {LU_IGNORE, LU_CAPS, LU_SHIFT} lockUsage;
				/* Indicates how to interpret lock modifier. */
    int numModKeyCodes;		/* Number of entries in modKeyCodes array
				 * below. */
    KeyCode *modKeyCodes;	/* Pointer to an array giving keycodes for
				 * all of the keys that have modifiers
				 * associated with them.  Malloc'ed, but
				 * may be NULL. */

    /*
     * Information used by tkError.c only:
     */

    struct TkErrorHandler *errorPtr;
				/* First in list of error handlers
				 * for this display.  NULL means
				 * no handlers exist at present. */
    int deleteCount;		/* Counts # of handlers deleted since
				 * last time inactive handlers were
				 * garbage-collected.  When this number
				 * gets big, handlers get cleaned up. */

    /*
     * Information used by tkSend.c only:
     */

    Tk_Window commTkwin;	/* Window used for communication
				 * between interpreters during "send"
				 * commands.  NULL means send info hasn't
				 * been initialized yet. */
    Atom commProperty;		/* X's name for comm property. */
    Atom registryProperty;	/* X's name for property containing
				 * registry of interpreter names. */
    Atom appNameProperty;	/* X's name for property used to hold the
				 * application name on each comm window. */

    /*
     * Information used by tkSelect.c and tkClipboard.c only:
     */

    struct TkSelectionInfo *selectionInfoPtr;
				/* First in list of selection information
				 * records.  Each entry contains information
				 * about the current owner of a particular
				 * selection on this display. */
    Atom multipleAtom;		/* Atom for MULTIPLE.  None means
				 * selection stuff isn't initialized. */
    Atom incrAtom;		/* Atom for INCR. */
    Atom targetsAtom;		/* Atom for TARGETS. */
    Atom timestampAtom;		/* Atom for TIMESTAMP. */
    Atom textAtom;		/* Atom for TEXT. */
    Atom compoundTextAtom;	/* Atom for COMPOUND_TEXT. */
    Atom applicationAtom;	/* Atom for TK_APPLICATION. */
    Atom windowAtom;		/* Atom for TK_WINDOW. */
    Atom clipboardAtom;		/* Atom for CLIPBOARD. */

    Tk_Window clipWindow;	/* Window used for clipboard ownership and to
				 * retrieve selections between processes. NULL
				 * means clipboard info hasn't been
				 * initialized. */
    int clipboardActive;	/* 1 means we currently own the clipboard
				 * selection, 0 means we don't. */
    struct TkMainInfo *clipboardAppPtr;
				/* Last application that owned clipboard. */
    struct TkClipboardTarget *clipTargetPtr;
				/* First in list of clipboard type information
				 * records.  Each entry contains information
				 * about the buffers for a given selection
				 * target. */

    /*
     * Information used by tkAtom.c only:
     */

    int atomInit;		/* 0 means stuff below hasn't been
				 * initialized yet. */
    Tcl_HashTable nameTable;	/* Maps from names to Atom's. */
    Tcl_HashTable atomTable;	/* Maps from Atom's back to names. */

    /*
     * Information used by tkCursor.c only:
     */

    Font cursorFont;		/* Font to use for standard cursors.
				 * None means font not loaded yet. */

    /*
     * Information used by tkGrab.c only:
     */

    struct TkWindow *grabWinPtr;
				/* Window in which the pointer is currently
				 * grabbed, or NULL if none. */
    struct TkWindow *eventualGrabWinPtr;
				/* Value that grabWinPtr will have once the
				 * grab event queue (below) has been
				 * completely emptied. */
    struct TkWindow *buttonWinPtr;
				/* Window in which first mouse button was
				 * pressed while grab was in effect, or NULL
				 * if no such press in effect. */
    struct TkWindow *serverWinPtr;
				/* If no application contains the pointer then
				 * this is NULL.  Otherwise it contains the
				 * last window for which we've gotten an
				 * Enter or Leave event from the server (i.e.
				 * the last window known to have contained
				 * the pointer).  Doesn't reflect events
				 * that were synthesized in tkGrab.c. */
    TkGrabEvent *firstGrabEventPtr;
				/* First in list of enter/leave events
				 * synthesized by grab code.  These events
				 * must be processed in order before any other
				 * events are processed.  NULL means no such
				 * events. */
    TkGrabEvent *lastGrabEventPtr;
				/* Last in list of synthesized events, or NULL
				 * if list is empty. */
    int grabFlags;		/* Miscellaneous flag values.  See definitions
				 * in tkGrab.c. */

    /*
     * Information used by tkXId.c only:
     */

    struct TkIdStack *idStackPtr;
				/* First in list of chunks of free resource
				 * identifiers, or NULL if there are no free
				 * resources. */
    XID (*defaultAllocProc) _ANSI_ARGS_((Display *display));
				/* Default resource allocator for display. */
    struct TkIdStack *windowStackPtr;
				/* First in list of chunks of window
				 * identifers that can't be reused right
				 * now. */
    int idCleanupScheduled;	/* 1 means a call to WindowIdCleanup has
				 * already been scheduled, 0 means it
				 * hasn't. */

    /*
     * Information maintained by tkWindow.c for use later on by tkXId.c:
     */


    int destroyCount;		/* Number of Tk_DestroyWindow operations
				 * in progress. */
    unsigned long lastDestroyRequest;
				/* Id of most recent XDestroyWindow request;
				 * can re-use ids in windowStackPtr when
				 * server has seen this request and event
				 * queue is empty. */

    /*
     * Information used by tkVisual.c only:
     */

    TkColormap *cmapPtr;	/* First in list of all non-default colormaps
				 * allocated for this display. */

    /*
     * Information used by tkFocus.c only:
     */

    struct TkWindow *focusWinPtr;
				/* Window that currently has the focus for
				 * this display, or NULL if none. */
    struct TkWindow *implicitWinPtr;
				/* If the focus arrived at a toplevel window
				 * implicitly via an Enter event (rather
				 * than via a FocusIn event), this points
				 * to the toplevel window.  Otherwise it is
				 * NULL. */
    struct TkWindow *focusOnMapPtr;
				/* This points to a toplevel window that is
				 * supposed to receive the X input focus as
				 * soon as it is mapped (needed to handle the
				 * fact that X won't allow the focus on an
				 * unmapped window).  NULL means no delayed
				 * focus op in progress. */
    int forceFocus;		/* Associated with focusOnMapPtr:  non-zero
				 * means claim the focus even if some other
				 * application currently has it. */

    /*
     * Used by tkColor.c only:
     */

    TkStressedCmap *stressPtr;	/* First in list of colormaps that have
				 * filled up, so we have to pick an
				 * approximate color. */

    /*
     * Miscellaneous information:
     */

#ifdef TK_USE_INPUT_METHODS
    XIM inputMethod;		/* Input method for this display */
#endif /* TK_USE_INPUT_METHODS */
    Tcl_HashTable winTable;	/* Maps from X window ids to TkWindow ptrs. */
} TkDisplay;

/*
 * One of the following structures exists for each error handler
 * created by a call to Tk_CreateErrorHandler.  The structure
 * is managed by tkError.c.
 */

typedef struct TkErrorHandler {
    TkDisplay *dispPtr;		/* Display to which handler applies. */
    unsigned long firstRequest;	/* Only errors with serial numbers
				 * >= to this are considered. */
    unsigned long lastRequest;	/* Only errors with serial numbers
				 * <= to this are considered.  This
				 * field is filled in when XUnhandle
				 * is called.  -1 means XUnhandle
				 * hasn't been called yet. */
    int error;			/* Consider only errors with this
				 * error_code (-1 means consider
				 * all errors). */
    int request;		/* Consider only errors with this
				 * major request code (-1 means
				 * consider all major codes). */
    int minorCode;		/* Consider only errors with this
				 * minor request code (-1 means
				 * consider all minor codes). */
    Tk_ErrorProc *errorProc;	/* Procedure to invoke when a matching
				 * error occurs.  NULL means just ignore
				 * errors. */
    ClientData clientData;	/* Arbitrary value to pass to
				 * errorProc. */
    struct TkErrorHandler *nextPtr;
				/* Pointer to next older handler for
				 * this display, or NULL for end of
				 * list. */
} TkErrorHandler;

/*
 * One of the following structures exists for each event handler
 * created by calling Tk_CreateEventHandler.  This information
 * is used by tkEvent.c only.
 */

typedef struct TkEventHandler {
    unsigned long mask;		/* Events for which to invoke
				 * proc. */
    Tk_EventProc *proc;		/* Procedure to invoke when an event
				 * in mask occurs. */
    ClientData clientData;	/* Argument to pass to proc. */
    struct TkEventHandler *nextPtr;
				/* Next in list of handlers
				 * associated with window (NULL means
				 * end of list). */
} TkEventHandler;

/*
 * Tk keeps one of the following data structures for each main
 * window (created by a call to Tk_CreateMainWindow).  It stores
 * information that is shared by all of the windows associated
 * with a particular main window.
 */

typedef struct TkMainInfo {
    int refCount;		/* Number of windows whose "mainPtr" fields
				 * point here.  When this becomes zero, can
				 * free up the structure (the reference
				 * count is zero because windows can get
				 * deleted in almost any order;  the main
				 * window isn't necessarily the last one
				 * deleted). */
    struct TkWindow *winPtr;	/* Pointer to main window. */
    Tcl_Interp *interp;		/* Interpreter associated with application. */
    Tcl_HashTable nameTable;	/* Hash table mapping path names to TkWindow
				 * structs for all windows related to this
				 * main window.  Managed by tkWindow.c. */
    Tk_BindingTable bindingTable;
				/* Used in conjunction with "bind" command
				 * to bind events to Tcl commands. */
    TkDisplay *curDispPtr;	/* Display for last binding command invoked
				 * in this application;  used only  by
				 * tkBind.c. */
    int curScreenIndex;		/* Index of screen for last binding command.
				 * Used only by tkBind.c. */
    int bindingDepth;		/* Number of active instances of Tk_BindEvent
				 * in this application.  Used only by
				 * tkBind.c. */
    struct TkFocusInfo *focusPtr;
				/* First in list of records containing focus
				 * information for each top-level in the
				 * application.  Used only by tkFocus.c. */
    unsigned long focusSerial;	/* Serial number of last request we made to
				 * change the focus.  Used to identify
				 * stale focus notifications coming from the
				 * X server. */
    struct TkWindow *lastFocusPtr;
				/* The most recent window that was given the
				 * focus via "focus" command.  Used to restore
				 * the focus when we get stale FocusIn
				 * events. */
    struct ElArray *optionRootPtr;
				/* Top level of option hierarchy for this
				 * main window.  NULL means uninitialized.
				 * Managed by tkOption.c. */
    Tcl_HashTable imageTable;	/* Maps from image names to Tk_ImageMaster
				 * structures.  Managed by tkImage.c. */
    int strictMotif;		/* This is linked to the tk_strictMotif
				 * global variable. */
    struct TkMainInfo *nextPtr;	/* Next in list of all main windows managed by
				 * this process. */
} TkMainInfo;

/*
 * Tk keeps one of the following structures for each window.
 * Some of the information (like size and location) is a shadow
 * of information managed by the X server, and some is special
 * information used here, such as event and geometry management
 * information.  This information is (mostly) managed by tkWindow.c.
 * WARNING: the declaration below must be kept consistent with the
 * Tk_FakeWin structure in tk.h.  If you change one, be sure to
 * change the other!!
 */

typedef struct TkWindow {

    /*
     * Structural information:
     */

    Display *display;		/* Display containing window. */
    TkDisplay *dispPtr;		/* Tk's information about display
				 * for window. */
    int screenNum;		/* Index of screen for window, among all
				 * those for dispPtr. */
    Visual *visual;		/* Visual to use for window.  If not default,
				 * MUST be set before X window is created. */
    int depth;			/* Number of bits/pixel. */
    Window window;		/* X's id for window.   NULL means window
				 * hasn't actually been created yet, or it's
				 * been deleted. */
    struct TkWindow *childList;	/* First in list of child windows,
				 * or NULL if no children. */
    struct TkWindow *lastChildPtr;
				/* Last in list of child windows, or NULL
				 * if no children. */
    struct TkWindow *parentPtr;	/* Pointer to parent window (logical
				 * parent, not necessarily X parent).  NULL
				 * means either this is the main window, or
				 * the window's parent has already been
				 * deleted. */
    struct TkWindow *nextPtr;	/* Next in list of children with
				 * same parent (NULL if end of
				 * list). */
    TkMainInfo *mainPtr;	/* Information shared by all windows
				 * associated with a particular main
				 * window.  NULL means this window is
				 * a rogue that isn't associated with
				 * any application (at present, this
				 * only happens for the dummy windows
				 * used for "send" communication).  */

    /*
     * Name and type information for the window:
     */

    char *pathName;		/* Path name of window (concatenation
				 * of all names between this window and
				 * its top-level ancestor).  This is a
				 * pointer into an entry in
				 * mainPtr->nameTable.  NULL means that
				 * the window hasn't been completely
				 * created yet. */
    Tk_Uid nameUid;		/* Name of the window within its parent
				 * (unique within the parent). */
    Tk_Uid classUid;		/* Class of the window.  NULL means window
				 * hasn't been given a class yet. */

    /*
     * Geometry and other attributes of window.  This information
     * may not be updated on the server immediately;  stuff that
     * hasn't been reflected in the server yet is called "dirty".
     * At present, information can be dirty only if the window
     * hasn't yet been created.
     */

    XWindowChanges changes;	/* Geometry and other info about
				 * window. */
    unsigned int dirtyChanges;	/* Bits indicate fields of "changes"
				 * that are dirty. */
    XSetWindowAttributes atts;	/* Current attributes of window. */
    unsigned long dirtyAtts;	/* Bits indicate fields of "atts"
				 * that are dirty. */

    unsigned int flags;		/* Various flag values:  these are all
				 * defined in tk.h (confusing, but they're
				 * needed there for some query macros). */

    /*
     * Information kept by the event manager (tkEvent.c):
     */

    TkEventHandler *handlerList;/* First in list of event handlers
				 * declared for this window, or
				 * NULL if none. */
#ifdef TK_USE_INPUT_METHODS
    XIC inputContext;		/* Input context (for input methods). */
#endif /* TK_USE_INPUT_METHODS */

    /*
     * Information used for event bindings (see "bind" and "bindtags"
     * commands in tkCmds.c):
     */

    ClientData *tagPtr;		/* Points to array of tags used for bindings
				 * on this window.  Each tag is a Tk_Uid.
				 * Malloc'ed.  NULL means no tags. */
    int numTags;		/* Number of tags at *tagPtr. */

    /*
     * Information used by tkOption.c to manage options for the
     * window.
     */

    int optionLevel;		/* -1 means no option information is
				 * currently cached for this window.
				 * Otherwise this gives the level in
				 * the option stack at which info is
				 * cached. */
    /*
     * Information used by tkSelect.c to manage the selection.
     */

    struct TkSelHandler *selHandlerList;
				/* First in list of handlers for
				 * returning the selection in various
				 * forms. */

    /*
     * Information used by tkGeometry.c for geometry management.
     */

    Tk_GeomMgr *geomMgrPtr;	/* Information about geometry manager for
				 * this window. */
    ClientData geomData;	/* Argument for geometry manager procedures. */
    int reqWidth, reqHeight;	/* Arguments from last call to
				 * Tk_GeometryRequest, or 0's if
				 * Tk_GeometryRequest hasn't been
				 * called. */
    int internalBorderWidth;	/* Width of internal border of window
				 * (0 means no internal border).  Geometry
				 * managers should not normally place children
				 * on top of the border. */

    /*
     * Information maintained by tkWm.c for window manager communication.
     */

    struct TkWmInfo *wmInfoPtr;	/* For top-level windows, points to
				 * structure with wm-related info (see
				 * tkWm.c).  For other windows, this
				 * is NULL. */
} TkWindow;

/*
 * Pointer to first entry in list of all displays currently known.
 */

extern TkDisplay *tkDisplayList;

/*
 * Flags passed to TkMeasureChars:
 */

#define TK_WHOLE_WORDS		 1
#define TK_AT_LEAST_ONE		 2
#define TK_PARTIAL_OK		 4
#define TK_NEWLINES_NOT_SPECIAL	 8
#define TK_IGNORE_TABS		16

/*
 * Return values from TkGrabState:
 */

#define TK_GRAB_NONE		0
#define TK_GRAB_IN_TREE		1
#define TK_GRAB_ANCESTOR	2
#define TK_GRAB_EXCLUDED	3

/*
 * Location of library directory containing Tk scripts.  This value
 * is put in the $tkLibrary variable for each application.
 */

#ifndef TK_LIBRARY
#define TK_LIBRARY "/usr/local/lib/tk"
#endif

/*
 * Special flag to pass to Tk_CreateFileHandler to indicate that
 * the file descriptor is actually for a display, not a file, and
 * should be treated specially.  Make sure that this value doesn't
 * conflict with TK_READABLE, TK_WRITABLE, or TK_EXCEPTION from tk.h.
 */

#define TK_IS_DISPLAY	32

/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */

#define UCHAR(c) ((unsigned char) (c))

/*
 * Miscellaneous variables shared among Tk modules but not exported
 * to the outside world:
 */

extern Tk_Uid			tkActiveUid;
extern Tk_ImageType		tkBitmapImageType;
extern void			(*tkDelayedEventProc) _ANSI_ARGS_((void));
extern Tk_Uid			tkDisabledUid;
extern Tk_PhotoImageFormat	tkImgFmtGIF;
extern Tk_PhotoImageFormat	tkImgFmtPPM;
extern TkMainInfo		*tkMainWindowList;
extern Tk_Uid			tkNormalUid;
extern Tk_ImageType		tkPhotoImageType;
extern int			tkSendSerial;

/*
 * Internal procedures shared among Tk modules but not exported
 * to the outside world:
 */

extern int		TkAreaToPolygon _ANSI_ARGS_((double *polyPtr,
			    int numPoints, double *rectPtr));
extern void		TkBezierPoints _ANSI_ARGS_((double control[],
			    int numSteps, double *coordPtr));
extern void		TkBezierScreenPoints _ANSI_ARGS_((Tk_Canvas canvas,
			    double control[], int numSteps,
			    XPoint *xPointPtr));
extern void		TkBindEventProc _ANSI_ARGS_((TkWindow *winPtr,
			    XEvent *eventPtr));
extern int		TkClipInit _ANSI_ARGS_((Tcl_Interp *interp,
			    TkDisplay *dispPtr));
extern int		TkCmapStressed _ANSI_ARGS_((Tk_Window tkwin,
			    Colormap colormap));
extern void		TkComputeTextGeometry _ANSI_ARGS_((
			    XFontStruct *fontStructPtr, char *string,
			    int numChars, int wrapLength, int *widthPtr,
			    int *heightPtr));
extern int		TkCopyAndGlobalEval _ANSI_ARGS_((Tcl_Interp *interp,
			    char *script));
extern Time		TkCurrentTime _ANSI_ARGS_((TkDisplay *dispPtr));
extern int		TkDeadAppCmd _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, int argc, char **argv));
extern void		TkDeleteAllImages _ANSI_ARGS_((TkMainInfo *mainPtr));
extern void		TkDisplayChars _ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc,
			    XFontStruct *fontStructPtr, char *string,
			    int numChars, int x, int y, int tabOrigin,
			    int flags));
extern void		TkDisplayText _ANSI_ARGS_((Display *display,
			    Drawable drawable, XFontStruct *fontStructPtr,
			    char *string, int numChars, int x, int y,
			    int length, Tk_Justify justify, int underline,
			    GC gc));
extern void		TkEventCleanupProc _ANSI_ARGS_((
			    ClientData clientData, Tcl_Interp *interp));
extern void		TkEventDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkFillPolygon _ANSI_ARGS_((Tk_Canvas canvas,
			    double *coordPtr, int numPoints, Display *display,
			    Drawable drawable, GC gc, GC outlineGC));
extern void		TkFocusDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern int		TkFocusFilterEvent _ANSI_ARGS_((TkWindow *winPtr,
			    XEvent *eventPtr));
extern void		TkFreeBindingTags _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkFreeWindowId _ANSI_ARGS_((TkDisplay *dispPtr,
			    Window w));
extern void		TkGetButtPoints _ANSI_ARGS_((double p1[], double p2[],
			    double width, int project, double m1[],
			    double m2[]));
extern TkDisplay *	TkGetDisplay _ANSI_ARGS_((Display *display));
extern TkWindow *	TkGetFocus _ANSI_ARGS_((TkWindow *winPtr));
extern int		TkGetInterpNames _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin));
extern int		TkGetMiterPoints _ANSI_ARGS_((double p1[], double p2[],
			    double p3[], double width, double m1[],
			    double m2[]));
extern void		TkGetPointerCoords _ANSI_ARGS_((Tk_Window tkwin,
			    int *xPtr, int *yPtr));
extern void		TkGetServerInfo _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin));
extern void		TkGrabDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern int		TkGrabState _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkGrabTriggerProc _ANSI_ARGS_((XEvent *eventPtr));
extern void		TkIncludePoint _ANSI_ARGS_((Tk_Item *itemPtr,
			    double *pointPtr));
extern char *		TkInitFrame _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, int toplevel, int argc,
			    char *argv[]));
extern void		TkInitXId _ANSI_ARGS_((TkDisplay *dispPtr));
extern void		TkInOutEvents _ANSI_ARGS_((XEvent *eventPtr,
			    TkWindow *sourcePtr, TkWindow *destPtr,
			    int leaveType, int EnterType));
extern int		TkLineToArea _ANSI_ARGS_((double end1Ptr[2],
			    double end2Ptr[2], double rectPtr[4]));
extern double		TkLineToPoint _ANSI_ARGS_((double end1Ptr[2],
			    double end2Ptr[2], double pointPtr[2]));
extern int		TkMakeBezierCurve _ANSI_ARGS_((Tk_Canvas canvas,
			    double *pointPtr, int numPoints, int numSteps,
			    XPoint xPoints[], double dblPoints[]));
extern void		TkMakeBezierPostscript _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Canvas canvas, double *pointPtr,
			    int numPoints));
extern int		TkMeasureChars _ANSI_ARGS_((XFontStruct *fontStructPtr,
			    char *source, int maxChars, int startX, int maxX,
			    int tabOrigin, int flags, int *nextXPtr));
extern void		TkOptionClassChanged _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkOptionDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern int		TkOvalToArea _ANSI_ARGS_((double *ovalPtr,
			    double *rectPtr));
extern double		TkOvalToPoint _ANSI_ARGS_((double ovalPtr[4],
			    double width, int filled, double pointPtr[2]));
extern int		TkPointerEvent _ANSI_ARGS_((XEvent *eventPtr,
			    TkWindow *winPtr));
extern int		TkPolygonToArea _ANSI_ARGS_((double *polyPtr,
			    int numPoints, double *rectPtr));
extern double		TkPolygonToPoint _ANSI_ARGS_((double *polyPtr,
			    int numPoints, double *pointPtr));
extern void		TkQueueEvent _ANSI_ARGS_((TkDisplay *dispPtr,
			    XEvent *eventPtr));
extern void		TkSelDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkSelEventProc _ANSI_ARGS_((Tk_Window tkwin,
			    XEvent *eventPtr));
extern void		TkSelInit _ANSI_ARGS_((Tk_Window tkwin));
extern void		TkSelPropProc _ANSI_ARGS_((XEvent *eventPtr));
extern int		TkThickPolyLineToArea _ANSI_ARGS_((double *coordPtr,
			    int numPoints, double width, int capStyle,
			    int joinStyle, double *rectPtr));
extern void		TkUnderlineChars _ANSI_ARGS_((Display *display,
			    Drawable drawable, GC gc,
			    XFontStruct *fontStructPtr, char *string,
			    int x, int y, int tabOrigin, int flags,
			    int firstChar, int lastChar));
extern void		TkWmAddToColormapWindows _ANSI_ARGS_((
			    TkWindow *winPtr));
extern void		TkWmDeadWindow _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkWmMapWindow _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkWmNewWindow _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkWmProtocolEventProc _ANSI_ARGS_((TkWindow *winPtr,
			    XEvent *evenvPtr));
extern void		TkWmRestackToplevel _ANSI_ARGS_((TkWindow *winPtr,
			    int aboveBelow, TkWindow *otherPtr));
extern void		TkWmSetClass _ANSI_ARGS_((TkWindow *winPtr));
extern void		TkWmUnmapWindow _ANSI_ARGS_((TkWindow *winPtr));
extern int		TkXFileProc _ANSI_ARGS_((ClientData clientData,
			    int mask, int flags));

#endif  /* _TKINT */
