/* $Id: togl.c,v 1.2 2003-08-13 05:49:46 juliensf Exp $ */

/*
 * Togl - a Tk OpenGL widget
 * Version 1.5
 * Copyright (C) 1996-1997  Brian Paul and Ben Bederson
 * See the LICENSE file for copyright details.
 */


/*
 * $Log: togl.c,v $
 * Revision 1.2  2003-08-13 05:49:46  juliensf
 * Estimated hours taken: 14.
 * Branches: main.
 *
 * Remove dependency between mercury_opengl and mercury_tcltk by making
 * mtogl a separate library.
 *
 * Modify togl so that it works with Tcl/Tk 8.0.5
 * (XXX It doesn't work (easily) with versions of Tcl/Tk greater than 8.0,
 *  we probably ought to consider using togl 1.6 at some point).
 *
 * Make a number of small changes (mainly adding casts and missing MR_* prefixes)
 * to prevent compiler warnings.
 *
 * extras/graphics/README:
 * 	Mention the `mtogl' package.
 *
 * extras/graphics/mercury_opengl/mercury_opengl.m:
 * extras/graphics/mercury_opengl/mglu.m:
 * extras/graphics/mercury_opengl/mogl.m:
 * 	Remove mtogl from mercury_opengl package.
 * 	Define quadric using pragma foreign_type
 * 	Add some missing MR_* prefixes.
 *
 * extras/graphics/mercury_opengl/mtogl:
 * 	Remove references to header files that are no longer generated
 * 	by the compiler.
 * 	Use the new foreign language interface.
 * 	Add some missing MR_* prefixes.
 *
 * extras/graphics/mercury_opengl/togl.c:
 * 	Allow the use of tk version 8.0.5
 * extras/graphics/mercury_opengl/tkInt8.0p5.h:
 * 	New file.  (This is a part of tk)
 *
 * extras/graphics/mercury_opengl/Mmakefile:
 * 	Update mmakefile to reflect above changes.
 *
 * extras/graphics/mercury_opengl/README:
 * 	Mention mtogl.
 *
 * extras/graphics/samples/calc/calc.m:
 * 	Add some missing underscores; this prevents compiler warnings
 * 	when compiling with intermodule-optimization enabled.
 *
 * extras/graphics/samples/maze/Mmakefile:
 * extras/graphics/samples/pent/Mmakefile:
 * 	Update mmakefiles to reflect above changes.
 *
 * Revision 1.1  1998/03/10 06:32:02  trd
 * Estimated hours taken: 3
 *
 * Add mercury_opengl as a library.
 *
 * extras/graphics/mercury_opengl/COPYING.LIB:
 * extras/graphics/mercury_opengl/HOWTO:
 * extras/graphics/mercury_opengl/LICENSE.TK:
 * extras/graphics/mercury_opengl/LICENSE.TOGL:
 * extras/graphics/mercury_opengl/Mmakefile:
 * extras/graphics/mercury_opengl/README:
 * extras/graphics/mercury_opengl/mercury_opengl.m:
 * extras/graphics/mercury_opengl/mglu.m:
 * extras/graphics/mercury_opengl/mogl.m:
 * extras/graphics/mercury_opengl/mtogl.m:
 * extras/graphics/mercury_opengl/tkConsole.c:
 * extras/graphics/mercury_opengl/tkFont.h:
 * extras/graphics/mercury_opengl/tkInt4.0.h:
 * extras/graphics/mercury_opengl/tkInt4.1.h:
 * extras/graphics/mercury_opengl/tkInt4.2.h:
 * extras/graphics/mercury_opengl/tkInt8.0.h:
 * extras/graphics/mercury_opengl/tkInt8.0p2.h:
 * extras/graphics/mercury_opengl/tkPort.h:
 * extras/graphics/mercury_opengl/tkWin.h:
 * extras/graphics/mercury_opengl/tkWinInt.h:
 * extras/graphics/mercury_opengl/tkWinPort.h:
 * extras/graphics/mercury_opengl/togl.c:
 * extras/graphics/mercury_opengl/togl.h:
 * 	Same old .m files (except mercury_opengl.m), new Mmakefile,
 * 	new LICENSEs, updated version of Togl.
 *
 * 	The old .m files will be removed from the extras/graphics
 * 	directory.
 *
 *
 * Estimated hours taken: _____
 *
 * <overview or general description of changes>
 *
 * <directory>/<file>:
 * 	<detailed description of changes>
 *
 * Revision 1.40  1997/12/13 02:27:46  brianp
 * only call Tcl_DeleteCommandFromToken() is using Tcl/Tk 8.0 or later
 *
 * Revision 1.39  1997/12/13 02:26:02  brianp
 * test for STEREO to enable SGI stereo code
 * general code clean-up
 *
 * Revision 1.38  1997/12/11 02:21:18  brianp
 * added support for Tcl/Tk 8.0p2
 *
 * Revision 1.37  1997/11/15 04:11:30  brianp
 * added Adrian J. Chung's widget destroy code
 *
 * Revision 1.36  1997/11/15 03:33:13  brianp
 * fixed multi-expose/redraw problem (Andy Colebourne)
 *
 * Revision 1.35  1997/11/15 03:28:42  brianp
 * removed code in Togl_Configure(), added for stereo, that caused a new bug
 *
 * Revision 1.34  1997/11/15 02:58:48  brianp
 * added Togl_TkWin() per Glenn Lewis
 *
 * Revision 1.33  1997/10/01 02:50:57  brianp
 * added SGI stereo functions from Ben Evans
 *
 * Revision 1.32  1997/10/01 00:25:22  brianp
 * made small change for HP compilation (Glenn Lewis)
 *
 * Revision 1.31  1997/09/17 02:41:07  brianp
 * added Geza Groma's Windows NT/95 patches
 *
 * Revision 1.30  1997/09/12 01:21:05  brianp
 * a few more tweaks for Windows compilation
 *
 * Revision 1.29  1997/09/09 23:57:51  brianp
 * avoid unneeded ConfigureNotify work (Glenn Lewis)
 *
 * Revision 1.28  1997/09/09 23:52:53  brianp
 * made a few small changes to stop C++ compiler warnings
 *
 * Revision 1.27  1997/08/26 02:05:19  brianp
 * added Togl_ResetDefaultCallbacks() and Togl_ClientData() (Greg Couch)
 *
 * Revision 1.26  1997/08/26 01:55:47  brianp
 * added many config flags (Matthias Ott)
 *
 * Revision 1.25  1997/08/26 01:35:41  brianp
 * added Togl_Set*Func() functions from Matthias Ott
 *
 * Revision 1.24  1997/08/22 02:49:31  brianp
 * misc changes for Tcl/Tk 8.0
 *
 * Revision 1.23  1997/07/29 02:13:20  brianp
 * integrated Robert Casto's initial NT code
 *
 * Revision 1.22  1997/05/03 02:22:22  brianp
 * only compile in Mesa color management if MESA_COLOR_HACK defined
 *
 * Revision 1.21  1997/04/11 01:37:22  brianp
 * added Togl_TimerFunc() and related code from Elmar Gerwalin
 *
 * Revision 1.20  1997/04/11 01:08:30  brianp
 * fixed bug in Togl_PostOverlayRedisplay, per Shalini Venkataraman
 *
 * Revision 1.19  1997/03/07 01:25:55  brianp
 * fixed overlay code so togl doesn't crash if can't get overlay
 *
 * Revision 1.18  1997/02/27 19:56:18  brianp
 * replaced _R, _G, and _B symbols with RLEVELS, GLEVELS, and BLEVELS
 *
 * Revision 1.17  1997/02/19 10:10:02  brianp
 * if compiling as C++ test visual->c_class instead of visual->class
 *
 * Revision 1.16  1997/02/16 01:26:00  brianp
 * added new overlay and EPS functions from Miguel A. De Riera Pasenau
 *
 * Revision 1.15  1997/02/16 00:24:29  brianp
 * don't call Tcl_PkgProvide() if version less than 7.4
 *
 * Revision 1.14  1997/01/06 21:00:22  brianp
 * use NO_TK_CURSOR to omit cursor code when using Tk 4.0
 *
 * Revision 1.13  1996/12/13 21:24:41  brianp
 * added Togl_DestroyFunc() contributed by scotter@iname.com
 *
 * Revision 1.12  1996/11/14 00:49:38  brianp
 * added Togl_Get/SetClientData() functions
 *
 * Revision 1.11  1996/11/05 02:38:52  brianp
 * added Togl_UnloadBitmapFont()
 * added Ramon Ramsan's Nov 4 patches for overlay plane support
*
 * Revision 1.10  1996/10/25 03:44:23  brianp
 * more code cleanup
 *
 * Revision 1.9  1996/10/25 00:50:45  brianp
 * added XSetWMColormapWindows() call
 *
 * Revision 1.8  1996/10/25 00:46:49  brianp
 * added call to Tcl_PkgProvide() in Togl_Init() per Karel Zuiderveld
 *
 * Revision 1.7  1996/10/25 00:35:26  brianp
 * fixed a bug in Togl_LoadBitmapFont()
 *
 * Revision 1.6  1996/10/24 01:20:42  brianp
 * added overlay functions, not all implemented yet
 *
 * Revision 1.5  1996/10/24 00:13:50  brianp
 * added const qualifier to many function parameters
 * renamed Togl struct members
 *
 * Revision 1.4  1996/10/23 23:59:21  brianp
 * general code cleanup
 *
 * Revision 1.3  1996/10/23 23:48:53  brianp
 * fixed wrong depth argument given to Tk_SetWindowVisual()
 * fixed potential bug in noFaultXAllocColor()
 *
 * Revision 1.2  1996/10/23 23:29:48  brianp
 * added Togl_LoadBitmapFont()
 * added swapbuffers and makecurrent commands
 * added -cursor config option
 *
 * Revision 1.1  1996/10/23 23:15:16  brianp
 * Initial revision
 *
 */


/*
 * Currently support X11 and WIN32
 */
#ifndef WIN32
#define X11
#endif


/*** Windows headers ***/
#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
#include <winnt.h>
#endif /* WIN32 */

/*** Standard C headers ***/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*** X Window System headers ***/
#ifdef X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* for XA_RGB_DEFAULT_MAP atom */
#if defined(__vms)
#include <X11/StdCmap.h>  /* for XmuLookupStandardColormap */
#else
#include <X11/Xmu/StdCmap.h>  /* for XmuLookupStandardColormap */
#endif
#include <GL/glx.h>
#endif /*X11*/

/*** Tcl/Tk headers ***/
#ifdef X11
#ifndef _TKPORT
#define _TKPORT  /* This eliminates need to include a bunch of Tk baggage */
#endif /* X11 */
#endif
#include <tcl.h>
#include <tk.h>
#if defined(X11)
#if TK_MAJOR_VERSION==4 && TK_MINOR_VERSION==0
#  include "tkInt4.0.h"
#  define NO_TK_CURSOR
#elif TK_MAJOR_VERSION==4 && TK_MINOR_VERSION==1
#  include "tkInt4.1.h"
#elif TK_MAJOR_VERSION==4 && TK_MINOR_VERSION==2
#  include "tkInt4.2.h"
#elif TK_MAJOR_VERSION==8 && TK_MINOR_VERSION==0 && TK_RELEASE_SERIAL==0
#  include "tkInt8.0.h"
#elif TK_MAJOR_VERSION==8 && TK_MINOR_VERSION==0 && TK_RELEASE_SERIAL==2
#  include "tkInt8.0p2.h"
#elif TK_MAJOR_VERSION==8 && TK_MINOR_VERSION==0 && TK_RELEASE_SERIAL==5
#  include "tkInt8.0p5.h"
#else
   Sorry, you will have to edit togl.c to include the right tkInt.h file
#endif
#elif defined(WIN32)
#if TK_MAJOR_VERSION<8
   Sorry Windows version requires Tcl/Tk ver 8.0 or higher.
#endif
#include "tkInt.h"
#include "tkWinInt.h"
#endif /* X11 */
#include "togl.h"



/* Defaults */
#define DEFAULT_WIDTH		"400"
#define DEFAULT_HEIGHT		"400"
#define DEFAULT_IDENT		""
#define DEFAULT_FONTNAME	"fixed"
#define DEFAULT_TIME		"1"


#ifdef WIN32
/* maximum size of a logical palette corresponding to a colormap in color index mode */
#define MAX_CI_COLORMAP_SIZE 4096
#endif /* WIN32 */

#define MAX(a,b)	(((a)>(b))?(a):(b))

#define TCL_ERR(interp, string) {Tcl_ResetResult(interp);		\
			         Tcl_AppendResult(interp, string, NULL);\
				 return(TCL_ERROR);}

#define ALL_EVENTS_MASK \
    KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|	\
    EnterWindowMask|LeaveWindowMask|PointerMotionMask|ExposureMask|	\
    VisibilityChangeMask|FocusChangeMask|PropertyChangeMask|ColormapChangeMask


struct Togl
{
#if defined(WIN32)
   HDC tglGLHdc;                /* Device context of device that OpenGL calls will be drawn on */
   HGLRC tglGLHglrc;            /* OpenGL rendering context to be made current */
   int CiColormapSize;          /* (Maximum) size of colormap in color index mode */
#elif defined(X11)
   GLXContext GlCtx;		/* Normal planes GLX context */
#endif /* WIN32 */
   Display *display;		/* X's token for the window's display. */
   Tk_Window  TkWin;		/* Tk window structure */
   Tcl_Interp *Interp;		/* Tcl interpreter */
   Tcl_Command widgetCmd;       /* Token for togl's widget command */
#ifndef NO_TK_CURSOR
   Tk_Cursor Cursor;		/* The widget's cursor */
#endif
   int Width, Height;		/* Dimensions of window */
   int Time;			/* Time value for timer */
   Tcl_TimerToken timerHandler; /* Token for togl's timer handler */

   int RgbaFlag;		/* configuration flags (ala GLX parameters) */
   int RgbaRed;
   int RgbaGreen;
   int RgbaBlue;
   int DoubleFlag;
   int DepthFlag;
   int DepthSize;
   int AccumFlag;
   int AccumRed;
   int AccumGreen;
   int AccumBlue;
   int AccumAlpha;
   int AlphaFlag;
   int AlphaSize;
   int StencilFlag;
   int StencilSize;
   int PrivateCmapFlag;
   int OverlayFlag;
   int StereoFlag;
   int AuxNumber;

   char *Ident;				/* User's identification string */
   ClientData Client_Data;		/* Pointer to user data */

   GLboolean UpdatePending;		/* Should normal planes be redrawn? */

   Togl_Callback *CreateProc;		/* Callback when widget is created */
   Togl_Callback *DisplayProc;		/* Callback when widget is rendered */
   Togl_Callback *ReshapeProc;		/* Callback when window size changes */
   Togl_Callback *DestroyProc;		/* Callback when widget is destroyed */
   Togl_Callback *TimerProc;		/* Callback when widget is idle */

   /* Overlay stuff */
#if defined(X11)
   GLXContext OverlayCtx;		/* Overlay planes OpenGL context */
#elif defined(WIN32)
   HGLRC tglGLOverlayHglrc;
#endif /* X11 */
   Window OverlayWindow;		/* The overlay window, or 0 */
   Togl_Callback *OverlayDisplayProc;	/* Overlay redraw proc */
   GLboolean OverlayUpdatePending;	/* Should overlay be redrawn? */
   Colormap OverlayCmap;		/* colormap for overlay is created */
   int OverlayTransparentPixel;		/* transparent pixel */
   int OverlayIsMapped;

   /* for DumpToEpsFile: Added by Miguel A. de Riera Pasenau 10.01.1997 */
   XVisualInfo *EpsVisual;		/* Visual info of the current */
					/* context needed for DumpToEpsFile */
   GLfloat *EpsRedMap;		/* Index2RGB Maps for Color index modes */
   GLfloat *EpsGreenMap;
   GLfloat *EpsBlueMap;
   GLint EpsMapSize;            	/* = Number of indices in our Togl */
};



/* NTNTNT need to change to handle Windows Data Types */
/*
 * Prototypes for functions local to this file
 */
static int Togl_Cmd(ClientData clientData, Tcl_Interp *interp,
                    int argc, char **argv);
static void Togl_EventProc(ClientData clientData, XEvent *eventPtr);
static int Togl_MakeWindowExist(struct Togl *togl);
#ifdef MESA_COLOR_HACK
static int get_free_color_cells( Display *display, int screen,
                                 Colormap colormap);
static void free_default_color_cells( Display *display, Colormap colormap);
#endif
static void ToglCmdDeletedProc( ClientData );



#if defined(__sgi) && defined(STEREO)
/* SGI-only stereo */
static void stereoMakeCurrent( Display *dpy, Window win, GLXContext ctx );
static void stereoInit( struct Togl *togl,int stereoEnabled );
#endif



/*
 * Setup Togl widget configuration options:
 */

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
     DEFAULT_HEIGHT, Tk_Offset(struct Togl, Height), 0, NULL},

    {TK_CONFIG_PIXELS, "-width", "width", "Width",
     DEFAULT_WIDTH, Tk_Offset(struct Togl, Width), 0, NULL},

    {TK_CONFIG_STRING, "-ident", "ident", "Ident",
     DEFAULT_IDENT, Tk_Offset(struct Togl, Ident), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-rgba", "rgba", "Rgba",
     "true", Tk_Offset(struct Togl, RgbaFlag), 0, NULL},

    {TK_CONFIG_INT, "-redsize", "redsize", "RedSize",
     "1", Tk_Offset(struct Togl, RgbaRed), 0, NULL},

    {TK_CONFIG_INT, "-greensize", "greensize", "GreenSize",
     "1", Tk_Offset(struct Togl, RgbaGreen), 0, NULL},

    {TK_CONFIG_INT, "-bluesize", "bluesize", "BlueSize",
     "1", Tk_Offset(struct Togl, RgbaBlue), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-double", "double", "Double",
     "false", Tk_Offset(struct Togl, DoubleFlag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-depth", "depth", "Depth",
     "false", Tk_Offset(struct Togl, DepthFlag), 0, NULL},

    {TK_CONFIG_INT, "-depthsize", "depthsize", "DepthSize",
     "1", Tk_Offset(struct Togl, DepthSize), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-accum", "accum", "Accum",
     "false", Tk_Offset(struct Togl, AccumFlag), 0, NULL},

    {TK_CONFIG_INT, "-accumredsize", "accumredsize", "AccumRedSize",
     "1", Tk_Offset(struct Togl, AccumRed), 0, NULL},

    {TK_CONFIG_INT, "-accumgreensize", "accumgreensize", "AccumGreenSize",
     "1", Tk_Offset(struct Togl, AccumGreen), 0, NULL},

    {TK_CONFIG_INT, "-accumbluesize", "accumbluesize", "AccumBlueSize",
     "1", Tk_Offset(struct Togl, AccumBlue), 0, NULL},

    {TK_CONFIG_INT, "-accumalphasize", "accumalphasize", "AccumAlphaSize",
     "1", Tk_Offset(struct Togl, AccumAlpha), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-alpha", "alpha", "Alpha",
     "false", Tk_Offset(struct Togl, AlphaFlag), 0, NULL},

    {TK_CONFIG_INT, "-alphasize", "alphasize", "AlphaSize",
     "1", Tk_Offset(struct Togl, AlphaSize), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-stencil", "stencil", "Stencil",
     "false", Tk_Offset(struct Togl, StencilFlag), 0, NULL},

    {TK_CONFIG_INT, "-stencilsize", "stencilsize", "StencilSize",
     "1", Tk_Offset(struct Togl, StencilSize), 0, NULL},

    {TK_CONFIG_INT, "-auxbuffers", "auxbuffers", "AuxBuffers",
     "0", Tk_Offset(struct Togl, AuxNumber), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-privatecmap", "privateCmap", "PrivateCmap",
     "false", Tk_Offset(struct Togl, PrivateCmapFlag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-overlay", "overlay", "Overlay",
     "false", Tk_Offset(struct Togl, OverlayFlag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-stereo", "stereo", "Stereo",
     "false", Tk_Offset(struct Togl, StereoFlag), 0, NULL},

#ifndef NO_TK_CURSOR
    { TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
     "", Tk_Offset(struct Togl, Cursor), TK_CONFIG_NULL_OK },
#endif

    {TK_CONFIG_INT, "-time", "time", "Time",
     DEFAULT_TIME, Tk_Offset(struct Togl, Time), 0, NULL},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, 0, NULL}
};


static Togl_Callback *CreateProc = NULL;
static Togl_Callback *DisplayProc = NULL;
static Togl_Callback *ReshapeProc = NULL;
static Togl_Callback *DestroyProc = NULL;
static Togl_Callback *OverlayDisplayProc = NULL;
static Togl_Callback *TimerProc = NULL;
static ClientData DefaultClientData = NULL;
static Tcl_HashTable CommandTable;



#if defined(X11)
/*
 * Return an X colormap to use for OpenGL RGB-mode rendering.
 * Input:  dpy - the X display
 *         scrnum - the X screen number
 *         visinfo - the XVisualInfo as returned by glXChooseVisual()
 * Return:  an X Colormap or 0 if there's a _serious_ error.
 */
static Colormap
get_rgb_colormap( Display *dpy, int scrnum, XVisualInfo *visinfo )
{
   Atom hp_cr_maps;
   Status status;
   int numCmaps;
   int i;
   XStandardColormap *standardCmaps;
   Window root = RootWindow(dpy,scrnum);
   int using_mesa;

   /*
    * First check if visinfo's visual matches the default/root visual.
    */
   if (visinfo->visual==DefaultVisual(dpy,scrnum)) {
      /* use the default/root colormap */
      Colormap cmap;
      cmap = DefaultColormap( dpy, scrnum );
#ifdef MESA_COLOR_HACK
      (void) get_free_color_cells( dpy, scrnum, cmap);
#endif
      return cmap;
   }

   /*
    * Check if we're using Mesa.
    */
   if (strstr(glXQueryServerString( dpy, scrnum, GLX_VERSION ), "Mesa")) {
      using_mesa = 1;
   }
   else {
      using_mesa = 0;
   }

   /*
    * Next, if we're using Mesa and displaying on an HP with the "Color
    * Recovery" feature and the visual is 8-bit TrueColor, search for a
    * special colormap initialized for dithering.  Mesa will know how to
    * dither using this colormap.
    */
   if (using_mesa) {
      hp_cr_maps = XInternAtom( dpy, "_HP_RGB_SMOOTH_MAP_LIST", True );
      if (hp_cr_maps
#ifdef __cplusplus
	  && visinfo->visual->c_class==TrueColor
#else
	  && visinfo->visual->class==TrueColor
#endif
	  && visinfo->depth==8) {
	 status = XGetRGBColormaps( dpy, root, &standardCmaps,
				    &numCmaps, hp_cr_maps );
	 if (status) {
	    for (i=0; i<numCmaps; i++) {
	       if (standardCmaps[i].visualid == visinfo->visual->visualid) {
                  Colormap cmap = standardCmaps[i].colormap;
                  XFree( standardCmaps );
		  return cmap;
	       }
	    }
            XFree(standardCmaps);
	 }
      }
   }

   /*
    * Next, try to find a standard X colormap.
    */
#ifndef SOLARIS_BUG
   status = XmuLookupStandardColormap( dpy, visinfo->screen,
				       visinfo->visualid, visinfo->depth,
				       XA_RGB_DEFAULT_MAP,
				       /* replace */ False, /* retain */ True);
   if (status == 1) {
      status = XGetRGBColormaps( dpy, root, &standardCmaps,
				 &numCmaps, XA_RGB_DEFAULT_MAP);
      if (status == 1) {
         for (i = 0; i < numCmaps; i++) {
	    if (standardCmaps[i].visualid == visinfo->visualid) {
               Colormap cmap = standardCmaps[i].colormap;
	       XFree(standardCmaps);
	       return cmap;
	    }
	 }
         XFree(standardCmaps);
      }
   }
#endif

   /*
    * If we get here, give up and just allocate a new colormap.
    */
   return XCreateColormap( dpy, root, visinfo->visual, AllocNone );
}
#elif defined(WIN32)

/* Code to create RGB palette is taken from the GENGL sample program
   of Win32 SDK */

static unsigned char threeto8[8] = {
    0, 0111>>1, 0222>>1, 0333>>1, 0444>>1, 0555>>1, 0666>>1, 0377
};

static unsigned char twoto8[4] = {
    0, 0x55, 0xaa, 0xff
};

static unsigned char oneto8[2] = {
    0, 255
};

static int defaultOverride[13] = {
    0, 3, 24, 27, 64, 67, 88, 173, 181, 236, 247, 164, 91
};

static PALETTEENTRY defaultPalEntry[20] = {
    { 0,   0,   0,    0 },
    { 0x80,0,   0,    0 },
    { 0,   0x80,0,    0 },
    { 0x80,0x80,0,    0 },
    { 0,   0,   0x80, 0 },
    { 0x80,0,   0x80, 0 },
    { 0,   0x80,0x80, 0 },
    { 0xC0,0xC0,0xC0, 0 },

    { 192, 220, 192,  0 },
    { 166, 202, 240,  0 },
    { 255, 251, 240,  0 },
    { 160, 160, 164,  0 },

    { 0x80,0x80,0x80, 0 },
    { 0xFF,0,   0,    0 },
    { 0,   0xFF,0,    0 },
    { 0xFF,0xFF,0,    0 },
    { 0,   0,   0xFF, 0 },
    { 0xFF,0,   0xFF, 0 },
    { 0,   0xFF,0xFF, 0 },
    { 0xFF,0xFF,0xFF, 0 }
};

static unsigned char
ComponentFromIndex(int i, UINT nbits, UINT shift)
{
    unsigned char val;

    val = (unsigned char) (i >> shift);
    switch (nbits) {

    case 1:
        val &= 0x1;
        return oneto8[val];

    case 2:
        val &= 0x3;
        return twoto8[val];

    case 3:
        val &= 0x7;
        return threeto8[val];

    default:
        return 0;
    }
}

static Colormap Win32CreateRgbColormap(PIXELFORMATDESCRIPTOR pfd)
{
    TkWinColormap *cmap = (TkWinColormap *) ckalloc(sizeof(TkWinColormap));
    LOGPALETTE *pPal;
    int n, i;

    n = 1 << pfd.cColorBits;
    pPal = (PLOGPALETTE)LocalAlloc(LMEM_FIXED, sizeof(LOGPALETTE) +
            n * sizeof(PALETTEENTRY));
    pPal->palVersion = 0x300;
    pPal->palNumEntries = n;
    for (i=0; i<n; i++) {
        pPal->palPalEntry[i].peRed =
                ComponentFromIndex(i, pfd.cRedBits, pfd.cRedShift);
        pPal->palPalEntry[i].peGreen =
                ComponentFromIndex(i, pfd.cGreenBits, pfd.cGreenShift);
        pPal->palPalEntry[i].peBlue =
                ComponentFromIndex(i, pfd.cBlueBits, pfd.cBlueShift);
        pPal->palPalEntry[i].peFlags = 0;
    }

    /* fix up the palette to include the default GDI palette */
    if ((pfd.cColorBits == 8)                           &&
        (pfd.cRedBits   == 3) && (pfd.cRedShift   == 0) &&
        (pfd.cGreenBits == 3) && (pfd.cGreenShift == 3) &&
        (pfd.cBlueBits  == 2) && (pfd.cBlueShift  == 6)
       ) {
        for (i = 1 ; i <= 12 ; i++)
            pPal->palPalEntry[defaultOverride[i]] = defaultPalEntry[i];
    }

    cmap->palette = CreatePalette(pPal);
    LocalFree(pPal);
    cmap->size = n;
    cmap->stale = 0;

    /* Since this is a private colormap of a fix size, we do not need
       a valid hash table, but a dummy one */

    Tcl_InitHashTable(&cmap->refCounts, TCL_ONE_WORD_KEYS);
    return (Colormap)cmap;
}

static Colormap Win32CreateCiColormap(struct Togl *togl)
{
    /* Create a colormap with size of togl->CiColormapSize and set all
       entries to black */

    LOGPALETTE logPalette;
    TkWinColormap *cmap = (TkWinColormap *) ckalloc(sizeof(TkWinColormap));

    logPalette.palVersion = 0x300;
    logPalette.palNumEntries = 1;
    logPalette.palPalEntry[0].peRed = 0;
    logPalette.palPalEntry[0].peGreen = 0;
    logPalette.palPalEntry[0].peBlue = 0;
    logPalette.palPalEntry[0].peFlags = 0;

    cmap->palette = CreatePalette(&logPalette);
    cmap->size = togl->CiColormapSize;
    ResizePalette(cmap->palette, cmap->size);  /* sets new entries to black */
    cmap->stale = 0;

    /* Since this is a private colormap of a fix size, we do not need
       a valid hash table, but a dummy one */

    Tcl_InitHashTable(&cmap->refCounts, TCL_ONE_WORD_KEYS);
    return (Colormap)cmap;
}
#endif /*X11*/



/*
 * Togl_Init
 *
 *   Called upon system startup to create Togl command.
 */
int Togl_Init(Tcl_Interp *interp)
{
   /* The Tcl_PkgProvide() function isn't available in Tcl 7.4 or earlier */
#if (TCL_MAJOR_VERSION * 100 + TCL_MINOR_VERSION) > 704
   /* The following allows Togl to be loaded dynamically into a running
    * tclsh, if togl is made into a shared lib.  Contributed by Kerel
    * Zuiderveld (karel.zuiderveld@cv.ruu.nl)
    */
   if (Tcl_PkgProvide(interp, "Togl", TOGL_VERSION) != TCL_OK) {
      return TCL_ERROR;
   }
#endif

   Tcl_CreateCommand(interp, "togl", Togl_Cmd,
                     (ClientData) Tk_MainWindow(interp), NULL);
   Tcl_InitHashTable(&CommandTable, TCL_STRING_KEYS);

   return TCL_OK;
}


/*
 * Register a C function to be called when an Togl widget is realized.
 */
void Togl_CreateFunc( Togl_Callback *proc )
{
   CreateProc = proc;
}


/*
 * Register a C function to be called when an Togl widget must be redrawn.
 */
void Togl_DisplayFunc( Togl_Callback *proc )
{
   DisplayProc = proc;
}


/*
 * Register a C function to be called when an Togl widget is resized.
 */
void Togl_ReshapeFunc( Togl_Callback *proc )
{
   ReshapeProc = proc;
}


/*
 * Register a C function to be called when an Togl widget is destroyed.
 */
void Togl_DestroyFunc( Togl_Callback *proc )
{
   DestroyProc = proc;
}


/*
 * Register a C function to be called from TimerEventHandler.
 */
void Togl_TimerFunc( Togl_Callback *proc )
{
   TimerProc = proc;
}


/*
 * Reset default callback pointers to NULL.
 */
void Togl_ResetDefaultCallbacks( void )
{
   CreateProc = NULL;
   DisplayProc = NULL;
   ReshapeProc = NULL;
   DestroyProc = NULL;
   OverlayDisplayProc = NULL;
   TimerProc = NULL;
   DefaultClientData = NULL;
}


/*
 * Chnage the create callback for a specific Togl widget.
 */
void Togl_SetCreateFunc( struct Togl *togl, Togl_Callback *proc )
{
   togl->CreateProc = proc;
}


/*
 * Change the display/redraw callback for a specific Togl widget.
 */
void Togl_SetDisplayFunc( struct Togl *togl, Togl_Callback *proc )
{
   togl->DisplayProc = proc;
}


/*
 * Change the reshape callback for a specific Togl widget.
 */
void Togl_SetReshapeFunc( struct Togl *togl, Togl_Callback *proc )
{
   togl->ReshapeProc = proc;
}


/*
 * Change the destroy callback for a specific Togl widget.
 */
void Togl_SetDestroyFunc( struct Togl *togl, Togl_Callback *proc )
{
   togl->DestroyProc = proc;
}


/*
 * Togl_Timer
 *
 * Gets called from Tk_CreateTimerHandler.
 */
static void Togl_Timer( ClientData clientData )
{
   struct Togl *togl = (struct Togl *) clientData;
   togl->TimerProc(togl);
#if (TK_MAJOR_VERSION * 100 + TK_MINOR_VERSION) >= 401
   togl->timerHandler =
       Tcl_CreateTimerHandler( togl->Time, Togl_Timer, (ClientData)togl );
#else
   togl->timerHandler =
       Tk_CreateTimerHandler( togl->Time, Togl_Timer, (ClientData)togl );
#endif
}


/*
 * Togl_CreateCommand
 *
 *   Declares a new C sub-command of Togl callable from Tcl.
 *   Every time the sub-command is called from Tcl, the
 *   C routine will be called with all the arguments from Tcl.
 */
void Togl_CreateCommand( char *cmd_name, Togl_CmdProc *cmd_proc)
{
   int new_item;
   Tcl_HashEntry *entry;
   entry = Tcl_CreateHashEntry(&CommandTable, cmd_name, &new_item);
   Tcl_SetHashValue(entry, cmd_proc);
}


/*
 * Togl_MakeCurrent
 *
 *   Bind the OpenGL rendering context to the specified
 *   Togl widget.
 */
void Togl_MakeCurrent( const struct Togl *togl )
{
#if defined(WIN32)
   int res = wglMakeCurrent(togl->tglGLHdc, togl->tglGLHglrc);
   assert(res == TRUE);
#elif defined(X11)
   glXMakeCurrent( Tk_Display(togl->TkWin),
                   Tk_WindowId(togl->TkWin),
                   togl->GlCtx );
#if defined(__sgi) && defined(STEREO)
   stereoMakeCurrent( Tk_Display(togl->TkWin),
                      Tk_WindowId(togl->TkWin),
                      togl->GlCtx );
#endif /*__sgi STEREO */
#endif /* WIN32 */
}


/*
 * Called when the widget's contents must be redrawn.  Basically, we
 * just call the user's render callback function.
 *
 * Note that the parameter type is ClientData so this function can be
 * passed to Tk_DoWhenIdle().
 */
static void Togl_Render( ClientData clientData )
{
   struct Togl *togl = (struct Togl *)clientData;

   if (togl->DisplayProc) {
      Togl_MakeCurrent(togl);
      togl->DisplayProc(togl);
   }
   togl->UpdatePending = GL_FALSE;
}


static void RenderOverlay( ClientData clientData )
{
   struct Togl *togl = (struct Togl *)clientData;

   if (togl->OverlayFlag && togl->OverlayDisplayProc) {
#if defined(WIN32)
      int res = wglMakeCurrent(togl->tglGLHdc, togl->tglGLHglrc);
      assert(res == TRUE);
#elif defined(X11)
      glXMakeCurrent( Tk_Display(togl->TkWin),
		      togl->OverlayWindow,
		      togl->OverlayCtx );
#if defined(__sgi) && defined(STEREO)
      stereoMakeCurrent( Tk_Display(togl->TkWin),
                         togl->OverlayWindow,
                         togl->OverlayCtx );	
#endif /*__sgi STEREO */	
#endif /* WIN32 */
      togl->OverlayDisplayProc(togl);
   }
   togl->OverlayUpdatePending = GL_FALSE;
}


/*
 * It's possible to change with this function or in a script some
 * options like RGBA - ColorIndex ; Z-buffer and so on
 */
int Togl_Configure(Tcl_Interp *interp, struct Togl *togl,
                   int argc, char *argv[], int flags)
{
   int oldRgbaFlag    = togl->RgbaFlag;
   int oldRgbaRed     = togl->RgbaRed;
   int oldRgbaGreen   = togl->RgbaGreen;
   int oldRgbaBlue    = togl->RgbaBlue;
   int oldDoubleFlag  = togl->DoubleFlag;
   int oldDepthFlag   = togl->DepthFlag;
   int oldDepthSize   = togl->DepthSize;
   int oldAccumFlag   = togl->AccumFlag;
   int oldAccumRed    = togl->AccumRed;
   int oldAccumGreen  = togl->AccumGreen;
   int oldAccumBlue   = togl->AccumBlue;
   int oldAccumAlpha  = togl->AccumAlpha;
   int oldAlphaFlag   = togl->AlphaFlag;
   int oldAlphaSize   = togl->AlphaSize;
   int oldStencilFlag = togl->StencilFlag;
   int oldStencilSize = togl->StencilSize;
   int oldAuxNumber   = togl->AuxNumber;

   if (Tk_ConfigureWidget(interp, togl->TkWin, configSpecs,
                          argc, argv, (char *)togl, flags) == TCL_ERROR) {
      return(TCL_ERROR);
   }

   Tk_GeometryRequest(togl->TkWin, togl->Width, togl->Height);

   if (togl->RgbaFlag != oldRgbaFlag
       || togl->RgbaRed != oldRgbaRed
       || togl->RgbaGreen != oldRgbaGreen
       || togl->RgbaBlue != oldRgbaBlue
       || togl->DoubleFlag != oldDoubleFlag
       || togl->DepthFlag != oldDepthFlag
       || togl->DepthSize != oldDepthSize
       || togl->AccumFlag != oldAccumFlag
       || togl->AccumRed != oldAccumRed
       || togl->AccumGreen != oldAccumGreen
       || togl->AccumBlue != oldAccumBlue
       || togl->AccumAlpha != oldAccumAlpha
       || togl->AlphaFlag != oldAlphaFlag
       || togl->AlphaSize != oldAlphaSize
       || togl->StencilFlag != oldStencilFlag
       || togl->StencilSize != oldStencilSize
       || togl->AuxNumber != oldAuxNumber) {
#ifdef MESA_COLOR_HACK
      free_default_color_cells( Tk_Display(togl->TkWin),
                                Tk_Colormap(togl->TkWin) );
#endif
      /* Have to recreate the window and GLX context */
      if (Togl_MakeWindowExist(togl)==TCL_ERROR) {
         return TCL_ERROR;
      }
   }

#if defined(__sgi) && defined(STEREO)
   stereoInit(togl,togl->StereoFlag);
#endif

   return TCL_OK;
}


int Togl_Widget(ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[])
{
   struct Togl *togl = (struct Togl *)clientData;
   int result = TCL_OK;
   Tcl_HashEntry *entry;
   Tcl_HashSearch search;
   Togl_CmdProc *cmd_proc;

   if (argc < 2) {
      Tcl_AppendResult(interp, "wrong # args: should be \"",
                       argv[0], " ?options?\"", NULL);
      return TCL_ERROR;
   }

   Tk_Preserve((ClientData)togl);

   if (!strncmp(argv[1], "configure", MAX(1, strlen(argv[1])))) {
      if (argc == 2) {
         /* Return list of all configuration parameters */
         result = Tk_ConfigureInfo(interp, togl->TkWin, configSpecs,
                                   (char *)togl, (char *)NULL, 0);
      }
      else if (argc == 3) {
         if (strcmp(argv[2],"-extensions")==0) {
            /* Return a list of OpenGL extensions available */
            char *extensions;
            extensions = (char *) glGetString(GL_EXTENSIONS);
            Tcl_SetResult( interp, extensions, TCL_STATIC );
            result = TCL_OK;
         }
         else {
            /* Return a specific configuration parameter */
            result = Tk_ConfigureInfo(interp, togl->TkWin, configSpecs,
                                      (char *)togl, argv[2], 0);
         }
      }
      else {
         /* Execute a configuration change */
         result = Togl_Configure(interp, togl, argc-2, argv+2,
                                TK_CONFIG_ARGV_ONLY);
      }
   }
   else if (!strncmp(argv[1], "render", MAX(1, strlen(argv[1])))) {
      /* force the widget to be redrawn */
      Togl_Render((ClientData) togl);
   }
   else if (!strncmp(argv[1], "swapbuffers", MAX(1, strlen(argv[1])))) {
      /* force the widget to be redrawn */
      Togl_SwapBuffers(togl);
   }
   else if (!strncmp(argv[1], "makecurrent", MAX(1, strlen(argv[1])))) {
      /* force the widget to be redrawn */
      Togl_MakeCurrent(togl);
   }
   else {
      /* Probably a user-defined function */
      entry = Tcl_FindHashEntry(&CommandTable, argv[1]);
      if (entry != NULL) {
         cmd_proc = (Togl_CmdProc *)Tcl_GetHashValue(entry);
         result = cmd_proc(togl, argc, argv);
      }
      else {
         Tcl_AppendResult(interp, "Togl: Unknown option: ", argv[1], "\n",
                          "Try: configure or render\n",
                          "or one of the user-defined commands:\n",
                          NULL);
         entry = Tcl_FirstHashEntry(&CommandTable, &search);
         while (entry) {
            Tcl_AppendResult(interp, "  ",
                             Tcl_GetHashKey(&CommandTable, entry),
                             "\n", NULL);
            entry = Tcl_NextHashEntry(&search);
         }
         result = TCL_ERROR;
      }
   }

   Tk_Release((ClientData)togl);
   return result;
}



/*
 * Togl_Cmd
 *
 *   Called when Togl is executed - creation of a Togl widget.
 *     * Creates a new window
 *     * Creates an 'Togl' data structure
 *     * Creates an event handler for this window
 *     * Creates a command that handles this object
 *     * Configures this Togl for the given arguments
 */
static int Togl_Cmd(ClientData clientData, Tcl_Interp *interp,
                    int argc, char **argv)
{
   char *name;
   Tk_Window main = (Tk_Window)clientData;
   Tk_Window tkwin;
   struct Togl *togl;

   if (argc <= 1) {
      TCL_ERR(interp, "wrong # args: should be \"pathName read filename\"");
   }

   /* Create the window. */
   name = argv[1];
   tkwin = Tk_CreateWindowFromPath(interp, main, name, (char *) NULL);
   if (tkwin == NULL) {
      return TCL_ERROR;
   }

   Tk_SetClass(tkwin, "Togl");

   /* Create Togl data structure */
   togl = (struct Togl *)malloc(sizeof(struct Togl));
   if (!togl) {
      return TCL_ERROR;
   }

#if defined(WIN32)
   togl->tglGLHdc = NULL;
   togl->tglGLHglrc = NULL;
#elif defined(X11)
   togl->GlCtx = NULL;
#endif /* WIN32 */
   togl->display = Tk_Display( tkwin );
   togl->TkWin = tkwin;
   togl->Interp = interp;
#ifndef NO_TK_CURSOR
   togl->Cursor = None;
#endif
   togl->Width = 0;
   togl->Height = 0;
   togl->Time = 0;
   togl->RgbaFlag = 1;
   togl->RgbaRed = 1;
   togl->RgbaGreen = 1;
   togl->RgbaBlue = 1;
   togl->DoubleFlag = 0;
   togl->DepthFlag = 0;
   togl->DepthSize = 1;
   togl->AccumFlag = 0;
   togl->AccumRed = 1;
   togl->AccumGreen = 1;
   togl->AccumBlue = 1;
   togl->AccumAlpha = 1;
   togl->AlphaFlag = 0;
   togl->AlphaSize = 1;
   togl->StencilFlag = 0;
   togl->StencilSize = 1;
   togl->OverlayFlag = 0;
   togl->StereoFlag = 0;
   togl->AuxNumber = 0;
   togl->UpdatePending = GL_FALSE;
   togl->CreateProc = CreateProc;
   togl->DisplayProc = DisplayProc;
   togl->ReshapeProc = ReshapeProc;
   togl->DestroyProc = DestroyProc;
   togl->TimerProc = TimerProc;
   togl->OverlayDisplayProc = OverlayDisplayProc;
   togl->Ident = NULL;  /* Per Benjamin on March 6, 1996 */
   togl->Client_Data = DefaultClientData;

   /* for EPS Output */
   togl->EpsRedMap = togl->EpsGreenMap = togl->EpsBlueMap = NULL;
   togl->EpsMapSize = 0;

   /* Create command event handler */
   togl->widgetCmd = Tcl_CreateCommand(interp, Tk_PathName(tkwin),
				       Togl_Widget, (ClientData)togl,
				       (Tcl_CmdDeleteProc*) ToglCmdDeletedProc);
   Tk_CreateEventHandler(tkwin,
                         ExposureMask | StructureNotifyMask,
                         Togl_EventProc,
                         (ClientData)togl);

   /* Configure Togl widget */
   if (Togl_Configure(interp, togl, argc-2, argv+2, 0) == TCL_ERROR) {
      Tk_DestroyWindow(tkwin);
      goto error;
   }

   /*
    * If OpenGL window wasn't already created by Togl_Configure() we
    * create it now.  We can tell by checking if the GLX context has
    * been initialized.
    */
#if defined(WIN32)
   if (!togl->tglGLHdc) {
#elif defined(X11)
   if (!togl->GlCtx) {
#endif /* WIN32 */
      if (Togl_MakeWindowExist(togl) == TCL_ERROR) {
         goto error;
      }
   }

   /* If defined, call create callback */
   if (togl->CreateProc) {
      togl->CreateProc(togl);
   }

   /* If defined, call reshape proc */
   if (togl->ReshapeProc) {
      togl->ReshapeProc(togl);
   }

   /* If defined, setup timer */
   if (togl->TimerProc){
      Tk_CreateTimerHandler( togl->Time, Togl_Timer, (ClientData)togl );
   }

   Tcl_AppendResult(interp, Tk_PathName(tkwin), NULL);
   return TCL_OK;

error:
   Tcl_DeleteCommand(interp, "togl");
   /*free(togl);   Don't free it, if we do a crash occurs later...*/
   return TCL_ERROR;
}



/*
 * Do all the setup for overlay planes
 * Return:   TCL_OK or TCL_ERROR
 */
static int SetupOverlay( struct Togl *togl )
{
#if defined(X11)

#ifdef GLX_TRANSPARENT_TYPE_EXT
   static int ovAttributeList[] = {
      GLX_BUFFER_SIZE, 2,
      GLX_LEVEL, 1,
      GLX_TRANSPARENT_TYPE_EXT, GLX_TRANSPARENT_INDEX_EXT,
      None
   };
#else
   static int ovAttributeList[] = {
      GLX_BUFFER_SIZE, 2,
      GLX_LEVEL, 1,
      None
   };
#endif

   Display *dpy;
   XVisualInfo *visinfo;
   TkWindow *winPtr = (TkWindow *) togl->TkWin;
   XSetWindowAttributes swa;
   Tcl_HashEntry *hPtr;
   int new_flag;

   dpy = Tk_Display(togl->TkWin);

   visinfo = glXChooseVisual( dpy, DefaultScreen(dpy), ovAttributeList );
   if (!visinfo){
      Tcl_AppendResult(togl->Interp,Tk_PathName(winPtr),
                       ": No suitable overlay index visual available",
                       (char *) NULL);
      togl->OverlayCtx = 0;
      togl->OverlayWindow = 0;
      togl->OverlayCmap = 0;
      return TCL_ERROR;
   }

#ifdef GLX_TRANSPARENT_INDEX_EXT
   {
      int fail = glXGetConfig(dpy, visinfo,GLX_TRANSPARENT_INDEX_VALUE_EXT,
                              &togl->OverlayTransparentPixel);
      if (fail)
         togl->OverlayTransparentPixel=0; /* maybe, maybe ... */
   }
#else
   togl->OverlayTransparentPixel=0; /* maybe, maybe ... */
#endif

   togl->OverlayCtx = glXCreateContext( dpy, visinfo, None, GL_TRUE );

   swa.colormap = XCreateColormap( dpy, RootWindow(dpy, visinfo->screen),
                                   visinfo->visual, AllocNone );
   togl->OverlayCmap = swa.colormap;

   swa.border_pixel = 0;
   swa.event_mask = ALL_EVENTS_MASK;
   togl->OverlayWindow = XCreateWindow( dpy, Tk_WindowId(togl->TkWin), 0, 0,
                                        togl->Width, togl->Height, 0,
                                        visinfo->depth, InputOutput,
                                        visinfo->visual,
                                        CWBorderPixel|CWColormap|CWEventMask,
                                        &swa );

   hPtr = Tcl_CreateHashEntry( &winPtr->dispPtr->winTable,
                               (char *) togl->OverlayWindow, &new_flag );
   Tcl_SetHashValue( hPtr, winPtr );

/*   XMapWindow( dpy, togl->OverlayWindow );*/
   togl->OverlayIsMapped = 0;

   /* Make sure window manager installs our colormap */
   XSetWMColormapWindows( dpy, togl->OverlayWindow, &togl->OverlayWindow, 1 );

   return TCL_OK;

#elif defined(WIN32)       /* not yet implemented on Windows*/

   return TCL_ERROR;

#endif /* X11 */
}


#ifdef WIN32
#define TOGL_CLASS_NAME "Togl Class"
static ToglClassInitialized = 0;

static LRESULT CALLBACK Win32WinProc( HWND hwnd, UINT message,
                                    WPARAM wParam, LPARAM lParam)
{
    LONG result;
    struct Togl *togl = (struct Togl*) GetWindowLong(hwnd, 0);
    TkWinColormap *cmap;
    HPALETTE OldPal;
    UINT i;

    switch( message ){
    case WM_WINDOWPOSCHANGED:
        /* Should be processed by DefWindowProc, otherwise a double buffered
        context is not properly resized when the corresponding window is resized.*/
        break;
    case WM_DESTROY:
        if (togl->tglGLHglrc) {
            wglDeleteContext(togl->tglGLHglrc);
        }
        if (togl->tglGLHdc) {
            ReleaseDC(hwnd, togl->tglGLHdc);
        }
        break;
    default:
        return TkWinChildProc(hwnd, message, wParam, lParam);
    }
    result = DefWindowProc(hwnd, message, wParam, lParam);
    Tcl_ServiceAll();
    return result;
}
#endif /* WIN32 */



/*
 * Togl_MakeWindowExist
 *
 *   Modified version of Tk_MakeWindowExist.
 *   Creates an OpenGL window for the Togl widget.
 */
static int Togl_MakeWindowExist(struct Togl *togl)
{
   XVisualInfo *visinfo;
   Display *dpy;
   int dummy;
   int attrib_list[1000];
   int attrib_count;
   TkWindow *winPtr = (TkWindow *) togl->TkWin;
   TkWindow *winPtr2;
   Window parent;
   Colormap cmap;
   XSetWindowAttributes swa;
   Tcl_HashEntry *hPtr;
   int new_flag;
   int scrnum;
   int attempt;

#if defined(X11)
#define MAX_ATTEMPTS 12
   static int ci_depths[MAX_ATTEMPTS] = {
      8, 4, 2, 1, 12, 16, 8, 4, 2, 1, 12, 16
   };
   static int dbl_flags[MAX_ATTEMPTS] = {
      0, 0, 0, 0,  0,  0, 1, 1, 1, 1,  1,  1
   };
#elif defined(WIN32)
   HWND hwnd, parentWin;
   int style, pixelformat;
   HANDLE hInstance;
   WNDCLASS ToglClass;
   PIXELFORMATDESCRIPTOR pfd;
   XVisualInfo VisInf;
#endif /* X11 */


   dpy = Tk_Display(togl->TkWin);

   if (winPtr->window != None) {
      XDestroyWindow(dpy, winPtr->window);
   }

#if defined(X11)
   /* Make sure OpenGL's GLX extension supported */
   if (!glXQueryExtension(dpy, &dummy, &dummy)) {
      TCL_ERR(togl->Interp, "Togl: X server has no OpenGL GLX extension");
   }

   /* It may take a few tries to get a visual */
   for (attempt=0; attempt<MAX_ATTEMPTS; attempt++) {
      attrib_count = 0;
      attrib_list[attrib_count++] = GLX_USE_GL;
      if (togl->RgbaFlag) {
         /* RGB[A] mode */
         attrib_list[attrib_count++] = GLX_RGBA;
         attrib_list[attrib_count++] = GLX_RED_SIZE;
         attrib_list[attrib_count++] = togl->RgbaRed;
         attrib_list[attrib_count++] = GLX_GREEN_SIZE;
         attrib_list[attrib_count++] = togl->RgbaGreen;
         attrib_list[attrib_count++] = GLX_BLUE_SIZE;
         attrib_list[attrib_count++] = togl->RgbaBlue;
         if (togl->AlphaFlag) {
            attrib_list[attrib_count++] = GLX_ALPHA_SIZE;
            attrib_list[attrib_count++] = togl->AlphaSize;
         }

	 /* for EPS Output */
	 if ( togl->EpsRedMap) free( ( char *)togl->EpsRedMap);
	 if ( togl->EpsGreenMap) free( ( char *)togl->EpsGreenMap);
	 if ( togl->EpsBlueMap) free( ( char *)togl->EpsBlueMap);
	 togl->EpsRedMap = togl->EpsGreenMap = togl->EpsBlueMap = NULL;
	 togl->EpsMapSize = 0;
      }
      else {
         /* Color index mode */
         int depth;
         attrib_list[attrib_count++] = GLX_BUFFER_SIZE;
         depth = ci_depths[attempt];
         attrib_list[attrib_count++] = depth;
      }
      if (togl->DepthFlag) {
         attrib_list[attrib_count++] = GLX_DEPTH_SIZE;
         attrib_list[attrib_count++] = togl->DepthSize;
      }
      if (togl->DoubleFlag || dbl_flags[attempt]) {
         attrib_list[attrib_count++] = GLX_DOUBLEBUFFER;
      }
      if (togl->StencilFlag) {
         attrib_list[attrib_count++] = GLX_STENCIL_SIZE;
         attrib_list[attrib_count++] = togl->StencilSize;
      }
      if (togl->AccumFlag) {
         attrib_list[attrib_count++] = GLX_ACCUM_RED_SIZE;
         attrib_list[attrib_count++] = togl->AccumRed;
         attrib_list[attrib_count++] = GLX_ACCUM_GREEN_SIZE;
         attrib_list[attrib_count++] = togl->AccumGreen;
         attrib_list[attrib_count++] = GLX_ACCUM_BLUE_SIZE;
         attrib_list[attrib_count++] = togl->AccumBlue;
         if (togl->AlphaFlag) {
            attrib_list[attrib_count++] = GLX_ACCUM_ALPHA_SIZE;
            attrib_list[attrib_count++] = togl->AccumAlpha;
         }
      }
      if (togl->AuxNumber != 0) {
         attrib_list[attrib_count++] = GLX_AUX_BUFFERS;
         attrib_list[attrib_count++] = togl->AuxNumber;
      }

      /* stereo hack */
      /*
	if (togl->StereoFlag) {
	attrib_list[attrib_count++] = GLX_STEREO;
	}
      */
      attrib_list[attrib_count++] = None;

      visinfo = glXChooseVisual( dpy, DefaultScreen(dpy), attrib_list );
      if (visinfo) {
         /* found a GLX visual! */
         break;
      }
   }
   if (visinfo==NULL) {
      TCL_ERR(togl->Interp, "Togl: couldn't get visual");
   }


   /* Create an OpenGL rendering context */
   /* No sharing of display lists */
   /* Direct rendering if possible */
   togl->GlCtx = glXCreateContext(dpy, visinfo, None, GL_TRUE);
   if (togl->GlCtx == NULL) {
      TCL_ERR(togl->Interp, "could not create rendering context");
   }
#endif /* X11 */

   /* Find parent of window */
   /* Necessary for creation */
   if ((winPtr->parentPtr == NULL) || (winPtr->flags & TK_TOP_LEVEL)) {
      parent = XRootWindow(winPtr->display, winPtr->screenNum);
   }
   else {
      if (winPtr->parentPtr->window == None) {
         Tk_MakeWindowExist((Tk_Window) winPtr->parentPtr);
      }
      parent = winPtr->parentPtr->window;
   }

#ifdef WIN32
   parentWin = Tk_GetHWND(parent);
   hInstance = Tk_GetHINSTANCE();
   if (ToglClassInitialized == 0) {
       ToglClassInitialized = 1;
       ToglClass.style = CS_HREDRAW | CS_VREDRAW;
       ToglClass.cbClsExtra = 0;
       ToglClass.cbWndExtra = 4;   /* to save struct Togl* */
       ToglClass.hInstance = hInstance;
       ToglClass.hbrBackground = NULL;
       ToglClass.lpszMenuName = NULL;
       ToglClass.lpszClassName = TOGL_CLASS_NAME;
       ToglClass.lpfnWndProc = Win32WinProc;
       ToglClass.hIcon = NULL;
       ToglClass.hCursor = NULL;
       if (!RegisterClass(&ToglClass)){
           TCL_ERR(togl->Interp, "unable register Togl window class");
       }
   }

   hwnd = CreateWindow(TOGL_CLASS_NAME, NULL, WS_CHILD | WS_CLIPCHILDREN
                       | WS_CLIPSIBLINGS, 0, 0, togl->Width, togl->Height,
                       parentWin, NULL, hInstance, NULL);
   SetWindowLong(hwnd, 0, (LONG) togl);
   SetWindowPos(hwnd, HWND_TOP, 0, 0, 0, 0,
  	            SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);

   togl->tglGLHdc = GetDC(hwnd);

   pfd.nSize = sizeof(PIXELFORMATDESCRIPTOR);
   pfd.nVersion = 1;
   pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL;
   if (togl->DoubleFlag) {
        pfd.dwFlags |= PFD_DOUBLEBUFFER;
   }
   /* The stereo flag is not supported in the current generic OpenGL
    * implementation, but may be supported by specific hardware devices.
    */
   if (togl->StereoFlag) {
        pfd.dwFlags |= PFD_STEREO;
   }

   pfd.cColorBits = togl->RgbaRed + togl->RgbaGreen + togl->RgbaBlue;
   pfd.iPixelType = togl->RgbaFlag ? PFD_TYPE_RGBA : PFD_TYPE_COLORINDEX;
   /* Alpha bitplanes are not supported in the current generic OpenGL
    * implementation, but may be supported by specific hardware devices.
    */
   pfd.cAlphaBits = togl->AlphaFlag ? togl->AlphaSize : 0;
   pfd.cAccumBits = togl->AccumFlag ? (togl->AccumRed + togl->AccumGreen +
                                       togl->AccumBlue +togl->AccumAlpha) : 0;
   pfd.cDepthBits = togl->DepthFlag ? togl->DepthSize : 0;
   pfd.cStencilBits = togl->StencilFlag ? togl->StencilSize : 0;
   /* Auxiliary buffers are not supported in the current generic OpenGL
    * implementation, but may be supported by specific hardware devices.
    */
   pfd.cAuxBuffers = togl->AuxNumber;
   pfd.iLayerType = PFD_MAIN_PLANE;

   if ( (pixelformat = ChoosePixelFormat(togl->tglGLHdc, &pfd)) == 0 ) {
        TCL_ERR(togl->Interp, "Togl: couldn't choose pixel format");
   }
   if (SetPixelFormat(togl->tglGLHdc, pixelformat, &pfd) == FALSE) {
        TCL_ERR(togl->Interp, "Togl: couldn't choose pixel format");
   }

   /* Get the actual pixel format */
   DescribePixelFormat(togl->tglGLHdc, pixelformat, sizeof(pfd), &pfd);

   /* Create an OpenGL rendering context */
   togl->tglGLHglrc = wglCreateContext(togl->tglGLHdc);
   if (!togl->tglGLHglrc) {
        TCL_ERR(togl->Interp, "could not create rendering context");
   }

   /* Just for portability, define the simplest visinfo */
   visinfo = &VisInf;
   visinfo->visual = DefaultVisual(dpy, DefaultScreen(dpy));
   visinfo->depth = visinfo->visual->bits_per_rgb;
#endif /*WIN32 */


   /*
    * find a colormap
    */
   scrnum = DefaultScreen(dpy);
   if (togl->RgbaFlag) {
      /* Colormap for RGB mode */
#if defined(X11)
      cmap = get_rgb_colormap( dpy, scrnum, visinfo );
#elif defined(WIN32)
      if (pfd.dwFlags & PFD_NEED_PALETTE) {
         cmap = Win32CreateRgbColormap(pfd);
      }
      else {
         cmap = DefaultColormap(dpy,scrnum);
      }

      /* for EPS Output */
      if ( togl->EpsRedMap) free( ( char *)togl->EpsRedMap);
      if ( togl->EpsGreenMap) free( ( char *)togl->EpsGreenMap);
      if ( togl->EpsBlueMap) free( ( char *)togl->EpsBlueMap);
      togl->EpsRedMap = togl->EpsGreenMap = togl->EpsBlueMap = NULL;
      togl->EpsMapSize = 0;
#endif /* X11 */
   }
   else {
      /* Colormap for CI mode */
#ifdef WIN32
      togl->CiColormapSize = 1 << pfd.cColorBits;
      togl->CiColormapSize = togl->CiColormapSize < MAX_CI_COLORMAP_SIZE ?
                             togl->CiColormapSize : MAX_CI_COLORMAP_SIZE;

#endif /* WIN32 */
      if (togl->PrivateCmapFlag) {
         /* need read/write colormap so user can store own color entries */
#if defined(X11)
         cmap = XCreateColormap(dpy, RootWindow(dpy, visinfo->screen),
                                visinfo->visual, AllocAll);
#elif defined(WIN32)
         cmap = Win32CreateCiColormap(togl);
#endif /* X11 */
      }
      else {
         if (visinfo->visual==DefaultVisual(dpy, scrnum)) {
            /* share default/root colormap */
            cmap = DefaultColormap(dpy,scrnum);
         }
         else {
            /* make a new read-only colormap */
            cmap = XCreateColormap(dpy, RootWindow(dpy, visinfo->screen),
                                   visinfo->visual, AllocNone);
         }
      }
   }

   /* Make sure Tk knows to switch to the new colormap when the cursor
    * is over this window when running in color index mode.
    */
   Tk_SetWindowVisual(togl->TkWin, visinfo->visual, visinfo->depth, cmap);
#ifdef WIN32
   /* Install the colormap */
   SelectPalette(togl->tglGLHdc, ((TkWinColormap *)cmap)->palette, TRUE);
   RealizePalette(togl->tglGLHdc);
#endif /* WIN32 */

   /* for DumpToEpsFile */
   togl->EpsVisual = visinfo;

#if defined(X11)
   swa.colormap = cmap;
   swa.border_pixel = 0;
   swa.event_mask = ALL_EVENTS_MASK;
   winPtr->window = XCreateWindow(dpy, parent,
                                  0, 0, togl->Width, togl->Height,
                                  0, visinfo->depth,
                                  InputOutput, visinfo->visual,
                                  CWBorderPixel | CWColormap | CWEventMask,
                                  &swa);

   /* Make sure window manager installs our colormap */
   XSetWMColormapWindows( dpy, winPtr->window, &winPtr->window, 1 );
#elif defined(WIN32)
   winPtr->window = Tk_AttachHWND((Tk_Window)winPtr, hwnd);
#endif /* X11 */

   hPtr = Tcl_CreateHashEntry(&winPtr->dispPtr->winTable,
                              (char *) winPtr->window, &new_flag);
   Tcl_SetHashValue(hPtr, winPtr);

   winPtr->dirtyAtts = 0;
   winPtr->dirtyChanges = 0;
#ifdef TK_USE_INPUT_METHODS
   winPtr->inputContext = NULL;
#endif /* TK_USE_INPUT_METHODS */

   if (!(winPtr->flags & TK_TOP_LEVEL)) {
      /*
       * If any siblings higher up in the stacking order have already
       * been created then move this window to its rightful position
       * in the stacking order.
       *
       * NOTE: this code ignores any changes anyone might have made
       * to the sibling and stack_mode field of the window's attributes,
       * so it really isn't safe for these to be manipulated except
       * by calling Tk_RestackWindow.
       */

      for (winPtr2 = winPtr->nextPtr; winPtr2 != NULL;
           winPtr2 = winPtr2->nextPtr) {
         if ((winPtr2->window != None) && !(winPtr2->flags & TK_TOP_LEVEL)) {
            XWindowChanges changes;
            changes.sibling = winPtr2->window;
            changes.stack_mode = Below;
            XConfigureWindow(winPtr->display, winPtr->window,
                             CWSibling|CWStackMode, &changes);
            break;
         }
      }

      /*
       * If this window has a different colormap than its parent, add
       * the window to the WM_COLORMAP_WINDOWS property for its top-level.
       */

      if ((winPtr->parentPtr != NULL) &&
          (winPtr->atts.colormap != winPtr->parentPtr->atts.colormap)) {
         TkWmAddToColormapWindows(winPtr);
      }
   }

   if (togl->OverlayFlag) {
      if (SetupOverlay( togl )==TCL_ERROR) {
         fprintf(stderr,"Warning: couldn't setup overlay.\n");
         togl->OverlayFlag = 0;
      }
   }

   /*
    * Issue a ConfigureNotify event if there were deferred configuration
    * changes (but skip it if the window is being deleted;  the
    * ConfigureNotify event could cause problems if we're being called
    * from Tk_DestroyWindow under some conditions).
    */
   if ((winPtr->flags & TK_NEED_CONFIG_NOTIFY)
       && !(winPtr->flags & TK_ALREADY_DEAD)){
      XEvent event;

      winPtr->flags &= ~TK_NEED_CONFIG_NOTIFY;

      event.type = ConfigureNotify;
      event.xconfigure.serial = LastKnownRequestProcessed(winPtr->display);
      event.xconfigure.send_event = False;
      event.xconfigure.display = winPtr->display;
      event.xconfigure.event = winPtr->window;
      event.xconfigure.window = winPtr->window;
      event.xconfigure.x = winPtr->changes.x;
      event.xconfigure.y = winPtr->changes.y;
      event.xconfigure.width = winPtr->changes.width;
      event.xconfigure.height = winPtr->changes.height;
      event.xconfigure.border_width = winPtr->changes.border_width;
      if (winPtr->changes.stack_mode == Above) {
         event.xconfigure.above = winPtr->changes.sibling;
      }
      else {
         event.xconfigure.above = None;
      }
      event.xconfigure.override_redirect = winPtr->atts.override_redirect;
      Tk_HandleEvent(&event);
   }

   /* Request the X window to be displayed */
   XMapWindow(dpy, Tk_WindowId(togl->TkWin));

   /* Bind the context to the window and make it the current context. */
   Togl_MakeCurrent(togl);

#if defined(X11)
   /* Check for a single/double buffering snafu */
   {
      int dbl_flag;
      if (glXGetConfig( dpy, visinfo, GLX_DOUBLEBUFFER, &dbl_flag )) {
         if (togl->DoubleFlag==0 && dbl_flag) {
            /* We requested single buffering but had to accept a */
            /* double buffered visual.  Set the GL draw buffer to */
            /* be the front buffer to simulate single buffering. */
            glDrawBuffer( GL_FRONT );
         }
      }
   }
#endif /* X11 */

   /* for EPS Output */
   if ( !togl->RgbaFlag) {
      GLint index_bits;
      int index_size;
#if defined(X11)
      glGetIntegerv( GL_INDEX_BITS, &index_bits );
      index_size = 1 << index_bits;
#elif defined(WIN32)
      index_size = togl->CiColormapSize;
#endif /* X11 */
      if ( togl->EpsMapSize != index_size) {
         if ( togl->EpsRedMap) free( ( char *)togl->EpsRedMap);
         if ( togl->EpsGreenMap) free( ( char *)togl->EpsGreenMap);
         if ( togl->EpsBlueMap) free( ( char *)togl->EpsBlueMap);
         togl->EpsMapSize = index_size;
         togl->EpsRedMap = ( GLfloat *)calloc( index_size, sizeof( GLfloat));
         togl->EpsGreenMap = ( GLfloat *)calloc( index_size, sizeof( GLfloat));
         togl->EpsBlueMap = ( GLfloat *)calloc( index_size, sizeof( GLfloat));
      }
   }

   return TCL_OK;
}

/*
 * ToglCmdDeletedProc
 *
 *      This procedure is invoked when a widget command is deleted.  If
 *      the widget isn't already in the process of being destroyed,
 *      this command destroys it.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The widget is destroyed.
 *
 *----------------------------------------------------------------------
 */ 
static void ToglCmdDeletedProc( ClientData clientData )
{
   struct Togl *togl = (struct Togl *)clientData;
   Tk_Window tkwin = togl->TkWin;

   /*
    * This procedure could be invoked either because the window was
    * destroyed and the command was then deleted (in which case tkwin
    * is NULL) or because the command was deleted, and then this procedure
    * destroys the widget.
    */

   if (tkwin != NULL) {
      togl->TkWin = NULL;
      Tk_DestroyWindow(tkwin);
   }
}


/*
 * Togl_Destroy
 *
 * Gets called when an Togl widget is destroyed.
 */
#if (TK_MAJOR_VERSION * 100 + TK_MINOR_VERSION) >= 401
static void Togl_Destroy( char *clientData )
#else
static void Togl_Destroy( ClientData clientData )
#endif
{
   struct Togl *togl = (struct Togl *)clientData;

   Tk_FreeOptions(configSpecs, (char *)togl, togl->display, 0);

#ifndef NO_TK_CURSOR
   if (togl->Cursor != None) {
      Tk_FreeCursor(togl->display, togl->Cursor);
   }
#endif
   if (togl->DestroyProc) {
      togl->DestroyProc(togl);
   }

   free(togl);
}



/*
 * This gets called to handle Togl window configuration events
 */
static void Togl_EventProc(ClientData clientData, XEvent *eventPtr)
{
   struct Togl *togl = (struct Togl *)clientData;

   switch (eventPtr->type) {
      case Expose:
         if ((eventPtr->xexpose.count == 0) && !togl->UpdatePending) {
            if(eventPtr->xexpose.window==Tk_WindowId(togl->TkWin)) {
               Togl_PostRedisplay(togl);
            }
#if defined(X11)
            else if (togl->OverlayFlag && togl->OverlayIsMapped &&
                     eventPtr->xexpose.window==togl->OverlayWindow){
               Togl_PostOverlayRedisplay(togl);
            }
#endif /*X11*/
         }
         break;
      case ConfigureNotify:
         if (togl->Width != Tk_Width(togl->TkWin) ||
             togl->Height != Tk_Height(togl->TkWin)) {
            togl->Width = Tk_Width(togl->TkWin);
            togl->Height = Tk_Height(togl->TkWin);
            XResizeWindow(Tk_Display(togl->TkWin), Tk_WindowId(togl->TkWin),
                          togl->Width, togl->Height);
#if defined(X11)
            if (togl->OverlayFlag) {
               XResizeWindow( Tk_Display(togl->TkWin), togl->OverlayWindow,
                              togl->Width, togl->Height );
               XRaiseWindow( Tk_Display(togl->TkWin), togl->OverlayWindow );
            }
#endif /*X11*/
            Togl_MakeCurrent(togl);
            if (togl->ReshapeProc) {
               togl->ReshapeProc(togl);
            }
            else {
               glViewport(0, 0, togl->Width, togl->Height);
#if defined(X11)
               if (togl->OverlayFlag) {
                  Togl_UseLayer( togl,TOGL_OVERLAY );
                  glViewport( 0, 0, togl->Width, togl->Height );
                  Togl_UseLayer( togl, TOGL_NORMAL );
               }
#endif /*X11*/
            }
#ifndef WIN32 /* causes double redisplay on Win32 platform */
            Togl_PostRedisplay(togl);
#endif /* WIN32 */
         }
         break;
      case MapNotify:
         break;
      case DestroyNotify:
	 if (togl->TkWin != NULL) {
	    togl->TkWin = NULL;
#if (TCL_MAJOR_VERSION * 100 + TCL_MINOR_VERSION) >= 800
            /* This function new in Tcl/Tk 8.0 */
            Tcl_DeleteCommandFromToken( togl->Interp, togl->widgetCmd );
#endif
	 }
	 if (togl->TimerProc != NULL) {
#if (TK_MAJOR_VERSION * 100 + TK_MINOR_VERSION) >= 401
	    Tcl_DeleteTimerHandler(togl->timerHandler);
#else
	    Tk_DeleteTimerHandler(togl->timerHandler);
#endif
	    
	 }
	 if (togl->UpdatePending) {
            Tcl_CancelIdleCall(Togl_Render, (ClientData) togl);
	 }

#if (TK_MAJOR_VERSION * 100 + TK_MINOR_VERSION) >= 401
         Tcl_EventuallyFree( (ClientData) togl, Togl_Destroy );
#else
         Tk_EventuallyFree((ClientData)togl, Togl_Destroy);
#endif

         break;
      default:
         /*nothing*/
         ;
   }
}



void Togl_PostRedisplay( struct Togl *togl )
{
   if (!togl->UpdatePending) {
      Tk_DoWhenIdle( Togl_Render, (ClientData) togl );
      togl->UpdatePending = GL_TRUE;
   }
}



void Togl_SwapBuffers( const struct Togl *togl )
{
   if (togl->DoubleFlag) {
#if defined(WIN32)
      int res = SwapBuffers(togl->tglGLHdc);
      assert(res == TRUE);
#elif defined(X11)
      glXSwapBuffers( Tk_Display(togl->TkWin), Tk_WindowId(togl->TkWin) );
#endif /* WIN32 */
   }
   else {
      glFlush();
   }
}



char *Togl_Ident( const struct Togl *togl )
{
   return togl->Ident;
}


int Togl_Width( const struct Togl *togl )
{
   return togl->Width;
}


int Togl_Height( const struct Togl *togl )
{
   return togl->Height;
}


Tcl_Interp *Togl_Interp( const struct Togl *togl )
{
   return togl->Interp;
}


Tk_Window Togl_TkWin( const struct Togl *togl )
{
   return togl->TkWin;
}


#if defined(X11)
/*
 * A replacement for XAllocColor.  This function should never
 * fail to allocate a color.  When XAllocColor fails, we return
 * the nearest matching color.  If we have to allocate many colors
 * this function isn't too efficient; the XQueryColors() could be
 * done just once.
 * Written by Michael Pichler, Brian Paul, Mark Kilgard
 * Input:  dpy - X display
 *         cmap - X colormap
 *         cmapSize - size of colormap
 * In/Out: color - the XColor struct
 * Output:  exact - 1=exact color match, 0=closest match
 */
static void
noFaultXAllocColor( Display *dpy, Colormap cmap, int cmapSize,
                    XColor *color, int *exact )
{
   XColor *ctable, subColor;
   int i, bestmatch;
   double mindist;       /* 3*2^16^2 exceeds long int precision.
                          */

   /* First try just using XAllocColor. */
   if (XAllocColor(dpy, cmap, color)) {
      *exact = 1;
      return;
   }

   /* Retrieve color table entries. */
   /* XXX alloca candidate. */
   ctable = (XColor *) malloc(cmapSize * sizeof(XColor));
   for (i = 0; i < cmapSize; i++) {
      ctable[i].pixel = i;
   }
   XQueryColors(dpy, cmap, ctable, cmapSize);

   /* Find best match. */
   bestmatch = -1;
   mindist = 0.0;
   for (i = 0; i < cmapSize; i++) {
      double dr = (double) color->red - (double) ctable[i].red;
      double dg = (double) color->green - (double) ctable[i].green;
      double db = (double) color->blue - (double) ctable[i].blue;
      double dist = dr * dr + dg * dg + db * db;
      if (bestmatch < 0 || dist < mindist) {
         bestmatch = i;
         mindist = dist;
      }
   }

   /* Return result. */
   subColor.red = ctable[bestmatch].red;
   subColor.green = ctable[bestmatch].green;
   subColor.blue = ctable[bestmatch].blue;
   free(ctable);
   /* Try to allocate the closest match color.  This should only
    * fail if the cell is read/write.  Otherwise, we're incrementing
    * the cell's reference count.
    */
   if (!XAllocColor(dpy, cmap, &subColor)) {
      /* do this to work around a problem reported by Frank Ortega */
      subColor.pixel = (unsigned long) bestmatch;
      subColor.red   = ctable[bestmatch].red;
      subColor.green = ctable[bestmatch].green;
      subColor.blue  = ctable[bestmatch].blue;
      subColor.flags = DoRed | DoGreen | DoBlue;
   }
   *color = subColor;
}

#elif defined(WIN32)

static UINT Win32AllocColor( const struct Togl *togl,
                             float red, float green, float blue )
{
/* Modified version of XAllocColor emulation of Tk.
*      - returns index, instead of color itself
*      - allocates logical palette entry even for non-palette devices
*/

    TkWinColormap *cmap = (TkWinColormap *) Tk_Colormap(togl->TkWin);
    UINT index;
    COLORREF newColor, closeColor;
    PALETTEENTRY entry, closeEntry;
    int new, refCount;
    Tcl_HashEntry *entryPtr;

    entry.peRed =  red*255 + .5;
    entry.peGreen = green*255 + .5;
    entry.peBlue = blue*255 + .5;
    entry.peFlags = 0;

	/*
	 * Find the nearest existing palette entry.
	 */

    newColor = RGB(entry.peRed, entry.peGreen, entry.peBlue);
    index = GetNearestPaletteIndex(cmap->palette, newColor);
    GetPaletteEntries(cmap->palette, index, 1, &closeEntry);
    closeColor = RGB(closeEntry.peRed, closeEntry.peGreen,  closeEntry.peBlue);

     /*
	 * If this is not a duplicate and colormap is not full, allocate a new entry.
	 */

	if (newColor != closeColor) {
        if (cmap->size == togl->CiColormapSize) {
            entry = closeEntry;
        }
        else {
            cmap->size++;
		    ResizePalette(cmap->palette, cmap->size);
		    index = cmap->size -1;
		    SetPaletteEntries(cmap->palette, index, 1, &entry);
		    SelectPalette(togl->tglGLHdc, cmap->palette, TRUE);
		    RealizePalette(togl->tglGLHdc);
		}
	}
	newColor = PALETTERGB(entry.peRed, entry.peGreen, entry.peBlue);
	entryPtr = Tcl_CreateHashEntry(&cmap->refCounts, (char *) newColor, &new);
	if (new) {
	    refCount = 1;
	} else {
	    refCount = ((int) Tcl_GetHashValue(entryPtr)) + 1;
	}
	Tcl_SetHashValue(entryPtr, (ClientData)refCount);

   /* for EPS output */
    togl->EpsRedMap[index] = entry.peRed / 255.0;
    togl->EpsGreenMap[index] = entry.peGreen / 255.0;
    togl->EpsBlueMap[index] = entry.peBlue / 255.0;
    return index;
}

static void Win32FreeColor( const struct Togl *togl, unsigned long index )
{
    TkWinColormap *cmap = (TkWinColormap *) Tk_Colormap(togl->TkWin);
    COLORREF cref;
    UINT count, refCount;
    PALETTEENTRY entry, *entries;
    Tcl_HashEntry *entryPtr;

	if (index >= cmap->size ) {
		panic("Tried to free a color that isn't allocated.");
	}
	GetPaletteEntries(cmap->palette, index, 1, &entry);
	cref = PALETTERGB(entry.peRed, entry.peGreen, entry.peBlue);
	entryPtr = Tcl_FindHashEntry(&cmap->refCounts, (char *) cref);
	if (!entryPtr) {
		panic("Tried to free a color that isn't allocated.");
	}
	refCount = (int) Tcl_GetHashValue(entryPtr) - 1;
	if (refCount == 0) {
		count = cmap->size - index;
		entries = (PALETTEENTRY *) ckalloc(sizeof(PALETTEENTRY)* count);
		GetPaletteEntries(cmap->palette, index+1, count, entries);
		SetPaletteEntries(cmap->palette, index, count, entries);
		SelectPalette(togl->tglGLHdc, cmap->palette, TRUE);
		RealizePalette(togl->tglGLHdc);
		ckfree((char *) entries);
		cmap->size--;
		Tcl_DeleteHashEntry(entryPtr);
	} else {
		Tcl_SetHashValue(entryPtr, (ClientData)refCount);
	}
}

static void Win32SetColor( const struct Togl *togl,
                    unsigned long index, float red, float green, float blue )
{
    TkWinColormap *cmap = (TkWinColormap *) Tk_Colormap(togl->TkWin);
    PALETTEENTRY entry;

    entry.peRed =  red*255 + .5;
    entry.peGreen = green*255 + .5;
    entry.peBlue = blue*255 + .5;
    entry.peFlags = 0;
    SetPaletteEntries(cmap->palette, index, 1, &entry);
	SelectPalette(togl->tglGLHdc, cmap->palette, TRUE);
	RealizePalette(togl->tglGLHdc);

	/* for EPS output */
    togl->EpsRedMap[index] = entry.peRed / 255.0;
    togl->EpsGreenMap[index] = entry.peGreen / 255.0;
    togl->EpsBlueMap[index] = entry.peBlue / 255.0;
}
#endif /* X11 */



unsigned long Togl_AllocColor( const struct Togl *togl,
                               float red, float green, float blue )
{
   XColor xcol;
   int exact;

   if (togl->RgbaFlag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return 0;
   }
   /* TODO: maybe not... */
   if (togl->PrivateCmapFlag) {
      fprintf(stderr,"Error: Togl_FreeColor illegal with private colormap\n");
      return 0;
   }

#if defined(X11)
   xcol.red   = (short) (red   * 65535.0);
   xcol.green = (short) (green * 65535.0);
   xcol.blue  = (short) (blue  * 65535.0);

   noFaultXAllocColor( Tk_Display(togl->TkWin), Tk_Colormap(togl->TkWin),
                       Tk_Visual(togl->TkWin)->map_entries, &xcol, &exact );


   /* for EPS output */
   togl->EpsRedMap[ xcol.pixel] = xcol.red / 65535.0;
   togl->EpsGreenMap[ xcol.pixel] = xcol.green / 65535.0;
   togl->EpsBlueMap[ xcol.pixel] = xcol.blue / 65535.0;

   return xcol.pixel;
#elif defined(WIN32)
   return Win32AllocColor( togl, red, green, blue );
#endif /* X11 */
}



void Togl_FreeColor( const struct Togl *togl, unsigned long pixel )
{
   if (togl->RgbaFlag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return;
   }
   /* TODO: maybe not... */
   if (togl->PrivateCmapFlag) {
      fprintf(stderr,"Error: Togl_FreeColor illegal with private colormap\n");
      return;
   }

#if defined(X11)
   XFreeColors( Tk_Display(togl->TkWin), Tk_Colormap(togl->TkWin),
                &pixel, 1, 0 );
#elif defined(WIN32)
   Win32FreeColor(togl, pixel);
#endif /* X11 */
}



void Togl_SetColor( const struct Togl *togl,
                    unsigned long index, float red, float green, float blue )
{
   XColor xcol;

   if (togl->RgbaFlag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return;
   }
   if (!togl->PrivateCmapFlag) {
      fprintf(stderr,"Error: Togl_SetColor requires a private colormap\n");
      return;
   }

#if defined(X11)
   xcol.pixel = index;
   xcol.red   = (short) (red   * 65535.0);
   xcol.green = (short) (green * 65535.0);
   xcol.blue  = (short) (blue  * 65535.0);
   xcol.flags = DoRed | DoGreen | DoBlue;

   XStoreColor( Tk_Display(togl->TkWin), Tk_Colormap(togl->TkWin), &xcol );

   /* for EPS output */
   togl->EpsRedMap[ xcol.pixel] = xcol.red / 65535.0;
   togl->EpsGreenMap[ xcol.pixel] = xcol.green / 65535.0;
   togl->EpsBlueMap[ xcol.pixel] = xcol.blue / 65535.0;

#elif defined(WIN32)
   Win32SetColor( togl, index, red, green, blue );
#endif /* X11 */
}



#if defined(WIN32)
#include "tkFont.h"

/*
 * The following structure represents Windows' implementation of a font.
 */

typedef struct WinFont {
    TkFont font;		/* Stuff used by generic font package.  Must
				 * be first in structure. */
    HFONT hFont;		/* Windows information about font. */
    HWND hwnd;			/* Toplevel window of application that owns
				 * this font, used for getting HDC. */
    int widths[256];		/* Widths of first 256 chars in this font. */
} WinFont;
#endif /* WIN32 */


#define MAX_FONTS 1000
static GLuint ListBase[MAX_FONTS];
static GLuint ListCount[MAX_FONTS];



/*
 * Load the named bitmap font as a sequence of bitmaps in a display list.
 * fontname may be one of the predefined fonts like TOGL_BITMAP_8_BY_13
 * or an X font name, or a Windows font name, etc.
 */
GLuint Togl_LoadBitmapFont( const struct Togl *togl, const char *fontname )
{
   static int FirstTime = 1;
#if defined(X11)
   XFontStruct *fontinfo;
#elif defined(WIN32)
   WinFont *winfont;
   HFONT oldFont;
   TEXTMETRIC tm;
#endif /* X11 */
   int first, last, count;
   GLuint fontbase;
   const char *name;

   /* Initialize the ListBase and ListCount arrays */
   if (FirstTime) {
      int i;
      for (i=0;i<MAX_FONTS;i++) {
         ListBase[i] = ListCount[i] = 0;
      }
      FirstTime = 0;
   }

   /*
    * This method of selecting X fonts according to a TOGL_ font name
    * is a kludge.  To be fixed when I find time...
    */
   if (fontname==TOGL_BITMAP_8_BY_13) {
      name = "8x13";
   }
   else if (fontname==TOGL_BITMAP_9_BY_15) {
      name = "9x15";
   }
   else if (fontname==TOGL_BITMAP_TIMES_ROMAN_10) {
      name = "-adobe-times-medium-r-normal--10-100-75-75-p-54-iso8859-1";
   }
   else if (fontname==TOGL_BITMAP_TIMES_ROMAN_24) {
      name = "-adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1";
   }
   else if (fontname==TOGL_BITMAP_HELVETICA_10) {
      name = "-adobe-helvetica-medium-r-normal--10-100-75-75-p-57-iso8859-1";
   }
   else if (fontname==TOGL_BITMAP_HELVETICA_12) {
      name = "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1";
   }
   else if (fontname==TOGL_BITMAP_HELVETICA_18) {
      name = "-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1";
   }
   else if (!fontname) {
      name = DEFAULT_FONTNAME;
   }
   else {
      name = (const char *) fontname;
   }

   assert( name );

#if defined(X11)
   fontinfo = XLoadQueryFont( Tk_Display(togl->TkWin), name );
   if (!fontinfo) {
      return 0;
   }

   first = fontinfo->min_char_or_byte2;
   last = fontinfo->max_char_or_byte2;
#elif defined(WIN32)
   winfont = (WinFont*) Tk_GetFont(togl->Interp, togl->TkWin, name);
   if (!winfont) {
      return 0;
   }
   oldFont = SelectObject(togl->tglGLHdc, winfont->hFont);
   GetTextMetrics(togl->tglGLHdc, &tm);
   first = tm.tmFirstChar;
   last = tm.tmLastChar;
#endif /* X11 */
   count = last-first+1;

   fontbase = glGenLists( (GLuint) (last+1) );
   if (fontbase==0) {
#ifdef WIN32
      SelectObject(togl->tglGLHdc, oldFont);
      Tk_FreeFont((Tk_Font) winfont);
#endif /* WIN32 */
      return 0;
   }
#if defined(WIN32)
   wglUseFontBitmaps(togl->tglGLHdc, first, count, (int) fontbase+first );
   SelectObject(togl->tglGLHdc, oldFont);
   Tk_FreeFont((Tk_Font) winfont);
#elif defined(X11)
   glXUseXFont( fontinfo->fid, first, count, (int) fontbase+first );
#endif

   /* Record the list base and number of display lists
    * for Togl_UnloadBitmapFont().
    */
   {
      int i;
      for (i=0;i<MAX_FONTS;i++) {
         if (ListBase[i]==0) {
            ListBase[i] = fontbase;
            ListCount[i] = last+1;
            break;
         }
      }
   }

   return fontbase;
}



/*
 * Release the display lists which were generated by Togl_LoadBitmapFont().
 */
void Togl_UnloadBitmapFont( const struct Togl *togl, GLuint fontbase )
{
   int i;
   (void) togl;
   for (i=0;i<MAX_FONTS;i++) {
      if (ListBase[i]==fontbase) {
         glDeleteLists( ListBase[i], ListCount[i] );
         ListBase[i] = ListCount[i] = 0;
         return;
      }
   }
}


/*
 * Overlay functions
 */


void Togl_UseLayer( struct Togl *togl, int layer )
{
   if (togl->OverlayWindow) {
      if (layer==TOGL_OVERLAY) {
#if defined(WIN32)
         int res = wglMakeCurrent(togl->tglGLHdc, togl->tglGLOverlayHglrc);
         assert(res == TRUE);
#elif defined(X11)
	 glXMakeCurrent( Tk_Display(togl->TkWin),
			 togl->OverlayWindow,
			 togl->OverlayCtx );
#if defined(__sgi) && defined(STEREO)
	stereoMakeCurrent( Tk_Display(togl->TkWin),
			   togl->OverlayWindow,
			   togl->OverlayCtx );
#endif /* __sgi STEREO */
#endif /*WIN32 */
      }
      else if (layer==TOGL_NORMAL) {
#if defined(WIN32)
	int res = wglMakeCurrent(togl->tglGLHdc, togl->tglGLHglrc);
	assert(res == TRUE);
#elif defined(X11)
	glXMakeCurrent( Tk_Display(togl->TkWin),
			Tk_WindowId(togl->TkWin),
			togl->GlCtx );
#if defined(__sgi) && defined(STEREO)
	stereoMakeCurrent( Tk_Display(togl->TkWin),
			Tk_WindowId(togl->TkWin),
			togl->GlCtx );
#endif /* __sgi STEREO */
#endif /* WIN32 */
      }
      else {
         /* error */
      }
   }
}



#if defined(X11)  /* not yet implemented on Windows*/
void Togl_ShowOverlay( struct Togl *togl )
{
   if (togl->OverlayWindow) {
      XMapWindow( Tk_Display(togl->TkWin), togl->OverlayWindow );
      XInstallColormap(Tk_Display(togl->TkWin),togl->OverlayCmap);
      togl->OverlayIsMapped = 1;
   }
}
#endif /* X11 */



void Togl_HideOverlay( struct Togl *togl )
{
   if (togl->OverlayWindow && togl->OverlayIsMapped) {
      XUnmapWindow( Tk_Display(togl->TkWin), togl->OverlayWindow );
      togl->OverlayIsMapped=0;
   }
}



void Togl_PostOverlayRedisplay( struct Togl *togl )
{
   if (togl->OverlayWindow && togl->OverlayDisplayProc) {
      Tk_DoWhenIdle( RenderOverlay, (ClientData) togl );
      togl->OverlayUpdatePending = 1;
   }
}


void Togl_OverlayDisplayFunc( Togl_Callback *proc )
{
   OverlayDisplayProc = proc;
}


int Togl_ExistsOverlay( const struct Togl *togl )
{
   return togl->OverlayFlag;
}


int Togl_GetOverlayTransparentValue( const struct Togl *togl )
{
   return togl->OverlayTransparentPixel;
}


int Togl_IsMappedOverlay( const struct Togl *togl )
{
   return togl->OverlayFlag && togl->OverlayIsMapped;
}


#if defined(X11) /* not yet implemented on Windows*/
unsigned long Togl_AllocColorOverlay( const struct Togl *togl,
                                      float red, float green, float blue )
{
   if (togl->OverlayFlag && togl->OverlayCmap) {
      XColor xcol;
      xcol.red   = (short) (red* 65535.0);
      xcol.green = (short) (green* 65535.0);
      xcol.blue  = (short) (blue* 65535.0);
      if (!XAllocColor(Tk_Display(togl->TkWin),togl->OverlayCmap,&xcol))
         return (unsigned long) -1;
      return xcol.pixel;
   }
   else {
      return (unsigned long) -1;
   }
}


void Togl_FreeColorOverlay( const struct Togl *togl, unsigned long pixel )
{

   if (togl->OverlayFlag && togl->OverlayCmap) {
      XFreeColors( Tk_Display(togl->TkWin), togl->OverlayCmap,
                   &pixel, 1, 0 );
   }
}
#endif /* X11 */



/*
 * User client data
 */

void Togl_ClientData( ClientData clientData )
{
   DefaultClientData = clientData;
}


ClientData Togl_GetClientData( const struct Togl *togl )
{
   return togl->Client_Data;
}


void Togl_SetClientData( struct Togl *togl, ClientData clientData )
{
   togl->Client_Data = clientData;
}



/*
 * X11-only functions
 * Contributed by Miguel A. De Riera Pasenau (miguel@DALILA.UPC.ES)
 */

Display* Togl_Display( const struct Togl *togl)
{
   return Tk_Display(togl->TkWin);
}

Screen* Togl_Screen( const struct Togl *togl)
{
   return Tk_Screen(togl->TkWin);
}

int Togl_ScreenNumber( const struct Togl *togl)
{
   return Tk_ScreenNumber(togl->TkWin);
}

Colormap Togl_Colormap( const struct Togl *togl)
{
   return Tk_Colormap(togl->TkWin);
}



#ifdef MESA_COLOR_HACK
/*
 * Let's know how many free colors do we have
 */
#if 0
static unsigned char rojo[] = { 4, 39, 74, 110, 145, 181, 216, 251},
                     verde[] = { 4, 39, 74, 110, 145, 181, 216, 251},
		     azul[] = { 4, 39, 74, 110, 145, 181, 216, 251};

unsigned char rojo[] = { 4, 36, 72, 109, 145, 182, 218, 251},
              verde[] = { 4, 36, 72, 109, 145, 182, 218, 251},
              azul[] = { 4, 36, 72, 109, 145, 182, 218, 251};
              azul[] = { 0, 85, 170, 255};
#endif

#define RLEVELS     5
#define GLEVELS     9
#define BLEVELS     5

/* to free dithered_rgb_colormap pixels allocated by Mesa */
static unsigned long *ToglMesaUsedPixelCells = NULL;
static int ToglMesaUsedFreeCells = 0;

static int get_free_color_cells( Display *display, int screen,
                                 Colormap colormap)
{
   if ( !ToglMesaUsedPixelCells) {
      XColor xcol;
      int i;
      int colorsfailed, ncolors = XDisplayCells( display, screen);

      long r, g, b;

      ToglMesaUsedPixelCells = ( unsigned long *)calloc( ncolors, sizeof( unsigned long));

      /* Allocate X colors and initialize color_table[], red_table[], etc */
      /* de Mesa 2.1: xmesa1.c setup_dithered_(...) */
      i = colorsfailed = 0;
      for (r = 0; r < RLEVELS; r++)
         for (g = 0; g < GLEVELS; g++)
            for (b = 0; b < BLEVELS; b++) {
               int exact;
               xcol.red   = ( r*65535)/(RLEVELS-1);
               xcol.green = ( g*65535)/(GLEVELS-1);
               xcol.blue  = ( b*65535)/(BLEVELS-1);
               noFaultXAllocColor( display, colormap, ncolors,
                                   &xcol, &exact );
               ToglMesaUsedPixelCells[ i++] = xcol.pixel;
               if (!exact) {
                  colorsfailed++;
               }
            }
      ToglMesaUsedFreeCells = i;

      XFreeColors( display, colormap, ToglMesaUsedPixelCells,
                   ToglMesaUsedFreeCells, 0x00000000);
   }
   return ToglMesaUsedFreeCells;
}


static void free_default_color_cells( Display *display, Colormap colormap)
{
   if ( ToglMesaUsedPixelCells) {
      XFreeColors( display, colormap, ToglMesaUsedPixelCells,
                   ToglMesaUsedFreeCells, 0x00000000);
      free( ( char *)ToglMesaUsedPixelCells);
      ToglMesaUsedPixelCells = NULL;
      ToglMesaUsedFreeCells = 0;
   }
}
#endif


/*
 * Generate EPS file.
 * Contributed by Miguel A. De Riera Pasenau (miguel@DALILA.UPC.ES)
 */

/* Function that creates a EPS File from a created pixmap on the current
 * context.
 * Based on the code from Copyright (c) Mark J. Kilgard, 1996.
 * Parameters: name_file, b&w / Color flag, redraw function.
 * The redraw function is needed in order to draw things into the new
 * created pixmap.
 */

/* Copyright (c) Mark J. Kilgard, 1996. */

static GLvoid *grabPixels(int inColor, unsigned int width, unsigned int height)
{
   GLvoid *buffer;
   GLint swapbytes, lsbfirst, rowlength;
   GLint skiprows, skippixels, alignment;
   GLenum format;
   unsigned int size;

   if (inColor) {
      format = GL_RGB;
      size = width * height * 3;
   }
   else {
      format = GL_LUMINANCE;
      size = width * height * 1;
   }

   buffer = (GLvoid *) malloc(size);
   if (buffer == NULL)
      return NULL;

   /* Save current modes. */
   glGetIntegerv(GL_PACK_SWAP_BYTES, &swapbytes);
   glGetIntegerv(GL_PACK_LSB_FIRST, &lsbfirst);
   glGetIntegerv(GL_PACK_ROW_LENGTH, &rowlength);
   glGetIntegerv(GL_PACK_SKIP_ROWS, &skiprows);
   glGetIntegerv(GL_PACK_SKIP_PIXELS, &skippixels);
   glGetIntegerv(GL_PACK_ALIGNMENT, &alignment);
   /* Little endian machines (DEC Alpha for example) could
      benefit from setting GL_PACK_LSB_FIRST to GL_TRUE
      instead of GL_FALSE, but this would require changing the
      generated bitmaps too. */
   glPixelStorei(GL_PACK_SWAP_BYTES, GL_FALSE);
   glPixelStorei(GL_PACK_LSB_FIRST, GL_FALSE);
   glPixelStorei(GL_PACK_ROW_LENGTH, 0);
   glPixelStorei(GL_PACK_SKIP_ROWS, 0);
   glPixelStorei(GL_PACK_SKIP_PIXELS, 0);
   glPixelStorei(GL_PACK_ALIGNMENT, 1);

   /* Actually read the pixels. */
   glReadPixels(0, 0, width, height, format,
                GL_UNSIGNED_BYTE, (GLvoid *) buffer);

   /* Restore saved modes. */
   glPixelStorei(GL_PACK_SWAP_BYTES, swapbytes);
   glPixelStorei(GL_PACK_LSB_FIRST, lsbfirst);
   glPixelStorei(GL_PACK_ROW_LENGTH, rowlength);
   glPixelStorei(GL_PACK_SKIP_ROWS, skiprows);
   glPixelStorei(GL_PACK_SKIP_PIXELS, skippixels);
   glPixelStorei(GL_PACK_ALIGNMENT, alignment);
   return buffer;
}


static int generateEPS(const char *filename, int inColor,
                       unsigned int width, unsigned int height)
{
   FILE *fp;
   GLvoid *pixels;
   unsigned char *curpix;
   unsigned int components, i;
   int pos;
   unsigned char bitpixel;

   pixels = grabPixels(inColor, width, height);
   if (pixels == NULL)
      return 1;
   if (inColor)
      components = 3;     /* Red, green, blue. */
   else
      components = 1;     /* Luminance. */

   fp = fopen(filename, "w");
   if (fp == NULL) {
      return 2;
   }
   fprintf(fp, "%%!PS-Adobe-2.0 EPSF-1.2\n");
   fprintf(fp, "%%%%Creator: OpenGL pixmap render output\n");
   fprintf(fp, "%%%%BoundingBox: 0 0 %d %d\n", width, height);
   fprintf(fp, "%%%%EndComments\n");

   i = ((( width * height) + 7) / 8 ) / 40; /* # of lines, 40 bytes per line */
   fprintf(fp, "%%%%BeginPreview: %d %d %d %d\n%%", width, height, 1, i);
   pos = 0;
   curpix = ( unsigned char *)pixels;
   for ( i = 0; i < width * height * components; ) {
      bitpixel = 0;
      if ( inColor) {
         double pix = 0.0;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x80;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x40;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x20;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x10;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x08;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x04;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x02;
         pix = 0.30 * ( double)curpix[ i++] + 0.59 * ( double)curpix[ i++] + 0.11 * ( double)curpix[ i++];
         if ( pix > 127.0) bitpixel |= 0x01;
      }
      else {
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x80;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x40;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x20;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x10;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x08;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x04;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x02;
         if ( curpix[ i++] > 0x7f) bitpixel |= 0x01;
      }
      fprintf(fp, "%02hx", bitpixel);
      if (++pos >= 40) {
         fprintf(fp, "\n%%");
         pos = 0;
      }
   }
   if (pos)
      fprintf(fp, "\n%%%%EndPreview\n");
   else
      fprintf(fp, "%%EndPreview\n");

   fprintf(fp, "gsave\n");
   fprintf(fp, "/bwproc {\n");
   fprintf(fp, "    rgbproc\n");
   fprintf(fp, "    dup length 3 idiv string 0 3 0\n");
   fprintf(fp, "    5 -1 roll {\n");
   fprintf(fp, "    add 2 1 roll 1 sub dup 0 eq\n");
   fprintf(fp, "    { pop 3 idiv 3 -1 roll dup 4 -1 roll dup\n");
   fprintf(fp, "        3 1 roll 5 -1 roll put 1 add 3 0 }\n");
   fprintf(fp, "    { 2 1 roll } ifelse\n");
   fprintf(fp, "    } forall\n");
   fprintf(fp, "    pop pop pop\n");
   fprintf(fp, "} def\n");
   fprintf(fp, "systemdict /colorimage known not {\n");
   fprintf(fp, "    /colorimage {\n");
   fprintf(fp, "        pop\n");
   fprintf(fp, "        pop\n");
   fprintf(fp, "        /rgbproc exch def\n");
   fprintf(fp, "        { bwproc } image\n");
   fprintf(fp, "    } def\n");
   fprintf(fp, "} if\n");
   fprintf(fp, "/picstr %d string def\n", width * components);
   fprintf(fp, "%d %d scale\n", width, height);
   fprintf(fp, "%d %d %d\n", width, height, 8);
   fprintf(fp, "[%d 0 0 %d 0 0]\n", width, height);
   fprintf(fp, "{currentfile picstr readhexstring pop}\n");
   fprintf(fp, "false %d\n", components);
   fprintf(fp, "colorimage\n");

   curpix = (unsigned char *) pixels;
   pos = 0;
   for (i = width * height * components; i > 0; i--) {
      fprintf(fp, "%02hx", *curpix++);
      if (++pos >= 40) {
	 fprintf(fp, "\n");
	 pos = 0;
      }
   }
   if (pos)
      fprintf(fp, "\n");

   fprintf(fp, "grestore\n");
   free(pixels);
   fclose(fp);
   return 0;
}


/* int Togl_DumpToEpsFile( const struct Togl *togl, const char *filename,
                        int inColor, void (*user_redraw)(void)) */
/* changed by GG */
int Togl_DumpToEpsFile( const struct Togl *togl, const char *filename,
                        int inColor, void (*user_redraw)( const struct Togl *))
{
   int using_mesa = 0;
#if 0
   Pixmap eps_pixmap;
   GLXPixmap eps_glxpixmap;
   XVisualInfo *vi = togl->EpsVisual;
   Window win = Tk_WindowId( togl->TkWin);
#endif
   Display *dpy = Tk_Display( togl->TkWin);
   int retval;
   int scrnum = Tk_ScreenNumber(togl->TkWin);
   unsigned int width = togl->Width, height = togl->Height;

#if defined(X11)
   if (strstr(glXQueryServerString( dpy, scrnum, GLX_VERSION ), "Mesa"))
      using_mesa = 1;
   else
#endif /* X11 */
      using_mesa = 0;
   /* I don't use Pixmap do drawn into, because the code should link
    * with Mesa libraries and OpenGL libraries, and the which library
    * we use at run time should not matter, but the name of the calls
    * differs one from another:
    * MesaGl: glXCreateGLXPixmapMESA( dpy, vi, eps_pixmap, Tk_Colormap(togl->TkWin))
    * OpenGl: glXCreateGLXPixmap( dpy, vi, eps_pixmap);
    *
    * instead of this I read direct from back buffer of the screeen.
    */
#if 0
   eps_pixmap = XCreatePixmap( dpy, win, width, height, vi->depth);
   if ( using_mesa)
      eps_glxpixmap = glXCreateGLXPixmapMESA( dpy, vi, eps_pixmap, Tk_Colormap(togl->TkWin));
   else
      eps_glxpixmap = glXCreateGLXPixmap( dpy, vi, eps_pixmap);

   glXMakeCurrent( dpy, eps_glxpixmap, togl->GlCtx);
   user_redraw();
#endif
   if ( !togl->RgbaFlag) {

#if defined(WIN32)
/* Due to the lack of a unique inverse mapping from the frame buffer to
   the logical palette we need a translation map from the complete
   logical palette. */
       {
           int n, i;
           TkWinColormap *cmap = (TkWinColormap *)Tk_Colormap(togl->TkWin);
           LPPALETTEENTRY entry = malloc(togl->EpsMapSize * sizeof(PALETTEENTRY));
           n = GetPaletteEntries(cmap->palette, 0, togl->EpsMapSize, entry);
           for (i=0; i<n; i++) {
               togl->EpsRedMap[i] = entry[i].peRed / 255.0;
               togl->EpsGreenMap[i] = entry[i].peGreen / 255.0;
               togl->EpsBlueMap[i] = entry[i].peBlue / 255.0;
           }
           free(entry);
       }
#endif /* WIN32 */

      glPixelMapfv( GL_PIXEL_MAP_I_TO_R, togl->EpsMapSize, togl->EpsRedMap);
      glPixelMapfv( GL_PIXEL_MAP_I_TO_G, togl->EpsMapSize, togl->EpsGreenMap);
      glPixelMapfv( GL_PIXEL_MAP_I_TO_B, togl->EpsMapSize, togl->EpsBlueMap);
   }
   /*  user_redraw(); */
   user_redraw(togl);  /* changed by GG */
   /* glReadBuffer( GL_FRONT); */
   /* by default it read GL_BACK in double buffer mode*/
   glFlush();
   retval = generateEPS( filename, inColor, width, height);
#if 0
   glXMakeCurrent( dpy, win, togl->GlCtx );
   glXDestroyGLXPixmap( dpy, eps_glxpixmap);
   XFreePixmap( dpy, eps_pixmap);
#endif
   return retval;
}

/*
 * Full screen stereo for SGI graphics
 * Contributed by Ben Evans (Ben.Evans@anusf.anu.edu.au)
 * This code was based on SGI's /usr/share/src/OpenGL/teach/stereo
 */

#if defined(__sgi) && defined(STEREO)

static struct stereoStateRec {
    Bool        useSGIStereo;
    Display     *currentDisplay;
    Window      currentWindow;
    GLXContext  currentContext;
    GLenum      currentDrawBuffer;
    int         currentStereoBuffer;
    Bool        enabled;
    char        *stereoCommand;
    char        *restoreCommand;
} stereo;

/* call instead of glDrawBuffer */
void
Togl_StereoDrawBuffer(GLenum mode)
{
  if (stereo.useSGIStereo) {
    stereo.currentDrawBuffer = mode;
    switch (mode) {
    case GL_FRONT:
    case GL_BACK:
    case GL_FRONT_AND_BACK:
      /*
      ** Simultaneous drawing to both left and right buffers isn't
      ** really possible if we don't have a stereo capable visual.
      ** For now just fall through and use the left buffer.
      */
    case GL_LEFT:
    case GL_FRONT_LEFT:
    case GL_BACK_LEFT:
      stereo.currentStereoBuffer = STEREO_BUFFER_LEFT;
      break;
    case GL_RIGHT:
    case GL_FRONT_RIGHT: 
      stereo.currentStereoBuffer = STEREO_BUFFER_RIGHT;
      mode = GL_FRONT;
      break;
    case GL_BACK_RIGHT:
      stereo.currentStereoBuffer = STEREO_BUFFER_RIGHT;
      mode = GL_BACK;
      break;
    default:
      break;
    }
    if (stereo.currentDisplay && stereo.currentWindow) {
      glXWaitGL();  /* sync with GL command stream before calling X */
      XSGISetStereoBuffer(stereo.currentDisplay,
			  stereo.currentWindow,
			  stereo.currentStereoBuffer);
      glXWaitX();   /* sync with X command stream before calling GL */
    }
  }
  glDrawBuffer(mode);
}

/* call instead of glClear */
void
Togl_StereoClear(GLbitfield mask)
{
  GLenum drawBuffer;
  if (stereo.useSGIStereo) {
    drawBuffer = stereo.currentDrawBuffer;
    switch (drawBuffer) {
    case GL_FRONT:
      stereoDrawBuffer(GL_FRONT_RIGHT);
      glClear(mask);
      stereoDrawBuffer(drawBuffer);
      break;
    case GL_BACK:
      stereoDrawBuffer(GL_BACK_RIGHT);
      glClear(mask);
      stereoDrawBuffer(drawBuffer);
      break;
    case GL_FRONT_AND_BACK:
      stereoDrawBuffer(GL_RIGHT);
      glClear(mask);
      stereoDrawBuffer(drawBuffer);
      break;
    case GL_LEFT:
    case GL_FRONT_LEFT:
    case GL_BACK_LEFT:
    case GL_RIGHT:
    case GL_FRONT_RIGHT:
    case GL_BACK_RIGHT:
    default:
      break;
    }
  }
  glClear(mask);
}

static void
stereoMakeCurrent(Display *dpy, Window win, GLXContext ctx)
{
  
  if (stereo.useSGIStereo) {
    if (dpy && (dpy != stereo.currentDisplay)) {
      int event, error;
      /* Make sure new Display supports SGIStereo */
      if (XSGIStereoQueryExtension(dpy, &event, &error) == False) {
	dpy = NULL;
      }
    }
    if (dpy && win && (win != stereo.currentWindow)) {
      /* Make sure new Window supports SGIStereo */
      if (XSGIQueryStereoMode(dpy, win) == X_STEREO_UNSUPPORTED) {
	win = None;
      }
    }
    if (ctx && (ctx != stereo.currentContext)) {
      GLint drawBuffer;
      glGetIntegerv(GL_DRAW_BUFFER, &drawBuffer);
      stereoDrawBuffer((GLenum) drawBuffer);
    }
    stereo.currentDisplay = dpy;
    stereo.currentWindow = win;
    stereo.currentContext = ctx;
  }
}


/* call before using stereo */
static void
stereoInit(struct Togl *togl,int stereoEnabled)
{
  stereo.useSGIStereo = stereoEnabled;
  stereo.currentDisplay = NULL;
  stereo.currentWindow = None;
  stereo.currentContext = NULL;
  stereo.currentDrawBuffer = GL_NONE;
  stereo.currentStereoBuffer = STEREO_BUFFER_NONE;
  stereo.enabled = False;
}


void
Togl_StereoFrustum(GLfloat left, GLfloat right, GLfloat bottom, GLfloat top,
               GLfloat near, GLfloat far, GLfloat eyeDist, GLfloat eyeOffset)
{
  GLfloat eyeShift = (eyeDist - near) * (eyeOffset / eyeDist);
  
  glFrustum(left+eyeShift, right+eyeShift, bottom, top, near, far);
  glTranslatef(-eyeShift, 0.0, 0.0);
}

#endif /* __sgi STEREO */
