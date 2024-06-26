Mercury Cairo Binding
=====================

This directory contains the package `mercury_cairo` which is a Mercury binding
to the [Cairo 2D graphics library](http://www.cairographics.org).

See the file [COPYING.LIB](COPYING.LIB) in this directory for copyright and
licensing information.


Building the binding
--------------------

1. Fill in the system-specific information for your system in the file
   [Cairo.options](Cairo.options).

2. Build and install with the following command:

   > $ mmc --make libmercury_cairo.install


Overview of the Mercury Cairo Binding
-------------------------------------

The remainder of this document outlines the mapping from the Cairo C API to
Mercury.  We follow the
[Cairo language binding guidelines](http://www.cairographics.org/manual/language-bindings.html),
where possible although some changes are necessary due to name clashes with
other Mercury libraries.


Mapping from C API types to Mercury types
-----------------------------------------

```
    cairo_t                       cairo.context/1.
    cairo_pattern_t               cairo.pattern/0.
    cairo_matrix_t                cairo.matrix/0.
    cairo_path_t                  cairo.path/0.
    cairo_font_options_t          cairo.font_options/0.
    cairo_region_t                cairo.region/0.
    cairo_scaled_font_t           cairo.scaled_font/1.    
    cairo_content_t               cairo.content/0
    cairo_format_t                cairo.format/0.
    cairo_status_t                cairo.status/0.
```

There are no types in the Mercury API that correspond directly to the C types
`cairo_surface_t` and `cairo_font_face_t`.  In the Mercury binding these are
represented by the type classes: `cairo.surface/1` and `cairo.font_face/1`.


Mapping from C functions to Mercury predicates
----------------------------------------------

In the Mercury binding, the Cairo API is split up into submodules, each of
which covers a specific area of functionality.

The top-level module in the binding is named [cairo](cairo.m).
It contains:

  * Bindings for the major types in Cairo.
  * Bindings for the operations on the Cairo drawing context.
  * Bindings for error handling functionality.

For example:

```
    cairo_create()          cairo.create_context/4.
    cairo_save()            cairo.save/3.           
    cairo_restore()         cairo.restore/2.
    ...
```

Operations that create and manipulate path data are in the submodule
[cairo.path](cairo.path.m).
For example:

```
    cairo_copy_path()       cairo.path.copy_path/4.
    cairo_copy_path_flat()  cairo.path.copy_path_flat/4.
    cairo_append_path()     cairo.path.append_path/4.
    ...
```

Operations on patterns, mainly those whose C names are begin with
`cairo_pattern_`, are in the submodule [cairo.pattern](cairo.pattern.m).
For example:

```
    cairo_pattern_add_color_stop_rgb()      cairo.pattern.add_color_stop_rgb/7
    cairo_pattern_add_color_stop_rgba()     cairo.pattern.add_color_stop_rgba/8
    cairo_pattern_create_rgb()              cairo.pattern.create_rgb/6
    ...
```

Operations on surfaces, those whose C name being with `cairo_surface_`,
are contained in the submodule [cairo.surface](cairo.surface.m).
For example:

```
    cairo_surface_finish()              cairo.surface.finish/3.
    cairo_surface_flush()               cairo.surface.flush/3.
    cairo_surface_get_font_options()    cairo.surface.get_font_options/4.
    ...
```

Backend Submodules
------------------

The Mercury binding has submodules that provide image, PDF, PostScript, SVG and
recording surfaces.  Each surface is represented by a type that is an instance
of the `cairo.surface/1 type class`.  For example, the interface to image
surfaces is in the submodule [cairo.image](cairo.image.m).

Image surfaces are always supported.  PDF, PostScript, SVG and recording
surfaces are optional.  The following predicates may be used to test if they
are supported by an installation:

```
    cairo.pdf.have_pdf_surface/0
    cairo.ps.have_ps_surface/0
    cairo.svg.have_svg_surface/0
    cairo.recording.have_recording_surface/0
```

(The predicate `cairo.png.png_is_supported/0` performs the same function for
determining whether the PNG reading and writing capabilities in the submodule
[cairo.png](cairo.png.m) are supported by an installation.)

Other surface types, e.g. X, Quartz, Win32, are not provided by the Mercury
Cairo binding.  The intention is that they can be provided by separate
libraries that would provide an instance of the `cairo.surface/1` type class as
well as bindings to the required backend specific Cairo functions.
