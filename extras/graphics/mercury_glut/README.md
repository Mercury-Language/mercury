Mercury GLUT Binding
====================

This directory contains the package `mercury_glut` which is a Mercury binding
to the GL utility toolkit (GLUT).

GLUT provides a (relatively) platform independent API for writing OpenGL
applications.  It provides most of the system dependent bits, such as window
management, that the OpenGL API does not.

If [freeglut](http://freeglut.sourceforge.net) is being used to provide GLUT
then `mercury_glut` also provides an interface to a number of freeglut specific
extensions.

The [Mmakefile](Mmakefile) includes instructions for building and installing
the library on Linux based systems.  Users of macOS, should instead see the
file [Mmakefile.MacOSX](Mmakefile.MacOSX) for instructions on how to build the
binding.
