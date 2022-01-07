Extra programs in the Mercury implementation
============================================

This directory contains various libraries and tools
that are not an integral part of the Mercury implementation.

Most of these can be built by running the commands
`mmake depend' and then `mmake' in the relevant subdirectory,
and many can be installed by running `mmake install'.

* base64 -- A library for base64 encoding and decoding.

* cgi -- Some Mercury library modules for doing HTML forms programming
  using CGI (the Common Gateway Interface).

* complex_numbers -- A Mercury library package containing support for
  complex and imaginary numbers.

* curs -- A Mercury library providing a somewhat more complete
  and more faithful binding to the curses and panel libraries
  (the latter is used to provide elementary windowing facilities,
  rather than doing so in Mercury).

* curses -- A Mercury library providing a (partial) binding to curses,
  a traditional Unix library that provides methods for manipulating
  terminal screens (creating windows, placing characters, etc).

* dynamic_linking -- An interface to the C functions dlopen(), dlsym(), etc.
  that are supported by most modern Unix systems.

* fixed -- An implementation of fixed-point arithmetic
  with the COBOL semantics.

* gmp_int -- A Mercury binding to GNU multi-precision library,
  providing integers with arbitrary precision.

* graphics -- Some packages for doing graphics programming and GUIs in Mercury: 
  - a binding to Tcl/Tk
  - a binding to OpenGL
  - a binding to GLUT
  - a binding to GLFW
  - a simplified binding to Xlib
  - a binding to Allegro/AllegroGL
  - a binding to Cairo.

* lex -- A lexer package for Mercury that works over the I/O state, strings,
  and so forth. It comes with a rich set of standard regular expressions,
  and users can add their own.                                                      
* log4m -- A Mercury implementation of logging, in the spirit of log4j
  <http://logging.apache.org/log4j/docs/>.

* moose -- A parser generator for Mercury. Moose works much like yacc or bison,
  in that it takes a grammar and generates a table driven LALR parser for it.
  You can add code to the grammar to handle synthesized or inherited
  attributes. Currently you need to write your own lexer to interface
  to moose.

* mopenssl -- A Mercury binding to the openssl library.

* morphine -- A trace analysis system for Mercury.

* mp_int -- A multi-precision integer type based on a binding
  to the libtommath library.

* net -- A network library which uses the standard library stream interface.

* odbc -- A Mercury interface to ODBC (Open Database Connectivity),
  for interfacing to standard relational database packages.

* old_term_parser -- A library containing versions of the standard library's
  lexer, parser, term, term_io and varset modules as they were on 2017-02-15.
  Intended for backwards compatibility with older code.

* posix -- A Mercury interface to some of the POSIX
  (Portable Operating System Interface) APIs.

* random -- Some additional instances of the random typeclasses from
  the standard library.

* references -- A library package containing modules for manipulating
  ML-style references (mutable state).

* solver_types -- Contains versions of some standard library modules
  adapted to make them suitable for use with solver types.

* trailed_update -- Some library modules that make use of backtrackable
  destructive update, including a module which provides some support
  for Prolog-style unification constraints.

* windows_installer_generator -- A library for generating WiX source files.
  WiX is an XML language that is used to generate Microsoft Windows Installer
  (.msi) packages.

* xml -- An XML parsing library.

* xml_stylesheets -- Sample stylesheets that can be used with the term_to_xml
  module in the standard library.
