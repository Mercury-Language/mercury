Mercury Sample Programs
=======================

This directory contains some example Mercury programs.

* [hello.m](hello.m) -- "Hello World" in Mercury.

* [cat.m](cat.m) -- An implementation of a simple version of the standard UNIX
  filter `cat`, which just copies its input files or the standard input stream
  to the standard output stream.

* [sort.m](sort.m) -- An implementation of a simple version of the standard
  UNIX filter `sort`, which reads lines from its input files or the standard
  input stream, sorts them, and then writes the result to the standard output
  stream.

* [calculator.m](calculator.m) -- A simple four-function arithmetic calculator,
  with a parser written using the Definite Clause Grammar notation.

* [calculator2.m](calculator2.m) -- A simple four-function arithmetic
  calculator, which uses the `mercury_term_parser` module in the standard
  library with a user-defined operator precedence table.

* [interpreter.m](interpreter.m) -- An simple interpreter for definite logic
  programs. A demonstration of meta-programming in Mercury.

* [expand_terms.m](expand_terms.m) -- Another example meta-program, showing how
  to emulate Prolog's `expand_term` mechanism.

* [e.m](e.m) -- A small program which calculates the base of natural logarithms
  to however many digits you choose. It illustrates one way to achieve lazy
  evaluation in Mercury.

* [eliza.m](eliza.m) -- An implementation of the famous computer
  psychotherapist.

* [beer.m](beer.m) -- A small program that prints the lyrics of the song
  "99 Bottle of Beer".

* [Mmakefile](Mmakefile) -- The file used by `mmake`, the Mercury Make program,
  to build the programs in this directory.

The [solutions](solutions) sub-directory contains some examples of the use of
nondeterminism, showing how a Mercury program can compute 

- one solution,
- all solutions, or
- some solutions (determined by a user-specified criteria)

for a query which has more than one logically correct answer.

The [concurrency](concurrency) sub-directory contains examples of how to use
Mercury's concurrency interface, i.e. using threads in Mercury programs.

There are also some sub-directories which contain examples of multi-module
Mercury programs:

* [appengine](appengine) -- A simple Google App Engine servlet.

* [diff](diff) -- This directory contains an implementation of a simple version
  of the standard UNIX utility `diff`, which prints the differences between two
  files.

* [c_interface](c_interface) -- This directory contains some examples of mixed
  Mercury/C/C++/Fortran programs using the C interface.

* [java_interface](java_interface) -- This directory contains some examples of
  mixed Mercury/Java programs using the foreign language interface.

* [rot13](rot13) -- This directory contains a few implementations of rot-13
  encoding.

* [muz](muz) -- This directory contains a syntax checker / type checker for the
  specification language Z.

* [solver_types](solver_types) -- This directory contains an example solver
  type implementation and some sample applications.

* [lazy_list](lazy_list) -- This directory contains an example of how the
  `lazy` module in the standard library can be used to implement lazy data
  structures, in this case a lazy list.
