-----------------------------------------------------------------------------

INTRODUCTION

This release of Mercury contains a port to Java,
in particular to Sun Microsystems' Java 2 Platform, Standard Edition (J2SE).
The Mercury compiler will generate Java source code that can be compiled into
Java bytecode suitable for running in the J2SE runtime system.

The port is mostly complete, but some parts of the Mercury standard
library are not yet implemented (for a full list see the FAQ below).

The port is currently targeted at J2SE version 1.2 or higher, but we recommend
version 1.4 for optimal performance and access to features.

PREREQUISITES

In order to try this system you will need 

	- The J2SE SDK, which can be downloaded for free from
		<http://java.sun.com/downloads/index.html>

	  OR any other compatible Java implementation.

	- The Mercury distribution -- installed as usual.  You can install
	  from either the source or binary distribution.

	  If you're reading this file from somewhere other than the
	  Mercury distribution, try the Mercury homepage at
	  	<http://www.cs.mu.oz.au/mercury/>

-----------------------------------------------------------------------------

THE JAVA GRADE

The Mercury compiler currently supports the grade `java' to target Java
bytecode.  The java grade is enabled by using any of the options
`--grade java', `--target java', or just `--java'.

Support for building and installation of this grade
is still somewhat rudimentary.

To run a Mercury program using the java grade, you need to build the Mercury
library and runtime in the java grade, using the Mercury source distribution.

You will also need to set the environment variable CLASSPATH to include the
mercury standard and runtime libraries for java, as well as the current
directory.  You should use a command of the form:

	CLASSPATH=<prefix>/mer_std.jar:<prefix>/mer_std.runtime.jar:.
	export CLASSPATH

where <prefix> is the location of the installed jar files, which will probably
be /usr/local/mercury/lib/mercury/lib/java or something similar.

You can now build programs such as hello.m or calculator.m in the samples
directory.
	
	cd samples
	mmc --make --java hello

Now you can run hello

	./hello

-----------------------------------------------------------------------------

USING JAVA

The Mercury standard library has not been fully ported to Java yet.
The use of unimplemented procedures will result in a run-time error,
with a message such as "Sorry, not implemented: foreign code for this
function", and a stack trace.

If you find missing functionality, you can interface to Java using Mercury's
foreign language interface.

For example:

:- pred to_string(T::in, string::out) is det.
:- pragma foreign_proc("Java", to_string(T::in, Str::out), [],
"
        Str = T.toString();
").

The implementation will include this Java code in the module's .java file, and
you can then call the predicate to_string exactly the same as if it were
implemented using pure mercury code.

For more information about the foreign language interface, see the Mercury
Language Reference Manual, which you can find at:
	<http://www.cs.mu.oz.au/mercury/information/documentation.html>

-----------------------------------------------------------------------------

RESOURCES

You might find the following pages useful:

	<http://www.cs.mu.oz.au/research/mercury/java.html>

	<http://java.sun.com/reference/api/index.html>

	<http://www.cs.mu.oz.au/mercury/information/documentation.html>

-----------------------------------------------------------------------------

FREQUENTLY ASKED QUESTIONS (FAQS)

Q. What are the advantages of using the Java back-end?

A. The main advantages are easy access to the wide range of libraries available
   for the J2SE platform, including web applet development, and the portability
   you get from using Java bytecode.


Q. What version of Java should I be using?

A. Java 2 Platform Standard Edition, version 1.2 or greater.  Note that some
   features require version 1.4 or greater, however.


Q. Which features?

A. When using a Java version earlier than 1.4, there are limitations on treating
   stdin and stdout as binary files with respect to seeking.  See the
   documentation for io.m in the standard library for more details.


Q. What features are not yet implemented?

A. The following standard library modules are completely unimplemented:

	benchmarking
	construct
	deconstruct
	store
	type_desc

   Type classes are not yet fully supported, nor are Mercury-level debugging
   and profiling for the Java back-end.

Q. So how do I enable Java-level debugging?

A. By default, javac already generates line number and source file debugging
   information.  You can include local variable debugging information by
   specifying "--target-debug" when invoking the Mercury compiler, or by
   setting the JAVACFLAGS variable to include "-g" when invoking mmake,
   e.g.

        mmc --make --java --target-debug <progname>

   or
   	
	mmake GRADE=java JAVACFLAGS=-g <progname>

   You can then use Sun's "jdb" debugging tool, which comes
   as part of the Java SDK distribution, to debug your program.
   For more information, see the documentation for javac and jdb.

-----------------------------------------------------------------------------
