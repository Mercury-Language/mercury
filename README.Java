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

To run a Mercury program using the java grade, you need to build the Mercury
library and runtime in the java grade, using the Mercury source distribution.

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

	construct
	deconstruct
	type_desc

   Type classes are not yet fully supported, nor are Mercury-level debugging
   and profiling for the Java back-end.

   In addition, the following individual procedures are incompletely
   implemented:

	benchmarking.report_stats/0:
	benchmarking.report_full_memory_stats/0:
		Memory usage statistics are not yet available, and cpu time
		is obtained via native code as per time.m.

	store.arg_ref/5:
	store.new_arg_ref/5:
		Due to the absence of RTTI, dynamic type checking is missing
		for these predicates.  They should be used with care.

	time.clock/3:
	time.clocks_per_sec/0:
	time.times/7:
	time.clk_tck/0:
		Because the current Java APIs do not provide any way of
		implementing these procedures in pure Java, we have implemented
		them using a native code library implemented in C and accessed
		via JNI.  This approach sacrifices some portability.

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
