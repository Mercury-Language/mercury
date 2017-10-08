-----------------------------------------------------------------------------

INTRODUCTION

The Mercury compiler can generate Java source code that can be compiled into
Java bytecode suitable for running the Java SE runtime system.  The port is
mostly complete, but some parts of the Mercury standard library are not yet
implemented (for a full list see the FAQ below).

The port requires Java SE 8 or higher -- older versions of Java are *not*
supported.

PREREQUISITES

In order to try this system you will need 

    * The J2SE SDK, which can be downloaded for free from
      <http://www.oracle.com/technetwork/indexes/downloads/index.html>

    * OR any other compatible Java implementation, such as OpenJDK.

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

Note that when building programs using the java grade you *must* use
mmc --make.

Now you can run hello

	./hello

Note that hello is a simple shell script that invokes the program using the
Java interpreter.  The actual class files are stored in the Mercury
subdirectory in `classs'.  These class files will be packaged up into Java
archive (JAR) named `hello.jar'.

If you are using the Windows command-line interpreter, i.e. cmd.exe, then
setting the value of the option --target-env-type to "windows" will cause the
compiler to generate a batch file that invokes the program, rather than a shell
script.

Problems at higher optimisation levels are still being resolved.
For now we recommend sticking with -O2 or below and not enabling intermodule
optimisation.

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
:- pragma foreign_proc("Java",
    to_string(T::in, Str::out),
    [promise_pure, will_not_call_mercury],
"
    Str = T.toString();
").

The implementation will include this Java code in the module's .java file, and
you can then call the predicate to_string exactly the same as if it were
implemented using pure mercury code.

For more information about the foreign language interface, see the Mercury
Language Reference Manual, which you can find at:
	
     <http://www.mercurylang.org/information/documentation.html>

Some short programs may run much more slowly in the Java grade than the C
grades.  The runtime is probably dominated by Java class loading and running
in interpreted mode.  A long running program should perform reasonably well
with a Just-In-Time compiler.  It may also be possible to use an Ahead-Of-Time
Java compiler, but we haven't tried that yet.

-----------------------------------------------------------------------------

BUILDING THE JAVA GRADE ON WINDOWS

When building the Java grade on Windows it is sometimes possible for the fully
qualified names of some generated files to exceed the maximum path length.
If this occurs the Mercury compiler will abort with a message like:

   Uncaught Mercury exception:
   Software Error: parse_tree.module_cmds: predicate \
   `parse_tree.module_cmds.list_class_files_for_jar'/6: \
   Unexpected: io.file_type failed: No such file or directory

In this case all that can (currently) be done is to reduce the length of the
build path, for example by shifting the build directory closer to the root of
the file system (e.g. C:\mercury).

-----------------------------------------------------------------------------

BUILDING THE MERCURY COMPILER IN THE JAVA GRADE

Building the Mercury compiler and other related tools in the Java grade
is NOT generally supported and should be considered experimental.
In particular, a Mercury compiler built in the Java grade may be slower than
normal and some features may not be available.

However, if you want to give it a try, the required steps are:

(1) Ensure that you have an existing working Mercury compiler in your PATH
and a clean version of the Mercury source tree.

(2) Run ./prepare.sh; ./configure as normal.

(3) Add the line:

      GRADE=java

    to a file named Mmake.params at the top-level of the source tree.

(4) Begin the build process using the following command:

    $ mmake --use-mmc-make GRADE=java

The Java version of the compiler MUST be built using mmake's --use-mmc-make
option; the build will not work otherwise.  Setting the variable GRADE in the
invocation of mmake is currently necessary in order to avoid some variable
definition ordering problems in Mmake.workspace.

(5) To install the Java version of the compiler, do:

   $ mmake --use-mmc-make install GRADE=java

-----------------------------------------------------------------------------

RESOURCES

You might find the following pages useful:
    
    <http://www.mercurylang.org/backends.html>

    <http://java.sun.com/reference/api/index.html>

    <http://www.mercurylang.org/information/documentation.html>>

-----------------------------------------------------------------------------

FREQUENTLY ASKED QUESTIONS (FAQS)

Q. What are the advantages of using the Java back-end?

A. The main advantages are easy access to the wide range of libraries available
   for the J2SE platform, including web applet development, and the portability
   you get from using Java bytecode.


Q. What version of Java should I be using?

A. Java 2 Platform Standard Edition, version 1.5 or greater.


Q. What features are not yet implemented for the Java back-end?

A. The following implementation features are not supported:

	Mercury-level debugging (but see next question)
	Mercury-level profiling
	trailing
	tabling
	backjumping

   In addition, the following individual procedures are incompletely
   implemented:

   	io.read_binary/{3,4}:
   	io.write_binary/{3,4}:
		io.read_binary is broken.

	benchmarking.report_stats/0:
	benchmarking.report_full_memory_stats/0:
		Memory usage statistics are not yet available, and cpu time
		is not the same as in the C backends, as per time.m.

	io.set_environment_var/4:
		The Java APIs do not allow setting environment variables hence
		this predicate simply throws an exception.

	store.arg_ref/5:
	store.new_arg_ref/5:
		Due to the absence of RTTI, dynamic type checking is missing
		for these predicates.  They should be used with care.

	time.clock/3:
	time.clocks_per_sec/0:
	time.times/7:
	time.clk_tck/0:
		Because the current Java APIs do not provide any way of
		implementing these procedures exactly in pure Java, we have
		approximated them with what is available.

   This list is probably not complete.


Q. How do I debug Mercury programs on Java?

A. The only Mercury-level debugger available for Java grades is the
   source-to-source debugger; see README.ssdebug.


Q. How do I enable Java-level debugging?

A. By default, javac already generates line number and source file debugging
   information.  You can include local variable debugging information by
   specifying "--target-debug" when invoking the Mercury compiler, e.g.

        mmc --make --java --target-debug <progname>

   You can then use Sun's "jdb" debugging tool, which comes
   as part of the Java SDK distribution, to debug your program.
   For more information, see the documentation for javac and jdb.


Q. The Java compiler runs out of memory!

A. You are probably hitting an artificial limit. You can pass an option to
   the javac program to increase the limit, e.g.

        mmc --make foo --java --java-flag -J-Xmx512m

   Or in a Mercury.options file:

        JAVACFLAGS += -J-Xmx512m

-----------------------------------------------------------------------------
