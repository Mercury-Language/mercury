% This module java_main_int defines a Mercury predicate java_main which acts as an
% interface to the Java method java_main(), which is defined in JavaMain.java.

% This source file is hereby placed in the public domain.

:- module java_main_int.
:- interface.
:- import_module io.

% Since the java_main() function has side effects, we declare the corresponding
% Mercury predicate as one that takes an io__state pair.  If we didn't do
% this, the Mercury compiler might optimize away calls to it!

:- pred java_main(io::di, io::uo) is det.

:- implementation.

	% Import the Java class containing the method java_main.
	% As usual for Java, this is not necessary; you may also
	% fully qualify the method at the call site.
:- pragma foreign_decl("Java", "import my_package.JavaMain;").

	% Define the Mercury predicate java_main to call the Java function
	% java_main.
:- pragma foreign_proc("Java",
	java_main(IO0::di, IO::uo),
	[promise_pure, will_not_call_mercury],
"
	JavaMain.java_main();
	IO = IO0;
").
