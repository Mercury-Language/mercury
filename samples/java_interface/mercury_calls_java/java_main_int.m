% This module defines a Mercury predicate java_main/2 that acts as an interface
% to the Java method java_main(), which is defined in the file JavaMain.java.

% This source file is hereby placed in the public domain.

:- module java_main_int.
:- interface.
:- import_module io.

% Since the java_main() function has side effects, we declare the corresponding
% Mercury predicate as one that takes an I/O satate pair.  If we did not do
% this, the Mercury compiler might optimize away calls to it!

:- pred java_main(io::di, io::uo) is det.

:- implementation.

    % Import the Java class containing the method java_main.
    % As usual for Java, this is not necessary; you may also
    % fully qualify the method at the call site.
:- pragma foreign_decl("Java", "import my_package.JavaMain;").

    % Define the Mercury predicate java_main/2 to call the Java method
    % java_main().
:- pragma foreign_proc("Java",
    java_main(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    JavaMain.java_main();
").
