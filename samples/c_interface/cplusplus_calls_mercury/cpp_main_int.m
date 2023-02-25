% This module cpp_main_int defines a Mercury predicate cpp_main which acts as
% an interface to the C++ function cpp_main(), which is defined in cpp_main.cc.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module cpp_main_int.
:- interface.
:- import_module io.

% Since the cpp_main() function has side effects, we declare the corresponding
% Mercury predicate as one that takes an io.state pair. If we didn't do this,
% the Mercury compiler might optimize away calls to it!

:- pred cpp_main(io::di, io::uo) is det.

:- implementation.

    % #include the header file containing the function prototype for
    % cpp_main(), using a `pragma foreign_decl' declaration.
    % Note that any double quotes or backslashes in the C code for the
    % `#include' line must be escaped, since the C code is given as a Mercury
    % string.
:- pragma foreign_decl("C", "#include \"cpp_main.h\"").

    % Define the Mercury predicate cpp_main to call the C++ function
    % cpp_main().
:- pragma foreign_proc("C",
    cpp_main(IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    cpp_main();
    IO = IO0;
").
