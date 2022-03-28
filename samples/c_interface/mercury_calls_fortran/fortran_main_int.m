% This module fortran_main_int defines a Mercury predicate fortran_main
% which acts as an interface to the Fortran subroutine FORTRAN_MAIN,
% which is defined in the file fortran_main.f.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module fortran_main_int.

:- interface.
:- import_module io.

% Since the fortran_main() function has side effects, we declare the
% corresponding Mercury predicate as one that takes an io.state pair.
% If we did not do this, the Mercury compiler might optimize away calls to it!

:- pred fortran_main(io::di, io::uo) is det.

:- implementation.

% Define the Mercury predicate fortran_main to call the Fortran function
% FORTRAN_MAIN. Note that by default gfortran mangles names by converting
% them to lowercase and appending an underscore.

:- pragma foreign_proc("C",
    fortran_main(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    fortran_main_();
").
