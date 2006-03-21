% This module fortran_main_int defines a Mercury predicate fortran_main
% which acts as an interface to the Fortran subroutine FORTRAN_MAIN,
% which is defined in fortran_main.f.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module fortran_main_int.

:- interface.
:- import_module io.

% Since the fortran_main() function has side effects, we declare the
% corresponding Mercury predicate as one that takes an io__state pair. 
% If we didn't do this, the Mercury compiler might optimize away calls to it!

:- pred fortran_main(io::di, io::uo) is det.

:- implementation.

% Define the Mercury predicate fortran_main to call the Fortran
% function FORTRAN_MAIN.  Note that g77 mangles names by converting
% them to lowercase, and appending one or two underscores
% (two if the name already contains any underscores, one otherwise).
% So we need to use the name "fortran_main__" rather than "FORTRAN_MAIN".

:- pragma import(fortran_main(di, uo), "fortran_main__").

% Alternatively, you could use the "-fno-underscoring" and "-fcase-preserve"
% options to g77 when compiling the Fortran.  Then the above declaration
% could be like this instead:
%	:- pragma import(fortran_main(di, uo), "FORTRAN_MAIN").
