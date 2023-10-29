%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module originally tested the effect of the presence of misplaced
% fact_table pragmas in the interface. We now test for that in
% invalid_make_int/fact_table_in_interface.m; this is a more general test.
%
% The .err_exp file is for systems that do not use the .exe extension.
% The .err_exp2 file is for systems that do use the .exe extension.
% Both are for mmake. The .err_exp3 is for when we use mmc --make (see below).

:- module bad_fact_table.

:- interface.

:- pred foo(int::in, int::out) is det.
:- func foo(int) = int.
:- pred bar(int::in, int::out) is det.
:- func bar(int) = int.
:- pred baz(int::in, int::out) is det.
:- func baz(int, int) = int.

:- implementation.

:- pragma fact_table(pred(foo/2), "nonexistent_foo").
:- pragma fact_table(func(bar/1), "nonexistent_bar").
:- pragma fact_table(baz/2,       "nonexistent_baz").   % Ambiguity error.

% No definition of pred foo/2, but there is a misplaced fact_table above.
% No definition of func foo/1. Should get an error message.
% No definition of pred bar/2. Should get an error message.
% No definition of func bar/1, but there is a misplaced fact_table above.
% No definition of pred baz/2. Should get an error message.
% No definition of func baz/2. Should get an error message.

% When we use mmc --make, as we do during bootchecks in Java and C# grades,
% the computation of the set of prerequisites needed to build the .err
% file of this module includes the nonexistent files named in the fact table
% declarations above. The compiler therefore does not even attempt to do
% semantic analysis on this module, which means that it generates none
% of the error messages in the .err_exp/.err_exp2 files. It does generate
% an error message about the first missing prerequisite but
%
% - this reports only the first missing prerequisite, nonexistent_foo,
%   presumably because having found that the action to generate the .err file
%   is missing some prerequisite, it does not go on to test whether the rest
%   of the prerequisites are present or not, and
%
% - the error message does not derive from semantic analysis of this module,
%   so it goes to a stream other than the one that we capture in the .err file.
%
% This means that with mmc --make, we expect the .err file to be empty.
%
% NOTE There is also the issue that the compiler does not support fact tables
% when targeting Java or C#, so having the compiler consider the files named
% in fact_table declarations to be prerequisites in those grades is sort-of
% strange in itself. (It *can* be justified on the grounds of checking
% their contents for errors.)
