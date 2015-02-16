%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2009-04-5 and before incorrectly reported that the function in the
% foreign_export pragam below did not exist; the correct error to report
% is that there are multiple matches for the second argument of the pragma.
% This was Mercury bug #83.

:- module bug83.
:- interface.

:- import_module io.

:- func make_io_error(string) = io.res.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% WORKS
% :- pragma foreign_export("C",
%   fe.make_io_error(in) = out,
%   "OAMQ_make_io_error").

:- pragma foreign_export("C", make_io_error(in) = out, "OAMQ_make_io_error").

make_io_error(Message) = io.error(Error) :-
    Error = io.make_io_error(Message).
