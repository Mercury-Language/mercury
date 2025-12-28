%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether, and if so how well, the compiler's error messages
% point out missing arguments as the root causes of type mismatches
% for the arguments of closures.
%
%---------------------------------------------------------------------------%

:- module missing_closure_arg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.

main(!IO) :-
    strs(Strs),
    list.foldl(foo(yes, 43.2, 'a', "x"), Strs, !IO),
    list.foldl(foo([no], 42, 43.2, 'a', "x"), Strs, !IO),
    list.foldl(foo(42, 43.2, 'a', "x"), Strs, !IO).

%---------------------------------------------------------------------------%

:- pred foo(bool::in, list(bool)::in, int::in, float::in, character::in,
    string::in, io::di, io::uo) is det.

foo(_, _, _, _, _, _, !IO).

:- pred strs(list(string)::out) is det.

strs(["abc", "def"]).
