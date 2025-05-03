%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% When the compiler generates a warning about unresolved polymorphism,
% sometimes the variable being complained about is an anonymous variable.
% Since by definition users cannot see such variables in their programs,
% the error message does not give them any guidance of what they can change
% to make the warning go away.
%
% This test case tests the operation of the code in the compiler
% (var_origins.m) that generates messages to tell users
%
% - both the context of the first occurrence of the anonymous variable
%   as a filename/linenumber pair,
%
% - and how the variable fits in with the code in which it occurs,
%   which should help focus their attention on the affected *part* of that
%   line.
%
% This test case is a cut-down form of the code that motivated the change
% being tested here: an unresolved polymorphism in extras/odbc/odbc.m.
%
% XXX It would be nice to test variable origins other than just "this variable
% represents a term constructed here". We can of coyrse add test cases
% for such situations as we come across them.
%
%---------------------------------------------------------------------------%

:- module unresolved_polymorphism_anon.

:- interface.

:- import_module list.

:- pred maybe_throw_pred(list(int)::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module maybe.
:- import_module univ.

maybe_throw_pred(Ints, Sum) :-
    decide_whether_to_throw(Ints, Sum, MaybeException),
    (
        MaybeException = no
    ;
        MaybeException = yes(Exception),
        rethrow(exception(Exception))
    ).

%---------------------------------------------------------------------------%

:- pred decide_whether_to_throw(list(int)::in, int::out,
    maybe(univ)::out) is det.

decide_whether_to_throw(Ints, Sum, MaybeException) :-
    list.foldl(int.plus, Ints, 0) = Sum,
    ( if list.length(Ints) > 3 then
        type_to_univ("bad_input", Exception),
        MaybeException = yes(Exception)
    else
        MaybeException = no
    ).
