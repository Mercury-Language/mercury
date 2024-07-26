%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% As of 2024 jul 26, this module got the following less-than-helpful
% error message:
%
% bad_ambiguity_msg.m:024: In clause for predicate `main'/2:
% bad_ambiguity_msg.m:024:   error: ambiguous overloading causes type
% bad_ambiguity_msg.m:024:   ambiguity.
% bad_ambiguity_msg.m:024:   The following variables have ambiguous types:
% For more information, recompile with `-E'.
%
% The problem is that there are no variables at all whose type is ambiguous.
% The ambiguity is in the identity of the "append" predicate, since it has
% a definition both in this module and in the standard list module, but a
% compiler bug prevented this fact from being printed.

:- module bad_ambiguity_msg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    L1 = [1, 2],
    L2 = [3, 4],
    append(L1, L2, L),
    io.write_line(L, !IO).

:- pred append(list(T)::in, list(T)::in, list(T)::out) is det.
:- pragma external_pred(append/3).
