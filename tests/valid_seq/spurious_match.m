%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for spurious errors if there are predicates
% module1.p and module2.module1.p.
%
% This test was originally called intermod_bug_nested.
%

:- module spurious_match.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module spurious_match.spurious_match_helper_1.
:- import_module list.

main(!IO) :-
    parse_tokens("foo", [1, 2], List),
    io.write_line(List, !IO).

%---------------------------------------------------------------------------%
    :- module spurious_match.spurious_match_helper_1.
    :- interface.

    :- pred parse_tokens(string::in, list(int)::in, list(int)::out) is det.

    :- implementation.

    :- import_module term_io.

    parse_tokens(_, X, X).

    :- end_module spurious_match.spurious_match_helper_1.
%---------------------------------------------------------------------------%
