%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for spurious errors if there are predicates
% module1.p and module2.module1.p.
%
% This test was originally called nested_module_bug.
%

:- module nested_module_ambiguity.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module nested_module_ambiguity.nested_module_ambiguity_helper_1.
:- import_module mercury_term_parser.

main(!IO) :-
    parse_tokens("foo", [1, 2], List),
    io.write_line(List, !IO).

    :- module nested_module_ambiguity.nested_module_ambiguity_helper_1.

    :- interface.

    :- pred parse_tokens(string::in, list(int)::in, list(int)::out) is det.

    :- implementation.

    parse_tokens(_, X, X).

    :- end_module nested_module_ambiguity.nested_module_ambiguity_helper_1.
