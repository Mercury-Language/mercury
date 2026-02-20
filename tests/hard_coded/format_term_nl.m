%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Regression test for bug with term_io.format_term_nl/5 in rotd-2026-02-19
% and before where the terminating ".\n" was not being appended to the term.
%---------------------------------------------------------------------------%

:- module format_term_nl.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module integer.
:- import_module list.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module term_context.
:- import_module term_int.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

main(!IO) :-
    varset.init(VarSet : varset(generic)),
    Args = [
        term.functor(term.atom("bar"), [], dummy_context),
        term_int.int_to_decimal_term(42, dummy_context)
    ],
    Term = term.functor(term.atom("foo"), Args, dummy_context),

    State0 = string.builder.init,
    term_io.format_term_nl(string.builder.handle, VarSet, Term,
        State0, State),
    String = string.builder.to_string(State),

    ( if string.suffix(String, ".\n") then
        io.print_line("PASSED", !IO)
    else
        io.print_line("FAILED", !IO),
        io.print_line(String, !IO),
        io.nl(!IO)
    ).

%---------------------------------------------------------------------------%
:- end_module format_term_nl.
%---------------------------------------------------------------------------%
