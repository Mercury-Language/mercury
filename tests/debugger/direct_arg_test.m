%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This program tests whether the debugger can handle printing
% the value of a direct_arg argument.
%
% This is a regression test. With versions of Mercury before 2026 mar 13,
% executing this program with mdb
%
% - could handle stopping at the EXIT port of get_maybe_t
%   and printing "MaybeT",
%
% - but could NOT handle printing "MaybeT^1".
%
% The symptom was a core dump. The cause was that
%
% - the argument of the yes_t function is a direct_arg, which is means that
%   this argument is not in an argument vector in the heap, yet
%
% - MR_arg in runtime/mercury_ml_expand_body.h gave MR_select_specified_subterm
%   in trace/mercury_trace_vars.c a non-NULL value of word_sized_arg_ptr,
%   which MR_select_specified_subterm interpreted to mean that it should be
%   treated as a word-sized arg in a heap argument vector.
%
%---------------------------------------------------------------------------%

:- module direct_arg_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

:- type maybe_t
    --->    no_t
    ;       yes_t(direct_arg :: t).

:- type t
    --->    t(int, int, int).

%---------------------------------------------------------------------------%

main(!IO) :-
    get_maybe_t(MaybeT),
    io.write_line(MaybeT, !IO).

:- pred get_maybe_t(maybe_t::out) is det.

get_maybe_t(MaybeT) :-
    T = t(1, 2, 43),
    MaybeT = yes_t(T).
