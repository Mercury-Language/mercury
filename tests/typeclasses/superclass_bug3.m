%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module superclass_bug3.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module unit.

main(!IO) :-
    parse_result_entry(unit, 1, unit, String),
    io.write_string(String, !IO),
    io.nl(!IO).

:- pred parse_result_entry(FuncInfo::in, Call::in,
    Answer::in, string::out) is det <= analysis(FuncInfo, Call, Answer).

parse_result_entry(_FuncInfo, Call, _Answer, String) :-
    String = to_string(Call).

:- typeclass analysis(FuncInfo, Call, Answer)
    <= call_pattern(FuncInfo, Call) where [].

:- typeclass call_pattern(FuncInfo, Call) <= to_string(Call) where [].

:- typeclass to_string(S) where [
    func to_string(S) = string
].

:- instance analysis(unit, int, unit) where [].
:- instance call_pattern(unit, int) where [].
:- instance to_string(int) where [
    to_string(S) = string.int_to_string(S)
].
