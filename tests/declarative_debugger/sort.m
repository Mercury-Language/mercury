%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

:- type line  == string.
:- type lines == list(string).

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    open_stream(Args, MaybeStream, !IO),
    (
        MaybeStream = ok(InputStream),
        some [!Words] (
            read_lines(InputStream, !:Words, !IO),
            sort_lines(!Words),
            print_lines(!.Words, !IO)
        )
    ;
        MaybeStream = error(ErrorMessage),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "%s\n", [s(ErrorMessage)], !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred open_stream(list(string)::in, maybe_error(io.input_stream)::out,
    io::di, io::uo) is det.

open_stream([], MaybeStream, !IO) :-
    io.stdin_stream(Stdin, !IO),
    MaybeStream = ok(Stdin).
open_stream([Arg], MaybeStream, !IO) :-
    io.open_input(Arg, MaybeStream0, !IO),
    (
        MaybeStream0 = ok(Stream),
        MaybeStream  = ok(Stream)
    ;
        MaybeStream0 = error(Error),
        io.error_message(Error, ErrorMessage),
        MaybeStream  = error(ErrorMessage)
    ).
open_stream([_, _ | _], error(ErrorMessage), !IO) :-
    ErrorMessage = "usage: sort [Input]".

%---------------------------------------------------------------------------%

:- pred read_lines(io.input_stream::in, lines::out, io::di, io::uo) is det.

read_lines(Stream, Lines, !IO) :-
    read_lines_2(Stream, [], Lines, !IO).

:- pred read_lines_2(io.input_stream::in, lines::in,
    lines::out, io::di, io::uo) is det.

read_lines_2(Stream, !Lines, !IO) :-
    io.read_line_as_string(Stream, Result, !IO),
    (
        Result = ok(Line),
        list.cons(Line, !Lines),
        read_lines_2(Stream, !Lines, !IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMessage),
        throw(ErrorMessage)
    ).

%---------------------------------------------------------------------------%

:- pred sort_lines(lines::in, lines::out) is det.

sort_lines(Us, Ss) :-
    N = list.length(Us),
    msort_n(N, Us, Ss, _).

:- pred msort_n(int::in, lines::in, lines::out, lines::out) is det.

msort_n(N, Unsorted, SortedPart, Rest) :-
    ( if N =< 0 then
        SortedPart = [],
        Rest = Unsorted
    else if N = 1 then
        (
            Unsorted = [U | Us],
            SortedPart = [U],
            Rest = Us
        ;
            Unsorted = [],
            throw("Unsorted = [] and N = 0")
        )
    else
        N1 = N // 2,
        sort.msort_n(N1, Unsorted, Ss1, Us2),
        N2 = N - N1,
        msort_n(N2, Us2, Ss2, Rest),
        sort.merge(Ss1, Ss2, SortedPart)
    ).

:- pred merge(lines::in, lines::in, lines::out) is det.

merge([], [], []).
merge([S | Ss], [], [S | Ss]).
merge([], [S | Ss], [S | Ss]).
merge([A | As], [B | Bs], [C | Cs]) :-
    compare(Cmp, A, B),
    ( if ( Cmp = (<) ; Cmp = (=) ) then
        sort.merge(As, [B | Bs], Cs),
        C = A
    else
        sort.merge(As, [B | Bs], Cs), % BUG
        C = B
    ).

%---------------------------------------------------------------------------%

:- pred print_lines(lines::in, io::di, io::uo) is det.

print_lines(Lines, !IO) :-
    io.write_list(Lines, "", io.write_string, !IO).

%---------------------------------------------------------------------------%
:- end_module sort.
%---------------------------------------------------------------------------%
