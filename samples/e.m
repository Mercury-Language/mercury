%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% File: e.m.
% Main author: bromage.
%
% This source file is hereby placed in the public domain.  -bromage.
%
% Calculate the base of natural logarithms using lazy evaluation.
%
% The algorithm is O(N^2) in the number of digits requested.
%
%-----------------------------------------------------------------------------%

:- module e.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Default number of digits displayed (if this is not specified
    % on the command line).
    %
:- func default_digits = int.
default_digits = 1000.

    % Change this for a base other than 10.  Any integer between
    % 2 and 36 makes sense.
    %
:- func base = int.
base = 10.

    % Number of columns on the terminal.
    %
:- func columns = int.
columns = 78.

%-----------------------------------------------------------------------------%

    % This is a simple implementation of an infinite lazy stream.
    %
:- type int_stream
    --->    [int | int_stream]
    ;   closure((func) = int_stream).

:- inst int_stream ==
    bound([ground | int_stream] ; closure((func) = is_out is det)).

:- mode is_in  == in(int_stream).
:- mode is_out == out(int_stream).

%-----------------------------------------------------------------------------%

    % An infinite stream of ones.
    % 
:- func ones = (int_stream :: is_out) is det.

ones = [1 | closure((func) = ones)].

    % All the digits of e in one stream.
    % 
:- func digits_of_e = (int_stream :: is_out) is det.

digits_of_e = next_digit(ones).

:- func next_digit(int_stream::is_in) = (int_stream::is_out) is det.

next_digit(Stream0) = [Digit | closure((func) = next_digit(Stream))] :-
    scale(2, Digit, Stream0, Stream).

:- pred scale(int::in, int::out, int_stream::is_in, int_stream::is_out) is det.

scale(C, Digit, closure(Func), Stream) :-
    scale(C, Digit, apply(Func), Stream).
scale(C, Digit, [D | Ds], Stream) :-
    K = base * D,
    KdC = K // C,
    ( KdC = (K + base - 1) // C ->
        % We have the next digit.  Construct a closure to
        % generate the rest.

        Digit = KdC,
        Stream = closure((func) = [K rem C + NextDigit | Stream0] :-
                scale(C + 1, NextDigit, Ds, Stream0)
            )
    ;
        % We have a carry to factor in, so calculate the next
        % digit eagerly then add it on.

        scale(C + 1, A1, Ds, B1),
        Digit = (K + A1) // C,
        Stream = [(K + A1) rem C | B1]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [Arg | _],
        string.to_int(Arg, Digits0)
    ->
        Digits = Digits0
    ;
        Digits = default_digits
    ),
    string.int_to_base_string(2, base, BaseString),
    io.write_strings([BaseString, "."], !IO),
    string.length(BaseString, BaseStringLength),
    main_2(Digits, columns - BaseStringLength - 1, digits_of_e, !IO),
    io.nl(!IO).

    % Print out digits until we don't have any more.
    %   
:- pred main_2(int::in, int::in, int_stream::is_in, io::di, io::uo) is det.

main_2(Digits, Columns, closure(Func), !IO) :-  
    main_2(Digits, Columns, apply(Func), !IO).
main_2(Digits, Columns, [I | Is], !IO) :-
    ( Digits = 0 ->
        true
    ; Columns = 0 ->
        io.nl(!IO),
        main_2(Digits, columns, [I | Is], !IO)
    ;
        char.det_int_to_digit(I, Digit),
        io.write_char(Digit, !IO),
        main_2(Digits - 1, Columns - 1, Is, !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module e.
%-----------------------------------------------------------------------------%
