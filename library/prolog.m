%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2003, 2005-2006, 2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: prolog.m.
% Main author: fjh.
% Stability: high.
%
% This file contains predicates that are intended to help people
% porting Prolog programs, or writing programs in the intersection
% of Mercury and Prolog.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module prolog.
:- interface.

:- import_module io.
:- import_module list.
:- import_module pair.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% Prolog arithmetic operators.
%

:- pred T =:= T.            % In Mercury, just use =
:- mode in =:= in is semidet.

:- pred T =\= T.            % In Mercury, just use \=
:- mode in =\= in is semidet.

:- pred is(T, T) is det.    % In Mercury, just use =
:- mode is(uo, di) is det.
:- mode is(out, in) is det.

%---------------------------------------------------------------------------%
%
% Prolog term comparison operators.
%

:- pred T == T.             % In Mercury, just use =
:- mode in == in is semidet.

:- pred T \== T.            % In Mercury, just use \=
:- mode in \== in is semidet.

% Prolog's so-called "univ" operator, `=..'.
% Note: this is not related to Mercury's "univ" type!
% In Mercury, use `deconstruct.deconstruct' instead.

:- pred T =.. univ_result.
:- mode in =.. out is det.

    % Note that the Mercury =.. is a bit different to the Prolog one.
    % We could make it slightly more similar by overloading '.'/2,
    % but that would cause ambiguities that might prevent type
    % inference in a lot of cases.
    %
% :- type univ_result ---> '.'(string, list(univ)).
:- type univ_result == pair(string, list(univ)).

    % arg/3.
    % In Mercury, use arg/4 (defined in module deconstruct) instead:
    %
    %   arg(ArgNum, Term, Data) :-
    %       deconstruct.arg(Term, canonicalize, ArgNum - 1, Data).
    %
:- pred arg(int::in, T::in, univ::out) is semidet.

    % det_arg/3: like arg/3, but calls error/1 rather than failing
    % if the index is out of range.
    %
:- pred det_arg(int::in, T::in, univ::out) is det.

%---------------------------------------------------------------------------%
%
% Prolog style predicates for opening and switching streams.
%

    % see(FileName, Result, !IO):
    % Attempts to open the named file for input, and if successful,
    % sets the current input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
    %
:- pred see(string::in, io.res::out, io::di, io::uo) is det.

    % Attempts to open a file for binary input, and if successful sets
    % the current binary input stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrorCode)'.
    %
:- pred see_binary(string::in, io.res::out, io::di, io::uo) is det.

%---------------------%

    % seen(!IO):
    % Closes the current input stream.
    % The current input stream reverts to standard input.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred seen(io::di, io::uo) is det.

    % Closes the current input stream. The current input stream reverts
    % to standard input. This will throw an io.error exception if
    % an I/O error occurs.
    %
:- pred seen_binary(io::di, io::uo) is det.

%---------------------%

    % tell(FileName, Result, !IO):
    % Attempts to open the named file for output, and if successful,
    % sets the current output stream to the newly opened stream.
    % Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred tell(string::in, io.res::out, io::di, io::uo) is det.

    % Attempts to open a file for binary output, and if successful sets
    % the current binary output stream to the newly opened stream.
    % As per Prolog tell/1. Result is either 'ok' or 'error(ErrCode)'.
    %
:- pred tell_binary(string::in, io.res::out, io::di, io::uo) is det.

%---------------------%

    % told(!IO):
    % Closes the current output stream.
    % The current output stream reverts to standard output.
    % This will throw an io.error exception if an I/O error occurs.
    %
:- pred told(io::di, io::uo) is det.

    % Closes the current binary output stream. The default binary output
    % stream reverts to standard output. As per Prolog told/0. This will
    % throw an io.error exception if an I/O error occurs.
    %
:- pred told_binary(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

'=:='(X, X).

'=\\='(X, Y) :-
    X \= Y.

is(X, X).

'=='(X, X).
'\\=='(X, Y) :-
    X \= Y.

'=..'(Term, Functor - Args) :-
    deconstruct(Term, canonicalize, Functor, _Arity, Args).

% We use module qualifiers here to avoid overriding
% the builtin Prolog versions.
prolog.arg(ArgumentIndex, Type, Univ) :-
    deconstruct.arg(Type, canonicalize, ArgumentIndex - 1, Arg),
    type_to_univ(Arg, Univ).

det_arg(ArgumentIndex, Type, Argument) :-
    ( if arg(ArgumentIndex, Type, Arg) then
        Argument = Arg
    else
        error("det_arg: arg failed")
    ).

%---------------------------------------------------------------------------%
%
% Prolog style predicates for opening and switching streams.
%

see(File, Result, !IO) :-
    io.open_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

see_binary(File, Result, !IO) :-
    io.open_binary_input(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_input_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

%---------------------%

seen(!IO) :-
    io.stdin_stream(Stdin, !IO),
    io.set_input_stream(Stdin, OldStream, !IO),
    io.close_input(OldStream, !IO).

seen_binary(!IO) :-
    io.stdin_binary_stream(Stdin, !IO),
    io.set_binary_input_stream(Stdin, OldStream, !IO),
    io.close_binary_input(OldStream, !IO).

%---------------------%

tell(File, Result, !IO) :-
    io.open_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

tell_binary(File, Result, !IO) :-
    io.open_binary_output(File, Result0, !IO),
    (
        Result0 = ok(Stream),
        io.set_binary_output_stream(Stream, _, !IO),
        Result = ok
    ;
        Result0 = error(Msg),
        Result = error(Msg)
    ).

%---------------------%

told(!IO) :-
    io.stdout_stream(Stdout, !IO),
    io.set_output_stream(Stdout, OldStream, !IO),
    io.close_output(OldStream, !IO).

told_binary(!IO) :-
    io.stdout_binary_stream(Stdout, !IO),
    io.set_binary_output_stream(Stdout, OldStream, !IO),
    io.close_binary_output(OldStream, !IO).

%---------------------------------------------------------------------------%
