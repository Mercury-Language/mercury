%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% file: ultra_sub.m
% author: conway.
%
% This source file is hereby placed in the public domain. -conway (the author).
%
% 'ultra_sub' is an extended version of zs' 'sub' command. The idea is that
% it takes a pattern, a template and some strings, and matches the strings
% against the pattern, binding some variables in the process. Then it
% substitutes the variables in the template for the bindings from the pattern.
%
% usage: ultra_sub <pattern> <template> [strings...]
%
% Variables in the pattern and template are represented by capital letters
% (unfortunately limiting the number of variables to 26 ). Real capital letters
% should be preceded by a \.
% Variables in the template that do not occur in the pattern are treated as
% real capital letters.
%
% eg
% $ ultra_sub 1X2Y3Z 7Z8Y9X 1a2b3c 1foo2bar3baz
% 7c8b9a
% 7baz8bar9foo
%
% Strings that do not match the pattern are ignored.
%
%------------------------------------------------------------------------------%

:- module ultra_sub.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.


:- import_module char.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module svmap.

%------------------------------------------------------------------------------%

main(!IO) :-
    % I really should add some options for switching whether
    % capitals or backslashed things are variables.
    io.command_line_arguments(Args, !IO),
    (
        Args = [Pattern0, Template0 | Rest]
    ->
        string.to_char_list(Pattern0, Pattern),
        string.to_char_list(Template0, Template),
        process_args(Rest, Pattern, Template, !IO)
    ;
        io.write_string(
            "usage: ultra_sub template pattern [strings]\n", !IO)
    ).

%------------------------------------------------------------------------------%

:- pred process_args(list(string)::in, list(char)::in, list(char)::in,
    io::di, io::uo) is det.

process_args([], _Pattern, _Template, !IO).
process_args([Str | Strs], Pattern, Template, !IO) :-
    (
        string.to_char_list(Str, Chars),
        map.init(Match0),
        match(Pattern, Chars, Match0, Match)
    ->
        % If the string matches, then apply the substitution.
        sub(Template, Match, ResultChars),
        string.from_char_list(ResultChars, Result),
        io.write_string(Result, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    process_args(Strs, Pattern, Template, !IO).

%------------------------------------------------------------------------------%

:- pred match(list(char)::in, list(char)::in, map(char, list(char))::in, 
    map(char, list(char))::out) is semidet.

match([], [], Match, Match).
match([T | Ts], Chars, !Match) :-
    (
        char.is_upper(T)
    ->
        % Match against a variable.
        match_2(T, Chars, [], Ts, !Match)
    ;
        T = ('\\') % don't you love ISO compliant syntax :-(
    ->
        Ts = [T1 | Ts1],
        Chars = [T1 | Chars1],
        match(Ts1, Chars1, !Match)
    ;
        Chars = [T | Chars1],
        match(Ts, Chars1, !Match)
    ).

:- pred match_2(char::in, list(char)::in, list(char)::in, list(char)::in, 
    map(char, list(char))::in, map(char, list(char))::out) is semidet.

match_2(X, Chars, Tail, Ts, !Match) :-
    (
        % Have we bound X? Does it match
        % an earlier binding?
        map.search(!.Match, X, Chars)
    ->
        true
    ;
        svmap.set(X, Chars, !Match)
    ),
    (
        % Try and match the remainder of the pattern.
        match(Ts, Tail, !Match)
    ->
        true
    ;
        % If the match failed, then try binding less of the string to X.
        remove_last(Chars, Chars1, C),
        match_2(X, Chars1, [C | Tail], Ts, !Match)
    ).

%------------------------------------------------------------------------------%

:- pred remove_last(list(char)::in, list(char)::out, char::out) is semidet.

remove_last([X | Xs], Ys, Z) :-
    remove_last_2(X, Xs, Ys, Z).

:- pred remove_last_2(char::in, list(char)::in, list(char)::out, char::out)
    is det.

remove_last_2(X, [], [], X).
remove_last_2(X, [Y | Ys], [X | Zs], W) :-
    remove_last_2(Y, Ys, Zs, W).

%------------------------------------------------------------------------------%

:- pred sub(list(char)::in, map(char, list(char))::in, list(char)::out) is det.

sub([], _Match, []).
sub([C | Cs], Match, Result) :-
    (
        char.is_upper(C),
        map.search(Match, C, Chars)
    ->
        sub(Cs, Match, Result0),
        list.append(Chars, Result0, Result)
    ;
        C = ('\\')
    ->
        (
            Cs = [C1 | Cs1]
        ->
            sub(Cs1, Match, Result0),
            Result = [C1 | Result0]
        ;
            sub(Cs, Match, Result0),
            Result = Result0
        )
    ;
        sub(Cs, Match, Result0),
        Result = [C | Result0]
    ).

%------------------------------------------------------------------------------%
:- end_module ultra_sub.
%------------------------------------------------------------------------------%
