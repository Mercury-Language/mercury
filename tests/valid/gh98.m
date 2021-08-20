%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Released by Transnat Games as public domain for testing purposes.
% Any copyright is dedicated to the Public Domain.
% https://creativecommons.org/publicdomain/zero/1.0/
%
% Crashes the following error when compiling:
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%     Key Type: term.var(parse_tree.prog_data.prog_var_type)
%     Key Value: var(25)
%     Value Type: hlds.hlds_goal.unify_mode
%
%---------------------------------------------------------------------------%

:- module gh98.

:- interface.

:- use_module array.
:- use_module stream.

:- pred crash(Stream::in, stream.result(array.array(int), string)::out,
    State::di, State::uo) is det
    <= (stream.reader(Stream, int, State, Error), stream.error(Error)).

%---------------------------------------------------------------------------%

:- implementation.

:- use_module string.

    % Just using stream.get in crash/4 doesn't trigger the crash.
:- pred get(Stream::in, stream.result(int, string)::out,
    State::di, State::uo) is det
    <= (stream.reader(Stream, int, State, Error), stream.error(Error)).

get(Stream, Result, !State) :-
    stream.get(Stream, NResult, !State),
    (
        NResult = stream.eof,
        Result = stream.eof
    ;
        NResult = stream.error(Error),
        Result = stream.error(stream.error_message(Error))
    ;
        NResult = stream.ok(N),
        Result = stream.ok(N)
    ).

%---------------------------------------------------------------------------%

% Removing the typeclass constraint stops the crash.
:- pred crash_loop(Stream::in, int::in, int::out,
    {stream.res(string), State}::di, {stream.res(string), State}::uo) is det
    <= (stream.reader(Stream, int, State, Error), stream.error(Error)).

crash_loop(_Stream, _N, 0, {Result, !.State}, {Result, !:State}).

%---------------------------------------------------------------------------%

crash(Stream, Result, !State) :-
    get(Stream, LenResult, !State),
    (
        LenResult = stream.eof,
        Result = stream.eof
    ;
        LenResult = stream.error(Error),
        Result = stream.error(Error)
    ;
        LenResult = stream.ok(Len),
        % The crash_loop predicate has two typeclass constraints on it,
        % so it takes two typeclass_info arguments. The polymorphism
        % transformation, when it converted this implicit lambda expression
        % into an explicit lambda expression, did add both typeclass_infos
        % to the start of crash_loop's argument list. The bug was that
        % it included only one of them in the rhs_lambda_goal's nonlocal set,
        % and quantification is not allowed to add any variables to that set.
        % Since the actual source of that typeclass_info is the clause head
        % (i.e. the crash predicate's own input arguments), this meant that
        % one of crash_loop's argument variables was not in the map that
        % the compiler tried to look it up in.
        Closure = crash_loop(Stream),
        array.generate_foldl(Len, Closure, Array,
            {stream.ok, !.State}, {LoopResult, !:State}),
        (
            LoopResult = stream.error(Error),
            Result = stream.error(Error)
        ;
            LoopResult = stream.ok,
            Result = stream.ok(Array)
        )
    ).
