%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for bug 130. Adapted from the ICFP2000 ray-tracer.

% This test case tests for a bug with the same symptoms as bug 130, but it's
% not actually bug 130.

:- module bug_130_unreachable.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.

:- type value
    % base values
    --->    boolean(bool)
    ;       int(int)
    ;       real(real)
    ;       string(string)
    % non-base values
    ;       closure(env, code).

:- type object_id == int.

    % Interpreter state.
    %
:- type state
    --->    state(
                s_global_object_counter     :: object_id,
                s_render_commands           :: list(render_params)
            ).

:- type render_params
    --->    render_params(
                depth       :: int,
                fov         :: real,        % the field of view
                wid         :: int,         % the width, in pixels
                ht          :: int,         % the height, in pixels
                file        :: string
            ).

:- type real == float.

:- type env == map(id, value).

:- type id == string.

:- type stack == list(value).

:- type token_list == list(token_group).

:- type token_group
    --->    single_token(token).

:- type token
    --->    identifier(string)
    ;       binder(string)
    ;       boolean(bool)
    ;       number(number)
    ;       string(string).

:- type number
    --->    integer(int)
    ;       real(float).

:- type code == token_list.

main(!IO) :-
    State0 = new_interpreter_state,
    interpret([], State0, State),
    io.write(State, !IO).

:- func new_interpreter_state = bug_130_unreachable.state.

new_interpreter_state =
    state(
        1,      % Global object counter
        []      % Render commands.
    ).

:- pred interpret(code::in,
    bug_130_unreachable.state::in, bug_130_unreachable.state::out) is det.

interpret(Code, !State) :-
    map.init(Env0),
    Stack0 = [],
    interpret(Code, Env0, Stack0, _Env, _Stack, !State).

:- pred interpret(code::in, env::in, stack::in, env::out, stack::out,
    bug_130_unreachable.state::in, bug_130_unreachable.state::out) is det.

interpret([], Env, Stack, Env, Stack) --> [].
interpret(Tokens0, Env0, Stack0, Env, Stack) -->
    { Tokens0 = [Token | Tokens] },
    (
        do_token_group(Token, Env0, Stack0, Env1, Stack1)
    &
        interpret(Tokens, Env1, Stack1, Env, Stack)
    ).

:- pred do_token_group(token_group::in, env::in, stack::in, env::out,
    stack::out,
    bug_130_unreachable.state::in, bug_130_unreachable.state::out) is det.

do_token_group(_, Env, Stack, Env, Stack, State, State) :-
    error("Predicate not implemented").
