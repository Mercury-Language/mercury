%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for bug 130. Adapted from the ICFP2000 ray-tracer.

:- module bug_130_should_push_signal.
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
:- type my_state
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
    --->    single_token(token)
    ;       two_tokens(token, token).

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

:- func new_interpreter_state = my_state.

new_interpreter_state =
    state(
        1,      % Global object counter
        []      % Render commands.
    ).

:- pred interpret(code::in, my_state::in, my_state::out) is det.

interpret(Code, !State) :-
    map.init(Env0),
    Stack0 = [],
    interpret(Code, Env0, Stack0, _Env, _Stack, !State).

:- pred interpret(code::in, env::in, stack::in, env::out, stack::out,
    my_state::in, my_state::out) is det.

interpret([], Env, Stack, Env, Stack) --> [].
interpret(Tokens0, Env0, Stack0, Env, Stack) -->
    { Tokens0 = [Token | Tokens] },
    (
        do_token_group(Token, Env0, Stack0, Env1, Stack1)
    &
        interpret(Tokens, Env1, Stack1, Env, Stack)
    ).

:- pragma no_inline(do_token_group/7).

:- pred do_token_group(token_group::in, env::in, stack::in,
    env::out, stack::out, my_state::in, my_state::out) is det.

do_token_group(TokenGroup, Env0, Stack0, Env, Stack, !State) :-
    (
        TokenGroup = single_token(Token),
        do_token(Token, Env0, Stack0, Env, Stack, !State)
    ;
        TokenGroup = two_tokens(_TokenA, _TokenB),
        error("")
    ).

:- pred do_token(token::in, env::in, stack::in,
    env::out, stack::out, my_state::in, my_state::out) is det.

do_token(_, Env, Stack, Env, Stack, !State) :-
    !:State = !.State ^ s_global_object_counter :=
        !.State ^ s_global_object_counter + 1.
