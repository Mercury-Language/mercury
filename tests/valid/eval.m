%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The compiler on 29 January, 2006 aborted with
% the message "Software Error: basic_block.m: Unexpected: extend_basic_blocks:
% fall through mismatch" when invoked with -O0 --optimize-repeat=0
% --optimize-saved-vars on this file.
%
% The contents of this file are an extract from eval.m in the CVS directory
% benchmarks/progs/icfp2000.

:- module eval.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

:- type token_list == list(token_group).

:- type token_group
    --->    single_token(token)
    ;       function(token_list)
    ;       array(token_list).

:- type token
    --->    identifier(string)
    ;       binder(string)
    ;       boolean(bool)
    ;       number(int)
    ;       string(string).

:- type code == token_list.

:- pred interpret(code::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module map.

:- type value
    --->    boolean(bool)
    ;       int(int)
    ;       real(float)
    ;       string(string).

:- type id == string.
:- type env == map(id, value).
:- type stack == list(value).

interpret(Code, !IO) :-
    initial_setup(Env0, Stack0, !IO),
    interpret(Code, Env0, Stack0, _Env, _Stack, !IO).

:- pred initial_setup(env::out, stack::out,
    io::di, io::uo) is det.

initial_setup(Env, [], !IO) :-
    map.init(Env).

:- pred interpret(code::in, env::in, stack::in,
    env::out, stack::out, io::di, io::uo) is det.

interpret(_, Env, Stack, Env, Stack, !IO).
