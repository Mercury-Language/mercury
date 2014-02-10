:- module exists_dummy.

% Test for Bug #309, existentially quantifying a dummy variable such as
% store(S) can cause the compiler to crash if the variable's register is
% clubbered and the value needs to be retrived from the stack,.

:- interface.
:- import_module map, string, store.

:- type lisp_val ---> lisp_val.

:- type mappings(S) == generic_mutvar(map(string, lisp_val), S).

:- type environment(S) ---> root_environment(mappings(S))
                          ; child_environment(environment(S), mappings(S)).

:- some[S] pred root_env(environment(S)::out, S::uo) is det => store(S).

:- implementation.

root_env(Environment, Store) :-
    store.init(Store0),
    store.new_mutvar(map.init, Mappings, Store0, Store),
    Environment = root_environment(Mappings).

/*
$ mmc -C environment.m

Making Mercury/int3s/environment.int3
Making Mercury/ints/environment.int
Making Mercury/cs/environment.c
Uncaught Mercury exception:
Software Error: ll_backend.llds_out.llds_out_data: predicate `ll_backend.llds_out.llds_out_data.output_lval'/4: Unexpected: stack var out of range
Stack dump not available in this grade.
** Error making `Mercury/cs/environment.c'.
make: *** [all] Error 1
*/
