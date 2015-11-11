%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. The compiler aborted if the structure reuse pass tried
% to reuse a cell which is known, at compile time, to be static data.

:- module reuse_static.
:- interface.

:- import_module list.

:- type foo_code_addr
    --->    code_addr_internal(string, foo_func_signature).

:- type foo_type
     --->      foo_generic_type.

:- type foo_entity_name
    --->    entity_data
    ;       entity_function(string).

:- type foo_func_signature
    --->    foo_func_signature(list(foo_type)).

:- type foo_func_params
    --->    foo_func_params(list(foo_argument)).

:- type foo_argument
    --->    foo_argument(foo_entity_name, foo_type).

:- pred gen_gc_trace_func(foo_entity_name::in, foo_code_addr::out,
    foo_func_params::out, foo_func_params::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

gen_gc_trace_func(FuncName, GCTraceFuncAddr, FuncParams, FuncParams) :-
    Argument = foo_argument(entity_data, foo_generic_type),
    Parameters = [Argument],
    FuncParams = foo_func_params(Parameters), % static data
    % Deforestation turns the following call into three goals.
    % The bug is not triggered if we write those goals directly.
    ParamTypes = foo_map(Parameters),
    Signature = foo_func_signature(ParamTypes),
    (
        FuncName = entity_function(PredLabel),
        GCTraceFuncAddr = code_addr_internal(PredLabel, Signature)
    ;
        FuncName = entity_data,
        error("gen_gc_trace_func: not a function")
    ).

:- func foo_map(list(foo_argument)) = list(foo_type).

foo_map([]) = [].
foo_map([H0 | T0]) = [H | T] :-
    H0 = foo_argument(_, H),
    foo_map(T0) = T.

%---------------------------------------------------------------------------%
