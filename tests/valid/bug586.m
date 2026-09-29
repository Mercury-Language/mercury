%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #586. A rare interaction between
% the saved vars const, follow code and simplification passes led to an abort
% in the code generator. The problem has only been observed in deep profiling
% grades with the split switch arms pass enabled, but otherwise is not
% related.
%
%   Uncaught Mercury exception:
%   Software Error: predicate `ll_backend.code_gen.generate_goal'/7:
%   Unexpected: semidet model in det context
%
% The problem is this. After the deep profiling transformation,
% the saved_vars_const pass runs and tries to delay the construction of Val0.
% When it gets to the switch, Val0 is required, so it is constructed,
% but because Val0 is a non-local variable, it also duplicated the
% construction goal at the end of the conjunction.
%
%   Val0 = auto,    % construction goal
%   ( % cannot_fail switch on Val0
%       Val0 = auto,
%       ...
%   ;
%       Val0 = computed(Val)
%   ),
%   ... ,
%   Val0 = auto     % duplicated construction goal
%
% After the followcode pass moves the unification into the switch arms,
% simplification determines that the unification "Val0 = auto" cannot succeed
% in the computed/1 arm, and replaces it with failure, making the entire
% switch semidet. This leads to the "semidet model in a det context" abort.

:- module bug586.
:- interface.

:- type parent
    --->    parent(
                align :: align
            ).

:- type unresolved_spec
    --->    initial
    ;       inherit
    ;       value(spec_align).

:- type spec_align
    --->    computed(align)
    ;       auto.

:- type align
    --->    start
    ;       end.

:- func get_computed_align(unresolved_spec, parent) = align.

%--------------------------------------------------------------------%

:- implementation.

get_computed_align(Spec, Parent) = Val :-
    (
        Spec = inherit,
        Val = start
    ;
        (
            Spec = initial,
            Val0 = auto
        ;
            Spec = value(Val0)
        ),
        ParentVal = Parent ^ align,
        (
            Val0 = auto,
            Val = ParentVal
        ;
            Val0 = computed(Val)
        )
    ).

%--------------------------------------------------------------------%
