%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: program_representation.m
% Authors: zs, dougl
%
% This module defines the representation of procedure bodies
% used by the declarative debugger.
%
% One of the things we want the declarative debugger to be able to do
% is to let the user specify which part of which output argument of an
% incorrect or inadmissible atom is suspicious, and then find out where
% that particular subterm came from, i.e. where it was bound. Doing this
% requires knowing what the bodies of that procedure and its descendants are.
%
% If the Mercury compiler is invoked with options requesting declarative
% debugging, it will include in each procedure layout a pointer to a simplified
% representation of the goal that is the body of the corresponding procedure.
% We use a simplified representation partly because we want to insulate the
% code of the declarative debugger from irrelevant changes in HLDS types,
% and partly because we want to minimize the space taken in up in executables
% by these representations.
%
% The current representation is intended to contain all the information
% we are pretty sure can be usefully exploited by the declarative debugger.

%-----------------------------------------------------------------------------%

:- module mdbcomp.program_representation.

:- interface.

:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module std_util.

    % A representation of the goal we execute. These need to be generated
    % statically and stored inside the executable.
    %
    % Each element of this structure will correspond one-to-one
    % to the original stage 90 HLDS.

:- type proc_rep
    --->    proc_rep(
                list(var_rep),      % The head variables, in order,
                                    % including the ones introduced
                                    % by the compiler.
                goal_rep            % The procedure body.
            ).

:- type goal_rep
    --->    conj_rep(
                list(goal_rep)      % The conjuncts in the original order.
            )
    ;       disj_rep(
                list(goal_rep)      % The disjuncts in the original order.
            )
    ;       switch_rep(
                list(goal_rep)      % The switch arms in the original order.
            )
    ;       ite_rep(
                goal_rep,           % Condition.
                goal_rep,           % Then branch.
                goal_rep            % Else branch.
            )
    ;       negation_rep(
                goal_rep            % The negated goal.
            )
    ;       scope_rep(
                goal_rep,           % The quantified goal.
                maybe_cut
            )
    ;   atomic_goal_rep(
                detism_rep,
                string,             % Filename of context.
                int,                % Line number of context.
                list(var_rep),      % The sorted list of the variables
                                    % bound by the atomic goal.
                atomic_goal_rep
            ).

:- type atomic_goal_rep
    --->    unify_construct_rep(
                var_rep,
                cons_id_rep,
                list(var_rep)
            )
    ;       unify_deconstruct_rep(
                var_rep,
                cons_id_rep,
                list(var_rep)
            )
        ;   partial_deconstruct_rep(
                % A partial deconstruction of the form
                % X = f(Y_1, Y_2, ..., Y_n)
                % where X is more instanciated after the unification
                % than before.
                var_rep,            % X
                cons_id_rep,        % f
                list(maybe(var_rep))
                                    % The list of Y_i's. Y_i's which are input
                                    % are wrapped in `yes', while the other
                                    % Y_i positions are `no'.
            )
        ;   partial_construct_rep(
                % A partial construction of the form
                % X = f(Y_1, Y_2, ..., Y_n)
                % where X is free before the unification and bound,
                % but not ground, after the unification.
                var_rep,            % X
                cons_id_rep,        % f
                list(maybe(var_rep))
                                    % The list of Y_i's.  Y_i's which are input
                                    % are wrapped in `yes', while the other
                                    % Y_i positions are `no'.
            )
    ;       unify_assign_rep(
                var_rep,            % target
                var_rep             % source
            )
    ;       cast_rep(
                var_rep,            % target
                var_rep             % source
            )
    ;       unify_simple_test_rep(
                var_rep,
                var_rep
            )
    ;       pragma_foreign_code_rep(
                list(var_rep)       % arguments
            )
    ;       higher_order_call_rep(
                var_rep,            % the closure to call
                list(var_rep)       % the call's plain arguments
            )
    ;       method_call_rep(
                var_rep,            % typeclass info var
                int,                % method number
                list(var_rep)       % the call's plain arguments
            )
    ;       plain_call_rep(
                string,             % name of called pred's module
                string,             % name of the called pred
                list(var_rep)       % the call's arguments
            )
    ;       builtin_call_rep(
                string,             % name of called pred's module
                string,             % name of the called pred
                list(var_rep)       % the call's arguments
            ).

:- type var_rep ==  int.

:- type cons_id_rep ==  string.

:- type detism_rep
    --->    det_rep
    ;       semidet_rep
    ;       nondet_rep
    ;       multidet_rep
    ;       cc_nondet_rep
    ;       cc_multidet_rep
    ;       erroneous_rep
    ;       failure_rep.

    % If the given atomic goal behaves like a call in the sense that it
    % generates events, then return the list of variables that are passed
    % as arguments.
    %
:- func atomic_goal_generates_event(atomic_goal_rep) = maybe(list(var_rep)).

    % If the given goal generates internal events directly then this
    % function will return yes and no otherwise.
    %
:- func goal_generates_internal_event(goal_rep) = bool.

    % call_does_not_generate_events(ModuleName, PredName, Arity): succeeds iff
    % a call to the named predicate will not generate events in a debugging
    % grade.
    %
:- pred call_does_not_generate_events(string::in, string::in, int::in)
    is semidet.

    % The atomic goal's module, name and arity.
:- type atomic_goal_id
    ---> atomic_goal_id(string, string, int).

    % Can we find out the atomic goals name, module and arity from
    % its atomic_goal_rep? If so return them, otherwise return no.
    %
:- func atomic_goal_identifiable(atomic_goal_rep) =
    maybe(atomic_goal_id).

%-----------------------------------------------------------------------------%

    % The following three types are derived from compiler/hlds_goal.m.

:- type goal_path == list(goal_path_step).

% This is similar to the type goal_path defined in the module
% compiler/hlds_goal.m.

:- type goal_path_string == string.

:- type goal_path_step 
    --->    conj(int)
    ;       disj(int)
    ;       switch(int)
    ;       ite_cond
    ;       ite_then
    ;       ite_else
    ;       neg
    ;       scope(maybe_cut)
    ;       first
    ;       later.

    % Does the scope goal have a different determinism inside than outside?
:- type maybe_cut
    --->    cut
    ;       no_cut.

:- pred path_from_string_det(string::in, goal_path::out) is det.

:- pred string_from_path(goal_path::in, string::out) is det.

:- pred path_from_string(string::in, goal_path::out) is semidet.

:- pred path_step_from_string(string::in, goal_path_step::out) is semidet.

:- pred is_path_separator(char::in) is semidet.

    % User-visible head variables are represented by a number from 1..N,
    % where N is the user-visible arity.
    %
    % Both user-visible and compiler-generated head variables can be
    % referred to via their position in the full list of head variables;
    % the first head variable is at position 1.

:- type arg_pos
    --->    user_head_var(int)  % Nth in the list of arguments after
                                % filtering out non-user-visible vars.
    ;       any_head_var(int)   % Nth in the list of all arguments.

    ;       any_head_var_from_back(int).
                                % (M-N+1)th argument in the list of all
                                % arguments, where N is the value of the int
                                % in the constructor and M is the total number
                                % of arguments.

    % A particular subterm within a term is represented by a term_path.
    % This is the list of argument positions that need to be followed
    % in order to travel from the root to the subterm. In contrast to
    % goal_paths, this list is in top-down order.
:- type term_path ==    list(int).

    % Returns type_of(_ `with_type` proc_rep), for use in C code.
    %
:- func proc_rep_type = type_desc.

    % Returns type_of(_ `with_type` goal_rep), for use in C code.
    %
:- func goal_rep_type = type_desc.

    % Construct a representation of the interface determinism of a
    % procedure. The code we have chosen is not sequential; instead
    % it encodes the various properties of each determinism.
    % This must match the encoding of MR_Determinism in
    % mercury_stack_layout.h.
    %
    % The 8 bit is set iff the context is first_solution.
    % The 4 bit is set iff the min number of solutions is more than zero.
    % The 2 bit is set iff the max number of solutions is more than zero.
    % The 1 bit is set iff the max number of solutions is more than one.
    %
:- func detism_rep(detism_rep) = int.

:- pred determinism_representation(detism_rep, int).
:- mode determinism_representation(in, out) is det.
:- mode determinism_representation(out, in) is semidet.

:- type bytecode_goal_type
    --->    goal_conj
    ;       goal_disj
    ;       goal_switch
    ;       goal_ite
    ;       goal_neg
    ;       goal_scope
    ;       goal_construct
    ;       goal_deconstruct
    ;       goal_partial_construct
    ;       goal_partial_deconstruct
    ;       goal_assign
    ;       goal_cast
    ;       goal_simple_test
    ;       goal_foreign
    ;       goal_ho_call
    ;       goal_method_call
    ;       goal_plain_call
    ;       goal_builtin_call.

:- func goal_type_to_byte(bytecode_goal_type) = int.

:- func byte_to_goal_type(int) = bytecode_goal_type is semidet.

    % A variable number is represented in a byte if there are no more than
    % 255 variables in the procedure.  Otherwise a short is used.
    %
:- type var_num_rep
    --->    byte
    ;       short.

:- pred var_num_rep_byte(var_num_rep, int).
:- mode var_num_rep_byte(in, out) is det.
:- mode var_num_rep_byte(out, in) is semidet.

    % Some predicates that operate on polymorphic values do not need
    % the type_infos describing the types bound to the variables.
    % It is of course faster not to pass type_infos to such predicates
    % (especially since may also be able to avoid constructing those
    % type_infos), and it can also be easier for a compiler module
    % (e.g. common.m, size_prof.m) that generates calls to such predicates
    % not to have to create those type_infos.
    %
    % All the predicates for whose names no_type_info_builtin succeeds
    % are defined by compiler implementors. They are all predicates
    % implemented by foreign language code in the standard library.
    % For some, but not all, the compiler generates code inline.
    %
:- pred no_type_info_builtin(module_name::in, string::in, int::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module require.
:- import_module string.

atomic_goal_generates_event(unify_construct_rep(_, _, _)) = no.
atomic_goal_generates_event(unify_deconstruct_rep(_, _, _)) = no.
atomic_goal_generates_event(partial_construct_rep(_, _, _)) = no.
atomic_goal_generates_event(partial_deconstruct_rep(_, _, _)) = no.
atomic_goal_generates_event(unify_assign_rep(_, _)) = no.
atomic_goal_generates_event(unify_simple_test_rep(_, _)) = no.
atomic_goal_generates_event(cast_rep(_, _)) = no.
atomic_goal_generates_event(pragma_foreign_code_rep(_)) = no.
atomic_goal_generates_event(higher_order_call_rep(_, Args)) = yes(Args).
atomic_goal_generates_event(method_call_rep(_, _, Args)) = yes(Args).
atomic_goal_generates_event(builtin_call_rep(_, _, _)) = no.
atomic_goal_generates_event(plain_call_rep(ModuleName, PredName, Args)) =
    ( call_does_not_generate_events(ModuleName, PredName, list.length(Args)) ->
        no
    ;
        yes(Args)
    ).

call_does_not_generate_events(ModuleName, PredName, Arity) :-
    (
        string_to_sym_name(ModuleName, ".", SymModuleName),
        non_traced_mercury_builtin_module(SymModuleName)
    ;
        % The debugger cannot handle calls to polymorphic builtins that 
        % do not take a type_info argument, so such calls are not traced.
        string_to_sym_name(ModuleName, ".", SymModuleName),
        no_type_info_builtin(SymModuleName, PredName, Arity)
    ;
        pred_is_external(ModuleName, PredName, Arity)
    ;
        % Events from compiler generated predicates are not included in the
        % annotated trace at the moment.
        (
            PredName = "__Unify__"
        ;
            PredName = "__Index__"
        ;
            PredName = "__Compare__"
        )
    ).

goal_generates_internal_event(conj_rep(_)) = no.
goal_generates_internal_event(disj_rep(_)) = yes.
goal_generates_internal_event(switch_rep(_)) = yes.
goal_generates_internal_event(ite_rep(_, _, _)) = yes.
goal_generates_internal_event(negation_rep(_)) = yes.
goal_generates_internal_event(scope_rep(_, _)) = no.
% Atomic goals may generate interface events, not internal events.
goal_generates_internal_event(atomic_goal_rep(_, _, _, _, _)) = no.

atomic_goal_identifiable(unify_construct_rep(_, _, _)) = no.
atomic_goal_identifiable(unify_deconstruct_rep(_, _, _)) = no.
atomic_goal_identifiable(partial_construct_rep(_, _, _)) = no.
atomic_goal_identifiable(partial_deconstruct_rep(_, _, _)) = no.
atomic_goal_identifiable(unify_assign_rep(_, _)) = no.
atomic_goal_identifiable(unify_simple_test_rep(_, _)) = no.
atomic_goal_identifiable(cast_rep(_, _)) = no.
atomic_goal_identifiable(pragma_foreign_code_rep(_)) = no.
atomic_goal_identifiable(higher_order_call_rep(_, _)) = no.
atomic_goal_identifiable(method_call_rep(_, _, _)) = no.
atomic_goal_identifiable(builtin_call_rep(Module, Name, Args)) =
    yes(atomic_goal_id(Module, Name, length(Args))).
atomic_goal_identifiable(plain_call_rep(Module, Name, Args)) =
    yes(atomic_goal_id(Module, Name, length(Args))).

:- pragma export(proc_rep_type = out, "ML_proc_rep_type").

proc_rep_type = type_of(_ `with_type` proc_rep).

:- pragma export(goal_rep_type = out, "ML_goal_rep_type").

goal_rep_type = type_of(_ `with_type` goal_rep).

%-----------------------------------------------------------------------------%

path_from_string_det(GoalPathStr, GoalPath) :-
    ( path_from_string(GoalPathStr, GoalPathPrime) ->
        GoalPath = GoalPathPrime
    ;
        error("path_from_string_det: path_from_string failed")
    ).

path_from_string(GoalPathStr, GoalPath) :-
    StepStrs = string__words(is_path_separator, GoalPathStr),
    list__map(path_step_from_string, StepStrs, GoalPath).

path_step_from_string(String, Step) :-
    string__first_char(String, First, Rest),
    path_step_from_string_2(First, Rest, Step).

:- pred path_step_from_string_2(char::in, string::in, goal_path_step::out)
    is semidet.

path_step_from_string_2('c', NStr, conj(N)) :-
    string__to_int(NStr, N).
path_step_from_string_2('d', NStr, disj(N)) :-
    string__to_int(NStr, N).
path_step_from_string_2('s', NStr, switch(N)) :-
    string__to_int(NStr, N).
path_step_from_string_2('?', "", ite_cond).
path_step_from_string_2('t', "", ite_then).
path_step_from_string_2('e', "", ite_else).
path_step_from_string_2('~', "", neg).
path_step_from_string_2('q', "!", scope(cut)).
path_step_from_string_2('q', "", scope(no_cut)).
path_step_from_string_2('f', "", first).
path_step_from_string_2('l', "", later).

is_path_separator(';').

string_from_path(GoalPath, GoalPathStr) :-
    list.map(string_from_path_step, GoalPath, GoalPathSteps),
    GoalPathStr = string.join_list(";", GoalPathSteps) ++ ";".

:- pred string_from_path_step(goal_path_step::in, string::out) is det.

string_from_path_step(conj(N), "c" ++ int_to_string(N)).
string_from_path_step(disj(N), "d" ++ int_to_string(N)).
string_from_path_step(switch(N), "s" ++ int_to_string(N)).
string_from_path_step(ite_cond, "?").
string_from_path_step(ite_then, "t").
string_from_path_step(ite_else, "e").
string_from_path_step(neg, "~").
string_from_path_step(scope(cut), "q!").
string_from_path_step(scope(no_cut), "q").
string_from_path_step(first, "f").
string_from_path_step(later, "l").

%-----------------------------------------------------------------------------%

detism_rep(Detism) = Rep :-
    determinism_representation(Detism, Rep).

% This encoding must match the encoding of MR_Determinism in
% runtime/mercury_stack_layout.h. The rationale for this encoding
% is documented there.

determinism_representation(det_rep, 6).
determinism_representation(semidet_rep, 2).
determinism_representation(nondet_rep, 3).
determinism_representation(multidet_rep, 7).
determinism_representation(erroneous_rep, 4).
determinism_representation(failure_rep, 0).
determinism_representation(cc_nondet_rep, 10).
determinism_representation(cc_multidet_rep, 14).

%-----------------------------------------------------------------------------%

goal_type_to_byte(Type) = TypeInt :-
    goal_type_byte(TypeInt, Type).

byte_to_goal_type(TypeInt) = Type :-
    goal_type_byte(TypeInt, Type).

:- pred goal_type_byte(int, bytecode_goal_type).
:- mode goal_type_byte(in, out) is semidet.
:- mode goal_type_byte(out, in) is det.

goal_type_byte(1, goal_conj).
goal_type_byte(2, goal_disj).
goal_type_byte(3, goal_switch).
goal_type_byte(4, goal_ite).
goal_type_byte(5, goal_neg).
goal_type_byte(6, goal_scope).
goal_type_byte(7, goal_construct).
goal_type_byte(8, goal_deconstruct).
goal_type_byte(9, goal_partial_construct).
goal_type_byte(10, goal_partial_deconstruct).
goal_type_byte(11, goal_assign).
goal_type_byte(12, goal_cast).
goal_type_byte(13, goal_simple_test).
goal_type_byte(14, goal_foreign).
goal_type_byte(15, goal_ho_call).
goal_type_byte(16, goal_method_call).
goal_type_byte(17, goal_plain_call).
goal_type_byte(18, goal_builtin_call).

%-----------------------------------------------------------------------------%

var_num_rep_byte(byte, 0).
var_num_rep_byte(short, 1).

%-----------------------------------------------------------------------------%

no_type_info_builtin(ModuleName, PredName, Arity) :-
    no_type_info_builtin_2(ModuleNameType, PredName, Arity),
    (
        ModuleNameType = builtin,
        mercury_public_builtin_module(ModuleName)
    ;
        ModuleNameType = private_builtin,
        mercury_private_builtin_module(ModuleName)
    ;
        ModuleNameType = table_builtin,
        mercury_table_builtin_module(ModuleName)
    ;
        ModuleNameType = term_size_prof_builtin,
        mercury_term_size_prof_builtin_module(ModuleName)
    ).

:- type builtin_mod
    --->    builtin
    ;       private_builtin
    ;       table_builtin
    ;       term_size_prof_builtin.

:- pred no_type_info_builtin_2(builtin_mod::out, string::in, int::in)
    is semidet.

no_type_info_builtin_2(private_builtin, "store_at_ref", 2).
no_type_info_builtin_2(private_builtin, "unsafe_type_cast", 2).
no_type_info_builtin_2(builtin, "unsafe_promise_unique", 2).
no_type_info_builtin_2(private_builtin,
    "superclass_from_typeclass_info", 3).
no_type_info_builtin_2(private_builtin,
    "instance_constraint_from_typeclass_info", 3).
no_type_info_builtin_2(private_builtin,
    "type_info_from_typeclass_info", 3).
no_type_info_builtin_2(private_builtin,
    "unconstrained_type_info_from_typeclass_info", 3).
no_type_info_builtin_2(table_builtin, "table_restore_any_answer", 3).
no_type_info_builtin_2(table_builtin, "table_lookup_insert_enum", 4).
no_type_info_builtin_2(table_builtin, "table_lookup_insert_typeinfo", 3).
no_type_info_builtin_2(table_builtin, "table_lookup_insert_typeclassinfo", 3).
no_type_info_builtin_2(term_size_prof_builtin, "increment_size", 2).

    % True iff the given predicate is defined with an :- external
    % declaration.  Note that the arity includes the hidden type info
    % arguments for polymorphic predicates.
    %
:- pred pred_is_external(string::in, string::in, int::in) is semidet.

pred_is_external("exception", "builtin_catch", 4).
pred_is_external("exception", "builtin_throw", 1).
pred_is_external("builtin", "unify", 3).
pred_is_external("builtin", "compare", 4).
pred_is_external("builtin", "compare_representation", 4).

%-----------------------------------------------------------------------------%
