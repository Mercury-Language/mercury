%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term.m.
% Main author: fjh.
% Stability: medium.
%
% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.
% Terms are polymorphic so that terms representing different kinds of
% thing can be made to be of different types so they don't get mixed up.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module term.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module map.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type term(T)
    --->    functor(
                const,
                list(term(T)),
                term.context
            )
    ;       variable(
                var(T),
                term.context
            ).

:- type const
    --->    atom(string)
    ;       integer(int)
    ;       string(string)
    ;       float(float)
    ;       implementation_defined(string).

:- type term.context
    --->    context(string, int).
            % file name, line number.

:- type var(T).
:- type var_supply(T).

:- type generic
    --->    generic.

:- type term    ==  term(generic).
:- type var     ==  var(generic).

:- func get_term_context(term(T)) = term.context.

%-----------------------------------------------------------------------------%

    % The following predicates can convert values of (almost) any type
    % to the type `term' and back again.

:- type term_to_type_result(T, U)
    --->    ok(T)
    ;       error(term_to_type_error(U)).

:- type term_to_type_result(T) == term_to_type_result(T, generic).

    % term.try_term_to_type(Term, Result):
    % Try to convert the given term to a ground value of type T.
    % If successful, return `ok(X)' where X is the converted value.
    % If Term is not ground, return `mode_error(Var, Context)',
    % where Var is a variable occurring in Term.
    % If Term is not a valid term of the specified type, return
    % `type_error(SubTerm, ExpectedType, Context, ArgContexts)',
    % where SubTerm is a sub-term of Term and ExpectedType is the type
    % expected for that part of Term.
    % Context specifies the file and line number where the
    % offending part of the term was read in from, if available.
    % ArgContexts specifies the path from the root of the term
    % to the offending subterm.
    %
:- func try_term_to_type(term(U)) = term_to_type_result(T, U).
:- pred try_term_to_type(term(U)::in, term_to_type_result(T, U)::out) is det.

:- type term_to_type_error(T)
    --->    type_error(
                term(T),
                type_desc.type_desc,
                context,
                term_to_type_context
            )
    ;       mode_error(
                var(T),
                term_to_type_context
            ).

:- type term_to_type_context == list(term_to_type_arg_context).

:- type term_to_type_arg_context
    --->    arg_context(
                const,      % functor
                int,        % argument number (starting from 1)
                context     % filename & line number
            ).

    % term_to_type(Term, Type) :- try_term_to_type(Term, ok(Type)).
    %
:- pred term_to_type(term(U)::in, T::out) is semidet.

    % Like term_to_type, but calls error/1 rather than failing.
    %
:- func det_term_to_type(term(_)) = T.
:- pred det_term_to_type(term(_)::in, T::out) is det.

    % Converts a value to a term representation of that value.
    %
:- func type_to_term(T) = term(_).
:- pred type_to_term(T::in, term(_)::out) is det.

    % Convert the value stored in the univ (as distinct from the univ itself)
    % to a term.
    %
:- func univ_to_term(univ) = term(_).
:- pred univ_to_term(univ::in, term(_)::out) is det.

%-----------------------------------------------------------------------------%

    % vars(Term, Vars):
    %
    % Vars is the list of variables contained in Term, in the order
    % obtained by traversing the term depth first, left-to-right.
    %
:- func vars(term(T)) = list(var(T)).
:- pred vars(term(T)::in, list(var(T))::out) is det.

    % As above, but with an accumulator.
    %
:- func vars_2(term(T), list(var(T))) = list(var(T)).
:- pred vars_2(term(T)::in, list(var(T))::in, list(var(T))::out) is det.

    % vars_list(TermList, Vars):
    %
    % Vars is the list of variables contained in TermList, in the order
    % obtained by traversing the list of terms depth-first, left-to-right.
    %
:- func vars_list(list(term(T))) = list(var(T)).
:- pred vars_list(list(term(T))::in, list(var(T))::out) is det.

    % contains_var(Term, Var):
    %
    % True if Term contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred contains_var(term(T), var(T)).
:- mode contains_var(in, in) is semidet.
:- mode contains_var(in, out) is nondet.

    % contains_var_list(TermList, Var):
    %
    % True if TermList contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred contains_var_list(list(term(T)), var(T)).
:- mode contains_var_list(in, in) is semidet.
:- mode contains_var_list(in, out) is nondet.

:- type substitution(T) == map(var(T), term(T)).
:- type substitution    == substitution(generic).

    % unify_term(Term1, Term2, Bindings0, Bindings):
    %
    % Unify (with occur check) two terms with respect to a set of bindings
    % and possibly update the set of bindings.
    %
:- pred unify_term(term(T)::in, term(T)::in, substitution(T)::in,
    substitution(T)::out) is semidet.

    % As above, but unify the corresponding elements of two lists of terms.
    % Fails if the lists are not of equal length.
    %
:- pred unify_term_list(list(term(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % unify_term_dont_bind(Term1, Term2, BoundVars, !Bindings):
    %
    % Unify (with occur check) two terms with respect to a set of bindings
    % and possibly update the set of bindings. Fails if any of the variables
    % in BoundVars would become bound by the unification.
    %
:- pred unify_term_dont_bind(term(T)::in, term(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % As above, but unify the corresponding elements of two lists of terms.
    % Fails if the lists are not of equal length.
    %
:- pred unify_term_list_dont_bind(list(term(T))::in, list(term(T))::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

    % list_subsumes(Terms1, Terms2, Subst) succeeds iff the list
    % Terms1 subsumes (is more general than) Terms2, producing a substitution
    % which when applied to Terms1 will give Terms2.
    %
:- pred list_subsumes(list(term(T))::in, list(term(T))::in,
    substitution(T)::out) is semidet.

    % substitute(Term0, Var, Replacement, Term):
    %
    % Replace all occurrences of Var in Term0 with Replacement,
    % and return the result in Term.
    %
:- func substitute(term(T), var(T), term(T)) = term(T).
:- pred substitute(term(T)::in, var(T)::in, term(T)::in, term(T)::out)
    is det.

    % As above, except for a list of terms rather than a single
    %
:- func substitute_list(list(term(T)), var(T), term(T)) = list(term(T)).
:- pred substitute_list(list(term(T))::in, var(T)::in, term(T)::in,
    list(term(T))::out) is det.

    % substitute_corresponding(Vars, Repls, Term0, Term):
    %
    % Replace all occurrences of variables in Vars with the corresponding
    % term in Repls, and return the result in Term. If Vars contains
    % duplicates, or if Vars is not the same length as Repls, the behaviour
    % is undefined and probably harmful.
    %
:- func substitute_corresponding(list(var(T)), list(term(T)),
    term(T)) = term(T).
:- pred substitute_corresponding(list(var(T))::in, list(term(T))::in,
    term(T)::in, term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func substitute_corresponding_list(list(var(T)),
    list(term(T)), list(term(T))) = list(term(T)).
:- pred substitute_corresponding_list(list(var(T))::in,
    list(term(T))::in, list(term(T))::in, list(term(T))::out) is det.

    % apply_rec_substitution(Term0, Substitution, Term):
    %
    % Recursively apply substitution to Term0 until no more substitutions
    % can be applied, and then return the result in Term.
    %
:- func apply_rec_substitution(term(T), substitution(T)) = term(T).
:- pred apply_rec_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func apply_rec_substitution_to_list(list(term(T)),
    substitution(T)) = list(term(T)).
:- pred apply_rec_substitution_to_list(list(term(T))::in,
    substitution(T)::in, list(term(T))::out) is det.

    % apply_substitution(Term0, Substitution, Term):
    %
    % Apply substitution to Term0 and return the result in Term.
    %
:- func apply_substitution(term(T), substitution(T)) = term(T).
:- pred apply_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func apply_substitution_to_list(list(term(T)),
    substitution(T)) = list(term(T)).
:- pred apply_substitution_to_list(list(term(T))::in,
    substitution(T)::in, list(term(T))::out) is det.

    % occurs(Term0, Var, Substitution):
    % True iff Var occurs in the term resulting after applying Substitution
    % to Term0. Var variable must not be mapped by Substitution.
    %
:- pred occurs(term(T)::in, var(T)::in, substitution(T)::in) is semidet.

    % As above, except for a list of terms rather than a single term.
    %
:- pred occurs_list(list(term(T))::in, var(T)::in, substitution(T)::in)
    is semidet.

    % relabel_variable(Term0, OldVar, NewVar, Term):
    %
    % Replace all occurrences of OldVar in Term0 with NewVar and put the result
    % in Term.
    %
:- func relabel_variable(term(T), var(T), var(T)) = term(T).
:- pred relabel_variable(term(T)::in, var(T)::in, var(T)::in, term(T)::out)
    is det.

    % As above, except applies to a list of terms rather than a single term.
    % XXX the name of the predicate is misleading.
    %
:- func relabel_variables(list(term(T)), var(T), var(T)) = list(term(T)).
:- pred relabel_variables(list(term(T))::in, var(T)::in, var(T)::in,
    list(term(T))::out) is det.

    % Same as relabel_variable, except relabels multiple variables.
    % If a variable is not in the map, it is not replaced.
    %
:- func apply_variable_renaming(term(T), map(var(T), var(T))) = term(T).
:- pred apply_variable_renaming(term(T)::in, map(var(T), var(T))::in,
    term(T)::out) is det.

    % Applies apply_variable_renaming to a list of terms.
    %
:- func apply_variable_renaming_to_list(list(term(T)),
    map(var(T), var(T))) = list(term(T)).
:- pred apply_variable_renaming_to_list(list(term(T))::in,
    map(var(T), var(T))::in, list(term(T))::out) is det.

    % Applies apply_variable_renaming to a var.
    %
:- func apply_variable_renaming_to_var(map(var(T), var(T)),
    var(T)) = var(T).
:- pred apply_variable_renaming_to_var(map(var(T), var(T))::in,
    var(T)::in, var(T)::out) is det.

    % Applies apply_variable_renaming to a list of vars.
    %
:- func apply_variable_renaming_to_vars(map(var(T), var(T)),
    list(var(T))) = list(var(T)).
:- pred apply_variable_renaming_to_vars(map(var(T), var(T))::in,
    list(var(T))::in, list(var(T))::out) is det.

    % is_ground_in_bindings(Term, Bindings) is true iff no variables contained
    % in Term are non-ground in Bindings.
    %
:- pred is_ground_in_bindings(term(T)::in, substitution(T)::in) is semidet.

    % is_ground(Term) is true iff Term contains no variables.
    %
:- pred is_ground(term(T)::in) is semidet.

%-----------------------------------------------------------------------------%

    % To manage a supply of variables, use the following 2 predicates.
    % (We might want to give these a unique mode later.)

    % init_var_supply(VarSupply):
    %
    % Returns a fresh var_supply for producing fresh variables.
    %
:- func init_var_supply = var_supply(T).
:- pred init_var_supply(var_supply(T)).
:- mode init_var_supply(out) is det.
:- mode init_var_supply(in) is semidet. % implied

    % create_var(VarSupply0, Variable, VarSupply):
    % Create a fresh variable (var) and return the updated var_supply.
    %
:- pred create_var(var(T)::out, var_supply(T)::in, var_supply(T)::out) is det.

    % var_id(Variable):
    % Returns a unique number associated with this variable w.r.t.
    % its originating var_supply.
    %
:- func var_id(var(T)) = int.

%-----------------------------------------------------------------------------%

    % from_int/1 should only be applied to integers returned by to_int/1.
    % This instance declaration is needed to allow sets of variables to be
    % represented using sparse_bitset.m.
:- instance enum(var(_)).

    % Convert a variable to an int. Different variables map to different ints.
    % Other than that, the mapping is unspecified.
    %
:- func var_to_int(var(T)) = int.
:- pred var_to_int(var(T)::in, int::out) is det.

%-----------------------------------------------------------------------------%

    % Given a term context, return the source line number.
    %
:- func context_line(context) = int.
:- pred context_line(context::in, int::out) is det.

    % Given a term context, return the source file.
    %
:- func context_file(context) = string.
:- pred context_file(context::in, string::out) is det.

    % Used to initialize the term context when reading in
    % (or otherwise constructing) a term.
    %
:- func context_init = context.
:- pred context_init(context::out) is det.
:- func context_init(string, int) = context.
:- pred context_init(string::in, int::in, context::out) is det.

    % Convert a list of terms which are all vars into a list of vars.
    % Abort (call error/1) if the list contains any non-variables.
    %
:- func term_list_to_var_list(list(term(T))) = list(var(T)).

    % Convert a list of terms which are all vars into a list of vars.
    %
:- pred term_list_to_var_list(list(term(T))::in, list(var(T))::out) is semidet.

    % Convert a list of terms which are all vars into a list of vars
    % (or vice versa).
    %
:- func var_list_to_term_list(list(var(T))) = list(term(T)).
:- pred var_list_to_term_list(list(var(T))::in, list(term(T))::out) is det.

%-----------------------------------------------------------------------------%

    % generic_term(Term) is true iff `Term' is a term of type
    % `term' ie `term(generic)'. It is useful because in some instances
    % it doesn't matter what the type of a term is, and passing it to this
    % predicate will ground the type avoiding unbound type variable warnings.
    %
:- pred generic_term(term::in) is det.

    % Coerce a term of type `T' into a term of type `U'.
    %
:- func coerce(term(T)) = term(U).
:- pred coerce(term(T)::in, term(U)::out) is det.

    % Coerce a var of type `T' into a var of type `U'.
    %
:- func coerce_var(var(T)) = var(U).
:- pred coerce_var(var(T)::in, var(U)::out) is det.

    % Coerce a var_supply of type `T' into a var_supply of type `U'.
    %
:- func coerce_var_supply(var_supply(T)) = var_supply(U).
:- pred coerce_var_supply(var_supply(T)::in, var_supply(U)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%

:- interface.

    % Returns the highest numbered variable returned from this var_supply.
    %
:- func var_supply_max_var(var_supply(T)) = var(T).

:- func var_supply_num_allocated(var_supply(T)) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module version_array.

%-----------------------------------------------------------------------------%

:- type var_supply(T)
    --->    var_supply(int).

:- type var(T)
    --->    var(int).

%-----------------------------------------------------------------------------%

get_term_context(Term) = Context :-
    ( Term = functor(_, _, Context)
    ; Term = variable(_, Context)
    ).

%-----------------------------------------------------------------------------%

term_to_type(Term, Val) :-
    try_term_to_type(Term, ok(Val)).

try_term_to_type(Term, Result) :-
    try_term_to_univ(Term, type_desc.type_of(ValTypedVar), UnivResult),
    (
        UnivResult = ok(Univ),
        det_univ_to_type(Univ, Val),
        same_type(Val, ValTypedVar),
        Result = ok(Val)
    ;
        UnivResult = error(Error),
        Result = error(Error)
    ).

:- pred try_term_to_univ(term(T)::in, type_desc.type_desc::in,
    term_to_type_result(univ, T)::out) is det.

try_term_to_univ(Term, Type, Result) :-
    try_term_to_univ_2(Term, Type, [], Result).

:- pred try_term_to_univ_2(term(T)::in,
    type_desc::in, term_to_type_context::in,
    term_to_type_result(univ, T)::out) is det.

try_term_to_univ_2(variable(Var, _), _Type, Context,
        error(mode_error(Var, Context))).
try_term_to_univ_2(Term, Type, Context, Result) :-
    Term = functor(Functor, ArgTerms, TermContext),
    (
        type_ctor_and_args(Type, TypeCtor, TypeArgs),
        term_to_univ_special_case(
            type_ctor_module_name(TypeCtor),
            type_ctor_name(TypeCtor),
            TypeArgs, Term, Type, Context, SpecialCaseResult)
    ->
        Result = SpecialCaseResult
    ;
        Functor = atom(FunctorName),
        list.length(ArgTerms, Arity),
        find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes),
        term_list_to_univ_list(ArgTerms, ArgTypes, Functor, 1, Context,
            TermContext, ArgsResult)
    ->
        (
            ArgsResult = ok(ArgValues),
            ( Value = construct.construct(Type, FunctorNumber, ArgValues) ->
                Result = ok(Value)
            ;
                error("term_to_type: construct/3 failed")
            )
        ;
            ArgsResult = error(Error),
            Result = error(Error)
        )
    ;
        % The arg contexts are built up in reverse order,
        % so we need to reverse them here.
        list.reverse(Context, RevContext),
        Result = error(type_error(Term, Type, TermContext, RevContext))
    ).

:- pred term_to_univ_special_case(string::in, string::in,
    list(type_desc)::in,
    term(T)::in(bound(functor(ground, ground, ground))),
    type_desc.type_desc::in, term_to_type_context::in,
    term_to_type_result(univ, T)::out) is semidet.

term_to_univ_special_case("builtin", "character", [], Term, _, _, ok(Univ)) :-
    Term = functor(atom(FunctorName), [], _),
    string.first_char(FunctorName, Char, ""),
    type_to_univ(Char, Univ).
term_to_univ_special_case("builtin", "int", [],
        Term, _, _, ok(Univ)) :-
    Term = functor(integer(Int), [], _),
    type_to_univ(Int, Univ).
term_to_univ_special_case("builtin", "string", [],
        Term, _, _, ok(Univ)) :-
    Term = functor(string(String), [], _),
    type_to_univ(String, Univ).
term_to_univ_special_case("builtin", "float", [], Term, _, _, ok(Univ)) :-
    Term = functor(float(Float), [], _),
    type_to_univ(Float, Univ).
term_to_univ_special_case("bitmap", "bitmap", [],
        Term, _Type, _PrevContext, ok(Univ)) :-
    % Bitmaps are represented as hex strings.
    Term = functor(string(String), [], _),
    type_to_univ(bitmap.from_string(String), Univ).
term_to_univ_special_case("array", "array", [ElemType],
        Term, _Type, PrevContext, Result) :-
    %
    % arrays are represented as terms of the form
    %   array([elem1, elem2, ...])
    %
    Term = functor(atom("array"), [ArgList], TermContext),

    % To convert such terms back to arrays, we first
    % convert the term representing the list of elements back to a list,
    % and then (if successful) we just call the array/1 function.
    %
    has_type(Elem, ElemType),
    ListType = type_of([Elem]),
    ArgContext = arg_context(atom("array"), 1, TermContext),
    NewContext = [ArgContext | PrevContext],
    try_term_to_univ_2(ArgList, ListType, NewContext, ArgResult),
    (
        ArgResult = ok(ListUniv),
        has_type(Elem2, ElemType),
        same_type(List, [Elem2]),
        det_univ_to_type(ListUniv, List),
        Array = array(List),
        Result = ok(univ(Array))
    ;
        ArgResult = error(Error),
        Result = error(Error)
    ).
term_to_univ_special_case("version_array", "version_array", [ElemType],
        Term, _Type, PrevContext, Result) :-
    % We handle version arrays in pretty much the same way as normal
    % arrays.
    Term = functor(atom("version_array"), [ArgList], TermContext),
    has_type(Elem, ElemType),
    ListType = type_of([Elem]),
    ArgContext = arg_context(atom("version_array"), 1, TermContext),
    NewContext = [ArgContext | PrevContext],
    try_term_to_univ_2(ArgList, ListType, NewContext, ArgResult),
    (
        ArgResult = ok(ListUniv),
        has_type(Elem2, ElemType),
        same_type(List, [Elem2]),
        det_univ_to_type(ListUniv, List),
        Array = version_array(List),
        Result = ok(univ(Array))
    ;
        ArgResult = error(Error),
        Result = error(Error)
    ).
term_to_univ_special_case("builtin", "c_pointer", _, _, _, _, _) :-
    fail.
term_to_univ_special_case("univ", "univ", [], Term, _, _, Result) :-
    % Implementing this properly would require keeping a global table mapping
    % from type names to type_infos for all of the types in the program...
    % so for the moment, we only allow it for basic types.
    Term = functor(atom("univ"), [Arg], _),
    Arg = functor(atom(":"), [Value, Type], _),
    (
        Type = functor(atom("int"), [], _),
        Value = functor(integer(Int), [], _),
        Univ = univ(Int)
    ;
        Type = functor(atom("string"), [], _),
        Value = functor(string(String), [], _),
        Univ = univ(String)
    ;
        Type = functor(atom("float"), [], _),
        Value = functor(float(Float), [], _),
        Univ = univ(Float)
    ),
    % The result is a `univ', but it is also wrapped in a `univ'
    % like all the other results returned from this procedure.
    Result = ok(univ(Univ)).

term_to_univ_special_case("type_desc", "type_desc", _, _, _, _, _) :-
    % Ditto.
    fail.

:- pred term_list_to_univ_list(list(term(T))::in,
    list(type_desc)::in, const::in, int::in,
    term_to_type_context::in, context::in,
    term_to_type_result(list(univ), T)::out) is semidet.

term_list_to_univ_list([], [], _, _, _, _, ok([])).
term_list_to_univ_list([ArgTerm | ArgTerms],
        [Type | Types], Functor, ArgNum, PrevContext, TermContext, Result) :-
    ArgContext = arg_context(Functor, ArgNum, TermContext),
    NewContext = [ArgContext | PrevContext],
    try_term_to_univ_2(ArgTerm, Type, NewContext, ArgResult),
    (
        ArgResult = ok(Arg),
        term_list_to_univ_list(ArgTerms, Types, Functor,
            ArgNum + 1, PrevContext, TermContext, RestResult),
        (
            RestResult = ok(Rest),
            Result = ok([Arg | Rest])
        ;
            RestResult = error(Error),
            Result = error(Error)
        )
    ;
        ArgResult = error(Error),
        Result = error(Error)
    ).

det_term_to_type(Term, X) :-
    ( term_to_type(Term, X1) ->
        X = X1
    ; \+ is_ground(Term) ->
        error("term.det_term_to_type failed, because the term wasn't ground")
    ;
        Message = "term.det_term_to_type failed, due to a type error:\n"
            ++ "the term wasn't a valid term for type `"
            ++ type_name(type_of(X)) ++ "'",
        error(Message)
    ).

%-----------------------------------------------------------------------------%

type_to_term(Val, Term) :-
    type_to_univ(Val, Univ),
    univ_to_term(Univ, Term).

univ_to_term(Univ, Term) :-
    context_init(Context),
    Type = univ_type(Univ),
    ( construct.num_functors(Type) = _ ->
        deconstruct(univ_value(Univ), canonicalize, FunctorString,
            _FunctorArity, FunctorArgs),
        univ_list_to_term_list(FunctorArgs, TermArgs),
        Term = functor(atom(FunctorString), TermArgs, Context)
    ;
        (
            type_ctor_and_args(Type, TypeCtor, TypeArgs),
            TypeName = type_ctor_name(TypeCtor),
            ModuleName = type_ctor_module_name(TypeCtor),
            univ_to_term_special_case(ModuleName, TypeName, TypeArgs,
                Univ, Context, SpecialCaseTerm)
        ->
            Term = SpecialCaseTerm
        ;
            Message = "term.type_to_term: unknown type `"
                ++ type_name(univ_type(Univ)) ++ "'",
            error(Message)
        )
    ).

:- pred univ_to_term_special_case(string::in, string::in,
    list(type_desc)::in, univ::in, context::in, term(T)::out)
    is semidet.

univ_to_term_special_case("builtin", "int", [], Univ, Context,
        functor(integer(Int), [], Context)) :-
    det_univ_to_type(Univ, Int).
univ_to_term_special_case("builtin", "float", [], Univ, Context,
        functor(float(Float), [], Context)) :-
    det_univ_to_type(Univ, Float).
univ_to_term_special_case("builtin", "character", [], Univ,
        Context, functor(atom(CharName), [], Context)) :-
    det_univ_to_type(Univ, Character),
    string.char_to_string(Character, CharName).
univ_to_term_special_case("builtin", "string", [], Univ, Context,
        functor(string(String), [], Context)) :-
    det_univ_to_type(Univ, String).
univ_to_term_special_case("type_desc", "type_desc", [], Univ, Context,
        functor(atom("type_info"), [Term], Context)) :-
    det_univ_to_type(Univ, TypeInfo),
    type_info_to_term(Context, TypeInfo, Term).
univ_to_term_special_case("univ", "univ", [], Univ, Context, Term) :-
    det_univ_to_type(Univ, NestedUniv),
    Term = functor(atom("univ"),
        [functor(atom(":"), [ValueTerm, TypeTerm], Context)], Context),
    type_info_to_term(Context, univ_type(NestedUniv), TypeTerm),
    NestedUnivValue = univ_value(NestedUniv),
    type_to_term(NestedUnivValue, ValueTerm).
univ_to_term_special_case("bitmap", "bitmap", [], Univ, Context,
        functor(string(BitmapStr), [], Context)) :-
    det_univ_to_type(Univ, Bitmap),
    BitmapStr = bitmap.to_string(Bitmap).

univ_to_term_special_case("array", "array", [ElemType], Univ, Context, Term) :-
    Term = functor(atom("array"), [ArgsTerm], Context),
    has_type(Elem, ElemType),
    same_type(List, [Elem]),
    det_univ_to_type(Univ, Array),
    array.to_list(Array, List),
    type_to_term(List, ArgsTerm).

univ_to_term_special_case("version_array", "version_array", [ElemType],
        Univ, Context, Term) :-
    Term = functor(atom("version_array"), [ArgsTerm], Context),
    has_type(Elem, ElemType),
    same_type(List, [Elem]),
    det_univ_to_type(Univ, Array),
    List = version_array.to_list(Array),
    type_to_term(List, ArgsTerm).

:- pred univ_list_to_term_list(list(univ)::in, list(term(T))::out) is det.

univ_list_to_term_list([], []).
univ_list_to_term_list([Value|Values], [Term|Terms]) :-
    univ_to_term(Value, Term),
    univ_list_to_term_list(Values, Terms).

    % Given a type_info, return a term that represents the name of that type.
    %
:- pred type_info_to_term(context::in, type_desc::in, term(T)::out) is det.

type_info_to_term(Context, TypeInfo, Term) :-
    type_ctor_and_args(TypeInfo, TypeCtor, ArgTypes),
    TypeName = type_ctor_name(TypeCtor),
    ModuleName = type_ctor_name(TypeCtor),
    list.map(type_info_to_term(Context), ArgTypes, ArgTerms),

    ( ModuleName = "builtin" ->
        Term = functor(atom(TypeName), ArgTerms, Context)
    ;
        Arg1 = functor(atom(ModuleName), [], Context),
        Arg2 = functor(atom(TypeName), ArgTerms, Context),
        Term = functor(atom(":"), [Arg1, Arg2], Context)
    ).

%-----------------------------------------------------------------------------%

    % vars(Term, Vars) is true if Vars is the list of variables
    % contained in Term obtained by depth-first left-to-right traversal
    % in that order.

vars(Term, Vars) :-
    vars_2(Term, [], Vars).

vars_list(Terms, Vars) :-
    vars_2_list(Terms, [], Vars).

vars_2(variable(Var, _), !Vars) :-
    !:Vars = [Var | !.Vars].
vars_2(functor(_, Args, _), !Vars) :-
    vars_2_list(Args, !Vars).

:- pred vars_2_list(list(term(T))::in, list(var(T))::in,
    list(var(T))::out) is det.

vars_2_list([], !Vars).
vars_2_list([Term | Terms], !Vars) :-
    vars_2_list(Terms, !Vars),
    vars_2(Term, !Vars).

%-----------------------------------------------------------------------------%

contains_var(variable(Var, _), Var).
contains_var(functor(_, Args, _), Var) :-
    contains_var_list(Args, Var).

contains_var_list([Term | _], Var) :-
    contains_var(Term, Var).
contains_var_list([_ | Terms], Var) :-
    contains_var_list(Terms, Var).

%-----------------------------------------------------------------------------%

context_line(context(_, LineNumber), LineNumber).
context_file(context(FileName, _), FileName).
context_init(context("", 0)).
context_init(File, LineNumber, context(File, LineNumber)).

%-----------------------------------------------------------------------------%

unify_term(variable(X, _), VarY @ variable(Y, _), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % Both X and Y already have bindings - just unify the terms
            % they are bound to.
            unify_term(BindingOfX, BindingOfY, !Bindings)
        ;
            % Y is a variable which hasn't been bound yet.
            apply_rec_substitution(BindingOfX, !.Bindings, SubstBindingOfX),
            ( SubstBindingOfX = variable(Y, _) ->
                true
            ;
                \+ occurs(SubstBindingOfX, Y, !.Bindings),
                map.set(Y, SubstBindingOfX, !Bindings)
            )
        )
    ;
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % X is a variable which hasn't been bound yet
            apply_rec_substitution(BindingOfY, !.Bindings, SubstBindingOfY),
            ( SubstBindingOfY = variable(X, _) ->
                true
            ;
                \+ occurs(SubstBindingOfY, X, !.Bindings),
                map.set(X, SubstBindingOfY, !Bindings)
            )
        ;
            % both X and Y are unbound variables -
            % bind one to the other
            ( X = Y ->
                true
            ;
                map.set(X, VarY, !Bindings)
            )
        )
    ).

unify_term(term.variable(X, _), term.functor(F, As, C), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        unify_term(BindingOfX, functor(F, As, C), !Bindings)
    ;
        \+ occurs_list(As, X, !.Bindings),
        map.set(X, functor(F, As, C), !Bindings)
    ).

unify_term(functor(F, As, C), variable(X, _), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        unify_term(functor(F, As, C), BindingOfX, !Bindings)
    ;
        \+ occurs_list(As, X, !.Bindings),
        map.set(X, functor(F, As, C), !Bindings)
    ).

unify_term(functor(F, AsX, _), functor(F, AsY, _), !Bindings) :-
    unify_term_list(AsX, AsY, !Bindings).

unify_term_list([], [], !Bindings).
unify_term_list([X | Xs], [Y | Ys], !Bindings) :-
    unify_term(X, Y, !Bindings),
    unify_term_list(Xs, Ys, !Bindings).

unify_term_dont_bind(variable(X, _), VarY @ variable(Y, _),
        BoundVars, !Bindings) :-
    ( list.member(Y, BoundVars) ->
        unify_term_bound_var(X, Y, BoundVars, !Bindings)
    ; list.member(X, BoundVars) ->
        unify_term_bound_var(Y, X, BoundVars, !Bindings)
    ; map.search(!.Bindings, X, BindingOfX) ->
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % Both X and Y already have bindings - just unify the
            % terms they are bound to.
            unify_term_dont_bind(BindingOfX, BindingOfY, BoundVars, !Bindings)
        ;
            apply_rec_substitution(BindingOfX, !.Bindings, SubstBindingOfX),
            % Y is a variable which hasn't been bound yet.
            ( SubstBindingOfX = variable(Y, _) ->
                true
            ;
                \+ occurs(SubstBindingOfX, Y, !.Bindings),
                map.det_insert(Y, SubstBindingOfX, !Bindings)
            )
        )
    ;
        ( map.search(!.Bindings, Y, BindingOfY) ->
            apply_rec_substitution(BindingOfY, !.Bindings, SubstBindingOfY),
            % X is a variable which hasn't been bound yet.
            ( SubstBindingOfY = variable(X, _) ->
                true
            ;
                \+ occurs(SubstBindingOfY, X, !.Bindings),
                map.det_insert(X, SubstBindingOfY, !Bindings)
            )
        ;
            % Both X and Y are unbound variables - bind one to the other.
            ( X = Y ->
                true
            ;
                map.det_insert(X, VarY, !Bindings)
            )
        )
    ).

unify_term_dont_bind(variable(X, _), functor(F, As, C), BoundVars, !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        unify_term_dont_bind(BindingOfX, functor(F, As, C), BoundVars,
            !Bindings)
    ;
        \+ occurs_list(As, X, !.Bindings),
        \+ list.member(X, BoundVars),
        map.det_insert(X, functor(F, As, C), !Bindings)
    ).

unify_term_dont_bind(functor(F, As, C), variable(X, _), BoundVars, !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        unify_term_dont_bind(functor(F, As, C), BindingOfX, BoundVars,
            !Bindings)
    ;
        \+ occurs_list(As, X, !.Bindings),
        \+ list.member(X, BoundVars),
        map.det_insert(X, functor(F, As, C), !Bindings)
    ).

unify_term_dont_bind(functor(FX, AsX, _CX), functor(FY, AsY, _CY), BoundVars,
        !Bindings) :-
    list.length(AsX, ArityX),
    list.length(AsY, ArityY),
    (
        FX = FY,
        ArityX = ArityY
    ->
        unify_term_list_dont_bind(AsX, AsY, BoundVars, !Bindings)
    ;
        fail
    ).

unify_term_list_dont_bind([], [], _, !Bindings).
unify_term_list_dont_bind([X | Xs], [Y | Ys], BoundVars, !Bindings) :-
    unify_term_dont_bind(X, Y, BoundVars, !Bindings),
    unify_term_list_dont_bind(Xs, Ys, BoundVars, !Bindings).

:- pred unify_term_bound_var(var(T)::in, var(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

unify_term_bound_var(Var, BoundVar, BoundVars, !Bindings) :-
    ( map.search(!.Bindings, Var, BindingOfVar) ->
        BindingOfVar = variable(Var2, _),
        unify_term_bound_var(Var2, BoundVar, BoundVars, !Bindings)
    ;
        ( Var = BoundVar ->
            true
        ;
            \+ list.member(Var, BoundVars),
            map.det_insert(Var, variable(BoundVar, context_init), !Bindings)
        )
    ).

list_subsumes(Terms1, Terms2, Subst) :-
    % Terms1 subsumes Terms2 iff Terms1 can be unified with Terms2
    % without binding any of the variables in Terms2.
    vars_list(Terms2, Terms2Vars),
    map.init(Subst0),
    unify_term_list_dont_bind(Terms1, Terms2, Terms2Vars, Subst0, Subst).

%-----------------------------------------------------------------------------%

occurs(variable(X, _), Y, Bindings) :-
    ( X = Y ->
        true
    ;
        map.search(Bindings, X, BindingOfX),
        occurs(BindingOfX, Y, Bindings)
    ).
occurs(functor(_F, As, _), Y, Bindings) :-
    occurs_list(As, Y, Bindings).

occurs_list([Term | Terms], Y, Bindings) :-
    ( occurs(Term, Y, Bindings) ->
        true
    ;
        occurs_list(Terms, Y, Bindings)
    ).

%-----------------------------------------------------------------------------%

substitute(V @ variable(Var, _), SearchVar, Replacement, Term) :-
    ( Var = SearchVar ->
        Term = Replacement
    ;
        Term = V
    ).
substitute(functor(Name, Args0, Context), Var, Replacement,
         functor(Name, Args, Context)) :-
    substitute_list(Args0, Var, Replacement, Args).

substitute_list([], _Var, _Replacement, []).
substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
    substitute(Term0, Var, Replacement, Term),
    substitute_list(Terms0, Var, Replacement, Terms).

substitute_corresponding(Ss, Rs, Term0, Term) :-
    map.init(Subst0),
    ( substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
        apply_substitution(Term0, Subst, Term)
    ;
        error("term.substitute_corresponding: different length lists")
    ).

substitute_corresponding_list(Ss, Rs, TermList0, TermList) :-
    map.init(Subst0),
    ( substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
        apply_substitution_to_list(TermList0, Subst, TermList)
    ;
        error("term.substitute_corresponding_list: different length lists")
    ).

:- pred substitute_corresponding_2(list(var(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

substitute_corresponding_2([], [], !Subst).
substitute_corresponding_2([S | Ss], [R | Rs], !Subst) :-
    map.set(S, R, !Subst),
    substitute_corresponding_2(Ss, Rs, !Subst).

%-----------------------------------------------------------------------------%

apply_rec_substitution(V @ variable(Var, _), Substitution, Term) :-
    ( map.search(Substitution, Var, Replacement) ->
        % Recursively apply the substitution to the replacement.
        apply_rec_substitution(Replacement, Substitution, Term)
    ;
        Term = V
    ).
apply_rec_substitution(functor(Name, Args0, Context), Substitution,
         functor(Name, Args, Context)) :-
    apply_rec_substitution_to_list(Args0, Substitution, Args).

apply_rec_substitution_to_list([], _Substitution, []).
apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
        [Term | Terms]) :-
    apply_rec_substitution(Term0, Substitution, Term),
    apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

apply_substitution(Term0, Substitution, Term) :-
    (
        Term0 = variable(Var, _),
        ( map.search(Substitution, Var, Replacement) ->
            Term = Replacement
        ;
            Term = Term0
        )
    ;
        Term0 = functor(Name, Args0, Context),
        apply_substitution_to_list(Args0, Substitution, Args),
        Term = functor(Name, Args, Context)
    ).

apply_substitution_to_list([], _Substitution, []).
apply_substitution_to_list([Term0 | Terms0], Substitution, [Term | Terms]) :-
    apply_substitution(Term0, Substitution, Term),
    apply_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

init_var_supply(var_supply(0)).

create_var(var(V), var_supply(V0), var_supply(V)) :-
    % We number variables using sequential numbers.
    V = V0 + 1.

%------------------------------------------------------------------------------%

var_id(var(V)) = V.

%-----------------------------------------------------------------------------%

:- instance enum(var(_)) where [
    to_int(X) = term.var_to_int(X),
    from_int(X) = term.unsafe_int_to_var(X)
].

var_to_int(var(Var), Var).

    % Cast an integer to a var(T), subverting the type-checking.
    %
:- func unsafe_int_to_var(int) = var(T).

unsafe_int_to_var(Var) = var(Var).

var_supply_max_var(var_supply(V)) = var(V).

var_supply_num_allocated(var_supply(V)) = V.

%-----------------------------------------------------------------------------%

relabel_variable(functor(Const, Terms0, Cont), OldVar, NewVar,
        functor(Const, Terms, Cont)) :-
    relabel_variables(Terms0, OldVar, NewVar, Terms).
relabel_variable(variable(Var0, Context),
        OldVar, NewVar, variable(Var, Context)) :-
    ( Var0 = OldVar ->
        Var = NewVar
    ;
        Var = Var0
    ).

relabel_variables([], _, _, []).
relabel_variables([Term0|Terms0], OldVar, NewVar, [Term|Terms]):-
    relabel_variable(Term0, OldVar, NewVar, Term),
    relabel_variables(Terms0, OldVar, NewVar, Terms).

apply_variable_renaming(functor(Const, Args0, Cont), Renaming,
        functor(Const, Args, Cont)) :-
    apply_variable_renaming_to_list(Args0, Renaming, Args).
apply_variable_renaming(variable(Var0, Context), Renaming,
        variable(Var, Context)) :-
    apply_variable_renaming_to_var(Renaming, Var0, Var).

apply_variable_renaming_to_list([], _, []).
apply_variable_renaming_to_list([Term0|Terms0], Renaming, [Term|Terms]) :-
    apply_variable_renaming(Term0, Renaming, Term),
    apply_variable_renaming_to_list(Terms0, Renaming, Terms).

apply_variable_renaming_to_var(Renaming, Var0, Var) :-
    ( map.search(Renaming, Var0, NewVar) ->
        Var = NewVar
    ;
        Var = Var0
    ).

apply_variable_renaming_to_vars(_Renaming, [], []).
apply_variable_renaming_to_vars(Renaming, [Var0 | Vars0], [Var | Vars]) :-
    apply_variable_renaming_to_var(Renaming, Var0, Var),
    apply_variable_renaming_to_vars(Renaming, Vars0, Vars).

%-----------------------------------------------------------------------------%

term_list_to_var_list([], []).
term_list_to_var_list([variable(Var, _) | Terms], [Var | Vars]) :-
    term_list_to_var_list(Terms, Vars).

var_list_to_term_list([], []).
var_list_to_term_list([Var | Vars], [variable(Var, context_init) | Terms]) :-
    var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

is_ground_in_bindings(variable(V, _), Bindings) :-
    map.search(Bindings, V, Binding),
    is_ground_in_bindings(Binding, Bindings).
is_ground_in_bindings(functor(_, Args, _), Bindings) :-
    is_ground_in_bindings_list(Args, Bindings).

:- pred is_ground_in_bindings_list(list(term(T))::in, substitution(T)::in)
    is semidet.

is_ground_in_bindings_list([], _Bindings).
is_ground_in_bindings_list([Term | Terms], Bindings) :-
    is_ground_in_bindings(Term, Bindings),
    is_ground_in_bindings_list(Terms, Bindings).

%-----------------------------------------------------------------------------%

is_ground(functor(_, Args, _)) :-
    is_ground_list(Args).

:- pred is_ground_list(list(term(T))::in) is semidet.

is_ground_list([]).
is_ground_list([Term | Terms]) :-
    is_ground(Term),
    is_ground_list(Terms).

%-----------------------------------------------------------------------------%

generic_term(_).

%-----------------------------------------------------------------------------%

coerce(A, B) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(A, B).

coerce_var(var(V), var(V)).

coerce_var_supply(var_supply(Supply), var_supply(Supply)).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

context_init = C :-
    context_init(C).

init_var_supply = VS :-
    init_var_supply(VS).

try_term_to_type(T) = TTTR :-
    try_term_to_type(T, TTTR).

det_term_to_type(T1) = T2 :-
    det_term_to_type(T1, T2).

type_to_term(T1) = T2 :-
    type_to_term(T1, T2).

univ_to_term(U) = T :-
    univ_to_term(U, T).

vars(T) = Vs :-
    vars(T, Vs).

vars_2(T, Vs1) = Vs2 :-
    vars_2(T, Vs1, Vs2).

vars_list(Ts) = Vs :-
    vars_list(Ts, Vs).

substitute(T1, V, T2) = T3 :-
    substitute(T1, V, T2, T3).

substitute_list(Ts1, V, T) = Ts2 :-
    substitute_list(Ts1, V, T, Ts2).

substitute_corresponding(Vs, T1s, T) = T2 :-
    substitute_corresponding(Vs, T1s, T, T2).

substitute_corresponding_list(Vs, Ts1, Ts2) = Ts3 :-
    substitute_corresponding_list(Vs, Ts1, Ts2, Ts3).

apply_rec_substitution(T1, S) = T2 :-
    apply_rec_substitution(T1, S, T2).

apply_rec_substitution_to_list(Ts1, S) = Ts2 :-
    apply_rec_substitution_to_list(Ts1, S, Ts2).

apply_substitution(T1, S) = T2 :-
    apply_substitution(T1, S, T2).

apply_substitution_to_list(Ts1, S) = Ts2 :-
    apply_substitution_to_list(Ts1, S, Ts2).

relabel_variable(T1, V1, V2) = T2 :-
    relabel_variable(T1, V1, V2, T2).

relabel_variables(Ts1, V1, V2) = Ts2 :-
    relabel_variables(Ts1, V1, V2, Ts2).

apply_variable_renaming(T1, M) = T2 :-
    apply_variable_renaming(T1, M, T2).

apply_variable_renaming_to_list(Ts1, M) = Ts2 :-
    apply_variable_renaming_to_list(Ts1, M, Ts2).

apply_variable_renaming_to_vars(M, Vs0) = Vs :-
    apply_variable_renaming_to_vars(M, Vs0, Vs).

apply_variable_renaming_to_var(M, V0) = V :-
    apply_variable_renaming_to_var(M, V0, V).

var_to_int(V) = N :-
    var_to_int(V, N).

context_line(C) = N :-
    context_line(C, N).

context_file(C) = S :-
    context_file(C, S).

context_init(S, N) = C :-
    context_init(S, N, C).

term_list_to_var_list(Ts) = Vs :-
    ( term_list_to_var_list(Ts, Vs0) ->
        Vs = Vs0
    ;
        error("term.term_list_to_var_list: not all vars")
    ).

var_list_to_term_list(Vs) = Ts :-
    var_list_to_term_list(Vs, Ts).

coerce(T1) = T2 :-
    coerce(T1, T2).

coerce_var(V1) = V2 :-
    coerce_var(V1, V2).

coerce_var_supply(VS1) = VS2 :-
    coerce_var_supply(VS1, VS2).
