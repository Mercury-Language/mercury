%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: term.m.
% Main author: fjh.
% Stability: medium.

% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.
% Terms are polymorphic so that terms representing different kinds of
% thing can be made to be of different types so they don't get mixed up.

%-----------------------------------------------------------------------------%

:- module term.
:- interface.

:- import_module enum.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

:- type term(T)
    --->    term.functor(
                const,
                list(term(T)),
                term.context
            )
    ;       term.variable(
                var(T)
            ).

:- type const
    --->    term.atom(string)
    ;       term.integer(int)
    ;       term.string(string)
    ;       term.float(float).

:- type term.context
    --->    term.context(string, int).
            % file name, line number.

:- type var(T).
:- type var_supply(T).

:- type generic
    --->    generic.

:- type term    ==  term(generic).
:- type var     ==  var(generic).

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
:- func term.try_term_to_type(term(U)) = term_to_type_result(T, U).
:- pred term.try_term_to_type(term(U)::in, term_to_type_result(T, U)::out)
    is det.

:- type term_to_type_error(T)
    --->    type_error(
                term(T),
                type_desc.type_desc,
                term.context,
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
            term.context   % filename & line number
        ).

    % term_to_type(Term, Type) :- try_term_to_type(Term, ok(Type)).
    %
:- pred term.term_to_type(term(U)::in, T::out) is semidet.

    % Like term_to_type, but calls error/1 rather than failing.
    %
:- func term.det_term_to_type(term(_)) = T.
:- pred term.det_term_to_type(term(_)::in, T::out) is det.

    % Converts a value to a term representation of that value.
    %
:- func term.type_to_term(T) = term(_).
:- pred term.type_to_term(T::in, term(_)::out) is det.

    % Convert the value stored in the univ (as distinct from the univ itself)
    % to a term.
    %
:- func term.univ_to_term(univ) = term(_).
:- pred term.univ_to_term(univ::in, term(_)::out) is det.

%-----------------------------------------------------------------------------%

    % term.vars(Term, Vars):
    %
    % Vars is the list of variables contained in Term, in the order
    % obtained by traversing the term depth first, left-to-right.
    %
:- func term.vars(term(T)) = list(var(T)).
:- pred term.vars(term(T)::in, list(var(T))::out) is det.

    % As above, but with an accumulator.
    %
:- func term.vars_2(term(T), list(var(T))) = list(var(T)).
:- pred term.vars_2(term(T)::in, list(var(T))::in, list(var(T))::out) is det.

    % term.vars_list(TermList, Vars):
    %
    % Vars is the list of variables contained in TermList, in the order
    % obtained by traversing the list of terms depth-first, left-to-right.
    %
:- func term.vars_list(list(term(T))) = list(var(T)).
:- pred term.vars_list(list(term(T))::in, list(var(T))::out) is det.

    % term.contains_var(Term, Var):
    %
    % True if Term contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred term.contains_var(term(T), var(T)).
:- mode term.contains_var(in, in) is semidet.
:- mode term.contains_var(in, out) is nondet.

    % term.contains_var_list(TermList, Var):
    %
    % True if TermList contains Var. On backtracking returns all the variables
    % contained in Term.
    %
:- pred term.contains_var_list(list(term(T)), var(T)).
:- mode term.contains_var_list(in, in) is semidet.
:- mode term.contains_var_list(in, out) is nondet.

:- type substitution(T) == map(var(T), term(T)).
:- type substitution    == substitution(generic).

    % term.unify(Term1, Term2, Bindings0, Bindings):
    %
    % Unify (with occur check) two terms with respect to a set of bindings
    % and possibly update the set of bindings.
    %
:- pred term.unify(term(T)::in, term(T)::in, substitution(T)::in,
    substitution(T)::out) is semidet.

    % As above, but unify the corresponding elements of two lists of terms.
    % Fails if the lists are not of equal length.
    %
:- pred term.unify_list(list(term(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % term.unify(Term1, Term2, BoundVars, !Bindings):
    %
    % Unify (with occur check) two terms with respect to a set of bindings
    % and possibly update the set of bindings. Fails if any of the variables
    % in BoundVars would become bound by the unification.
    %
:- pred term.unify(term(T)::in, term(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

    % As above, but unify the corresponding elements of two lists of terms.
    % Fails if the lists are not of equal length.
    %
:- pred term.unify_list(list(term(T))::in, list(term(T))::in,
    list(var(T))::in, substitution(T)::in, substitution(T)::out) is semidet.

    % term.list_subsumes(Terms1, Terms2, Subst) succeeds iff the list
    % Terms1 subsumes (is more general than) Terms2, producing a substitution
    % which when applied to Terms1 will give Terms2.
    %
:- pred term.list_subsumes(list(term(T))::in, list(term(T))::in,
    substitution(T)::out) is semidet.

    % term.substitute(Term0, Var, Replacement, Term):
    %
    % Replace all occurrences of Var in Term0 with Replacement,
    % and return the result in Term.
    %
:- func term.substitute(term(T), var(T), term(T)) = term(T).
:- pred term.substitute(term(T)::in, var(T)::in, term(T)::in, term(T)::out)
    is det.

    % As above, except for a list of terms rather than a single term.
    %
:- func term.substitute_list(list(term(T)), var(T), term(T)) = list(term(T)).
:- pred term.substitute_list(list(term(T))::in, var(T)::in, term(T)::in,
    list(term(T))::out) is det.

    % term.substitute_corresponding(Vars, Repls, Term0, Term):
    %
    % Replace all occurrences of variables in Vars with the corresponding
    % term in Repls, and return the result in Term. If Vars contains
    % duplicates, or if Vars is not the same length as Repls, the behaviour
    % is undefined and probably harmful.
    %
:- func term.substitute_corresponding(list(var(T)), list(term(T)),
    term(T)) = term(T).
:- pred term.substitute_corresponding(list(var(T))::in, list(term(T))::in,
    term(T)::in, term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func term.substitute_corresponding_list(list(var(T)),
    list(term(T)), list(term(T))) = list(term(T)).
:- pred term.substitute_corresponding_list(list(var(T))::in,
    list(term(T))::in, list(term(T))::in, list(term(T))::out) is det.

    % term.apply_rec_substitution(Term0, Substitution, Term):
    %
    % Recursively apply substitution to Term0 until no more substitutions
    % can be applied, and then return the result in Term.
    %
:- func term.apply_rec_substitution(term(T), substitution(T)) = term(T).
:- pred term.apply_rec_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func term.apply_rec_substitution_to_list(list(term(T)),
    substitution(T)) = list(term(T)).
:- pred term.apply_rec_substitution_to_list(list(term(T))::in,
    substitution(T)::in, list(term(T))::out) is det.

    % term.apply_substitution(Term0, Substitution, Term):
    %
    % Apply substitution to Term0 and return the result in Term.
    %
:- func term.apply_substitution(term(T), substitution(T)) = term(T).
:- pred term.apply_substitution(term(T)::in, substitution(T)::in,
    term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    %
:- func term.apply_substitution_to_list(list(term(T)),
    substitution(T)) = list(term(T)).
:- pred term.apply_substitution_to_list(list(term(T))::in,
    substitution(T)::in, list(term(T))::out) is det.

    % term.occurs(Term0, Var, Substitution):
    % True iff Var occurs in the term resulting after applying Substitution
    % to Term0. Var variable must not be mapped by Substitution.
    %
:- pred term.occurs(term(T)::in, var(T)::in, substitution(T)::in) is semidet.

    % As above, except for a list of terms rather than a single term.
    %
:- pred term.occurs_list(list(term(T))::in, var(T)::in, substitution(T)::in)
    is semidet.

    % term.relabel_variable(Term0, OldVar, NewVar, Term):
    %
    % Replace all occurrences of OldVar in Term0 with NewVar and put the result
    % in Term.
    %
:- func term.relabel_variable(term(T), var(T), var(T)) = term(T).
:- pred term.relabel_variable(term(T)::in, var(T)::in, var(T)::in,
    term(T)::out) is det.

    % As above, except applies to a list of terms rather than a single term.
    % XXX the name of the predicate is misleading.
    %
:- func term.relabel_variables(list(term(T)), var(T), var(T)) = list(term(T)).
:- pred term.relabel_variables(list(term(T))::in, var(T)::in, var(T)::in,
    list(term(T))::out) is det.

    % Same as term.relabel_variable, except relabels multiple variables.
    % If a variable is not in the map, it is not replaced.
    %
:- func term.apply_variable_renaming(term(T), map(var(T), var(T))) = term(T).
:- pred term.apply_variable_renaming(term(T)::in, map(var(T), var(T))::in,
    term(T)::out) is det.

    % Applies term.apply_variable_renaming to a list of terms.
    %
:- func term.apply_variable_renaming_to_list(list(term(T)),
    map(var(T), var(T))) = list(term(T)).
:- pred term.apply_variable_renaming_to_list(list(term(T))::in,
    map(var(T), var(T))::in, list(term(T))::out) is det.

    % Applies term.apply_variable_renaming to a var.
    %
:- func term.apply_variable_renaming_to_var(map(var(T), var(T)),
    var(T)) = var(T).
:- pred term.apply_variable_renaming_to_var(map(var(T), var(T))::in,
    var(T)::in, var(T)::out) is det.

    % Applies term.apply_variable_renaming to a list of vars.
    %
:- func term.apply_variable_renaming_to_vars(map(var(T), var(T)),
    list(var(T))) = list(var(T)).
:- pred term.apply_variable_renaming_to_vars(map(var(T), var(T))::in,
    list(var(T))::in, list(var(T))::out) is det.

    % term.is_ground(Term, Bindings) is true iff no variables contained
    % in Term are non-ground in Bindings.
    %
:- pred term.is_ground(term(T)::in, substitution(T)::in) is semidet.

    % term.is_ground(Term) is true iff Term contains no variables.
    %
:- pred term.is_ground(term(T)::in) is semidet.

%-----------------------------------------------------------------------------%

    % To manage a supply of variables, use the following 2 predicates.
    % (We might want to give these a unique mode later.)

    % term.init_var_supply(VarSupply):
    %
    % Returns a fresh var_supply for producing fresh variables.
    %
:- func term.init_var_supply = var_supply(T).
:- pred term.init_var_supply(var_supply(T)).
:- mode term.init_var_supply(out) is det.
:- mode term.init_var_supply(in) is semidet. % implied

    % term.create_var(VarSupply0, Variable, VarSupply):
    % Create a fresh variable (var) and return the updated var_supply.
    %
:- pred term.create_var(var_supply(T), var(T), var_supply(T)).
:- mode term.create_var(in, out, out) is det.

    % term.var_id(Variable):
    % Returns a unique number associated with this variable w.r.t.
    % its originating var_supply.
    %
:- func term.var_id(var(T)) = int.

%-----------------------------------------------------------------------------%

    % from_int/1 should only be applied to integers returned by to_int/1.
    % This instance declaration is needed to allow sets of variables to be
    % represented using sparse_bitset.m.
:- instance enum(var(_)).

    % Convert a variable to an int. Different variables map to different ints.
    % Other than that, the mapping is unspecified.
    %
:- func term.var_to_int(var(T)) = int.
:- pred term.var_to_int(var(T)::in, int::out) is det.

%-----------------------------------------------------------------------------%

    % Given a term context, return the source line number.
    %
:- func term.context_line(term.context) = int.
:- pred term.context_line(term.context::in, int::out) is det.

    % Given a term context, return the source file.
    %
:- func term.context_file(term.context) = string.
:- pred term.context_file(term.context::in, string::out) is det.

    % Used to initialize the term context when reading in
    % (or otherwise constructing) a term.
    %
:- func term.context_init = term.context.
:- pred term.context_init(term.context::out) is det.
:- func term.context_init(string, int) = term.context.
:- pred term.context_init(string::in, int::in, term.context::out) is det.

    % Convert a list of terms which are all vars into a list of vars.
    % Abort (call error/1) if the list contains any non-variables.
    %
:- func term.term_list_to_var_list(list(term(T))) = list(var(T)).
:- pred term.term_list_to_var_list(list(term(T))::in, list(var(T))::out)
    is det.

    % Convert a list of terms which are all vars into a list of vars
    % (or vice versa).
    %
:- func term.var_list_to_term_list(list(var(T))) = list(term(T)).
:- pred term.var_list_to_term_list(list(var(T)), list(term(T))).
:- mode term.var_list_to_term_list(in, out) is det.
:- mode term.var_list_to_term_list(out, in) is semidet.

%-----------------------------------------------------------------------------%

    % term.generic_term(Term) is true iff `Term' is a term of type
    % `term' ie `term(generic)'. It is useful because in some instances
    % it doesn't matter what the type of a term is, and passing it to this
    % predicate will ground the type avoiding unbound type variable warnings.
    %
:- pred term.generic_term(term::in) is det.

    % Coerce a term of type `T' into a term of type `U'.
    %
:- func term.coerce(term(T)) = term(U).
:- pred term.coerce(term(T)::in, term(U)::out) is det.

    % Coerce a var of type `T' into a var of type `U'.
    %
:- func term.coerce_var(var(T)) = var(U).
:- pred term.coerce_var(var(T)::in, var(U)::out) is det.

    % Coerce a var_supply of type `T' into a var_supply of type `U'.
    %
:- func term.coerce_var_supply(var_supply(T)) = var_supply(U).
:- pred term.coerce_var_supply(var_supply(T)::in, var_supply(U)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%

:- interface.

% This is the same as term_to_type, except that an integer is allowed
% where a character is expected. This is needed by extras/aditi/aditi.m
% because Aditi does not have a builtin character type. This also allows
% an integer where a float is expected.

:- pred term.term_to_type_with_int_instead_of_char(term(U)::in, T::out)
    is semidet.

    % Returns the highest numbered variable returned from this var_supply.
    %
:- func term.var_supply_max_var(var_supply(T)) = var(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module construct.
:- import_module float.
:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

:- type var_supply(T)
    --->    var_supply(int).

:- type var(T)
    --->    var(int).

%-----------------------------------------------------------------------------%

term.term_to_type(Term, Val) :-
    term.try_term_to_type(Term, ok(Val)).

term.term_to_type_with_int_instead_of_char(Term, Val) :-
    IsAditiTuple = yes,
    term.try_term_to_type(IsAditiTuple, Term, ok(Val)).

term.try_term_to_type(Term, Result) :-
    IsAditiTuple = no,
    term.try_term_to_type(IsAditiTuple, Term, Result).

:- pred term.try_term_to_type(bool::in, term(U)::in,
    term_to_type_result(T, U)::out) is det.

term.try_term_to_type(IsAditiTuple, Term, Result) :-
    term.try_term_to_univ(IsAditiTuple, Term, type_desc.type_of(ValTypedVar),
        UnivResult),
    (
        UnivResult = ok(Univ),
        det_univ_to_type(Univ, Val),
        same_type(Val, ValTypedVar),
        Result = ok(Val)
    ;
        UnivResult = error(Error),
        Result = error(Error)
    ).

:- pred term.try_term_to_univ(bool::in, term(T)::in, type_desc.type_desc::in,
    term_to_type_result(univ, T)::out) is det.

term.try_term_to_univ(IsAditiTuple, Term, Type, Result) :-
    term.try_term_to_univ_2(IsAditiTuple, Term, Type, [], Result).

:- pred term.try_term_to_univ_2(bool::in, term(T)::in,
    type_desc.type_desc::in, term_to_type_context::in,
    term_to_type_result(univ, T)::out) is det.

term.try_term_to_univ_2(_, term.variable(Var), _Type, Context,
        error(mode_error(Var, Context))).
term.try_term_to_univ_2(IsAditiTuple, Term, Type, Context, Result) :-
    Term = term.functor(Functor, ArgTerms, TermContext),
    (
        type_desc.type_ctor_and_args(Type, TypeCtor, TypeArgs),
        term.term_to_univ_special_case(IsAditiTuple,
            type_desc.type_ctor_module_name(TypeCtor),
            type_desc.type_ctor_name(TypeCtor),
            TypeArgs, Term, Type, Context, SpecialCaseResult)
    ->
        Result = SpecialCaseResult
    ;
        Functor = term.atom(FunctorName),
        list.length(ArgTerms, Arity),
        find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes),
        term.term_list_to_univ_list(IsAditiTuple, ArgTerms,
            ArgTypes, Functor, 1, Context, TermContext, ArgsResult)
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

:- pred term.term_to_univ_special_case(bool::in, string::in, string::in,
    list(type_desc.type_desc)::in,
    term(T)::in(bound(term.functor(ground, ground, ground))),
    type_desc.type_desc::in, term_to_type_context::in,
    term_to_type_result(univ, T)::out) is semidet.

term.term_to_univ_special_case(IsAditiTuple, "builtin", "character", [],
        Term, _, _, ok(Univ)) :-
    (
        IsAditiTuple = no,
        Term = term.functor(term.atom(FunctorName), [], _),
        string.first_char(FunctorName, Char, "")
    ;
        IsAditiTuple = yes,
        Term = term.functor(term.integer(Int), [], _),
        char.to_int(Char, Int)
    ),
    type_to_univ(Char, Univ).
term.term_to_univ_special_case(_, "builtin", "int", [],
        Term, _, _, ok(Univ)) :-
    Term = term.functor(term.integer(Int), [], _),
    type_to_univ(Int, Univ).
term.term_to_univ_special_case(_, "builtin", "string", [],
        Term, _, _, ok(Univ)) :-
    Term = term.functor(term.string(String), [], _),
    type_to_univ(String, Univ).
term.term_to_univ_special_case(IsAditiTuple, "builtin", "float", [],
        Term, _, _, ok(Univ)) :-
    ( Term = term.functor(term.float(Float), [], _) ->
        type_to_univ(Float, Univ)
    ;
        IsAditiTuple = yes,
        Term = term.functor(term.integer(Int), [], _),
        Float = float.float(Int),
        type_to_univ(Float, Univ)
    ).
term.term_to_univ_special_case(IsAditiTuple, "array", "array", [ElemType],
        Term, _Type, PrevContext, Result) :-
    %
    % arrays are represented as terms of the form
    %   array([elem1, elem2, ...])
    %
    Term = term.functor(term.atom("array"), [ArgList], TermContext),

    % To convert such terms back to arrays, we first
    % convert the term representing the list of elements back to a list,
    % and then (if successful) we just call the array/1 function.
    %
    type_desc.has_type(Elem, ElemType),
    ListType = type_desc.type_of([Elem]),
    ArgContext = arg_context(term.atom("array"), 1, TermContext),
    NewContext = [ArgContext | PrevContext],
    term.try_term_to_univ_2(IsAditiTuple, ArgList, ListType, NewContext,
        ArgResult),
    (
        ArgResult = ok(ListUniv),
        type_desc.has_type(Elem2, ElemType),
        same_type(List, [Elem2]),
        det_univ_to_type(ListUniv, List),
        Array = array(List),
        Result = ok(univ(Array))
    ;
        ArgResult = error(Error),
        Result = error(Error)
    ).
term.term_to_univ_special_case(_, "builtin", "c_pointer", _, _, _, _, _) :-
    fail.
term.term_to_univ_special_case(_, "std_util", "univ", [],
        Term, _, _, Result) :-
    % Implementing this properly would require keeping a global table mapping
    % from type names to type_infos for all of the types in the program...
    % so for the moment, we only allow it for basic types.
    Term = term.functor(term.atom("univ"), [Arg], _),
    Arg = term.functor(term.atom(":"), [Value, Type], _),
    (
        Type = term.functor(term.atom("int"), [], _),
        Value = term.functor(term.integer(Int), [], _),
        Univ = univ(Int)
    ;
        Type = term.functor(term.atom("string"), [], _),
        Value = term.functor(term.string(String), [], _),
        Univ = univ(String)
    ;
        Type = term.functor(term.atom("float"), [], _),
        Value = term.functor(term.float(Float), [], _),
        Univ = univ(Float)
    ),
    % The result is a `univ', but it is also wrapped in a `univ'
    % like all the other results returned from this procedure.
    Result = ok(univ(Univ)).

term.term_to_univ_special_case(_, "std_util", "type_info", _, _, _, _, _) :-
    % Ditto.
    fail.

:- pred term.term_list_to_univ_list(bool::in, list(term(T))::in,
    list(type_desc.type_desc)::in, term.const::in, int::in,
    term_to_type_context::in, term.context::in,
    term_to_type_result(list(univ), T)::out) is semidet.

term.term_list_to_univ_list(_, [], [], _, _, _, _, ok([])).
term.term_list_to_univ_list(IsAditiTuple, [ArgTerm | ArgTerms],
        [Type | Types], Functor, ArgNum, PrevContext, TermContext, Result) :-
    ArgContext = arg_context(Functor, ArgNum, TermContext),
    NewContext = [ArgContext | PrevContext],
    term.try_term_to_univ_2(IsAditiTuple, ArgTerm, Type, NewContext,
        ArgResult),
    (
        ArgResult = ok(Arg),
        term.term_list_to_univ_list(IsAditiTuple, ArgTerms, Types, Functor,
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

:- pred term.find_functor(type_desc.type_desc::in, string::in, int::in,
    int::out, list(type_desc.type_desc)::out) is semidet.

term.find_functor(Type, Functor, Arity, FunctorNumber, ArgTypes) :-
    N = construct.num_functors(Type),
    term.find_functor_2(Type, Functor, Arity, N, FunctorNumber, ArgTypes).

:- pred term.find_functor_2(type_desc.type_desc::in, string::in, int::in,
    int::in, int::out, list(type_desc.type_desc)::out) is semidet.

term.find_functor_2(TypeInfo, Functor, Arity, Num, FunctorNumber, ArgTypes) :-
    Num >= 0,
    Num1 = Num - 1,
    ( std_util.get_functor(TypeInfo, Num1, Functor, Arity, ArgTypes1) ->
        ArgTypes = ArgTypes1,
        FunctorNumber = Num1
    ;
        term.find_functor_2(TypeInfo, Functor, Arity, Num1,
            FunctorNumber, ArgTypes)
    ).

term.det_term_to_type(Term, X) :-
    ( term.term_to_type(Term, X1) ->
        X = X1
    ; \+ term.is_ground(Term) ->
        error("term.det_term_to_type failed, because the term wasn't ground")
    ;
        Message = "term.det_term_to_type failed, due to a type error:\n"
            ++ "the term wasn't a valid term for type `"
            ++ type_desc.type_name(type_desc.type_of(X)) ++ "'",
        error(Message)
    ).

%-----------------------------------------------------------------------------%

term.type_to_term(Val, Term) :- type_to_univ(Val, Univ),
    term.univ_to_term(Univ, Term).

term.univ_to_term(Univ, Term) :-
    term.context_init(Context),
    Type = univ_type(Univ),
    % NU-Prolog barfs on `num_functors(Type) < 0'
    ( construct.num_functors(Type) = N, N < 0 ->
        (
            type_desc.type_ctor_and_args(Type, TypeCtor, TypeArgs),
            TypeName = type_desc.type_ctor_name(TypeCtor),
            ModuleName = type_desc.type_ctor_module_name(TypeCtor),
            term.univ_to_term_special_case(ModuleName, TypeName, TypeArgs,
                Univ, Context, SpecialCaseTerm)
        ->
            Term = SpecialCaseTerm
        ;
            Message = "term.type_to_term: unknown type `"
                ++ type_desc.type_name(univ_type(Univ)) ++ "'",
            error(Message)
        )
    ;
        deconstruct(univ_value(Univ), FunctorString, _FunctorArity,
            FunctorArgs),
        term.univ_list_to_term_list(FunctorArgs, TermArgs),
        Term = term.functor(term.atom(FunctorString), TermArgs, Context)
    ).

:- pred term.univ_to_term_special_case(string::in, string::in,
    list(type_desc.type_desc)::in, univ::in, term.context::in, term(T)::out)
    is semidet.

term.univ_to_term_special_case("builtin", "int", [], Univ, Context,
        term.functor(term.integer(Int), [], Context)) :-
    det_univ_to_type(Univ, Int).
term.univ_to_term_special_case("builtin", "float", [], Univ, Context,
        term.functor(term.float(Float), [], Context)) :-
    det_univ_to_type(Univ, Float).
term.univ_to_term_special_case("builtin", "character", [], Univ,
        Context, term.functor(term.atom(CharName), [], Context)) :-
    det_univ_to_type(Univ, Character),
    string.char_to_string(Character, CharName).
term.univ_to_term_special_case("builtin", "string", [], Univ, Context,
        term.functor(term.string(String), [], Context)) :-
    det_univ_to_type(Univ, String).
term.univ_to_term_special_case("std_util", "type_info", [], Univ, Context,
        term.functor(term.atom("type_info"), [Term], Context)) :-
    det_univ_to_type(Univ, TypeInfo),
    type_info_to_term(Context, TypeInfo, Term).
term.univ_to_term_special_case("std_util", "univ", [], Univ, Context, Term) :-
    det_univ_to_type(Univ, NestedUniv),
    Term = term.functor(term.atom("univ"),
        % XXX what operator should we use for type qualification?
        [term.functor(term.atom(":"),  % TYPE_QUAL_OP
            [ValueTerm, TypeTerm], Context)], Context),
    type_info_to_term(Context, univ_type(NestedUniv), TypeTerm),
    NestedUnivValue = univ_value(NestedUniv),
    term.type_to_term(NestedUnivValue, ValueTerm).

term.univ_to_term_special_case("array", "array", [ElemType], Univ, Context,
        Term) :-
    Term = term.functor(term.atom("array"), [ArgsTerm], Context),
    type_desc.has_type(Elem, ElemType),
    same_type(List, [Elem]),
    det_univ_to_type(Univ, Array),
    array.to_list(Array, List),
    term.type_to_term(List, ArgsTerm).

:- pred same_type(T::unused, T::unused) is det.

same_type(_, _).

:- pred term.univ_list_to_term_list(list(univ)::in, list(term(T))::out)
    is det.

term.univ_list_to_term_list([], []).
term.univ_list_to_term_list([Value|Values], [Term|Terms]) :-
    term.univ_to_term(Value, Term),
    term.univ_list_to_term_list(Values, Terms).

    % Given a type_info, return a term that represents the name of that type.
    %
:- pred type_info_to_term(term.context::in, type_desc.type_desc::in,
    term(T)::out) is det.

type_info_to_term(Context, TypeInfo, Term) :-
    type_desc.type_ctor_and_args(TypeInfo, TypeCtor, ArgTypes),
    TypeName = type_desc.type_ctor_name(TypeCtor),
    ModuleName = type_desc.type_ctor_name(TypeCtor),
    list.map(type_info_to_term(Context), ArgTypes, ArgTerms),

    ( ModuleName = "builtin" ->
        Term = term.functor(term.atom(TypeName), ArgTerms, Context)
    ;
        Arg1 = term.functor(term.atom(ModuleName), [], Context),
        Arg2 = term.functor(term.atom(TypeName), ArgTerms, Context),
        Term = term.functor(term.atom(":"), [Arg1, Arg2], Context)
    ).

%-----------------------------------------------------------------------------%

    % term.vars(Term, Vars) is true if Vars is the list of variables
    % contained in Term obtained by depth-first left-to-right traversal
    % in that order.

term.vars(Term, Vars) :-
    term.vars_2(Term, [], Vars).

term.vars_list(Terms, Vars) :-
    term.vars_2_list(Terms, [], Vars).

term.vars_2(term.variable(Var), !Vars) :-
    !:Vars = [Var | !.Vars].
term.vars_2(term.functor(_, Args, _), !Vars) :-
    term.vars_2_list(Args, !Vars).

:- pred term.vars_2_list(list(term(T))::in, list(var(T))::in,
    list(var(T))::out) is det.

term.vars_2_list([], !Vars).
term.vars_2_list([Term | Terms], !Vars) :-
    term.vars_2_list(Terms, !Vars),
    term.vars_2(Term, !Vars).

%-----------------------------------------------------------------------------%

term.contains_var(term.variable(Var), Var).
term.contains_var(term.functor(_, Args, _), Var) :-
    term.contains_var_list(Args, Var).

term.contains_var_list([Term | _], Var) :-
    term.contains_var(Term, Var).
term.contains_var_list([_ | Terms], Var) :-
    term.contains_var_list(Terms, Var).

%-----------------------------------------------------------------------------%

term.context_line(term.context(_, LineNumber), LineNumber).
term.context_file(term.context(FileName, _), FileName).
term.context_init(term.context("", 0)).
term.context_init(File, LineNumber, term.context(File, LineNumber)).

%-----------------------------------------------------------------------------%

term.unify(term.variable(X), term.variable(Y), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % Both X and Y already have bindings - just unify the terms
            % they are bound to.
            term.unify(BindingOfX, BindingOfY, !Bindings)
        ;
            % Y is a variable which hasn't been bound yet.
            term.apply_rec_substitution(BindingOfX, !.Bindings,
                SubstBindingOfX),
            ( SubstBindingOfX = term.variable(Y) ->
                true
            ;
                \+ term.occurs(SubstBindingOfX, Y, !.Bindings),
                map.set(!.Bindings, Y, SubstBindingOfX, !:Bindings)
            )
        )
    ;
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % X is a variable which hasn't been bound yet
            term.apply_rec_substitution(BindingOfY, !.Bindings,
                SubstBindingOfY),
            ( SubstBindingOfY = term.variable(X) ->
                true
            ;
                \+ term.occurs(SubstBindingOfY, X, !.Bindings),
                map.set(!.Bindings, X, SubstBindingOfY, !:Bindings)
            )
        ;
            % both X and Y are unbound variables -
            % bind one to the other
            ( X = Y ->
                true
            ;
                map.set(!.Bindings, X, term.variable(Y), !:Bindings)
            )
        )
    ).

term.unify(term.variable(X), term.functor(F, As, C), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        term.unify(BindingOfX, term.functor(F, As, C), !Bindings)
    ;
        \+ term.occurs_list(As, X, !.Bindings),
        map.set(!.Bindings, X, term.functor(F, As, C), !:Bindings)
    ).

term.unify(term.functor(F, As, C), term.variable(X), !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        term.unify(term.functor(F, As, C), BindingOfX, !Bindings)
    ;
        \+ term.occurs_list(As, X, !.Bindings),
        map.set(!.Bindings, X, term.functor(F, As, C), !:Bindings)
    ).

term.unify(term.functor(F, AsX, _), term.functor(F, AsY, _), !Bindings) :-
    term.unify_list(AsX, AsY, !Bindings).

term.unify_list([], [], !Bindings).
term.unify_list([X | Xs], [Y | Ys], !Bindings) :-
    term.unify(X, Y, !Bindings),
    term.unify_list(Xs, Ys, !Bindings).

term.unify(term.variable(X), term.variable(Y), BoundVars, !Bindings) :-
    ( list.member(Y, BoundVars) ->
        unify_bound_var(X, Y, BoundVars, !Bindings)
    ; list.member(X, BoundVars) ->
        unify_bound_var(Y, X, BoundVars, !Bindings)
    ; map.search(!.Bindings, X, BindingOfX) ->
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % Both X and Y already have bindings - just unify the
            % terms they are bound to.
            term.unify(BindingOfX, BindingOfY, BoundVars, !Bindings)
        ;
            term.apply_rec_substitution(BindingOfX, !.Bindings,
                SubstBindingOfX),
            % Y is a variable which hasn't been bound yet.
            ( SubstBindingOfX = term.variable(Y) ->
                true
            ;
                \+ term.occurs(SubstBindingOfX, Y, !.Bindings),
                svmap.det_insert(Y, SubstBindingOfX, !Bindings)
            )
        )
    ;
        ( map.search(!.Bindings, Y, BindingOfY) ->
            term.apply_rec_substitution(BindingOfY, !.Bindings,
                SubstBindingOfY),
            % X is a variable which hasn't been bound yet.
            ( SubstBindingOfY = term.variable(X) ->
                true
            ;
                \+ term.occurs(SubstBindingOfY, X, !.Bindings),
                svmap.det_insert(X, SubstBindingOfY, !Bindings)
            )
        ;
            % Both X and Y are unbound variables - bind one to the other.
            ( X = Y ->
                true
            ;
                svmap.det_insert(X, term.variable(Y), !Bindings)
            )
        )
    ).

term.unify(term.variable(X), term.functor(F, As, C), BoundVars,
        !Bindings) :-
    ( map.search(!.Bindings, X, BindingOfX) ->
        term.unify(BindingOfX, term.functor(F, As, C), BoundVars, !Bindings)
    ;
        \+ term.occurs_list(As, X, !.Bindings),
        \+ list.member(X, BoundVars),
        svmap.det_insert(X, term.functor(F, As, C), !Bindings)
    ).

term.unify(term.functor(F, As, C), term.variable(X), BoundVars,
        !Bindings) :-
    (
        map.search(!.Bindings, X, BindingOfX)
    ->
        term.unify(term.functor(F, As, C), BindingOfX, BoundVars, !Bindings)
    ;
        \+ term.occurs_list(As, X, !.Bindings),
        \+ list.member(X, BoundVars),
        svmap.det_insert(X, term.functor(F, As, C), !Bindings)
    ).

term.unify(term.functor(FX, AsX, _CX), term.functor(FY, AsY, _CY),
        BoundVars, !Bindings) :-
    list.length(AsX, ArityX),
    list.length(AsY, ArityY),
    (
        FX = FY,
        ArityX = ArityY
    ->
        term.unify_list(AsX, AsY, BoundVars, !Bindings)
    ;
        fail
    ).

term.unify_list([], [], _, !Bindings).
term.unify_list([X | Xs], [Y | Ys], BoundVars, !Bindings) :-
    term.unify(X, Y, BoundVars, !Bindings),
    term.unify_list(Xs, Ys, BoundVars, !Bindings).

:- pred unify_bound_var(var(T)::in, var(T)::in, list(var(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

unify_bound_var(Var, BoundVar, BoundVars, !Bindings) :-
    ( map.search(!.Bindings, Var, BindingOfVar) ->
        BindingOfVar = term.variable(Var2),
        unify_bound_var(Var2, BoundVar, BoundVars, !Bindings)
    ;
        ( Var = BoundVar ->
            true
        ;
            \+ list.member(Var, BoundVars),
            svmap.det_insert(Var, term.variable(BoundVar), !Bindings)
        )
    ).

term.list_subsumes(Terms1, Terms2, Subst) :-
    % Terms1 subsumes Terms2 iff Terms1 can be unified with Terms2
    % without binding any of the variables in Terms2.
    term.vars_list(Terms2, Terms2Vars),
    map.init(Subst0),
    term.unify_list(Terms1, Terms2, Terms2Vars, Subst0, Subst).

%-----------------------------------------------------------------------------%

term.occurs(term.variable(X), Y, Bindings) :-
    ( X = Y ->
        true
    ;
        map.search(Bindings, X, BindingOfX),
        term.occurs(BindingOfX, Y, Bindings)
    ).
term.occurs(term.functor(_F, As, _), Y, Bindings) :-
    term.occurs_list(As, Y, Bindings).

term.occurs_list([Term | Terms], Y, Bindings) :-
    ( term.occurs(Term, Y, Bindings) ->
        true
    ;
        term.occurs_list(Terms, Y, Bindings)
    ).

%-----------------------------------------------------------------------------%

term.substitute(term.variable(Var), SearchVar, Replacement, Term) :-
    ( Var = SearchVar ->
        Term = Replacement
    ;
        Term = term.variable(Var)
    ).
term.substitute(term.functor(Name, Args0, Context), Var, Replacement,
         term.functor(Name, Args, Context)) :-
    term.substitute_list(Args0, Var, Replacement, Args).

term.substitute_list([], _Var, _Replacement, []).
term.substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
    term.substitute(Term0, Var, Replacement, Term),
    term.substitute_list(Terms0, Var, Replacement, Terms).

term.substitute_corresponding(Ss, Rs, Term0, Term) :-
    map.init(Subst0),
    ( term.substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
        term.apply_substitution(Term0, Subst, Term)
    ;
        error("term.substitute_corresponding: different length lists")
    ).

term.substitute_corresponding_list(Ss, Rs, TermList0, TermList) :-
    map.init(Subst0),
    ( term.substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
        term.apply_substitution_to_list(TermList0, Subst, TermList)
    ;
        error("term.substitute_corresponding_list: different length lists")
    ).

:- pred term.substitute_corresponding_2(list(var(T))::in, list(term(T))::in,
    substitution(T)::in, substitution(T)::out) is semidet.

term.substitute_corresponding_2([], [], !Subst).
term.substitute_corresponding_2([S | Ss], [R | Rs], !Subst) :-
    map.set(!.Subst, S, R, !:Subst),
    term.substitute_corresponding_2(Ss, Rs, !Subst).

%-----------------------------------------------------------------------------%

term.apply_rec_substitution(term.variable(Var), Substitution, Term) :-
    ( map.search(Substitution, Var, Replacement) ->
        % Recursively apply the substition to the replacement.
        term.apply_rec_substitution(Replacement, Substitution, Term)
    ;
        Term = term.variable(Var)
    ).
term.apply_rec_substitution(term.functor(Name, Args0, Context), Substitution,
         term.functor(Name, Args, Context)) :-
    term.apply_rec_substitution_to_list(Args0, Substitution, Args).

term.apply_rec_substitution_to_list([], _Substitution, []).
term.apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
        [Term | Terms]) :-
    term.apply_rec_substitution(Term0, Substitution, Term),
    term.apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term.apply_substitution(term.variable(Var), Substitution, Term) :-
    ( map.search(Substitution, Var, Replacement) ->
        Term = Replacement
    ;
        Term = term.variable(Var)
    ).
term.apply_substitution(term.functor(Name, Args0, Context), Substitution,
         term.functor(Name, Args, Context)) :-
    term.apply_substitution_to_list(Args0, Substitution, Args).

term.apply_substitution_to_list([], _Substitution, []).
term.apply_substitution_to_list([Term0 | Terms0], Substitution,
        [Term | Terms]) :-
    term.apply_substitution(Term0, Substitution, Term),
    term.apply_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term.init_var_supply(var_supply(0)).

term.create_var(var_supply(V0), var(V), var_supply(V)) :-
    % We number variables using sequential numbers,
    V = V0 + 1.

%------------------------------------------------------------------------------%

term.var_id(var(V)) = V.

%-----------------------------------------------------------------------------%

:- instance enum(var(_)) where [
    to_int(X) = term.var_to_int(X),
    from_int(X) = term.unsafe_int_to_var(X)
].

term.var_to_int(var(Var), Var).

    % Cast an integer to a var(T), subverting the type-checking.
    %
:- func unsafe_int_to_var(int) = var(T).

term.unsafe_int_to_var(Var) = var(Var).

term.var_supply_max_var(var_supply(V)) = var(V).

%-----------------------------------------------------------------------------%

term.relabel_variable(term.functor(Const, Terms0, Cont), OldVar, NewVar,
        term.functor(Const, Terms, Cont)) :-
    term.relabel_variables(Terms0, OldVar, NewVar, Terms).
term.relabel_variable(term.variable(Var0), OldVar, NewVar,
        term.variable(Var)) :-
    ( Var0 = OldVar ->
        Var = NewVar
    ;
        Var = Var0
    ).

term.relabel_variables([], _, _, []).
term.relabel_variables([Term0|Terms0], OldVar, NewVar, [Term|Terms]):-
    term.relabel_variable(Term0, OldVar, NewVar, Term),
    term.relabel_variables(Terms0, OldVar, NewVar, Terms).

term.apply_variable_renaming(term.functor(Const, Args0, Cont), Renaming,
        term.functor(Const, Args, Cont)) :-
    term.apply_variable_renaming_to_list(Args0, Renaming, Args).
term.apply_variable_renaming(term.variable(Var0), Renaming,
        term.variable(Var)) :-
    term.apply_variable_renaming_to_var(Renaming, Var0, Var).

term.apply_variable_renaming_to_list([], _, []).
term.apply_variable_renaming_to_list([Term0|Terms0], Renaming, [Term|Terms]) :-
    term.apply_variable_renaming(Term0, Renaming, Term),
    term.apply_variable_renaming_to_list(Terms0, Renaming, Terms).

term.apply_variable_renaming_to_var(Renaming, Var0, Var) :-
    ( map.search(Renaming, Var0, NewVar) ->
        Var = NewVar
    ;
        Var = Var0
    ).

term.apply_variable_renaming_to_vars(_Renaming, [], []).
term.apply_variable_renaming_to_vars(Renaming, [Var0 | Vars0],
        [Var | Vars]) :-
    term.apply_variable_renaming_to_var(Renaming, Var0, Var),
    term.apply_variable_renaming_to_vars(Renaming, Vars0, Vars).

%-----------------------------------------------------------------------------%

term.term_list_to_var_list(Terms, Vars) :-
    ( term.var_list_to_term_list(Vars0, Terms) ->
        Vars = Vars0
    ;
        error("term.term_list_to_var_list")
    ).

term.var_list_to_term_list([], []).
term.var_list_to_term_list([Var | Vars], [term.variable(Var) | Terms]) :-
    term.var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

term.is_ground(term.variable(V), Bindings) :-
    map.search(Bindings, V, Binding),
    term.is_ground(Binding, Bindings).
term.is_ground(term.functor(_, Args, _), Bindings) :-
    term.is_ground_2(Args, Bindings).

:- pred term.is_ground_2(list(term(T))::in, substitution(T)::in) is semidet.

term.is_ground_2([], _Bindings).
term.is_ground_2([Term | Terms], Bindings) :-
    term.is_ground(Term, Bindings),
    term.is_ground_2(Terms, Bindings).

%-----------------------------------------------------------------------------%

term.is_ground(term.functor(_, Args, _)) :-
    term.is_ground_2(Args).

:- pred term.is_ground_2(list(term(T))::in) is semidet.

term.is_ground_2([]).
term.is_ground_2([Term | Terms]) :-
    term.is_ground(Term),
    term.is_ground_2(Terms).

%-----------------------------------------------------------------------------%

term.generic_term(_).

%-----------------------------------------------------------------------------%

term.coerce(A, B) :-
    % Normally calls to this predicate should only be generated by the
    % compiler, but type coercion by copying was taking about 3% of the
    % compiler's runtime.
    private_builtin.unsafe_type_cast(A, B).

term.coerce_var(var(V), var(V)).

term.coerce_var_supply(var_supply(Supply), var_supply(Supply)).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

term.context_init = C :-
    term.context_init(C).

term.init_var_supply = VS :-
    term.init_var_supply(VS).

term.try_term_to_type(T) = TTTR :-
    term.try_term_to_type(T, TTTR).

term.det_term_to_type(T1) = T2 :-
    term.det_term_to_type(T1, T2).

term.type_to_term(T1) = T2 :-
    term.type_to_term(T1, T2).

term.univ_to_term(U) = T :-
    term.univ_to_term(U, T).

term.vars(T) = Vs :-
    term.vars(T, Vs).

term.vars_2(T, Vs1) = Vs2 :-
    term.vars_2(T, Vs1, Vs2).

term.vars_list(Ts) = Vs :-
    term.vars_list(Ts, Vs).

term.substitute(T1, V, T2) = T3 :-
    term.substitute(T1, V, T2, T3).

term.substitute_list(Ts1, V, T) = Ts2 :-
    term.substitute_list(Ts1, V, T, Ts2).

term.substitute_corresponding(Vs, T1s, T) = T2 :-
    term.substitute_corresponding(Vs, T1s, T, T2).

term.substitute_corresponding_list(Vs, Ts1, Ts2) = Ts3 :-
    term.substitute_corresponding_list(Vs, Ts1, Ts2, Ts3).

term.apply_rec_substitution(T1, S) = T2 :-
    term.apply_rec_substitution(T1, S, T2).

term.apply_rec_substitution_to_list(Ts1, S) = Ts2 :-
    term.apply_rec_substitution_to_list(Ts1, S, Ts2).

term.apply_substitution(T1, S) = T2 :-
    term.apply_substitution(T1, S, T2).

term.apply_substitution_to_list(Ts1, S) = Ts2 :-
    term.apply_substitution_to_list(Ts1, S, Ts2).

term.relabel_variable(T1, V1, V2) = T2 :-
    term.relabel_variable(T1, V1, V2, T2).

term.relabel_variables(Ts1, V1, V2) = Ts2 :-
    term.relabel_variables(Ts1, V1, V2, Ts2).

term.apply_variable_renaming(T1, M) = T2 :-
    term.apply_variable_renaming(T1, M, T2).

term.apply_variable_renaming_to_list(Ts1, M) = Ts2 :-
    term.apply_variable_renaming_to_list(Ts1, M, Ts2).

term.apply_variable_renaming_to_vars(M, Vs0) = Vs :-
    term.apply_variable_renaming_to_vars(M, Vs0, Vs).

term.apply_variable_renaming_to_var(M, V0) = V :-
    term.apply_variable_renaming_to_var(M, V0, V).

term.var_to_int(V) = N :-
    term.var_to_int(V, N).

term.context_line(C) = N :-
    term.context_line(C, N).

term.context_file(C) = S :-
    term.context_file(C, S).

term.context_init(S, N) = C :-
    term.context_init(S, N, C).

term.term_list_to_var_list(Ts) = Vs :-
    term.term_list_to_var_list(Ts, Vs).

term.var_list_to_term_list(Vs) = Ts :-
    term.var_list_to_term_list(Vs, Ts).

term.coerce(T1) = T2 :-
    term.coerce(T1, T2).

term.coerce_var(V1) = V2 :-
    term.coerce_var(V1, V2).

term.coerce_var_supply(VS1) = VS2 :-
    term.coerce_var_supply(VS1, VS2).
