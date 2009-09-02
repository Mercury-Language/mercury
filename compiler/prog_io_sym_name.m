%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_sym_name.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.
:- import_module varset.

% A QualifiedTerm is one of
%   Name(Args)
%   Module.Name(Args)
% (or if Args is empty, one of
%   Name
%   Module.Name)
% where Module is a SymName. For backwards compatibility, we allow `__'
% as an alternative to `.'.

    % Sym_name_and_args takes a term and returns a sym_name that is its
    % top function symbol, and a list of its argument terms. It fails
    % if the input is not valid syntax for a QualifiedTerm.
    %
:- pred parse_sym_name_and_args(term(T)::in, sym_name::out, list(term(T))::out)
    is semidet.

    % parse_qualified_term(Term, _ContainingTerm, VarSet, ContextPieces,
    %   Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % (parse_qualified_term thus does the same job as sym_name_and_args
    % if it succeeds.) However, in case it does not succced,
    % parse_qualified_term also takes as input Varset (from which the variables
    % in Term are taken), the term containing Term, and a format_component
    % list describing the context from which it was called, e.g.
    % "In clause head:". XXX Currently, _ContainingTerm isn't used;
    % maybe it should be deleted.
    %
    % Note: parse_qualified_term is used for places where a symbol is _used_,
    % where no default module name exists for the sym_name. For places
    % where a symbol is _defined_, use parse_implicitly_qualified_term.
    %
    % If you care only about the case where Result = ok2(SymName, Args),
    % use sym_name_and_args.
    %
:- pred parse_qualified_term(term(T)::in, term(T)::in, varset::in,
    list(format_component)::in, maybe_functor(T)::out) is det.

    % parse_implicitly_qualified_term(ModuleName, Term, _ContainingTerm,
    %   VarSet, ContextPieces, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % This predicate thus does almost the same job as the predicate
    % parse_implicitly_qualified_term above, the difference being that
    % that if the sym_name is qualified, then we check whether it is qualified
    % with ModuleName, and if it isn't qualified, then we qualify it with
    % Modulename (unless ModuleName is root_module_name). This is the
    % right thing to do for clause heads, which is the intended job of
    % parse_implicitly_qualified_term.
    %
:- pred parse_implicitly_qualified_term(module_name::in, term(T)::in,
    term(T)::in, varset::in, list(format_component)::in, maybe_functor(T)::out)
    is det.

    % A SymbolName is one of
    %   Name
    %       Matches symbols with the specified name in the
    %       current namespace.
    %   Module.Name
    %       Matches symbols with the specified name exported
    %       by the specified module (where Module is itself a SymbolName).
    %
    % We also allow the syntax `Module__Name' as an alternative
    % for `Module.Name'.
    %
:- pred parse_symbol_name(varset(T)::in, term(T)::in, maybe1(sym_name)::out)
    is det.

:- pred parse_implicitly_qualified_symbol_name(module_name::in, varset::in,
    term::in, maybe1(sym_name)::out) is det.

    % A SymbolNameSpecifier is one of
    %   SymbolName
    %   SymbolName/Arity
    %       Matches only symbols of the specified arity.
    %
:- pred parse_symbol_name_specifier(varset::in, term::in,
    maybe1(sym_name_specifier)::out) is det.

:- pred parse_implicitly_qualified_symbol_name_specifier(module_name::in,
    varset::in, term::in, maybe1(sym_name_specifier)::out) is det.

    % We use the empty module name ('') as the "root" module name; when adding
    % default module qualifiers in parse_implicitly_qualified_{term,symbol},
    % if the default module is the root module then we don't add any qualifier.
    %
:- pred root_module_name(module_name::out) is det.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.mercury_to_mercury.

:- import_module int.

parse_sym_name_and_args(Term, SymName, Args) :-
    % The values of VarSet and ContextPieces do not matter here, since
    % we succeed only if they aren't used.
    VarSet = varset.init,
    ContextPieces = [],
    parse_qualified_term(Term, Term, VarSet, ContextPieces,
        ok2(SymName, Args)).

parse_qualified_term(Term, _ContainingTerm, VarSet, ContextPieces,
        MaybeSymNameAndArgs) :-
    % XXX We should delete the _ContainingTerm argument.
    (
        Term = term.functor(Functor, FunctorArgs, TermContext),
        Functor = term.atom("."),
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    ->
        ( NameArgsTerm = term.functor(term.atom(Name), Args, _) ->
            varset.coerce(VarSet, GenericVarSet),
            parse_symbol_name(GenericVarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                MaybeSymNameAndArgs = ok2(qualified(Module, Name), Args)
            ;
                MaybeModule = error1(_),
                ModuleTermStr = describe_error_term(GenericVarSet, ModuleTerm),
                % XXX We should say "module name" OR "identifier", not both.
                Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                    words("Error: module name identifier expected before '.'"),
                    words("in qualified symbol name, not"),
                    words(ModuleTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeSymNameAndArgs = error2([Spec])
            )
        ;
            varset.coerce(VarSet, GenericVarSet),
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Error: identifier expected after '.'"),
                words("in qualified symbol name, not"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        )
    ;
        varset.coerce(VarSet, GenericVarSet),
        ( Term = term.functor(term.atom(Name), Args, _) ->
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymNameAndArgs = ok2(SymName, Args)
        ;
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                words("Error: atom expected at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        )
    ).

parse_implicitly_qualified_term(DefaultModuleName, Term, ContainingTerm,
        VarSet, ContextPieces, MaybeSymNameAndArgs) :-
    parse_qualified_term(Term, ContainingTerm, VarSet, ContextPieces,
        MaybeSymNameAndArgs0),
    (
        MaybeSymNameAndArgs0 = ok2(SymName, Args),
        (
            root_module_name(DefaultModuleName)
        ->
            MaybeSymNameAndArgs = MaybeSymNameAndArgs0
        ;
            SymName = qualified(ModuleName, _),
            \+ match_sym_name(ModuleName, DefaultModuleName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), quote(":- module"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameAndArgs = error2([Spec])
        ;
            UnqualName = unqualify_name(SymName),
            QualSymName = qualified(DefaultModuleName, UnqualName),
            MaybeSymNameAndArgs = ok2(QualSymName, Args)
        )
    ;
        MaybeSymNameAndArgs0 = error2(_),
        MaybeSymNameAndArgs = MaybeSymNameAndArgs0
    ).

%-----------------------------------------------------------------------------e

parse_symbol_name(VarSet, Term, MaybeSymName) :-
    (
        Term = term.functor(term.atom(FunctorName), [ModuleTerm, NameTerm],
            TermContext),
        ( FunctorName = ":"
        ; FunctorName = "."
        )
    ->
        ( NameTerm = term.functor(term.atom(Name), [], _) ->
            parse_symbol_name(VarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                MaybeSymName = ok1(qualified(Module, Name))
            ;
                MaybeModule = error1(_ModuleResultSpecs),
                % XXX We should say "module name" OR "identifier", not both.
                Pieces = [words("Error: module name identifier"),
                    words("expected before"), quote(FunctorName),
                    words("in qualified symbol name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                % XXX Should we include _ModuleResultSpecs?
                MaybeSymName = error1([Spec])
            )
        ;
            Pieces = [words("Error: identifier expected after"),
                quote(FunctorName), words("in qualified symbol name."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeSymName = error1([Spec])
        )
    ;
        ( Term = term.functor(term.atom(Name), [], _) ->
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymName = ok1(SymName)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: symbol name expected at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymName = error1([Spec])
        )
    ).

parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet, Term,
        MaybeSymName) :-
    parse_symbol_name(VarSet, Term, MaybeSymName0),
    (
        MaybeSymName0 = ok1(SymName),
        (
            root_module_name(DefaultModuleName)
        ->
            MaybeSymName = MaybeSymName0
        ;
            SymName = qualified(ModuleName, _),
            \+ match_sym_name(ModuleName, DefaultModuleName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), quote(":- module"),
                words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymName = error1([Spec])
        ;
            UnqualName = unqualify_name(SymName),
            MaybeSymName = ok1(qualified(DefaultModuleName, UnqualName))
        )
    ;
        MaybeSymName0 = error1(_),
        MaybeSymName = MaybeSymName0
    ).

%-----------------------------------------------------------------------------e

parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier) :-
    root_module_name(DefaultModule),
    parse_implicitly_qualified_symbol_name_specifier(DefaultModule, VarSet,
        Term, MaybeSymNameSpecifier).

parse_implicitly_qualified_symbol_name_specifier(DefaultModule, VarSet, Term,
        MaybeSymNameSpecifier) :-
    ( Term = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) ->
        ( ArityTerm = term.functor(term.integer(Arity), [], _) ->
            ( Arity >= 0 ->
                parse_implicitly_qualified_symbol_name(DefaultModule, VarSet,
                    NameTerm, MaybeName),
                (
                    MaybeName = error1(Specs),
                    MaybeSymNameSpecifier = error1(Specs)
                ;
                    MaybeName = ok1(Name),
                    MaybeSymNameSpecifier = ok1(name_arity(Name, Arity))
                )
            ;
                Pieces = [words("Error: arity in symbol name specifier"),
                    words("must be a non-negative integer."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                MaybeSymNameSpecifier = error1([Spec])
            )
        ;
            Pieces = [words("Error: arity in symbol name specifier"),
                words("must be an integer."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeSymNameSpecifier = error1([Spec])
        )
    ;
        parse_implicitly_qualified_symbol_name(DefaultModule, VarSet, Term,
            MaybeSymbolName),
        (
            MaybeSymbolName = error1(Specs),
            MaybeSymNameSpecifier = error1(Specs)
        ;
            MaybeSymbolName = ok1(SymbolName),
            MaybeSymNameSpecifier = ok1(name(SymbolName))
        )
    ).

%-----------------------------------------------------------------------------e

root_module_name(unqualified("")).

%-----------------------------------------------------------------------------e
