%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Many of the predicates below exist in two or three forms.
%
% - The first form, whose name is of the form parse_<something>, always
%   succeeds; if it doesn't find the thing it is looking for, it returns
%   an error message.
% - The second form, whose name is of the form try_parse_<something>, exists
%   because building this error message is a waste of time if not finding
%   <something> is not in fact an error, which happens often. This second form
%   simply fails when it does not find <something>.
% - The third form exists in cases where our caller imposes further
%   restrictions on the form of the thing being looked for, such as a list
%   of arguments being empty.

:- module parse_tree.prog_io_sym_name.
:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.
:- import_module varset.

    % A SymNameAndArgs is one of
    %   Name(Args)
    %   Module.Name(Args)
    % (or if Args is empty, one of
    %   Name
    %   Module.Name)
    % where Module is a SymName. For backwards compatibility, we allow `__'
    % as an alternative to `.'.

    % parse_sym_name_and_args(Term, VarSet, ContextPieces, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % However, in case it does not succced, parse_sym_name_and_args
    % also takes as input Varset (from which the variables in Term are taken),
    % and a format_component list describing the context from which it was
    % called, e.g. "In clause head:".
    %
    % Note: parse_sym_name_and_args is intended for places where a symbol
    % is _used_, where no default module name exists for the sym_name.
    % For places where a symbol is _defined_, use the related predicate
    % parse_implicitly_qualified_sym_name_and_args.
    %
:- pred parse_sym_name_and_args(term(T)::in, varset::in,
    list(format_component)::in, maybe_functor(T)::out) is det.
:- pred try_parse_sym_name_and_args(term(T)::in,
    sym_name::out, list(term(T))::out) is semidet.
:- pred try_parse_sym_name_and_no_args(term(T)::in, sym_name::out)
    is semidet.

    % When given the first two argumeents Functor and FunctorArgs,
    % try_parse_sym_name_and_args_from_f_args will do does exactly the same as
    % what try_parse_sym_name_and_args would do when given the term
    % term.functor(Functor, FunctorArgs, _), but it does so without
    % requiring its caller to construct that term.
    %
:- pred try_parse_sym_name_and_args_from_f_args(const::in, list(term(T))::in,
    sym_name::out, list(term(T))::out) is semidet.

    % parse_implicitly_qualified_sym_name_and_args(ModuleName, Term,
    %   VarSet, ContextPieces, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % This predicate thus does almost the same job as the predicate
    % parse_sym_name_and_args above, the difference being that
    % if the sym_name is qualified, then we check whether it is qualified
    % with ModuleName, and if it isn't qualified, then we qualify it with
    % ModuleName (unless ModuleName is root_module_name). This is the
    % right thing to do for clause heads, which is the intended job of
    % parse_implicitly_qualified_sym_name_and_args.
    %
:- pred parse_implicitly_qualified_sym_name_and_args(module_name::in,
    term(T)::in, varset::in, list(format_component)::in, maybe_functor(T)::out)
    is det.
:- pred try_parse_implicitly_qualified_sym_name_and_args(module_name::in,
    term(T)::in, sym_name::out, list(term(T))::out) is semidet.
:- pred try_parse_implicitly_qualified_sym_name_and_no_args(module_name::in,
    term(T)::in, sym_name::out) is semidet.

    % A SymbolNameSpecifier is one of
    %   SymbolName
    %   SymbolName/Arity
    %       Matches only symbols of the specified arity.
    %
:- pred parse_symbol_name_specifier(varset::in, term::in,
    maybe1(sym_name_specifier)::out) is det.

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
:- pred try_parse_symbol_name(term(T)::in, sym_name::out) is semidet.

:- pred parse_implicitly_qualified_symbol_name(module_name::in, varset::in,
    term::in, maybe1(sym_name)::out) is det.

:- pred parse_implicitly_qualified_symbol_name_specifier(module_name::in,
    varset::in, term::in, maybe1(sym_name_specifier)::out) is det.

    % We use the empty module name ('') as the "root" module name; when adding
    % default module qualifiers in parse_implicitly_qualified_*,
    % if the default module is the root module then we don't add any qualifier.
    %
:- func root_module_name = module_name.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.mercury_to_mercury.

:- import_module int.

parse_sym_name_and_args(Term, VarSet, ContextPieces, MaybeSymNameAndArgs) :-
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

try_parse_sym_name_and_args(Term, SymName, Args) :-
    Term = term.functor(Functor, FunctorArgs, _TermContext),
    try_parse_sym_name_and_args_from_f_args(Functor, FunctorArgs,
        SymName, Args).

:- pragma inline(try_parse_sym_name_and_args_from_f_args/4).

try_parse_sym_name_and_args_from_f_args(Functor, FunctorArgs, SymName, Args) :-
    Functor = term.atom(FunctorName),
    (
        FunctorName = ".",
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    ->
        NameArgsTerm = term.functor(term.atom(Name), Args, _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    ;
        SymName = string_to_sym_name_sep(FunctorName, "__"),
        Args = FunctorArgs
    ).

try_parse_sym_name_and_no_args(Term, SymName) :-
    Term = term.functor(Functor, FunctorArgs, _TermContext),
    Functor = term.atom(FunctorName),
    (
        FunctorName = ".",
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    ->
        NameArgsTerm = term.functor(term.atom(Name), [], _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    ;
        FunctorArgs = [],
        SymName = string_to_sym_name_sep(FunctorName, "__")
    ).

%-----------------------------------------------------------------------------e

parse_implicitly_qualified_sym_name_and_args(DefaultModuleName, Term,
        VarSet, ContextPieces, MaybeSymNameAndArgs) :-
    parse_sym_name_and_args(Term, VarSet, ContextPieces, MaybeSymNameAndArgs0),
    (
        MaybeSymNameAndArgs0 = ok2(SymName, Args),
        (
            DefaultModuleName = root_module_name
        ->
            MaybeSymNameAndArgs = MaybeSymNameAndArgs0
        ;
            SymName = qualified(ModuleName, _),
            \+ match_sym_name(ModuleName, DefaultModuleName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), decl("module"),
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

try_parse_implicitly_qualified_sym_name_and_args(DefaultModuleName, Term,
        SymName, Args) :-
    try_parse_sym_name_and_args(Term, SymName0, Args),
    (
        DefaultModuleName = root_module_name
    ->
        SymName = SymName0
    ;
        SymName0 = qualified(ModuleName, _),
        \+ match_sym_name(ModuleName, DefaultModuleName)
    ->
        fail
    ;
        UnqualName = unqualify_name(SymName0),
        SymName = qualified(DefaultModuleName, UnqualName)
    ).

try_parse_implicitly_qualified_sym_name_and_no_args(DefaultModuleName, Term,
        SymName) :-
    try_parse_sym_name_and_no_args(Term, SymName0),
    (
        DefaultModuleName = root_module_name
    ->
        SymName = SymName0
    ;
        SymName0 = qualified(ModuleName, _),
        \+ match_sym_name(ModuleName, DefaultModuleName)
    ->
        fail
    ;
        UnqualName = unqualify_name(SymName0),
        SymName = qualified(DefaultModuleName, UnqualName)
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

try_parse_symbol_name(Term, SymName) :-
    (
        Term = term.functor(term.atom(FunctorName), [ModuleTerm, NameTerm],
            _TermContext),
        ( FunctorName = ":"
        ; FunctorName = "."
        )
    ->
        NameTerm = term.functor(term.atom(Name), [], _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    ;
        Term = term.functor(term.atom(Name), [], _),
        SymName = string_to_sym_name_sep(Name, "__")
    ).

%-----------------------------------------------------------------------------e

parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet, Term,
        MaybeSymName) :-
    parse_symbol_name(VarSet, Term, MaybeSymName0),
    (
        MaybeSymName0 = ok1(SymName),
        (
            DefaultModuleName = root_module_name
        ->
            MaybeSymName = MaybeSymName0
        ;
            SymName = qualified(ModuleName, _),
            \+ match_sym_name(ModuleName, DefaultModuleName)
        ->
            Pieces = [words("Error: module qualifier in definition"),
                words("does not match preceding"), decl("module"),
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
    parse_implicitly_qualified_symbol_name_specifier(root_module_name, VarSet,
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

root_module_name = unqualified("").

%-----------------------------------------------------------------------------e
:- end_module parse_tree.prog_io_sym_name.
%-----------------------------------------------------------------------------e
