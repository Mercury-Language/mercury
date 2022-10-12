%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%
%-----------------------------------------------------------------------------e

:- module parse_tree.parse_sym_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

:- type maybe_functor    ==  maybe_functor(generic).
:- type maybe_functor(T) ==  maybe2(sym_name, list(term(T))).

    % A SymNameAndArgs is one of
    %   Name(Args)
    %   Module.Name(Args)
    % (or if Args is empty, one of
    %   Name
    %   Module.Name)
    % where Module is a SymName. For backwards compatibility, we allow `__'
    % as an alternative to `.'.

    % parse_sym_name_and_args(VarSet, ContextPieces, Term, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % However, in case it does not succeed, parse_sym_name_and_args
    % also takes as input Varset (from which the variables in Term are taken),
    % and a format_piece list describing the context from which it was
    % called, e.g. "In clause head:".
    %
    % Note: parse_sym_name_and_args is intended for places where a symbol
    % is _used_, where no default module name exists for the sym_name.
    % For places where a symbol is _defined_, use the related predicate
    % parse_implicitly_qualified_sym_name_and_args.
    %
:- pred parse_sym_name_and_args(varset::in, cord(format_piece)::in,
    term(T)::in, maybe_functor(T)::out) is det.
:- pred try_parse_sym_name_and_args(term(T)::in,
    sym_name::out, list(term(T))::out) is semidet.

    % When given the first two arguments Functor and FunctorArgs,
    % try_parse_sym_name_and_args_from_f_args will do exactly the same as
    % what try_parse_sym_name_and_args would do when given the term
    % term.functor(Functor, FunctorArgs, _), but it does so without
    % requiring its caller to construct that term.
    %
:- pred try_parse_sym_name_and_args_from_f_args(const::in, list(term(T))::in,
    sym_name::out, list(term(T))::out) is semidet.

    % Versions of parse_sym_name_and_args and try_parse_sym_name_and_args
    % that require the given term to have no arguments.
    %
:- pred parse_sym_name_and_no_args(varset::in, cord(format_piece)::in,
    term(T)::in, maybe1(sym_name)::out) is det.
:- pred try_parse_sym_name_and_no_args(term(T)::in, sym_name::out) is semidet.

    % parse_implicitly_qualified_sym_name_and_args(DefaultModuleName,
    %   VarSet, ContextPieces, Term, Result):
    %
    % Parse Term into a sym_name that is its top function symbol and a
    % list of its argument terms, and if successful return them in Result.
    % This predicate thus does almost the same job as the predicate
    % parse_sym_name_and_args above, the difference being that
    % if the sym_name is qualified, then we check whether it is qualified
    % with (possibly a part of) DefaultModuleName, and if it isn't qualified,
    % then we qualify it with DefaultModuleName . This is the right thing
    % to do for clause heads, which is the intended job of
    % parse_implicitly_qualified_sym_name_and_args.
    %
:- pred parse_implicitly_qualified_sym_name_and_args(module_name::in,
    varset::in, cord(format_piece)::in, term(T)::in,
    maybe_functor(T)::out) is det.
:- pred parse_implicitly_qualified_sym_name_and_no_args(module_name::in,
    varset::in, cord(format_piece)::in, term(T)::in,
    maybe1(sym_name)::out) is det.

    % These predicates do just the "implicitly qualifying" part of the
    % job of the predicates above.
    %
:- pred implicitly_qualify_sym_name(module_name::in, term(T)::in,
    sym_name::in, maybe1(sym_name)::out) is det.
:- pred try_to_implicitly_qualify_sym_name(module_name::in,
    sym_name::in, sym_name::out) is semidet.

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

:- type sym_name_specifier
    --->    sym_name_specifier_name(sym_name)
    ;       sym_name_specifier_name_arity(sym_name, user_arity).

    % A SymbolNameSpecifier is one of
    %   SymbolName
    %   SymbolName/Arity
    %       Matches only symbols of the specified arity.
    %
:- pred parse_symbol_name_specifier(varset::in, term::in,
    maybe1(sym_name_specifier)::out) is det.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.parse_tree_out_term.

:- import_module int.
:- import_module term_int.

parse_sym_name_and_args(VarSet, ContextPieces, Term, MaybeSymNameAndArgs) :-
    ( if
        Term = term.functor(Functor, FunctorArgs, TermContext),
        Functor = term.atom("."),
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    then
        ( if NameArgsTerm = term.functor(term.atom(Name), Args, _) then
            varset.coerce(VarSet, GenericVarSet),
            parse_symbol_name(GenericVarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                MaybeSymNameAndArgs = ok2(qualified(Module, Name), Args)
            ;
                MaybeModule = error1(_),
                ModuleTermStr = describe_error_term(GenericVarSet, ModuleTerm),
                Pieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first,
                    words("Error: expected module name before '.'"),
                    words("in qualified symbol name, got"),
                    words(ModuleTermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeSymNameAndArgs = error2([Spec])
            )
        else
            varset.coerce(VarSet, GenericVarSet),
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected identifier after '.'"),
                words("in qualified symbol name, got"),
                words(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeSymNameAndArgs = error2([Spec])
        )
    else
        varset.coerce(VarSet, GenericVarSet),
        ( if Term = term.functor(term.atom(Name), Args, _) then
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymNameAndArgs = ok2(SymName, Args)
        else
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected a symbol name, got"),
                quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeSymNameAndArgs = error2([Spec])
        )
    ).

try_parse_sym_name_and_args(Term, SymName, Args) :-
    Term = term.functor(Functor, FunctorArgs, _TermContext),
    try_parse_sym_name_and_args_from_f_args(Functor, FunctorArgs,
        SymName, Args).

:- pragma inline(pred(try_parse_sym_name_and_args_from_f_args/4)).

try_parse_sym_name_and_args_from_f_args(Functor, FunctorArgs, SymName, Args) :-
    Functor = term.atom(FunctorName),
    ( if
        FunctorName = ".",
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    then
        NameArgsTerm = term.functor(term.atom(Name), Args, _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    else
        SymName = string_to_sym_name_sep(FunctorName, "__"),
        Args = FunctorArgs
    ).

parse_sym_name_and_no_args(VarSet, ContextPieces, Term, MaybeSymName) :-
    ( if
        Term = term.functor(Functor, FunctorArgs, TermContext),
        Functor = term.atom("."),
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    then
        ( if NameArgsTerm = term.functor(term.atom(Name), Args, _) then
            varset.coerce(VarSet, GenericVarSet),
            parse_symbol_name(GenericVarSet, ModuleTerm, MaybeModule),
            (
                MaybeModule = ok1(Module),
                SymName = qualified(Module, Name),
                insist_on_no_args(ContextPieces, NameArgsTerm, SymName,
                    Args, MaybeSymName)
            ;
                MaybeModule = error1(_),
                ModuleTermStr = describe_error_term(GenericVarSet, ModuleTerm),
                Pieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first,
                    words("Error: expected module name expected before '.'"),
                    words("in qualified symbol name, got"),
                    words(ModuleTermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                MaybeSymName = error1([Spec])
            )
        else
            varset.coerce(VarSet, GenericVarSet),
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected identifier after '.'"),
                words("in qualified symbol name, got"),
                words(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeSymName = error1([Spec])
        )
    else
        varset.coerce(VarSet, GenericVarSet),
        ( if Term = term.functor(term.atom(Name), Args, _) then
            SymName = string_to_sym_name_sep(Name, "__"),
            insist_on_no_args(ContextPieces, Term, SymName, Args, MaybeSymName)
        else
            TermStr = describe_error_term(GenericVarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: atom expected at"),
                words(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeSymName = error1([Spec])
        )
    ).

:- pred insist_on_no_args(cord(format_piece)::in, term(T)::in,
    sym_name::in, list(term(T))::in, maybe1(sym_name)::out) is det.

insist_on_no_args(ContextPieces, Term, SymName, Args, MaybeSymName) :-
    (
        Args = [],
        MaybeSymName = ok1(SymName)
    ;
        Args = [_ | _],
        Name = sym_name_to_string(SymName),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error:"), quote(Name), words("has arguments,"),
            words("expected none."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeSymName = error1([Spec])
    ).

try_parse_sym_name_and_no_args(Term, SymName) :-
    Term = term.functor(Functor, FunctorArgs, _TermContext),
    Functor = term.atom(FunctorName),
    ( if
        FunctorName = ".",
        FunctorArgs = [ModuleTerm, NameArgsTerm]
    then
        NameArgsTerm = term.functor(term.atom(Name), [], _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    else
        FunctorArgs = [],
        SymName = string_to_sym_name_sep(FunctorName, "__")
    ).

%-----------------------------------------------------------------------------e

parse_implicitly_qualified_sym_name_and_args(DefaultModuleName, VarSet,
        ContextPieces, Term, MaybeSymNameAndArgs) :-
    parse_sym_name_and_args(VarSet, ContextPieces, Term, MaybeSymNameAndArgs0),
    (
        MaybeSymNameAndArgs0 = ok2(SymName0, Args),
        implicitly_qualify_sym_name_and_args(DefaultModuleName, Term,
            SymName0, Args, MaybeSymNameAndArgs)
    ;
        MaybeSymNameAndArgs0 = error2(_),
        MaybeSymNameAndArgs = MaybeSymNameAndArgs0
    ).

parse_implicitly_qualified_sym_name_and_no_args(DefaultModuleName, VarSet,
        ContextPieces, Term, MaybeSymName) :-
    parse_sym_name_and_args(VarSet, ContextPieces, Term, MaybeSymNameAndArgs0),
    (
        MaybeSymNameAndArgs0 = ok2(SymName0, Args0),
        implicitly_qualify_sym_name(DefaultModuleName, Term,
            SymName0, MaybeSymName1),
        (
            Args0 = [],
            ArgSpecs = []
        ;
            Args0 = [_ | _],
            ArgPieces = [words("Error: did not expect"),
                qual_sym_name(SymName0), words("to have any arguments."), nl],
            ArgSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), ArgPieces),
            ArgSpecs = [ArgSpec]
        ),
        ( if
            MaybeSymName1 = ok1(SymName),
            ArgSpecs = []
        then
            MaybeSymName = ok1(SymName)
        else
            Specs = get_any_errors1(MaybeSymName1) ++ ArgSpecs,
            MaybeSymName = error1(Specs)
        )
    ;
        MaybeSymNameAndArgs0 = error2(Specs),
        MaybeSymName = error1(Specs)
    ).

%-----------------------------------------------------------------------------e

:- pred implicitly_qualify_sym_name_and_args(module_name::in, term(T)::in,
    sym_name::in, list(term(T))::in, maybe2(sym_name, list(term(T)))::out)
    is det.

implicitly_qualify_sym_name_and_args(DefaultModuleName, Term, SymName0, Args,
        MaybeSymNameAndArgs) :-
    ( if
        try_to_implicitly_qualify_sym_name(DefaultModuleName, SymName0,
            SymName)
    then
        MaybeSymNameAndArgs = ok2(SymName, Args)
    else
        Pieces = [words("Error: the module qualifier in"),
            qual_sym_name(SymName0), words("does not match"),
            words("the preceding"), decl("module"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeSymNameAndArgs = error2([Spec])
    ).

implicitly_qualify_sym_name(DefaultModuleName, Term, SymName0, MaybeSymName) :-
    ( if
        try_to_implicitly_qualify_sym_name(DefaultModuleName, SymName0,
            SymName)
    then
        MaybeSymName = ok1(SymName)
    else
        Pieces = [words("Error: the module qualifier in"),
            qual_sym_name(SymName0), words("does not match"),
            words("the preceding"), decl("module"), words("declaration."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeSymName = error1([Spec])
    ).

try_to_implicitly_qualify_sym_name(DefaultModuleName, SymName0, SymName) :-
    (
        SymName0 = qualified(ModuleName0, Name0),
        ( if partial_sym_name_matches_full(ModuleName0, DefaultModuleName) then
            SymName = qualified(DefaultModuleName, Name0)
        else
            fail
        )
    ;
        SymName0 = unqualified(Name0),
        SymName = qualified(DefaultModuleName, Name0)
    ).

%-----------------------------------------------------------------------------e

parse_symbol_name(VarSet, Term, MaybeSymName) :-
    ( if
        Term = term.functor(term.atom(FunctorName), [ModuleTerm, NameTerm],
            TermContext),
        ( FunctorName = ":"
        ; FunctorName = "."
        )
    then
        ( if NameTerm = term.functor(term.atom(Name), [], _) then
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
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, TermContext, Pieces),
                % XXX Should we include _ModuleResultSpecs?
                MaybeSymName = error1([Spec])
            )
        else
            Pieces = [words("Error: identifier expected after"),
                quote(FunctorName), words("in qualified symbol name."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, TermContext, Pieces),
            MaybeSymName = error1([Spec])
        )
    else
        ( if Term = term.functor(term.atom(Name), [], _) then
            SymName = string_to_sym_name_sep(Name, "__"),
            MaybeSymName = ok1(SymName)
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: symbol name expected at"),
                words(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeSymName = error1([Spec])
        )
    ).

try_parse_symbol_name(Term, SymName) :-
    ( if
        Term = term.functor(term.atom(FunctorName), [ModuleTerm, NameTerm],
            _TermContext),
        ( FunctorName = ":"
        ; FunctorName = "."
        )
    then
        NameTerm = term.functor(term.atom(Name), [], _),
        try_parse_symbol_name(ModuleTerm, Module),
        SymName = qualified(Module, Name)
    else
        Term = term.functor(term.atom(Name), [], _),
        SymName = string_to_sym_name_sep(Name, "__")
    ).

%-----------------------------------------------------------------------------e

parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet, Term,
        MaybeSymName) :-
    parse_symbol_name(VarSet, Term, MaybeSymName0),
    (
        MaybeSymName0 = ok1(SymName0),
        implicitly_qualify_sym_name(DefaultModuleName, Term,
            SymName0, MaybeSymName)
    ;
        MaybeSymName0 = error1(_),
        MaybeSymName = MaybeSymName0
    ).

%-----------------------------------------------------------------------------e

parse_symbol_name_specifier(VarSet, Term, MaybeSymNameSpecifier) :-
    ( if Term = term.functor(term.atom("/"), [NameTerm, ArityTerm], _) then
        ( if term_int.decimal_term_to_int(ArityTerm, Arity) then
            ( if Arity >= 0 then
                parse_symbol_name(VarSet, NameTerm, MaybeName),
                (
                    MaybeName = error1(Specs),
                    MaybeSymNameSpecifier = error1(Specs)
                ;
                    MaybeName = ok1(Name),
                    UserArity = user_arity(Arity),
                    MaybeSymNameSpecifier =
                        ok1(sym_name_specifier_name_arity(Name, UserArity))
                )
            else
                Pieces = [words("Error: arity in symbol name specifier"),
                    words("must be a non-negative integer."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeSymNameSpecifier = error1([Spec])
            )
        else
            Pieces = [words("Error: arity in symbol name specifier"),
                words("must be an integer."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeSymNameSpecifier = error1([Spec])
        )
    else
        parse_symbol_name(VarSet, Term, MaybeSymbolName),
        (
            MaybeSymbolName = error1(Specs),
            MaybeSymNameSpecifier = error1(Specs)
        ;
            MaybeSymbolName = ok1(SymbolName),
            MaybeSymNameSpecifier = ok1(sym_name_specifier_name(SymbolName))
        )
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.parse_sym_name.
%-----------------------------------------------------------------------------e
