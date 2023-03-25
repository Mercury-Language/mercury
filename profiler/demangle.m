%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2008, 2011 The University of Melbourne.
% Copyright (C) 2015, 2019, 2022-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: demangle.m.
% Author: fjh.
%
% A Mercury symbol demangler. This is used to convert symbol names back
% into a form that users can understand.
%
% BEWARE: the code here is duplicated in util/mdemangle.c,
% so any changes here will need to be duplicated there.
%
%---------------------------------------------------------------------------%

:- module demangle.
:- interface.

%---------------------------------------------------------------------------%

:- pred demangle(string::in, string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module pair.

%---------------------------------------------------------------------------%

:- type pred_category
    --->    index
    ;       unify
    ;       compare
    ;       ordinary
    ;       introduced(introduced_pred_type, int, int, string)
            % type, line, sequence number, pred name
    .

:- type introduced_pred_type
    --->    ipt_lambda
    ;       ipt_deforestation
    ;       ipt_accumulator
    ;       ipt_type_spec(string).

:- type data_category
    --->    common
    ;       info
    ;       layout
    ;       functors.

demangle(MangledName, Name) :-
    ( if demangle_from_asm(MangledName, DemangledName) then
        Name = DemangledName
    else
        Name = MangledName
    ).

:- pred demangle_from_asm(string::in, string::out) is semidet.

demangle_from_asm(!Name) :-
    % Skip any leading underscore inserted by the C compiler,
    % and skip the `_entry_' prefix, if any.
    ( if string.remove_prefix("_entry_", !Name) then
        true
    else
        !:Name = string.remove_prefix_if_present("_", !.Name),
        !:Name = string.remove_prefix_if_present("_entry_", !.Name)
    ),
    demangle_from_c(!Name).

:- pred demangle_from_c(string::in, string::out) is semidet.

demangle_from_c(!Str) :-
    ( if demangle_proc_hl(!Str) then
        true
    else if demangle_proc_ll(!Str) then
        true
    else if demangle_data(!Str) then
        true
    else if demangle_typeclass_info(!Str) then
        true
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- pred demangle_proc_ll(string::in, string::out) is semidet.

demangle_proc_ll(!Str) :-
    remove_prefix("mercury__", !Str),

    % Strip off the `fn__' prefix, if any.
    ( if remove_prefix("fn__", !Str) then
        PredOrFunc = "function"
    else
        PredOrFunc = "predicate"
    ),

    % Get integer from end of string (it might be the mode number,
    % it might be the internal label number).
    remove_trailing_int(Int, !Str),

    ( if m_remove_suffix("i", !Str) then
        % If we got to an `i', that means it is an internal label
        % of the form `mercury__append_3_0_i1'; in that case,
        % save the internal label number and then get the mode number.
        MaybeInternalLabelNum = yes(Int),
        m_remove_suffix("_", !Str),
        remove_trailing_int(ModeNum0, !Str)
    else
        MaybeInternalLabelNum = no,
        ModeNum0 = Int
    ),

    % Scan back past the arity number and then parse it.
    m_remove_suffix("_", !Str),
    remove_trailing_int(Arity, !Str),
    m_remove_suffix("_", !Str),

    % Now start processing from the start of the string again.
    % Check whether the start of the string matches the name of
    % one of the special compiler-generated predicates; if so,
    % set the `category' to the appropriate value and then
    % skip past the prefix.
    handle_compiler_generated_pred(ModeNum0, Category0, !Str),

    % Fix any ascii codes mangled in the predicate name.
    fix_mangled_ascii(!Str),

    % Process the mangling introduced by unused_args.m and higher_order.m.
    % This involves stripping off the `__ua<m>', `__uab<m>', and/or `__ho<n>'
    % added to the end of the predicate/function name, where m is the mode
    % number.
    demangle_unused_args(UnusedArgs, ModeNum0, ModeNum1, !Str),
    demangle_higher_order(HigherOrder, ModeNum1, ModeNum, !Str),

    % Make sure special predicates with unused_args are reported correctly.
    ( if UnusedArgs = yes(_), Category0 \= ordinary then
        remove_trailing_int(Arity, !Str)
    else
        true
    ),

    % Separate the module name from the type name for the compiler
    % generated predicates.
    ( if Category0 = ordinary then
        remove_maybe_module_prefix(MaybeModule,
            ["IntroducedFrom__", "DeforestationIn__",
            "AccFrom__", "TypeSpecOf__", "UnusedArgs__"], !Str)
    else
        remove_prefix("_", !Str),
        remove_maybe_module_prefix(MaybeModule,
            ["IntroducedFrom__", "DeforestationIn__",
            "AccFrom__", "TypeSpecOf__", "UnusedArgs__"], !Str),
        MaybeModule \= yes("")
    ),

    % Remove any prefixes added for introduced predicates,
    % and get the predicate name.
    % XXX We should NOT ignore _Str.
    handle_category_etc(PredName, Category0, Category, !.Str, _Str),

    % Now, finally, we can construct the demangled symbol name.
    format_proc(Category, MaybeModule, PredOrFunc, PredName,
        Arity, ModeNum, HigherOrder, UnusedArgs, MaybeInternalLabelNum,
        DemangledName),
    !:Str = DemangledName.

:- pred demangle_proc_hl(string::in, string::out) is semidet.

demangle_proc_hl(!Str) :-
    % Symbols in the Mercury standard library get an additional
    % "mercury__" prefix in their mangled name.
    !:Str = string.remove_prefix_if_present("mercury__", !.Str),

    % Get integer from end of string (it might be the mode number,
    % it might be the internal label number).
    remove_trailing_int(Int, !Str),
    ( if
        % If we got to another int, that means it is an internal label
        % of the form `append_3_p_0_1'. In that case, save the internal label
        % number and then get the mode number.
        m_remove_suffix("_", !Str),
        remove_trailing_int(ModeNum0, !Str)
    then
        ModeNum1 = ModeNum0,
        MaybeInternalLabelNum0 = yes(Int)
    else
        ModeNum1 = Int,
        MaybeInternalLabelNum0 = no
    ),

    % Handle the "f_" or "p_" suffix which indicates whether
    % the procedure is a function or a predicate.
    ( if m_remove_suffix("f_", !Str) then
        PredOrFunc = "function",
        Normal = yes
    else if m_remove_suffix("p_", !Str) then
        PredOrFunc = "predicate",
        Normal = yes
    else
        % It could be a compiler-generated unify or compare predicate.
        PredOrFunc = "predicate",
        Normal = no
    ),

    ( if
        % Scan back past the arity number and then parse it.
        m_remove_suffix("_", !Str),
        remove_trailing_int(Arity0, !Str)
    then
        Arity = Arity0,
        ModeNum2 = ModeNum1,
        MaybeInternalLabelNum = MaybeInternalLabelNum0
    else
        % It must be a compiler-generated unify or compare.
        % What we thought were the mode number and label number
        % were actually the arity and mode number.
        Normal = no,
        Arity = ModeNum1,
        yes(ModeNum2) = MaybeInternalLabelNum0,
        MaybeInternalLabelNum = no
    ),
    m_remove_suffix("_", !Str),

    % Process the mangling introduced by unused_args.m and higher_order.m.
    % This involves stripping off the `__ua<m>', `__uab<m>', and/or `__ho<n>'
    % added to the end of the predicate/function name, where m is the mode
    % number.
    demangle_unused_args(UnusedArgs, ModeNum2, ModeNum3, !Str),
    demangle_higher_order(HigherOrder, ModeNum3, ModeNum, !Str),

    % Make sure special predicates with unused_args are reported correctly.
    ( if UnusedArgs = yes(_), Normal = no then
        remove_trailing_int(Arity, !Str)
    else
        true
    ),

    % Separate the module name from the predicate name.
    remove_maybe_module_prefix(MaybeModule0,
        ["IntroducedFrom__", "DeforestationIn__",
        "AccFrom__", "TypeSpecOf__", "UnusedArgs__", "__"], !Str),

    % Check whether the start of the string matches the name of one of the
    % special compiler-generated predicates; if so, set the `category'
    % to the appropriate value and then skip past the prefix.
    % Also check that the mode number is valid for the specified category.
    handle_compiler_generated_pred(ModeNum, Category0, !Str),
    ( if Category0 = ordinary then
        true
    else
        remove_prefix("__", !Str)
    ),

    % Check that the setting of the category matches the setting
    % of `Normal' determined above.
    ( Normal = yes, Category0 = ordinary
    ; Normal = no, Category0 \= ordinary
    ),

    % Fix any mangled ascii codes in the predicate name.
    %
    % XXX This should be done *before* stripping off the mangling added by
    % HLDS->HLDS passes such as unused_args.m and higher_order.m.
    % (Doing it here means that we won't properly demangle names that
    % involve both special characters and unused_args/higher_order
    % specializations.) But for the MLDS back-end, it needs to be done *after*
    % removing the module prefix, and currently that can't be done until after
    % stripping off the `__ua*' and `__ho*' suffixes.
    fix_mangled_ascii(!Str),

    % Fix any mangled ascii codes in the module name, if any.
    (
        MaybeModule0 = no,
        MaybeModule = no
    ;
        MaybeModule0 = yes(ModuleName0),
        fix_mangled_ascii(ModuleName0, ModuleName),
        MaybeModule = yes(ModuleName)
    ),

    % Remove any prefixes added for introduced predicates,
    % and get the predicate name.
    % XXX We should NOT ignore _Str.
    handle_category_etc(PredName, Category0, Category, !.Str, _Str),

    % Now, finally, we can construct the demangled symbol name.
    format_proc(Category, MaybeModule, PredOrFunc, PredName,
        Arity, ModeNum, HigherOrder, UnusedArgs, MaybeInternalLabelNum,
        DemangledName),
    !:Str = DemangledName.

:- pred demangle_unused_args(maybe(pair(int, bool))::out, int::in, int::out,
    string::in, string::out) is det.

demangle_unused_args(UnusedArgs, ModeNum0, ModeNum, !Str) :-
    % Process the mangling introduced by unused_args.m.
    % This involves stripping off the `__ua<m>' or `__uab<m>' added to
    % the end of the predicate/function name, where m is the mode number.
    % XXX This is out-of-date. The compiler now generates names
    % such as UnusedArgs__p__[1].
    ( if
        remove_trailing_int(UA_ModeNum, !Str),
        m_remove_suffix("__ua", !Str)
    then
        UnusedArgs = yes(ModeNum0 - no),
        ModeNum = UA_ModeNum mod 10000
    else if
        remove_trailing_int(UA_ModeNum, !Str),
        m_remove_suffix("__uab", !Str)
    then
        UnusedArgs = yes(ModeNum0 - yes),
        ModeNum = UA_ModeNum mod 10000
    else
        UnusedArgs = no,
        ModeNum = ModeNum0
    ).

:- pred demangle_higher_order(maybe(int)::out, int::in, int::out,
    string::in, string::out) is det.

demangle_higher_order(HigherOrder, ModeNum0, ModeNum, !Str) :-
    % Process the mangling introduced by higher_order.m.
    % This involves stripping off the `__ho<n>' where
    % n is a unique identifier for this specialized version.
    ( if
        remove_trailing_int(HO_Num, !Str),
        m_remove_suffix("__ho", !Str)
    then
        HigherOrder = yes(HO_Num)
    else
        HigherOrder = no
    ),
    ModeNum = ModeNum0.

    % Check whether the start of the string matches the name of one of the
    % special compiler-generated predicates; if so, set the category
    % to the appropriate value and then skip past the prefix.
    % Fails if the mode number is invalid for the specified category.
    %
:- pred handle_compiler_generated_pred(int::in, pred_category::out,
    string::in, string::out) is semidet.

handle_compiler_generated_pred(ModeNum0, Category0, !Str) :-
    ( if remove_prefix("__Unify__", !Str) then
        Category0 = unify
    else if remove_prefix("__Compare__", !Str) then
        Category0 = compare,
        % There should only be one mode for compare/3 preds.
        ModeNum0 = 0
    else if remove_prefix("__Index__", !Str) then
        Category0 = index,
        % There should only be one mode for index/2 preds.
        ModeNum0 = 0
    else
        Category0 = ordinary
    ).

    % Remove any prefixes added for introduced predicates,
    % and get the predicate name.
    %
:- pred handle_category_etc(string::out, pred_category::in, pred_category::out,
    string::in, string::out) is semidet.

handle_category_etc(PredName, Category0, Category, !Str) :-
    % We need to look at the pred name and see if it is an introduced predicate
    % (lambda, deforestation, accumulator, etc.).
    % XXX handle multiple prefixes

    PredName0 = !.Str,
    ( if
        ( if
            remove_prefix("IntroducedFrom__", !Str)
        then
            IntroducedPredType0 = ipt_lambda
        else if
            remove_prefix("DeforestationIn__", !Str)
        then
            IntroducedPredType0 = ipt_deforestation
        else if
            remove_prefix("AccFrom__", !Str)
        then
            IntroducedPredType0 = ipt_accumulator
        else
            remove_prefix("TypeSpecOf__", !Str),
            IntroducedPredType0 = ipt_type_spec("")
        )
    then
        ( if
            remove_prefix("pred__", !Str)
        then
            LambdaPredOrFunc = "pred"
        else if
            remove_prefix("func__", !Str)
        then
            LambdaPredOrFunc = "func"
        else if
            IntroducedPredType0 = ipt_type_spec(_),
            remove_prefix("pred_or_func__", !Str)
        then
            LambdaPredOrFunc = ""
        else
            fail
        ),
        ( if
            remove_maybe_pred_name(MPredName, !Str),
            MPredName = yes(PredName1),
            ( if IntroducedPredType0 = ipt_type_spec(_) then
                remove_type_spec(TypeSpec, !Str),
                IntroducedPredType = ipt_type_spec(TypeSpec),
                Seq = 0,
                Line = 0

                % The compiler adds a redundant mode number to the predicate
                % name to avoid creating two predicates with the same name
                % (deep profiling doesn't like that). It isn't used here
                % so we just ignore it. The compiler also adds a version number
                % for the argument order used for specialized versions,
                % which can also be ignored.
            else
                IntroducedPredType = IntroducedPredType0,
                remove_int(Line, !Str),
                remove_prefix("__", !Str),
                remove_int(Seq, !Str)
            )
        then
            PredName = PredName1,
            Category = introduced(IntroducedPredType, Line,
                Seq, LambdaPredOrFunc)
        else
            % If we get here it usually means that there were multiple
            % prefixes, which aren't dealt with properly yet. Just treat it
            % as an ordinary name for now.
            Category = ordinary,
            PredName = PredName0
        )
    else
        Category = Category0,
        PredName = PredName0
    ).

:- pred format_proc(pred_category::in, maybe(string)::in, string::in,
    string::in, int::in, int::in, maybe(int)::in,
    maybe(pair(int, bool))::in, maybe(int)::in,
    string::out) is det.

format_proc(Category, MaybeModule, PredOrFunc, PredName, Arity, ModeNum,
        HigherOrder, UnusedArgs, MaybeInternalLabelNum, DemangledName) :-
    format_maybe_module(MaybeModule, PredName, QualifiedName),
    (
        Category = unify,
        string.format("unification predicate for type `%s/%d' mode %d",
            [s(QualifiedName), i(Arity), i(ModeNum)], MainStr)
    ;
        Category = compare,
        string.format("compare/3 predicate for type `%s/%d'",
            [s(QualifiedName), i(Arity)], MainStr)
    ;
        Category = index,
        string.format("index/2 predicate for type `%s/%d'",
            [s(QualifiedName), i(Arity)], MainStr)
    ;
        Category = ordinary,
        string.format("%s `%s/%d' mode %d",
            [s(PredOrFunc), s(QualifiedName), i(Arity), i(ModeNum)], MainStr)
    ;
        Category = introduced(Type, Line, Seq, IntroPredOrFunc),
        (
            Type = ipt_lambda,
            string.format("%s goal (#%d) from `%s' line %d",
                [s(IntroPredOrFunc), i(Seq), s(QualifiedName),
                i(Line)], MainStr)
        ;
            Type = ipt_deforestation,
            string.format(
            "deforestation procedure (#%d) from `%s' line %d",
                [i(Seq), s(QualifiedName), i(Line)], MainStr)
        ;
            Type = ipt_accumulator,
            string.format(
                "accumulator procedure from `%s' line %d",
                [s(QualifiedName), i(Line)], MainStr)
        ;
            Type = ipt_type_spec(TypeSpec),
            string.format(
                "%s `%s/%d' mode %d (type specialized %s)",
                [s(PredOrFunc), s(QualifiedName), i(Arity), i(ModeNum),
                s(TypeSpec)], MainStr)
        )
    ),
    (
        HigherOrder = yes(HO_Num),
        string.format(" (specialized [#%d])", [i(HO_Num)], HOStr)
    ;
        HigherOrder = no,
        HOStr = ""
    ),
    (
        UnusedArgs = yes(UA_Num - Extra),
        (
            Extra = yes,
            string.format(" (minus extra unused args [#%d])", [i(UA_Num)],
                UAStr)
        ;
            Extra = no,
            string.format(" (minus unused args [#%d])", [i(UA_Num)],
                UAStr)
        )
    ;
        UnusedArgs = no,
        UAStr = ""
    ),
    (
        MaybeInternalLabelNum = yes(Internal),
        string.format(" label %d", [i(Internal)], LabelStr)
    ;
        MaybeInternalLabelNum = no,
        LabelStr = ""
    ),
    DemangledName = "<" ++ MainStr ++ HOStr ++ UAStr ++ LabelStr ++ ">".

%---------------------------------------------------------------------------%
%
% Code to deal with mercury_data items.
%

:- pred demangle_data(string::in, string::out) is semidet.

demangle_data(!Str) :-
    ( if remove_prefix("mercury_data_", !Str) then
        % LLDS mangled data
        HighLevel = no
    else
        % MLDS mangled data
        HighLevel = yes,
        !:Str = string.remove_prefix_if_present("mercury__", !.Str)
    ),
    remove_maybe_module_prefix(MaybeModule0,
        ["type_ctor_info_", "type_ctor_layout_",
        "type_ctor_functors_", "common_"], !Str),
    ( if
        MaybeModule0 = yes("")
    then
        MaybeModule = no
    else if
        % for the MLDS back-end,
        % the module qualifiers get include twice (XXX why?)
        HighLevel = yes,
        MaybeModule0 = yes(Twice)
    then
        Once = string.left(Twice, string.length(Twice) // 2),
        Once = string.right(Twice, string.length(Twice) // 2),
        MaybeModule = yes(Once)
    else
        MaybeModule = MaybeModule0
    ),
    ( if remove_prefix("type_ctor_info_", !Str) then
        DataCategory = info,
        remove_trailing_int(Arity, !Str),
        m_remove_suffix("_", !Str)
    else if remove_prefix("type_ctor_layout_", !Str) then
        DataCategory = layout,
        remove_trailing_int(Arity, !Str),
        m_remove_suffix("_", !Str)
    else if remove_prefix("type_ctor_functors_", !Str) then
        DataCategory = functors,
        remove_trailing_int(Arity, !Str),
        m_remove_suffix("_", !Str)
    else if remove_prefix("common_", !Str) then
        DataCategory = common,
        remove_trailing_int(Arity, !Str)
    else
        fail
    ),

    fix_mangled_ascii(!Str),
    format_data(DataCategory, MaybeModule, !.Str, Arity, !:Str).

:- pred format_data(data_category::in, maybe(string)::in, string::in, int::in,
    string::out) is semidet.

format_data(Category, MaybeModule, Name, Arity, Result) :-
    (
        Category = info,
        (
            MaybeModule = yes(Module),
            string.format("<type_ctor_info for type `%s.%s/%d'>",
                [s(Module), s(Name), i(Arity)], Result)
        ;
            MaybeModule = no,
            string.format("<type_ctor_info for type `%s/%d'>",
                [s(Name), i(Arity)], Result)
        )
    ;
        Category = layout,
        (
            MaybeModule = yes(Module),
            string.format("<type_ctor_layout for type `%s.%s/%d'>",
                [s(Module), s(Name), i(Arity)], Result)
        ;
            MaybeModule = no,
            string.format("<type_ctor_layout for type `%s/%d'>",
                [s(Name), i(Arity)], Result)
        )
    ;
        Category = functors,
        (
            MaybeModule = yes(Module),
            string.format("<type_ctor_functors for type `%s.%s/%d'>",
                [s(Module), s(Name), i(Arity)], Result)
        ;
            MaybeModule = no,
            string.format("<type_ctor_functors for type `%s/%d'>",
                [s(Name), i(Arity)], Result)
        )
    ;
        Category = common,
        (
            MaybeModule = yes(Module),
            string.format("<shared constant number %d for module %s>",
                [i(Arity), s(Module)], Result)
        ;
            MaybeModule = no,
            fail
        )
    ).

:- pred demangle_typeclass_info(string::in, string::out) is semidet.

demangle_typeclass_info(!Str) :-
    !:Str = string.remove_prefix_if_present("mercury_data___", !.Str),
    remove_prefix("base_typeclass_info_", !Str),
    remove_maybe_module_prefix(yes(ClassName), ["arity"], !Str),
    ClassName \= "",
    remove_prefix("arity", !Str),
    remove_int(ClassArity, !Str),
    remove_prefix("__", !Str),
    fix_mangled_ascii(!Str),
    % XXX We should NOT ignore _Str.
    demangle_class_args(ClassArity, Args, !.Str, _Str),
    string.format("<instance declaration for %s(%s)>",
        [s(ClassName), s(Args)], !:Str).

:- pred demangle_class_args(int::in, string::out, string::in, string::out)
    is semidet.

demangle_class_args(Num, FormattedArgs, !Str) :-
    remove_maybe_module_prefix(yes(TypeName), ["arity"], !Str),
    TypeName \= "",
    remove_prefix("arity", !Str),
    remove_int(TypeArity, !Str),
    remove_prefix("__", !Str),
    ( if Num > 1 then
        Sep = ", ",
        Num1 = Num - 1,
        demangle_class_args(Num1, Rest, !Str)
    else
        Sep = "",
        Rest = ""
    ),
    string.format("%s/%d%s%s",
        [s(TypeName), i(TypeArity), s(Sep), s(Rest)],
        FormattedArgs).

%---------------------------------------------------------------------------%

    % The compiler changes all names starting with `f_' so that they start
    % with `f__' instead, and uses names starting with `f_' for mangled names
    % which are either descriptions (such as `f_greater_than' for `>')
    % or sequences of decimal representations of ASCII codes separated
    % by underscores. If the name starts with `f__', we must change it back to
    % start with `f_'. Otherwise, if it starts with `f_' we must convert
    % the mnemonic or list of ASCII codes back into an identifier.
    %
:- pred fix_mangled_ascii(string::in, string::out) is semidet.

fix_mangled_ascii(!Str) :-
    ( if remove_prefix("f__", !Str) then
        insert_prefix("f_", !Str)
    else if remove_prefix("f_not_equal", !Str) then
        insert_prefix("\\=", !Str)
    else if remove_prefix("f_greater_or_equal", !Str) then
        insert_prefix(">=", !Str)
    else if remove_prefix("f_less_or_equal", !Str) then
        insert_prefix("=<", !Str)
    else if remove_prefix("f_equal", !Str) then
        insert_prefix("=", !Str)
    else if remove_prefix("f_less_than", !Str) then
        insert_prefix("<", !Str)
    else if remove_prefix("f_greater_than", !Str) then
        insert_prefix(">", !Str)
    else if remove_prefix("f_minus", !Str) then
        insert_prefix("-", !Str)
    else if remove_prefix("f_plus", !Str) then
        insert_prefix("+", !Str)
    else if remove_prefix("f_times", !Str) then
        insert_prefix("*", !Str)
    else if remove_prefix("f_slash", !Str) then
        insert_prefix("/", !Str)
    else if remove_prefix("f_comma", !Str) then
        insert_prefix(",", !Str)
    else if remove_prefix("f_semicolon", !Str) then
        insert_prefix(";", !Str)
    else if remove_prefix("f_cut", !Str) then
        insert_prefix("!", !Str)
    else if remove_prefix("f_tuple", !Str) then
        insert_prefix("{}", !Str)
    else if remove_prefix("f_cons", !Str) then
        insert_prefix("[|]", !Str)
    else if remove_prefix("f_nil", !Str) then
        insert_prefix("[]", !Str)
    else if remove_prefix("f_", !Str) then
        fix_mangled_ascii_chars(!Str)
    else
        true
    ).

:- pred fix_mangled_ascii_chars(string::in, string::out) is semidet.

fix_mangled_ascii_chars(!Str) :-
    remove_int(I, !Str),
    ( if remove_prefix("_", !Str) then
        disable_warning [suspicious_recursion] (
            fix_mangled_ascii_chars(!Str)
        )
    else
        true
    ),
    char.to_int(C, I) ,
    insert_prefix_char(C, !Str).

%---------------------------------------------------------------------------%

:- pred remove_int(int::out, string::in, string::out) is semidet.

remove_int(Int, !Str) :-
    remove_digit(Digit, !Str),
    remove_int_2(Digit, Int, !Str).

:- pred remove_int_2(int::in, int::out, string::in, string::out) is semidet.

remove_int_2(Int0, Int, !Str) :-
    ( if remove_digit(Next, !Str) then
        Int1 = Int0 * 10 + Next,
        remove_int_2(Int1, Int, !Str)
    else
        Int = Int0
    ).

:- pred remove_digit(int::out, string::in, string::out) is semidet.

remove_digit(Digit, String0, String) :-
    string.first_char(String0, Char, String),
    decimal_digit_to_int(Char, Digit).

%---------------------------------------------------------------------------%

:- pred remove_maybe_module_prefix(maybe(string)::out, list(string)::in,
    string::in, string::out) is det.

remove_maybe_module_prefix(MaybeModule, StringsToStopAt, String0, String) :-
    ( if
        list.member(StopString, StringsToStopAt),
        string.prefix(String0, StopString)
    then
        MaybeModule = no,
        String = String0
    else if
        string.sub_string_search(String0, "__", Index)
    then
        string.left(String0, Index, Module),
        string.length(String0, Len),
        Index2 = Index + 2,
        string.between(String0, Index2, Len, String1),
        ( if
            remove_maybe_module_prefix(yes(SubModule),
                StringsToStopAt, String1, String2)
        then
            string.append_list([Module, ".", SubModule], QualifiedModule),
            MaybeModule = yes(QualifiedModule),
            String = String2
        else
            MaybeModule = yes(Module),
            String = String1
        )
    else
        String = String0,
        MaybeModule = no
    ).

:- pred remove_maybe_pred_name(maybe(string)::out, string::in, string::out)
    is det.

remove_maybe_pred_name(MaybePredName, String0, String) :-
    ( if string.sub_string_search(String0, "__", Index) then
        string.left(String0, Index, PredName),
        string.length(String0, Len),
        Index2 = Index + 2,
        string.between(String0, Index2, Len, String),
        MaybePredName = yes(PredName)
    else
        String = String0,
        MaybePredName = no
    ).

:- pred remove_type_spec(string::out, string::in, string::out) is semidet.

remove_type_spec(TypeSpec, String0, String) :-
    string.length(String0, Length),
    Length > 2,
    string.unsafe_index(String0, 0, '['),
    NumBrackets = 0,
    find_matching_close_bracket(NumBrackets, Length,
        String0, 1, Index),
    string.split(String0, Index + 1, TypeSpec, String).

:- pred find_matching_close_bracket(int::in, int::in, string::in, int::in,
    int::out) is semidet.

find_matching_close_bracket(NumBrackets0, Length, String, Index0, Index) :-
    Index0 < Length,
    string.unsafe_index(String, Index0, Char),
    ( if Char = ']', NumBrackets0 = 0 then
        Index = Index0
    else
        % Handle matching brackets in type names.
        ( if Char = '[' then
            NumBrackets = NumBrackets0 + 1
        else if Char = ']' then
            NumBrackets = NumBrackets0 - 1
        else
            NumBrackets = NumBrackets0
        ),
        find_matching_close_bracket(NumBrackets, Length, String,
            Index0 + 1, Index)
    ).

:- pred m_remove_suffix(string::in, string::in, string::out) is semidet.

m_remove_suffix(Suffix, Name0, Name) :-
    string.remove_suffix(Name0, Suffix, Name).

:- pred insert_prefix(string::in, string::in, string::out) is det.

insert_prefix(Prefix, Name0, Name) :-
    string.append(Prefix, Name0, Name).

:- pred insert_prefix_char(char::in, string::in, string::out) is det.

insert_prefix_char(Prefix, Name0, Name) :-
    string.first_char(Name, Prefix, Name0).

:- pred format_maybe_module(maybe(string)::in, string::in, string::out) is det.

format_maybe_module(no, Name, QualifiedName) :-
    string.format("%s", [s(Name)], QualifiedName).
format_maybe_module(yes(Module), Name, QualifiedName) :-
    string.format("%s.%s", [s(Module), s(Name)], QualifiedName).

:- pred remove_trailing_int(int::out, string::in, string::out) is semidet.

remove_trailing_int(Int, !String) :-
    remove_trailing_digit(Digit, !String),
    ( if remove_trailing_int(Rest, !String) then
        Int = Rest * 10 + Digit
    else
        Int = Digit
    ).

:- pred remove_trailing_digit(int::out, string::in, string::out) is semidet.

remove_trailing_digit(Digit, String0, String) :-
    string_last_char(String0, Char, String),
    decimal_digit_to_int(Char, Digit).

:- pred string_last_char(string::in, character::out, string::out) is semidet.

string_last_char(String0, Char, String) :-
    string.length(String0, Len),
    Len1 = Len - 1,
    string.index(String0, Len1, Char),
    string.left(String0, Len1, String).

%---------------------------------------------------------------------------%
:- end_module demangle.
%---------------------------------------------------------------------------%
