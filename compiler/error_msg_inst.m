%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module formats insts for use in error messages, with the objective
% of making the generated output easily readable by humans. This sometimes
% requires departing from strict Mercury syntax, so the output is NOT
% expected to parseable by machine.
%
% The module also formats modes, since this is needed for printing the insts
% of higher order types. This capability is not yet exported.
%
%---------------------------------------------------------------------------%

:- module hlds.error_msg_inst.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type maybe_expand_named_insts
    --->    dont_expand_named_insts
    ;       expand_named_insts.

:- type short_inst
    --->    quote_short_inst
    ;       fixed_short_inst.

    % report_inst(ModuleInfo, InstVarSet, ShortInstQF, ShortInstSuffix,
    %    LongInstPrefix, LongInstSuffix, Inst0) = Pieces:
    %
    % Format Inst0 for use in an error message, in a short form that fits at
    % the end of the current line if possible, and in a long form that starts
    % on a separate line, if it is not.
    %
    % When using the short form, put the inst's text representation into quotes
    % if ShortInstSuffix = quote_short_inst. Don't put anything before it
    % (our caller will do that), but add ShortInstSuffix after it. Normally,
    % ShortInstSuffix will end with either nl or nl_indent_delta.
    %
    % When using the long form, leave the inst's text representation as is,
    % without quotations, put LongInstPrefix before it, and LongInstSuffix
    % after it. Normally, LongInstPrefix will start with nl or nl_indent_delta
    % to start the inst on a new line, and LongInstSuffix will end with nl
    % or nl_indent_delta as well.
    %
:- func error_msg_inst(module_info, inst_varset, maybe_expand_named_insts,
    short_inst, list(format_component),
    list(format_component), list(format_component), mer_inst)
    = list(format_component).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- type inst_msg_info
    --->    inst_msg_info(
                imi_module_info         :: module_info,
                imi_inst_varset         :: inst_varset,
                imi_expand_named_insts  :: maybe_expand_named_insts
            ).

error_msg_inst(ModuleInfo, InstVarSet, ExpandNamedInsts,
        ShortInstQF, ShortInstSuffix, LongInstPrefix, LongInstSuffix, Inst0)
        = Pieces :-
    Info = inst_msg_info(ModuleInfo, InstVarSet, ExpandNamedInsts),
    strip_builtin_qualifiers_from_inst(Inst0, Inst),
    set.init(Expansions0),
    InlinePieces = inst_to_inline_pieces(Info, Expansions0, Inst, []),
    InlineStr = error_pieces_to_string(InlinePieces),
    % We could base the decision on whether to put the inst inline
    % on criteria other than its length, such as
    %
    % - on a count of the parentheses in it (which would effectively be
    %   a test of the inst's depth);
    % - on a count of the commas in it (which would effectively be a test
    %   of the number of arguments in the inst); or
    % - on a composite test, such as "the number of left parentheses
    %   plus the number of commas cannot exceed six".
    %
    % The current test is just a guess; experience will give us feedback.
    ( if
        string.length(InlineStr, Len),
        Len < 40
    then
        (
            ShortInstQF = quote_short_inst,
            % An inst that is shown on the same line as English text needs
            % something to visually separate it from the English text.
            % The quotes provide that separation.
            %
            % Without a small length limit, we would use words_quote
            % instead of quote to wrap InlineStr.
            InlinePiece = quote(InlineStr)
        ;
            % Our caller has told us that it ensured this separation already.
            ShortInstQF = fixed_short_inst,
            InlinePiece = fixed(InlineStr)
        ),
        Pieces = [InlinePiece | ShortInstSuffix]
    else
        % Showing the inst on a separate line from the English text
        % provides enough separation by itself.
        Pieces = LongInstPrefix ++
            inst_to_pieces(Info, Expansions0, Inst, LongInstSuffix)
    ).

%---------------------------------------------------------------------------%
%
% The following functions are sort-of duplicated. The functions whose names
% end in "to_pieces" generate output that put different parts of the inst
% on (mostly) separate lines, showing the structure of the inst through
% indentation. The functions whose names end in "to_inline_pieces" generate
% output that contains the same components, but all on one line.
%

:- func inst_to_pieces(inst_msg_info, set(inst_name), mer_inst,
    list(format_component)) = list(format_component).

inst_to_pieces(Info, Expansions0, Inst, Suffix) = Pieces :-
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        Pieces = [fixed("free") | Suffix]
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", "", BoundName),
        (
            BoundInsts = [],
            Pieces = [fixed(BoundName) | Suffix]
        ;
            BoundInsts = [HeadBoundInst | TailBoundInsts],
            BoundPieces = bound_insts_to_pieces(Info,
                Expansions0, HeadBoundInst, TailBoundInsts,
                [nl_indent_delta(-1)]),
            Pieces = [fixed(BoundName), suffix("("), nl_indent_delta(1) |
                BoundPieces] ++ [fixed(")") | Suffix]
        )
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            HOPieces = pred_inst_info_to_pieces(Info,
                Expansions0, "", Uniq, PredInstInfo),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            HOPieces = pred_inst_info_to_pieces(Info,
                Expansions0, "any_", Uniq, PredInstInfo),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = inst_var(Var),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_var(InstVarSet, print_name_only, Var, "", Name),
        Pieces = [fixed(Name) | Suffix]
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_vars(InstVarSet, print_name_only,
            set.to_sorted_list(Vars), "", Names),
        Pieces = [fixed("("), words(Names), fixed("=<") |
            inst_to_pieces(Info, Expansions0,
                ConstrainedInst, [fixed(")") | Suffix])]
    ;
        Inst = abstract_inst(Name, ArgInsts),
        InstName = user_inst(Name, ArgInsts),
        Pieces = inst_name_to_pieces(Info, Expansions0,
            InstName, Suffix)
    ;
        Inst = defined_inst(InstName),
        Pieces = inst_name_to_pieces(Info, Expansions0,
            InstName, Suffix)
    ;
        Inst = not_reached,
        Pieces = [fixed("not_reached") | Suffix]
    ).

:- func inst_to_inline_pieces(inst_msg_info, set(inst_name),
    mer_inst, list(format_component)) = list(format_component).

inst_to_inline_pieces(Info, Expansions0, Inst, Suffix)
        = Pieces :-
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        Pieces = [fixed("free") | Suffix]
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", "", BoundName),
        (
            BoundInsts = [],
            Pieces = [fixed(BoundName) | Suffix]
        ;
            BoundInsts = [HeadBoundInst | TailBoundInsts],
            BoundPieces = bound_insts_to_inline_pieces(Info,
                Expansions0, HeadBoundInst, TailBoundInsts, []),
            Pieces = [prefix(BoundName ++ "(") | BoundPieces] ++
                [suffix(")") | Suffix]
        )
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            HOPieces = pred_inst_info_to_inline_pieces(Info,
                Expansions0, "", Uniq, PredInstInfo),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            HOPieces = pred_inst_info_to_inline_pieces(Info,
                Expansions0, "any_", Uniq, PredInstInfo),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = inst_var(Var),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_var(InstVarSet, print_name_only, Var, "", Name),
        Pieces = [fixed(Name) | Suffix]
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_vars(InstVarSet, print_name_only,
            set.to_sorted_list(Vars), "", Names),
        Pieces = [fixed("("), words(Names), fixed("=<") |
            inst_to_inline_pieces(Info, Expansions0,
                ConstrainedInst, [fixed(")") | Suffix])]
    ;
        Inst = abstract_inst(Name, ArgInsts),
        InstName = user_inst(Name, ArgInsts),
        Pieces = inst_name_to_inline_pieces(Info,
            Expansions0, InstName, Suffix)
    ;
        Inst = defined_inst(InstName),
        Pieces = inst_name_to_inline_pieces(Info,
            Expansions0, InstName, Suffix)
    ;
        Inst = not_reached,
        Pieces = [fixed("not_reached") | Suffix]
    ).

%---------------------------------------------------------------------------%

:- func insts_to_pieces(inst_msg_info, set(inst_name),
    mer_inst, list(mer_inst), list(format_component)) = list(format_component).

insts_to_pieces(Info, Expansions0, HeadInst, TailInsts,
        Suffix) = Pieces :-
    (
        TailInsts = [],
        HeadSuffix = Suffix
    ;
        TailInsts = [HeadTailInst | TailTailInsts],
        TailPieces = insts_to_pieces(Info, Expansions0,
            HeadTailInst, TailTailInsts, Suffix),
        HeadSuffix = [suffix(","), nl] ++ TailPieces
    ),
    Pieces = inst_to_pieces(Info, Expansions0, HeadInst,
        HeadSuffix).

:- func insts_to_inline_pieces(inst_msg_info, set(inst_name),
    mer_inst, list(mer_inst), list(format_component)) = list(format_component).

insts_to_inline_pieces(Info, Expansions0,
        HeadInst, TailInsts, Suffix) = Pieces :-
    (
        TailInsts = [],
        HeadSuffix = Suffix
    ;
        TailInsts = [HeadTailInst | TailTailInsts],
        TailPieces = insts_to_inline_pieces(Info,
            Expansions0, HeadTailInst, TailTailInsts, Suffix),
        HeadSuffix = [suffix(",")] ++ TailPieces
    ),
    Pieces = inst_to_inline_pieces(Info, Expansions0,
        HeadInst, HeadSuffix).

%---------------------------------------------------------------------------%

:- func bound_insts_to_pieces(inst_msg_info, set(inst_name),
    bound_inst, list(bound_inst), list(format_component))
    = list(format_component).

bound_insts_to_pieces(Info, Expansions0,
        HeadBoundInst, TailBoundInsts, Suffix) = Pieces :-
    (
        TailBoundInsts = [],
        HeadSuffix = Suffix
    ;
        TailBoundInsts = [HeadTailBoundInst | TailTailBoundInsts],
        TailPieces = bound_insts_to_pieces(Info, Expansions0,
            HeadTailBoundInst, TailTailBoundInsts, Suffix),
        HeadSuffix = [nl_indent_delta(-1), fixed(";"), nl_indent_delta(1) |
            TailPieces]
    ),
    HeadBoundInst = bound_functor(ConsId0, ArgInsts),
    ( if
        ConsId0 = cons(SymName, Arity, TypeCtor),
        % The module names of the cons_ids are uniquely specified
        % by the types of the variables whose we are printing. Printing them
        % would therefore generate more clutter than enlightenment.
        SymName = qualified(_ModuleName, BaseName)
    then
        ConsId = cons(unqualified(BaseName), Arity, TypeCtor)
    else
        ConsId = ConsId0
    ),
    mercury_format_cons_id(does_not_need_brackets, ConsId, "", ConsIdStr),
    Pieces = name_and_arg_insts_to_pieces(Info, Expansions0,
        ConsIdStr, ArgInsts, HeadSuffix).

:- func bound_insts_to_inline_pieces(inst_msg_info, set(inst_name),
    bound_inst, list(bound_inst), list(format_component))
    = list(format_component).

bound_insts_to_inline_pieces(Info, Expansions0,
        HeadBoundInst, TailBoundInsts, Suffix) = Pieces :-
    (
        TailBoundInsts = [],
        HeadSuffix = Suffix
    ;
        TailBoundInsts = [HeadTailBoundInst | TailTailBoundInsts],
        TailPieces = bound_insts_to_inline_pieces(Info,
            Expansions0, HeadTailBoundInst, TailTailBoundInsts, Suffix),
        HeadSuffix = [fixed(";") | TailPieces]
    ),
    HeadBoundInst = bound_functor(ConsId0, ArgInsts),
    ( if
        ConsId0 = cons(SymName, Arity, TypeCtor),
        % The module names of the cons_ids are uniquely specified
        % by the types of the variables whose we are printing. Printing them
        % would therefore generate more clutter than enlightenment.
        SymName = qualified(_ModuleName, BaseName)
    then
        ConsId = cons(unqualified(BaseName), Arity, TypeCtor)
    else
        ConsId = ConsId0
    ),
    mercury_format_cons_id(does_not_need_brackets, ConsId, "", ConsIdStr),
    Pieces = name_and_arg_insts_to_inline_pieces(Info,
        Expansions0, ConsIdStr, ArgInsts, HeadSuffix).

%---------------------------------------------------------------------------%

:- func inst_name_to_pieces(inst_msg_info, set(inst_name),
    inst_name, list(format_component)) = list(format_component).

inst_name_to_pieces(Info, Expansions0, InstName, Suffix)
        = Pieces :-
    (
        InstName = user_inst(SymName, ArgInsts),
        ModuleInfo = Info ^ imi_module_info,
        (
            SymName = qualified(ModuleName, BaseName),
            module_info_get_name(ModuleInfo, CurModuleName),
            ( if ModuleName = CurModuleName then
                SymNameStr = BaseName
            else
                SymNameStr = sym_name_to_string(SymName)
            )
        ;
            SymName = unqualified(BaseName),
            SymNameStr = BaseName
        ),
        set.insert(InstName, Expansions0, Expansions1),
        ExpandInsts = Info ^ imi_expand_named_insts,
        (
            ExpandInsts = dont_expand_named_insts,
            Pieces = name_and_arg_insts_to_pieces(Info,
                Expansions1, SymNameStr, ArgInsts, Suffix)
        ;
            ExpandInsts = expand_named_insts,
            % We do the circularity check by looking up InstName
            % in Expansions0, but do everything else with Expansions1.
            ( if set.contains(Expansions0, InstName) then
                NamePieces = name_and_arg_insts_to_pieces(Info,
                    Expansions1, SymNameStr, ArgInsts, Suffix),
                Pieces = [words("named inst") | NamePieces]
            else
                inst_lookup(ModuleInfo, InstName, EqvInst),
                ( if
                    ( EqvInst = defined_inst(InstName)
                    ; EqvInst = abstract_inst(SymName, ArgInsts)
                    )
                then
                    NamePieces = name_and_arg_insts_to_pieces(Info,
                        Expansions1, SymNameStr, ArgInsts, Suffix),
                    Pieces = [words("named inst") | NamePieces]
                else
                    NamePieces = name_and_arg_insts_to_pieces(Info,
                        Expansions1, SymNameStr, ArgInsts, [suffix(","), nl]),
                    ExpandedPieces = inst_to_pieces(Info, Expansions1,
                        EqvInst, Suffix),
                    Pieces = [words("named inst") | NamePieces] ++
                        [words("which expands to"), nl | ExpandedPieces]
                )
            )
        )
    ;
        InstName = typed_inst(_Type, SubInstName),
        % The user doesn't care about the typed_inst wrapper.
        Pieces = inst_name_to_pieces(Info, Expansions0, SubInstName, Suffix)
    ;
        InstName = typed_ground(Uniq, _Type),
        % The user doesn't care about the typed_ground wrapper.
        EqvInst = ground(Uniq, none_or_default_func),
        Pieces = inst_to_pieces(Info, Expansions0, EqvInst, Suffix)
    ;
        (
            InstName = unify_inst(_, _, _, _),
            InstNameStr = "$unify_inst"
        ;
            InstName = merge_inst(_, _),
            InstNameStr = "$merge_inst"
        ;
            InstName = ground_inst(_, _, _, _),
            InstNameStr = "$ground_inst"
        ;
            InstName = any_inst(_, _, _, _),
            InstNameStr = "$any_inst"
        ;
            InstName = shared_inst(_),
            InstNameStr = "$shared_inst"
        ;
            InstName = mostly_uniq_inst(_),
            InstNameStr = "$mostly_uniq_inst"
        ),
        ModuleInfo = Info ^ imi_module_info,
        inst_lookup(ModuleInfo, InstName, EqvInst),
        ( if EqvInst = defined_inst(InstName) then
            Pieces = [fixed(InstNameStr) | Suffix]
        else
            Pieces = inst_to_pieces(Info, Expansions0,
                EqvInst, Suffix)
        )
    ).

:- func inst_name_to_inline_pieces(inst_msg_info, set(inst_name),
    inst_name, list(format_component)) = list(format_component).

inst_name_to_inline_pieces(Info, Expansions0,
        InstName, Suffix) = Pieces :-
    (
        InstName = user_inst(SymName, ArgInsts),
        ModuleInfo = Info ^ imi_module_info,
        (
            SymName = qualified(ModuleName, BaseName),
            module_info_get_name(ModuleInfo, CurModuleName),
            ( if ModuleName = CurModuleName then
                SymNameStr = BaseName
            else
                SymNameStr = sym_name_to_string(SymName)
            )
        ;
            SymName = unqualified(BaseName),
            SymNameStr = BaseName
        ),
        set.insert(InstName, Expansions0, Expansions1),
        ExpandInsts = Info ^ imi_expand_named_insts,
        (
            ExpandInsts = dont_expand_named_insts,
            Pieces = name_and_arg_insts_to_pieces(Info,
                Expansions1, SymNameStr, ArgInsts, Suffix)
        ;
            ExpandInsts = expand_named_insts,
            % We do the circularity check by looking up InstName
            % in Expansions0, but do everything else with Expansions1.
            ( if set.contains(Expansions0, InstName) then
                NamePieces = name_and_arg_insts_to_inline_pieces(Info,
                    Expansions1, SymNameStr, ArgInsts, Suffix),
                Pieces = [words("named inst") | NamePieces]
            else
                inst_lookup(ModuleInfo, InstName, EqvInst),
                ( if
                    ( EqvInst = defined_inst(InstName)
                    ; EqvInst = abstract_inst(SymName, ArgInsts)
                    )
                then
                    NamePieces = name_and_arg_insts_to_inline_pieces(Info,
                        Expansions1, SymNameStr, ArgInsts, Suffix),
                    Pieces = [words("named inst") | NamePieces]
                else
                    NamePieces = name_and_arg_insts_to_inline_pieces(Info,
                        Expansions1, SymNameStr, ArgInsts, [suffix(","), nl]),
                    ExpandedPieces = inst_to_inline_pieces(Info,
                        Expansions1, EqvInst, Suffix),
                    Pieces = [words("named inst") | NamePieces] ++
                        [words("which expands to"), nl | ExpandedPieces]
                )
            )
        )
    ;
        InstName = typed_inst(_Type, SubInstName),
        % The user doesn't care about the typed_inst wrapper.
        Pieces = inst_name_to_inline_pieces(Info, Expansions0,
            SubInstName, Suffix)
    ;
        InstName = typed_ground(Uniq, _Type),
        % The user doesn't care about the typed_ground wrapper.
        EqvInst = ground(Uniq, none_or_default_func),
        Pieces = inst_to_inline_pieces(Info, Expansions0, EqvInst, Suffix)
    ;
        (
            InstName = unify_inst(_, _, _, _),
            InstNameStr = "$unify_inst"
        ;
            InstName = merge_inst(_, _),
            InstNameStr = "$merge_inst"
        ;
            InstName = ground_inst(_, _, _, _),
            InstNameStr = "$ground_inst"
        ;
            InstName = any_inst(_, _, _, _),
            InstNameStr = "$any_inst"
        ;
            InstName = shared_inst(_),
            InstNameStr = "$shared_inst"
        ;
            InstName = mostly_uniq_inst(_),
            InstNameStr = "$mostly_uniq_inst"
        ),
        ModuleInfo = Info ^ imi_module_info,
        inst_lookup(ModuleInfo, InstName, EqvInst),
        ( if EqvInst = defined_inst(InstName) then
            Pieces = [fixed(InstNameStr) | Suffix]
        else
            Pieces = inst_to_inline_pieces(Info, Expansions0,
                EqvInst, Suffix)
        )
    ).

%---------------------------------------------------------------------------%

:- func pred_inst_info_to_pieces(inst_msg_info, set(inst_name),
    string, uniqueness, pred_inst_info) = list(format_component).

pred_inst_info_to_pieces(Info, Expansions0, AnyPrefix, Uniq,
        PredInstInfo) = Pieces :-
    PredInstInfo = pred_inst_info(PredOrFunc, ArgModes, _MaybeArgRegs, Det),
    (
        Uniq = shared,
        UniqPieces = []
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        mercury_format_uniqueness(Uniq, "ground", "", BoundName),
        UniqPieces = [fixed("/*"), fixed(BoundName), fixed("*/")]
    ),
    ArgModesPieces = list.map(
        mode_to_pieces(Info, Expansions0), ArgModes),
    mercury_format_det(Det, "is ", IsDetStr),
    % XXX Should we print each argument mode on a separate line?
    (
        PredOrFunc = pf_predicate,
        (
            ArgModes = [],
            ModesDetPieces = [fixed("(" ++ AnyPrefix ++ "pred"),
                fixed(IsDetStr), suffix(")")]
        ;
            ArgModes = [_ | _],
            JoinedArgModePieces =
                strict_component_lists_to_pieces(ArgModesPieces),
            ModesDetPieces = [prefix("(" ++ AnyPrefix ++ "pred(") |
                JoinedArgModePieces] ++
                [suffix(")"), fixed(IsDetStr), suffix(")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgModesPieces,
            RealArgModesPieces, RetModePieces),
        JoinedRealArgModePieces =
            component_list_to_line_pieces(RealArgModesPieces, []),
        % XXX Should we put parentheses around RetModePieces?
        % If it prints as "InitInst >> FinalInst", then the parentheses
        % could make the output easier to read, but if it prints as a
        % simple name, they would probably be just in the way.
        (
            ArgModes = [],
            ModesDetPieces = [fixed("(" ++ AnyPrefix ++ "func =") |
                RetModePieces] ++
                [fixed(IsDetStr), suffix(")")]
        ;
            ArgModes = [_ | _],
            ModesDetPieces = [fixed("(" ++ AnyPrefix ++ "func("),
                nl_indent_delta(1) |
                JoinedRealArgModePieces] ++
                [nl_indent_delta(-1), fixed(") =") | RetModePieces] ++
                [fixed(IsDetStr), suffix(")")]
        )
    ),
    Pieces = UniqPieces ++ ModesDetPieces.

:- func pred_inst_info_to_inline_pieces(inst_msg_info,
    set(inst_name), string, uniqueness, pred_inst_info)
    = list(format_component).

pred_inst_info_to_inline_pieces(Info, Expansions0,
        AnyPrefix, Uniq, PredInstInfo) = Pieces :-
    PredInstInfo = pred_inst_info(PredOrFunc, ArgModes, _MaybeArgRegs, Det),
    (
        Uniq = shared,
        UniqPieces = []
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        mercury_format_uniqueness(Uniq, "ground", "", BoundName),
        UniqPieces = [fixed("/*"), fixed(BoundName), fixed("*/")]
    ),
    ArgModesPieces = list.map(
        mode_to_inline_pieces(Info, Expansions0), ArgModes),
    mercury_format_det(Det, "is ", IsDetStr),
    (
        PredOrFunc = pf_predicate,
        (
            ArgModes = [],
            ModesDetPieces = [fixed("(" ++ AnyPrefix ++ "pred"),
                fixed(IsDetStr), suffix(")")]
        ;
            ArgModes = [_ | _],
            JoinedArgModePieces =
                strict_component_lists_to_pieces(ArgModesPieces),
            ModesDetPieces = [prefix("(" ++ AnyPrefix ++ "pred(") |
                JoinedArgModePieces] ++
                [suffix(")"), fixed(IsDetStr), suffix(")")]
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgModesPieces,
            RealArgModesPieces, RetModePieces),
        JoinedRealArgModePieces =
            strict_component_lists_to_pieces(RealArgModesPieces),
        % XXX Should we put parentheses around RetModePieces?
        % If it prints as "InitInst >> FinalInst", then the parentheses
        % could make the output easier to read, but if it prints as a
        % simple name, they would probably be just in the way.
        (
            ArgModes = [],
            ModesDetPieces = [fixed("(" ++ AnyPrefix ++ "func =") |
                RetModePieces] ++
                [fixed(IsDetStr), suffix(")")]
        ;
            ArgModes = [_ | _],
            ModesDetPieces = [prefix("(" ++ AnyPrefix ++ "func(") |
                JoinedRealArgModePieces] ++
                [suffix(")"), fixed("=") | RetModePieces] ++
                [fixed(IsDetStr), suffix(")")]
        )
    ),
    Pieces = UniqPieces ++ ModesDetPieces.

%---------------------------------------------------------------------------%

:- func mode_to_pieces(inst_msg_info, set(inst_name), mer_mode)
    = list(format_component).

mode_to_pieces(Info, Expansions0, Mode) = Pieces :-
    (
        Mode = from_to_mode(InitInst0, FinalInst0),
        % XXX We should strip these wrappers everywhere in both insts,
        % not just at the top.
        ( if InitInst0 = defined_inst(typed_inst(_, SubInitInstName)) then
            InitInst = defined_inst(SubInitInstName)
        else
            InitInst = InitInst0
        ),
        ( if FinalInst0 = defined_inst(typed_inst(_, SubFinalInstName)) then
            FinalInst = defined_inst(SubFinalInstName)
        else
            FinalInst = FinalInst0
        ),
        % XXX Should we special case the situation where InitInst = FinalInst?
        ( if
            InitInst = ground(shared, none_or_default_func),
            FinalInst = ground(shared, none_or_default_func)
        then
            Pieces = [fixed("in")]
        else if
            InitInst = free,
            FinalInst = ground(shared, none_or_default_func)
        then
            Pieces = [fixed("out")]
        else if
            InitInst = ground(unique, none_or_default_func),
            FinalInst = ground(clobbered, none_or_default_func)
        then
            Pieces = [fixed("di")]
        else if
            InitInst = ground(unique, none_or_default_func),
            FinalInst = ground(unique, none_or_default_func)
        then
            Pieces = [fixed("ui")]
        else if
            InitInst = free,
            FinalInst = ground(unique, none_or_default_func)
        then
            Pieces = [fixed("uo")]
        else if
            InitInst = ground(mostly_unique, none_or_default_func),
            FinalInst = ground(mostly_clobbered, none_or_default_func)
        then
            Pieces = [fixed("mdi")]
        else if
            InitInst = ground(mostly_unique, none_or_default_func),
            FinalInst = ground(mostly_unique, none_or_default_func)
        then
            Pieces = [fixed("mui")]
        else if
            InitInst = free,
            FinalInst = ground(mostly_unique, none_or_default_func)
        then
            Pieces = [fixed("muo")]
        else
            InitPieces = inst_to_pieces(Info, Expansions0,
                InitInst, []),
            FinalPieces = inst_to_pieces(Info, Expansions0,
                FinalInst, []),
            Pieces = InitPieces ++ [fixed(">>") | FinalPieces]
        )
    ;
        Mode = user_defined_mode(ModeName, ArgInsts),
        BaseModeName = unqualify_name(ModeName),
        (
            ArgInsts = [],
            Pieces = [fixed(BaseModeName)]
        ;
            ArgInsts = [_ | _],
            ArgInstPieces = list.map(
                arg_inst_to_pieces(Info, Expansions0),
                ArgInsts),
            Pieces =
                [prefix(BaseModeName ++ "(") |
                    strict_component_lists_to_pieces(ArgInstPieces)] ++
                    [suffix(")")]
        )
    ).

:- func mode_to_inline_pieces(inst_msg_info, set(inst_name),
    mer_mode) = list(format_component).

mode_to_inline_pieces(Info, Expansions0, Mode) = Pieces :-
    (
        Mode = from_to_mode(InitInst0, FinalInst0),
        % XXX We should strip these wrappers everywhere in both insts,
        % not just at the top.
        ( if InitInst0 = defined_inst(typed_inst(_, SubInitInstName)) then
            InitInst = defined_inst(SubInitInstName)
        else
            InitInst = InitInst0
        ),
        ( if FinalInst0 = defined_inst(typed_inst(_, SubFinalInstName)) then
            FinalInst = defined_inst(SubFinalInstName)
        else
            FinalInst = FinalInst0
        ),
        % XXX Should we special case the expanded versions of other
        % "builtin" modes?
        % XXX Should we special case the situation where InitInst = FinalInst?
        ( if
            InitInst = ground(shared, none_or_default_func),
            FinalInst = ground(shared, none_or_default_func)
        then
            Pieces = [fixed("in")]
        else if
            InitInst = ground(unique, none_or_default_func),
            FinalInst = ground(clobbered, none_or_default_func)
        then
            Pieces = [fixed("di")]
        else if
            InitInst = free,
            FinalInst = ground(shared, none_or_default_func)
        then
            Pieces = [fixed("out")]
        else if
            InitInst = free,
            FinalInst = ground(unique, none_or_default_func)
        then
            Pieces = [fixed("uo")]
        else
            InitPieces = inst_to_inline_pieces(Info,
                Expansions0, InitInst, []),
            FinalPieces = inst_to_inline_pieces(Info,
                Expansions0, FinalInst, []),
            Pieces = InitPieces ++ [fixed(">>") | FinalPieces]
        )
    ;
        Mode = user_defined_mode(ModeName, ArgInsts),
        BaseModeName = unqualify_name(ModeName),
        (
            ArgInsts = [],
            Pieces = [fixed(BaseModeName)]
        ;
            ArgInsts = [_ | _],
            ArgInstPieces = list.map(
                arg_inst_to_inline_pieces(Info, Expansions0),
                ArgInsts),
            Pieces =
                [prefix(BaseModeName ++ "(") |
                    strict_component_lists_to_pieces(ArgInstPieces)] ++
                    [suffix(")")]
        )
    ).

:- func arg_inst_to_pieces(inst_msg_info, set(inst_name), mer_inst)
    = list(format_component).

arg_inst_to_pieces(Info, Expansions0, Inst) =
    inst_to_pieces(Info, Expansions0, Inst, []).

:- func arg_inst_to_inline_pieces(inst_msg_info, set(inst_name),
    mer_inst) = list(format_component).

arg_inst_to_inline_pieces(Info, Expansions0, Inst) =
    inst_to_inline_pieces(Info, Expansions0, Inst, []).

%---------------------------------------------------------------------------%

:- func name_and_arg_insts_to_pieces(inst_msg_info, set(inst_name),
    string, list(mer_inst), list(format_component)) = list(format_component).

name_and_arg_insts_to_pieces(Info, Expansions0, Name,
        ArgInsts, Suffix) = Pieces :-
    (
        ArgInsts = [],
        Pieces = [fixed(Name) | Suffix]
    ;
        ArgInsts = [HeadArgInst | TailArgInsts],
        ArgPieces = insts_to_pieces(Info, Expansions0,
            HeadArgInst, TailArgInsts, [nl_indent_delta(-1)]),
        ( if
            summarize_a_few_arg_insts(ArgPieces, 4, ArgSummary)
        then
            Pieces = [fixed(Name), suffix("(" ++ ArgSummary ++ ")") | Suffix]
        else
            Pieces = [fixed(Name), suffix("("), nl_indent_delta(1)] ++
                ArgPieces ++ [fixed(")") | Suffix]
        )
    ).

:- func name_and_arg_insts_to_inline_pieces(inst_msg_info,
    set(inst_name), string, list(mer_inst), list(format_component))
    = list(format_component).

name_and_arg_insts_to_inline_pieces(Info, Expansions0,
        Name, ArgInsts, Suffix) = Pieces :-
    (
        ArgInsts = [],
        Pieces = [fixed(Name) | Suffix]
    ;
        ArgInsts = [HeadArgInst | TailArgInsts],
        ArgPieces = insts_to_inline_pieces(Info, Expansions0,
            HeadArgInst, TailArgInsts, []),
        ( if
            summarize_a_few_arg_insts(ArgPieces, 4, ArgSummary)
        then
            Pieces = [fixed(Name ++ "(" ++ ArgSummary ++ ")") | Suffix]
        else
            Pieces = [prefix(Name ++ "(") | ArgPieces] ++
                [suffix(")") | Suffix]
        )
    ).

%---------------------------------------------------------------------------%

:- pred summarize_a_few_arg_insts(list(format_component)::in,
    int::in, string::out) is semidet.

summarize_a_few_arg_insts(Pieces, Left, Summary) :-
    Left > 0,
    Pieces = [fixed(FirstFixed) | AfterFirstFixed],
    (
        AfterFirstFixed = [nl_indent_delta(-1)],
        Summary = FirstFixed
    ;
        AfterFirstFixed = [suffix(","), nl |  TailPieces],
        summarize_a_few_arg_insts(TailPieces, Left - 1, TailSummary),
        Summary = FirstFixed ++ ", " ++ TailSummary
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.error_msg_inst.
%---------------------------------------------------------------------------%
