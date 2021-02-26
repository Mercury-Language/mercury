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
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type inst_msg_info
    --->    inst_msg_info(
                imi_module_info         :: module_info,
                imi_inst_varset         :: inst_varset,
                imi_expand_named_insts  :: maybe_expand_named_insts
            ).

    % We record which inst names we have seen before. The main reason why
    % we do this is to prevent infinite loops when expanding inst names
    % whose definitions are recursive, but even in the absence of recursion,
    % printing just the name of a named inst on its second and later
    % occurrences can make the output smaller and easier to read.
    %
:- type expansions_info
    --->    expansions_info(
                % We put every inst name into this map if it can be recursive.
                % The value associated with an inst name is what we should
                % print on any later references to the inst name.
                %
                % XXX Internally generated inst names can be quite big.
                % If comparisons on these inside map lookups ever become
                % a problem, we could make the key not the inst_name,
                % but its address.
                ei_seen_inst_names      :: map(inst_name,
                                            list(format_component)),

                % Inst names generated internally by the compiler don't have
                % intuitively understandable names we can print, so
                % we give these inst names a number. We allocate these
                % using this counter.
                ei_inst_num_counter     :: counter
            ).

error_msg_inst(ModuleInfo, InstVarSet, ExpandNamedInsts,
        ShortInstQF, ShortInstSuffix, LongInstPrefix, LongInstSuffix, Inst0)
        = Pieces :-
    Info = inst_msg_info(ModuleInfo, InstVarSet, ExpandNamedInsts),
    strip_builtin_qualifiers_from_inst(Inst0, Inst),
    Expansions0 = expansions_info(map.init, counter.init(1)),
    SuffixPieces = [],
    inst_to_inline_pieces(Info, Expansions0, _InlineExpansions, Inst,
        SuffixPieces, InlinePieces),
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
        inst_to_pieces(Info, Expansions0, _NonInlineExpansions, Inst,
            LongInstSuffix, NonInlinePieces),
        Pieces = LongInstPrefix ++ NonInlinePieces
    ).

%---------------------------------------------------------------------------%
%
% The following predicates are sort-of duplicated. The predicates whose names
% end in "to_pieces" generate output that put different parts of the inst
% on (mostly) separate lines, showing the structure of the inst through
% indentation. The predicates whose names end in "to_inline_pieces" generate
% output that contains the same components, but all on one line.
%

:- pred inst_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out, mer_inst::in,
    list(format_component)::in, list(format_component)::out) is det.

inst_to_pieces(Info, !Expansions, Inst, Suffix, Pieces) :-
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        Pieces = [fixed("free") | Suffix]
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", unit, "", BoundName),
        (
            BoundInsts = [],
            Pieces = [fixed(BoundName) | Suffix]
        ;
            BoundInsts = [HeadBoundInst | TailBoundInsts],
            bound_insts_to_pieces(Info, !Expansions,
                HeadBoundInst, TailBoundInsts, [], BoundPieces),
            Pieces = [fixed(BoundName), suffix("("), nl_indent_delta(1) |
                BoundPieces] ++ [nl_indent_delta(-1), fixed(")") | Suffix]
        )
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_pieces(Info, !Expansions, "", Uniq,
                PredInstInfo, HOPieces),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", unit, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_pieces(Info, !Expansions, "any_", Uniq,
                PredInstInfo, HOPieces),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, unit, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = inst_var(Var),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_var(InstVarSet, print_name_only, Var, unit, "", Name),
        Pieces = [fixed(Name) | Suffix]
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_vars(InstVarSet, print_name_only,
            set.to_sorted_list(Vars), unit, "", Names),
        inst_to_pieces(Info, !Expansions, ConstrainedInst,
            [fixed(")") | Suffix], InstPieces),
        Pieces = [fixed("("), words(Names), fixed("=<") | InstPieces]
    ;
        Inst = abstract_inst(Name, ArgInsts),
        InstName = user_inst(Name, ArgInsts),
        inst_name_to_pieces(Info, !Expansions, InstName, Suffix, Pieces)
    ;
        Inst = defined_inst(InstName),
        inst_name_to_pieces(Info, !Expansions, InstName, Suffix, Pieces)
    ;
        Inst = not_reached,
        Pieces = [fixed("not_reached") | Suffix]
    ).

:- pred inst_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out, mer_inst::in,
    list(format_component)::in, list(format_component)::out) is det.

inst_to_inline_pieces(Info, !Expansions, Inst, Suffix, Pieces) :-
    (
        ( Inst = free
        ; Inst = free(_)
        ),
        Pieces = [fixed("free") | Suffix]
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", unit, "", BoundName),
        (
            BoundInsts = [],
            Pieces = [fixed(BoundName) | Suffix]
        ;
            BoundInsts = [HeadBoundInst | TailBoundInsts],
            bound_insts_to_inline_pieces(Info, !Expansions,
                HeadBoundInst, TailBoundInsts, [], BoundPieces),
            Pieces = [prefix(BoundName ++ "(") | BoundPieces] ++
                [suffix(")") | Suffix]
        )
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_inline_pieces(Info, !Expansions, "", Uniq,
                PredInstInfo, HOPieces),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", unit, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_inline_pieces(Info, !Expansions, "any_", Uniq,
                PredInstInfo, HOPieces),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, unit, "", Str),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = inst_var(Var),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_var(InstVarSet, print_name_only, Var, unit, "", Name),
        Pieces = [fixed(Name) | Suffix]
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        InstVarSet = Info ^ imi_inst_varset,
        mercury_format_vars(InstVarSet, print_name_only,
            set.to_sorted_list(Vars), unit, "", Names),
        inst_to_inline_pieces(Info, !Expansions, ConstrainedInst,
            [fixed(")") | Suffix], InstPieces),
        Pieces = [fixed("("), words(Names), fixed("=<") | InstPieces]
    ;
        Inst = abstract_inst(Name, ArgInsts),
        InstName = user_inst(Name, ArgInsts),
        inst_name_to_inline_pieces(Info, !Expansions, InstName, Suffix, Pieces)
    ;
        Inst = defined_inst(InstName),
        inst_name_to_inline_pieces(Info, !Expansions, InstName, Suffix, Pieces)
    ;
        Inst = not_reached,
        Pieces = [fixed("not_reached") | Suffix]
    ).

%---------------------------------------------------------------------------%

:- pred insts_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    mer_inst::in, list(mer_inst)::in,
    list(format_component)::in, list(format_component)::out) is det.

insts_to_pieces(Info, !Expansions, HeadInst, TailInsts, Suffix, Pieces) :-
    (
        TailInsts = [],
        HeadSuffix = Suffix
    ;
        TailInsts = [HeadTailInst | TailTailInsts],
        insts_to_pieces(Info, !Expansions, HeadTailInst, TailTailInsts,
            Suffix, TailPieces),
        HeadSuffix = [suffix(","), nl] ++ TailPieces
    ),
    inst_to_pieces(Info, !Expansions, HeadInst, HeadSuffix, Pieces).

:- pred insts_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    mer_inst::in, list(mer_inst)::in,
    list(format_component)::in, list(format_component)::out) is det.

insts_to_inline_pieces(Info, !Expansions, HeadInst, TailInsts, Suffix,
        Pieces) :-
    (
        TailInsts = [],
        HeadSuffix = Suffix
    ;
        TailInsts = [HeadTailInst | TailTailInsts],
        insts_to_inline_pieces(Info, !Expansions, HeadTailInst, TailTailInsts,
            Suffix, TailPieces),
        HeadSuffix = [suffix(",")] ++ TailPieces
    ),
    inst_to_inline_pieces(Info, !Expansions, HeadInst, HeadSuffix, Pieces).

%---------------------------------------------------------------------------%

:- pred bound_insts_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    bound_inst::in, list(bound_inst)::in,
    list(format_component)::in, list(format_component)::out) is det.

bound_insts_to_pieces(Info, !Expansions, HeadBoundInst, TailBoundInsts,
        Suffix, Pieces) :-
    (
        TailBoundInsts = [],
        HeadSuffix = Suffix
    ;
        TailBoundInsts = [HeadTailBoundInst | TailTailBoundInsts],
        bound_insts_to_pieces(Info, !Expansions,
            HeadTailBoundInst, TailTailBoundInsts, Suffix, TailPieces),
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
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    name_and_arg_insts_to_pieces(Info, !Expansions, ConsIdStr, ArgInsts,
        HeadSuffix, Pieces).

:- pred bound_insts_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    bound_inst::in, list(bound_inst)::in,
    list(format_component)::in, list(format_component)::out) is det.

bound_insts_to_inline_pieces(Info, !Expansions, HeadBoundInst, TailBoundInsts,
        Suffix, Pieces) :-
    (
        TailBoundInsts = [],
        HeadSuffix = Suffix
    ;
        TailBoundInsts = [HeadTailBoundInst | TailTailBoundInsts],
        bound_insts_to_inline_pieces(Info, !Expansions,
            HeadTailBoundInst, TailTailBoundInsts, Suffix, TailPieces),
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
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    name_and_arg_insts_to_inline_pieces(Info, !Expansions, ConsIdStr, ArgInsts,
        HeadSuffix, Pieces).

%---------------------------------------------------------------------------%

:- pred inst_name_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out, inst_name::in,
    list(format_component)::in, list(format_component)::out) is det.

inst_name_to_pieces(Info, !Expansions, InstName, Suffix, Pieces) :-
    ( if have_we_expanded_inst_name(!.Expansions, InstName, PastPieces) then
        Pieces = PastPieces
    else
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
            name_and_arg_insts_to_pieces(Info, !Expansions,
                SymNameStr, ArgInsts, [], NamePieces),
            NamedNamePieces = [words("named inst") | NamePieces],
            record_user_inst_name(InstName, NamedNamePieces, !Expansions),

            ExpandInsts = Info ^ imi_expand_named_insts,
            (
                ExpandInsts = dont_expand_named_insts,
                % XXX Would NamedNamePieces look better in the output?
                Pieces = NamePieces ++ Suffix
            ;
                ExpandInsts = expand_named_insts,
                inst_lookup(ModuleInfo, InstName, EqvInst),
                ( if
                    ( EqvInst = defined_inst(InstName)
                    ; EqvInst = abstract_inst(SymName, ArgInsts)
                    )
                then
                    % XXX Would NamePieces look better in the output?
                    Pieces = NamedNamePieces ++ Suffix
                else
                    inst_to_pieces(Info, !Expansions, EqvInst,
                        Suffix, EqvPieces),
                    Pieces = NamedNamePieces ++ [suffix(","), nl,
                        words("which expands to"),
                        nl_indent_delta(1) | EqvPieces] ++
                        [nl_indent_delta(-1)]
                )
            )
        ;
            InstName = typed_inst(_Type, SubInstName),
            % The user doesn't care about the typed_inst wrapper.
            inst_name_to_pieces(Info, !Expansions, SubInstName, Suffix, Pieces)
        ;
            InstName = typed_ground(Uniq, _Type),
            % The user doesn't care about the typed_ground wrapper.
            EqvInst = ground(Uniq, none_or_default_func),
            inst_to_pieces(Info, !Expansions, EqvInst, Suffix, Pieces)
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
                record_internal_inst_name(InstName, InstNameStr, InstNumPieces,
                    !Expansions),
                inst_to_inline_pieces(Info, !Expansions, EqvInst, Suffix,
                    EqvPieces),
                Pieces = InstNumPieces ++ [suffix(","), nl,
                    words("which expands to"),
                    nl_indent_delta(1) | EqvPieces] ++ [nl_indent_delta(-1)]
            )
        )
    ).

:- pred inst_name_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out, inst_name::in,
    list(format_component)::in, list(format_component)::out) is det.

inst_name_to_inline_pieces(Info, !Expansions, InstName, Suffix, Pieces) :-
    ( if have_we_expanded_inst_name(!.Expansions, InstName, PastPieces) then
        Pieces = PastPieces
    else
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
            name_and_arg_insts_to_inline_pieces(Info, !Expansions,
                SymNameStr, ArgInsts, [], NamePieces),
            NamedNamePieces = [words("named inst") | NamePieces],
            record_user_inst_name(InstName, NamedNamePieces, !Expansions),

            ExpandInsts = Info ^ imi_expand_named_insts,
            (
                ExpandInsts = dont_expand_named_insts,
                % XXX Would NamedNamePieces look better in the output?
                Pieces = NamePieces ++ Suffix
            ;
                ExpandInsts = expand_named_insts,
                inst_lookup(ModuleInfo, InstName, EqvInst),
                ( if
                    ( EqvInst = defined_inst(InstName)
                    ; EqvInst = abstract_inst(SymName, ArgInsts)
                    )
                then
                    % XXX Would NamePieces look better in the output?
                    Pieces = NamedNamePieces ++ Suffix
                else
                    inst_to_inline_pieces(Info, !Expansions, EqvInst,
                        Suffix, ExpandedPieces),
                    Pieces = NamedNamePieces ++ [suffix(","),
                        words("which expands to"),
                        prefix("<") | ExpandedPieces] ++ [suffix(">")]
                )
            )
        ;
            InstName = typed_inst(_Type, SubInstName),
            % The user doesn't care about the typed_inst wrapper,
            % and the wrapper cannot make an inst recursive.
            inst_name_to_inline_pieces(Info, !Expansions, SubInstName, Suffix,
                Pieces)
        ;
            InstName = typed_ground(Uniq, _Type),
            % The user doesn't care about the typed_ground wrapper,
            % and the wrapper cannot make an inst recursive.
            EqvInst = ground(Uniq, none_or_default_func),
            inst_to_inline_pieces(Info, !Expansions, EqvInst, Suffix, Pieces)
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
                record_internal_inst_name(InstName, InstNameStr, InstNumPieces,
                    !Expansions),
                inst_to_inline_pieces(Info, !Expansions, EqvInst, Suffix,
                    EqvPieces),
                Pieces = InstNumPieces ++ [suffix(","),
                    words("which expands to"),
                    prefix("<") | EqvPieces] ++ [suffix(">")]
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred have_we_expanded_inst_name(expansions_info::in, inst_name::in,
    list(format_component)::out) is semidet.

have_we_expanded_inst_name(Expansions, InstName, PastPieces) :-
    Expansions = expansions_info(ExpansionsMap, _),
    map.search(ExpansionsMap, InstName, PastPieces).

:- pred record_user_inst_name(inst_name::in, list(format_component)::in,
    expansions_info::in, expansions_info::out) is det.

record_user_inst_name(InstName, Pieces, !Expansions) :-
    !.Expansions = expansions_info(ExpansionsMap0, ExpansionsCounter0),
    ( if map.insert(InstName, Pieces, ExpansionsMap0, ExpansionsMap) then
        !:Expansions = expansions_info(ExpansionsMap, ExpansionsCounter0)
    else
        % An inst_name IN may occur as its own argument, like this:
        %
        %   IN(..., IN(...), ...)
        %
        % By the time our caller calls us for the outer occurrence of IN,
        % the code handling an inner occurrence may have already added IN
        % to !Expansions.
        true
    ).

:- pred record_internal_inst_name(inst_name::in, string::in,
    list(format_component)::out,
    expansions_info::in, expansions_info::out) is det.

record_internal_inst_name(InstName, InstNameStr, InstNumPieces, !Expansions) :-
    !.Expansions = expansions_info(ExpansionsMap0, ExpansionsCounter0),
    counter.allocate(InstNum, ExpansionsCounter0, ExpansionsCounter),
    InstNameNumStr = "internal " ++ InstNameStr ++
        " #" ++ int_to_string(InstNum),
    InstNumPieces = [fixed(InstNameNumStr)],
    map.det_insert(InstName, InstNumPieces, ExpansionsMap0, ExpansionsMap),
    !:Expansions = expansions_info(ExpansionsMap, ExpansionsCounter).

%---------------------------------------------------------------------------%

:- pred pred_inst_info_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    string::in, uniqueness::in, pred_inst_info::in,
    list(format_component)::out) is det.

pred_inst_info_to_pieces(Info, !Expansions, AnyPrefix, Uniq,
        PredInstInfo, Pieces) :-
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
        mercury_format_uniqueness(Uniq, "ground", unit, "", BoundName),
        UniqPieces = [fixed("/*"), fixed(BoundName), fixed("*/")]
    ),
    modes_to_pieces(Info, !Expansions, ArgModes, ArgModesPieces),
    mercury_format_det(Det, unit, "is ", IsDetStr),
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

:- pred pred_inst_info_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    string::in, uniqueness::in, pred_inst_info::in,
    list(format_component)::out) is det.

pred_inst_info_to_inline_pieces(Info, !Expansions, AnyPrefix, Uniq,
        PredInstInfo, Pieces) :-
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
        mercury_format_uniqueness(Uniq, "ground", unit, "", BoundName),
        UniqPieces = [fixed("/*"), fixed(BoundName), fixed("*/")]
    ),
    modes_to_inline_pieces(Info, !Expansions, ArgModes, ArgModesPieces),
    mercury_format_det(Det, unit, "is ", IsDetStr),
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

%---------------------%

:- pred modes_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    list(mer_mode)::in, list(list(format_component))::out) is det.

modes_to_pieces(_Info, !Expansions, [], []).
modes_to_pieces(Info, !Expansions, [HeadMode | TailModes],
        [HeadPieces | TailPieces]) :-
    mode_to_pieces(Info, !Expansions, HeadMode, HeadPieces),
    modes_to_pieces(Info, !Expansions, TailModes, TailPieces).

:- pred modes_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    list(mer_mode)::in, list(list(format_component))::out) is det.

modes_to_inline_pieces(_Info, !Expansions, [], []).
modes_to_inline_pieces(Info, !Expansions, [HeadMode | TailModes],
        [HeadPieces | TailPieces]) :-
    mode_to_inline_pieces(Info, !Expansions, HeadMode, HeadPieces),
    modes_to_inline_pieces(Info, !Expansions, TailModes, TailPieces).

%---------------------%

:- pred mode_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    mer_mode::in, list(format_component)::out) is det.

mode_to_pieces(Info, !Expansions, Mode0, Pieces) :-
    strip_typed_insts_from_mode(Mode0, Mode1),
    (
        Mode1 = from_to_mode(FromInst1, ToInst1),
        insts_to_mode(FromInst1, ToInst1, Mode),
        (
            Mode = from_to_mode(FromInst, ToInst),
            inst_to_pieces(Info, !Expansions, FromInst, [], FromPieces),
            inst_to_pieces(Info, !Expansions, ToInst, [], ToPieces),
            Pieces = FromPieces ++ [fixed(">>") | ToPieces]
        ;
            Mode = user_defined_mode(ModeName, ArgInsts),
            user_defined_mode_to_pieces(Info, !Expansions, ModeName, ArgInsts,
                Pieces)
        )
    ;
        Mode1 = user_defined_mode(ModeName, ArgInsts),
        user_defined_mode_to_pieces(Info, !Expansions, ModeName, ArgInsts,
            Pieces)
    ).

%---------------------%

:- pred mode_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    mer_mode::in, list(format_component)::out) is det.

mode_to_inline_pieces(Info, !Expansions, Mode0, Pieces) :-
    strip_typed_insts_from_mode(Mode0, Mode1),
    (
        Mode1 = from_to_mode(FromInst1, ToInst1),
        insts_to_mode(FromInst1, ToInst1, Mode),
        (
            Mode = from_to_mode(FromInst, ToInst),
            inst_to_inline_pieces(Info, !Expansions, FromInst, [], FromPieces),
            inst_to_inline_pieces(Info, !Expansions, ToInst, [], ToPieces),
            Pieces = FromPieces ++ [fixed(">>") | ToPieces]
        ;
            Mode = user_defined_mode(ModeName, ArgInsts),
            user_defined_mode_to_inline_pieces(Info, !Expansions,
                ModeName, ArgInsts, Pieces)
        )
    ;
        Mode1 = user_defined_mode(ModeName, ArgInsts),
        user_defined_mode_to_inline_pieces(Info, !Expansions,
            ModeName, ArgInsts, Pieces)
    ).

%---------------------%

:- pred user_defined_mode_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    sym_name::in, list(mer_inst)::in, list(format_component)::out) is det.

user_defined_mode_to_pieces(Info, !Expansions, ModeName, ArgInsts, Pieces) :-
    BaseModeName = unqualify_name(ModeName),
    (
        ArgInsts = [],
        Pieces = [fixed(BaseModeName)]
    ;
        ArgInsts = [_ | _],
        arg_insts_to_pieces(Info, !Expansions, ArgInsts, ArgInstPieces),
        Pieces =
            [prefix(BaseModeName ++ "(") |
                strict_component_lists_to_pieces(ArgInstPieces)] ++
                [suffix(")")]
    ).

:- pred user_defined_mode_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    sym_name::in, list(mer_inst)::in, list(format_component)::out) is det.

user_defined_mode_to_inline_pieces(Info, !Expansions, ModeName, ArgInsts,
        Pieces) :-
    BaseModeName = unqualify_name(ModeName),
    (
        ArgInsts = [],
        Pieces = [fixed(BaseModeName)]
    ;
        ArgInsts = [_ | _],
        arg_insts_to_inline_pieces(Info, !Expansions,
            ArgInsts, ArgInstPieces),
        Pieces =
            [prefix(BaseModeName ++ "(") |
                strict_component_lists_to_pieces(ArgInstPieces)] ++
                [suffix(")")]
    ).

%---------------------%

:- pred arg_insts_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    list(mer_inst)::in, list(list(format_component))::out) is det.

arg_insts_to_pieces(_Info, !Expansions, [], []).
arg_insts_to_pieces(Info, !Expansions, [HeadArgInst | TailArgInsts],
        [HeadPieces | TailPieces]) :-
    inst_to_pieces(Info, !Expansions, HeadArgInst, [], HeadPieces),
    arg_insts_to_pieces(Info, !Expansions, TailArgInsts, TailPieces).

:- pred arg_insts_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    list(mer_inst)::in, list(list(format_component))::out) is det.

arg_insts_to_inline_pieces(_Info, !Expansions, [], []).
arg_insts_to_inline_pieces(Info, !Expansions, [HeadArgInst | TailArgInsts],
        [HeadPieces | TailPieces]) :-
    inst_to_inline_pieces(Info, !Expansions, HeadArgInst, [], HeadPieces),
    arg_insts_to_inline_pieces(Info, !Expansions, TailArgInsts, TailPieces).

%---------------------%

:- pred name_and_arg_insts_to_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    string::in, list(mer_inst)::in, list(format_component)::in,
    list(format_component)::out) is det.

name_and_arg_insts_to_pieces(Info, !Expansions, Name, ArgInsts, Suffix,
        Pieces) :-
    (
        ArgInsts = [],
        Pieces = [fixed(Name) | Suffix]
    ;
        ArgInsts = [HeadArgInst | TailArgInsts],
        insts_to_pieces(Info, !Expansions, HeadArgInst, TailArgInsts,
            [], ArgPieces),
        ( if summarize_a_few_arg_insts(ArgPieces, 4, ArgSummary) then
            Pieces = [fixed(Name), suffix("(" ++ ArgSummary ++ ")") | Suffix]
        else
            Pieces = [fixed(Name), suffix("("), nl_indent_delta(1)] ++
                ArgPieces ++ [nl_indent_delta(-1), fixed(")") | Suffix]
        )
    ).

:- pred name_and_arg_insts_to_inline_pieces(inst_msg_info::in,
    expansions_info::in, expansions_info::out,
    string::in, list(mer_inst)::in, list(format_component)::in,
    list(format_component)::out) is det.

name_and_arg_insts_to_inline_pieces(Info, !Expansions, Name, ArgInsts, Suffix,
        Pieces) :-
    (
        ArgInsts = [],
        Pieces = [fixed(Name) | Suffix]
    ;
        ArgInsts = [HeadArgInst | TailArgInsts],
        insts_to_inline_pieces(Info, !Expansions, HeadArgInst, TailArgInsts,
            [], ArgPieces),
        ( if summarize_a_few_arg_insts(ArgPieces, 4, ArgSummary) then
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
        AfterFirstFixed = [],
        Summary = FirstFixed
    ;
        AfterFirstFixed = [suffix(","), nl |  TailPieces],
        summarize_a_few_arg_insts(TailPieces, Left - 1, TailSummary),
        Summary = FirstFixed ++ ", " ++ TailSummary
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.error_msg_inst.
%---------------------------------------------------------------------------%
