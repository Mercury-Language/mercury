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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type maybe_expand_named_insts
    --->    dont_expand_named_insts
    ;       expand_named_insts.

:- type user_or_developer
    --->    uod_user
    ;       uod_developer(tvarset).

:- type short_inst
    --->    quote_short_inst
    ;       fixed_short_inst.

    % error_msg_inst(ModuleInfo, InstVarSet, ExpandNamedInsts, UserOrDeveloper,
    %   QuoteShortInst, ShortInstSuffix, LongInstPrefix, LongInstSuffix,
    %   Inst0) = Pieces:
    %
    % Format Inst0 for use in an error message, in a short form that fits at
    % the end of the current line if possible, and in a long form that starts
    % on a separate line, if it is not.
    %
    % When using the short form, put the inst's text representation into quotes
    % if QuoteShortInst = quote_short_inst. Don't put anything before it
    % (our caller will do that), but add ShortInstSuffix after it. Normally,
    % ShortInstSuffix will end with either nl or nl_indent_delta.
    %
    % When using the long form, leave the inst's text representation as is,
    % without quotations, put LongInstPrefix before it, and LongInstSuffix
    % after it. Normally, LongInstPrefix will start with nl or nl_indent_delta
    % to start the inst on a new line, and LongInstSuffix will end with nl
    % or nl_indent_delta as well. (The second nl_indent_delta will usually
    % undo the effect of the first.)
    %
:- func error_msg_inst(module_info, inst_varset, maybe_expand_named_insts,
    user_or_developer, short_inst, list(format_piece),
    list(format_piece), list(format_piece), mer_inst) = list(format_piece).

    % Do the same job as error_msg_inst, but for inst names.
    %
:- func error_msg_inst_name(module_info, inst_varset, maybe_expand_named_insts,
    user_or_developer, short_inst, list(format_piece),
    list(format_piece), list(format_piece), inst_name) = list(format_piece).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_lookup.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.write_error_spec.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- type inst_msg_info
    --->    inst_msg_info(
                imi_module_info         :: module_info,
                imi_inst_varset         :: inst_varset,
                imi_named_insts         :: maybe_expand_named_insts,
                imi_audience            :: user_or_developer
            ).

:- type maybe_inline_pieces
    --->    multi_line_pieces
    ;       inline_pieces.

:- inst multi_line_pieces for maybe_inline_pieces/0
    --->    multi_line_pieces.
:- inst inline_pieces for maybe_inline_pieces/0
    --->    inline_pieces.

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
                                            list(format_piece)),

                % Inst names generated internally by the compiler don't have
                % intuitively understandable names we can print, so
                % we give these inst names a number. We allocate these
                % using this counter.
                ei_inst_num_counter     :: counter
            ).

error_msg_inst(ModuleInfo, InstVarSet, ExpandNamedInsts, UserOrDeveloper,
        QuoteShortInst, ShortInstSuffix, LongInstPrefix, LongInstSuffix,
        Inst) = Pieces :-
    Info = inst_msg_info(ModuleInfo, InstVarSet, ExpandNamedInsts,
        UserOrDeveloper),
    % We used to call strip_module_names_from_inst to strip builtin
    % module names from all parts of the inst we are asked to convert
    % to Pieces. This worked well when we used the code in this module
    % only for generating descriptions of insts for error messages.
    % However, it became a problem when we
    %
    % - started using this code to describe insts in HLDS dumps, *and*
    % - we modified inst_lookup.m to report inst names that were effectively
    %   *dangling references*, i.e. references to an entries in the inst tables
    %   that weren't actually there.
    %
    % The sequence of events that led to the problem was as follows.
    %
    % - Mode analysis creates an entry in the ground inst table. Both the key
    %   and the value of this entry contains the same higher order inst, and
    %   the modes of the arguments of the pred_inst_info have the form
    %
    %       user_defined_mode(qualified(unqualified("builtin"), "in"), [])
    %
    % - When printing the value in the ground table entry in a HLDS dump,
    %   the call to strip_module_names_from_inst replaces all the argument
    %   modes in that pred_inst_info with modes of the form
    %
    %       user_defined_mode(unqualified("in"), [])
    %
    % - Execution continues to the call to inst_to_pieces below, which calls
    %   inst_name_to_pieces, which calls inst_lookup_debug. The difference
    %   in module qualification then causes the search of the ground inst
    %   table there to fail, which in turn causes inst_lookup_debug to return
    %   an inst constructed by make_missing_inst_name, which will then contain
    %   "MISSING_INST" in its name.
    %
    % - The occurence of this string in the HLDS dump is *supposed* to mean
    %   that the process of *constructing* the inst has a bug, but in this case
    %   it is the process of *printing* the inst that has a bug. This can be
    %   quite misleading.
    %
    % It was misleading enough to cause me, zs, to add a significant amount
    % of code to help me track down what I thought was a problem in inst
    % construction, only to find that the problem was in code I previously
    % added to help find another bug :-) I don't want this to happen again
    % in the future, so from now on.
    %
    % The general approach of the fix is two-fold.
    %
    % - First, when we generate Pieces for users, we want to strip any
    %   module qualifiers for builtin modules from insts only *after*
    %   we have called inst_lookup_debug on those insts. This should prevent
    %   the problem above.
    %
    % - Second, for insts and inst names that contain other insts or inst
    %   names, we want to strip away module qualifiers for builtin modules
    %   only from the parts of the inst or inst name that are *outside*
    %   the contained insts or inst names. We want to leave the contained
    %   insts and inst names intact, because if we didn't, the original
    %   problem could still occur, just for the contained insts/inst names.
    %   Any builtin module qualifiers in those contained insts/inst names
    %   would still get stripped away later, when we get to process *them*.
    Expansions0 = expansions_info(map.init, counter.init(1)),
    InlineSuffixPieces = [],
    inst_to_pieces(Info, inline_pieces, Inst, InlineSuffixPieces,
        InlinePieces, Expansions0, _InlineExpansions),
    InlineStr = error_pieces_to_one_line_string(InlinePieces),
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
            QuoteShortInst = quote_short_inst,
            % An inst that is shown on the same line as English text needs
            % something to visually separate it from the English text.
            % The quotes provide that separation.
            %
            % Without a small length limit, we would use words_quote
            % instead of quote to wrap InlineStr.
            InlinePiece = quote(InlineStr)
        ;
            QuoteShortInst = fixed_short_inst,
            % Our caller has told us that it ensured this separation already.
            InlinePiece = fixed(InlineStr)
        ),
        Pieces = [InlinePiece | ShortInstSuffix]
    else
        % Showing the inst on a separate line from the English text
        % provides enough separation by itself.
        inst_to_pieces(Info, multi_line_pieces, Inst, LongInstSuffix,
            MultiLinePieces, Expansions0, _MultiLineExpansions),
        Pieces = LongInstPrefix ++ MultiLinePieces
    ).

%---------------------%

error_msg_inst_name(ModuleInfo, InstVarSet, ExpandNamedInsts, UserOrDeveloper,
        QuoteShortInst, ShortInstSuffix, LongInstPrefix, LongInstSuffix,
        InstName) = Pieces :-
    Info = inst_msg_info(ModuleInfo, InstVarSet, ExpandNamedInsts,
        UserOrDeveloper),
    Expansions0 = expansions_info(map.init, counter.init(1)),
    InlineSuffixPieces = [],
    inst_name_to_pieces(Info, inline_pieces, InstName, InlineSuffixPieces,
        InlinePieces, Expansions0, _InlineExpansions),
    InlineStr = error_pieces_to_one_line_string(InlinePieces),
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
            QuoteShortInst = quote_short_inst,
            % An inst that is shown on the same line as English text needs
            % something to visually separate it from the English text.
            % The quotes provide that separation.
            %
            % Without a small length limit, we would use words_quote
            % instead of quote to wrap InlineStr.
            InlinePiece = quote(InlineStr)
        ;
            % Our caller has told us that it ensured this separation already.
            QuoteShortInst = fixed_short_inst,
            InlinePiece = fixed(InlineStr)
        ),
        Pieces = [InlinePiece | ShortInstSuffix]
    else
        % Showing the inst on a separate line from the English text
        % provides enough separation by itself.
        inst_name_to_pieces(Info, multi_line_pieces, InstName, LongInstSuffix,
            MultiLinePieces, Expansions0, _MultiLineExpansions),
        Pieces = LongInstPrefix ++ MultiLinePieces
    ).

%---------------------------------------------------------------------------%
%
% The following predicates are sort-of duplicated. The predicates whose names
% end in "to_pieces" generate output that put different parts of the inst
% on (mostly) separate lines, showing the structure of the inst through
% indentation. The predicates whose names end in "to_inline_pieces" generate
% output that contains the same components, but all on one line.
%

:- pred inst_to_pieces(inst_msg_info, maybe_inline_pieces, mer_inst,
    list(format_piece), list(format_piece), expansions_info, expansions_info).
:- mode inst_to_pieces(in, in(multi_line_pieces),
    in, in, out, in, out) is det.
:- mode inst_to_pieces(in, in(inline_pieces),
    in, in, out, in, out) is det.

inst_to_pieces(Info, MaybeInline, Inst, Suffix, Pieces, !Expansions) :-
    (
        Inst = free,
        Pieces = [fixed("free") | Suffix]
    ;
        Inst = bound(Uniq, _, BoundInsts),
        BoundName = mercury_uniqueness_to_string(Uniq, "bound"),
        (
            BoundInsts = [],
            Pieces = [fixed(BoundName) | Suffix]
        ;
            BoundInsts = [HeadBoundInst | TailBoundInsts],
            bound_insts_to_pieces(Info, MaybeInline,
                HeadBoundInst, TailBoundInsts, [], BoundPieces, !Expansions),
            (
                MaybeInline = multi_line_pieces,
                Pieces = [fixed(BoundName ++ "("), nl_indent_delta(1) |
                    BoundPieces] ++ [nl_indent_delta(-1), fixed(")") | Suffix]
            ;
                MaybeInline = inline_pieces,
                Pieces = [prefix(BoundName ++ "(") | BoundPieces] ++
                    [suffix(")") | Suffix]
            )
        )
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_pieces(Info, MaybeInline, "", Uniq,
                PredInstInfo, HOPieces, !Expansions),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            Str = mercury_uniqueness_to_string(Uniq, "ground"),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            pred_inst_info_to_pieces(Info, MaybeInline, "any_", Uniq,
                PredInstInfo, HOPieces, !Expansions),
            Pieces = HOPieces ++ Suffix
        ;
            HOInstInfo = none_or_default_func,
            Str = mercury_any_uniqueness_to_string(Uniq),
            Pieces = [fixed(Str) | Suffix]
        )
    ;
        Inst = inst_var(Var),
        InstVarSet = Info ^ imi_inst_varset,
        Name = mercury_var_to_string_vs(InstVarSet, print_name_only, Var),
        Pieces = [fixed(Name) | Suffix]
    ;
        Inst = constrained_inst_vars(Vars, SubInst),
        InstVarSet = Info ^ imi_inst_varset,
        Names = mercury_vars_to_string_vs(InstVarSet, print_name_only,
            set.to_sorted_list(Vars)),
        inst_to_pieces(Info, MaybeInline, SubInst, [], SubInstPieces,
            !Expansions),
        Pieces = [fixed("("), words(Names), fixed("=<") | SubInstPieces] ++
            [fixed(")") | Suffix]
    ;
        Inst = defined_inst(InstName),
        inst_name_to_pieces(Info, MaybeInline, InstName, Suffix, Pieces,
            !Expansions)
    ;
        Inst = not_reached,
        Pieces = [fixed("not_reached") | Suffix]
    ).

%---------------------------------------------------------------------------%

:- pred insts_to_pieces(inst_msg_info, maybe_inline_pieces,
    mer_inst, list(mer_inst), list(format_piece), list(format_piece),
    expansions_info, expansions_info).
:- mode insts_to_pieces(in, in(multi_line_pieces),
    in, in, in, out, in, out) is det.
:- mode insts_to_pieces(in, in(inline_pieces),
    in, in, in, out, in, out) is det.

insts_to_pieces(Info, MaybeInline, HeadInst, TailInsts, Suffix, Pieces,
        !Expansions) :-
    (
        TailInsts = [],
        HeadSuffix = Suffix,
        inst_to_pieces(Info, MaybeInline, HeadInst, HeadSuffix,
            Pieces, !Expansions)
    ;
        TailInsts = [HeadTailInst | TailTailInsts],
        (
            MaybeInline = multi_line_pieces,
            HeadSuffix = [suffix(","), nl]
        ;
            MaybeInline = inline_pieces,
            HeadSuffix = [suffix(",")]
        ),
        inst_to_pieces(Info, MaybeInline, HeadInst, HeadSuffix,
            HeadPieces, !Expansions),
        insts_to_pieces(Info, MaybeInline, HeadTailInst, TailTailInsts, Suffix,
            TailPieces, !Expansions),
        Pieces = HeadPieces ++ TailPieces
    ).

%---------------------------------------------------------------------------%

:- pred bound_insts_to_pieces(inst_msg_info, maybe_inline_pieces,
    bound_inst, list(bound_inst), list(format_piece), list(format_piece),
    expansions_info, expansions_info).
:- mode bound_insts_to_pieces(in, in(multi_line_pieces),
    in, in, in, out, in, out) is det.
:- mode bound_insts_to_pieces(in, in(inline_pieces),
    in, in, in, out, in, out) is det.

bound_insts_to_pieces(Info, MaybeInline, HeadBoundInst, TailBoundInsts,
        Suffix, Pieces, !Expansions) :-
    (
        TailBoundInsts = [],
        HeadSuffix = Suffix
    ;
        TailBoundInsts = [HeadTailBoundInst | TailTailBoundInsts],
        bound_insts_to_pieces(Info, MaybeInline,
            HeadTailBoundInst, TailTailBoundInsts, Suffix, TailPieces,
            !Expansions),
        (
            MaybeInline = multi_line_pieces,
            HeadSuffix = [nl_indent_delta(-1), fixed(";"), nl_indent_delta(1) |
                TailPieces]
        ;
            MaybeInline = inline_pieces,
            HeadSuffix = [fixed(";") | TailPieces]
        )
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
    name_and_arg_insts_to_pieces(Info, MaybeInline, ConsIdStr, ArgInsts,
        HeadSuffix, Pieces, !Expansions).

%---------------------------------------------------------------------------%

:- pred inst_name_to_pieces(inst_msg_info, maybe_inline_pieces, inst_name,
    list(format_piece), list(format_piece), expansions_info, expansions_info).
:- mode inst_name_to_pieces(in, in(multi_line_pieces),
    in, in, out, in, out) is det.
:- mode inst_name_to_pieces(in, in(inline_pieces),
    in, in, out, in, out) is det.

inst_name_to_pieces(Info, MaybeInline, InstName, Suffix, Pieces,
        !Expansions) :-
    ( if have_we_expanded_inst_name(!.Expansions, InstName, PastPieces) then
        Pieces = PastPieces ++ Suffix
    else
        (
            InstName = user_inst(SymName, ArgInsts),
            sym_name_to_min_qual_string(Info, SymName, SymNameStr),
            % XXX If ArgInsts contains named user insts, expanding them
            % inside the name of another inst is far from ideal. However,
            % leaving them to be expanded in the call to inst_to_pieces
            % on EqvInst below also has a problem. This is that we construct
            % lists of insts, such as the (expansion of ArgInsts in EqvInst)
            % in a back-to-front order, which means that if a named inst
            % occurs in EqvInst more than once, it will be the *last*
            % occurrence, not the first, which will be expanded.
            NameInfo = Info ^ imi_named_insts := dont_expand_named_insts,
            ModuleInfo = Info ^ imi_module_info,
            module_info_get_inst_table(ModuleInfo, InstTable),
            inst_table_get_user_insts(InstTable, UserInstTable),
            list.length(ArgInsts, Arity),
            InstCtor = inst_ctor(SymName, Arity),
            ( if
                is_unknown_or_missing_user_inst_name(InstName)
            then
                name_and_arg_insts_to_pieces(NameInfo, MaybeInline,
                    SymNameStr, ArgInsts, Suffix, Pieces, !.Expansions, _)
            else if
                map.search(UserInstTable, InstCtor, InstDefn)
            then
                name_and_arg_insts_to_pieces(NameInfo, MaybeInline,
                    SymNameStr, ArgInsts, [], NamePieces, !.Expansions, _),
                NamedNamePieces = [words("named inst") | NamePieces],
                ExpandInsts = Info ^ imi_named_insts,
                (
                    ExpandInsts = dont_expand_named_insts,
                    Pieces = NamePieces ++ Suffix
                ;
                    ExpandInsts = expand_named_insts,
                    record_user_inst_name(InstName, NamedNamePieces,
                        !Expansions),
                    InstDefn = hlds_inst_defn(_VarSet, Params, InstBody, _MMTC,
                        _Context, _Status),
                    InstBody = eqv_inst(EqvInst0),
                    inst_substitute_arg_list(Params, ArgInsts,
                        EqvInst0, EqvInst),
                    ( if EqvInst = defined_inst(InstName) then
                        % XXX Would NamePieces look better in the output?
                        Pieces = NamedNamePieces ++ Suffix
                    else
                        inst_to_pieces(Info, MaybeInline, EqvInst,
                            Suffix, EqvPieces, !Expansions),
                        (
                            MaybeInline = multi_line_pieces,
                            Pieces = NamedNamePieces ++
                                [nl, words("which expands to"),
                                nl_indent_delta(1) | EqvPieces] ++
                                [nl_indent_delta(-1)]
                        ;
                            MaybeInline = inline_pieces,
                            Pieces = NamedNamePieces ++
                                [words("which expands to"),
                                prefix("<") | EqvPieces] ++ [suffix(">")]
                        )
                    )
                )
            else if
                SymName = unqualified(BaseName),
                Builtin = mercury_public_builtin_module,
                BuiltinInstCtor =
                    inst_ctor(qualified(Builtin, BaseName), Arity),
                map.search(UserInstTable, BuiltinInstCtor, _InstDefn)
            then
                % check_mutable_insts in add_mutable_aux_pred.m removes
                % any module qualifications by mercury_public_builtin_module
                % from the inst name, to signal us that the qualification
                % should not be printed in the error message.
                name_and_arg_insts_to_pieces(NameInfo, MaybeInline,
                    SymNameStr, ArgInsts, [], NamePieces, !.Expansions, _),
                Pieces = NamePieces ++ Suffix
            else
                ( if
                    SymName = qualified(unqualified("FAKE_CONS_ID"),
                        ConsIdName)
                then
                    % mode_error_unify_var_functor_to_spec created InstName,
                    % asking us to treat it just as a wrapper around ArgInsts.
                    name_and_arg_insts_to_pieces(NameInfo, MaybeInline,
                        ConsIdName, ArgInsts, [], NamePieces, !.Expansions, _),
                    Pieces = NamePieces ++ Suffix
                else
                    InstCtorName = sym_name_to_string(SymName),
                    string.format("undefined inst %s/%d",
                        [s(InstCtorName), i(Arity)], Msg),
                    unexpected($pred, Msg)
                )
            )
        ;
            InstName = typed_inst(Type, SubInstName),
            Audience = Info ^ imi_audience,
            (
                Audience = uod_user,
                % The user doesn't care about the typed_inst wrapper,
                % and the wrapper cannot make an inst recursive.
                inst_name_to_pieces(Info, MaybeInline, SubInstName, Suffix,
                    Pieces, !Expansions)
            ;
                Audience = uod_developer(TVarSet),
                InstVarSet = Info ^ imi_inst_varset,
                TypePieces = type_to_pieces(TVarSet, InstVarSet,
                    print_name_and_num, do_not_add_quotes, [], Type),
                inst_name_to_pieces(Info, MaybeInline, SubInstName, [],
                    SubInstNamePieces, !Expansions),
                (
                    MaybeInline = multi_line_pieces,
                    Pieces = [fixed("typed_inst("), nl_indent_delta(1)] ++
                        TypePieces ++ [suffix(","), nl] ++
                        SubInstNamePieces ++
                        [nl_indent_delta(-1), fixed(")") | Suffix]
                ;
                    MaybeInline = inline_pieces,
                    Pieces = [fixed("typed_inst(")] ++
                        TypePieces ++ [suffix(",")] ++
                        SubInstNamePieces ++ [suffix(")") | Suffix]
                )
            )
        ;
            InstName = typed_ground(Uniq, Type),
            Audience = Info ^ imi_audience,
            (
                Audience = uod_user,
                % The user doesn't care about the typed_inst wrapper,
                % and the wrapper cannot make an inst recursive.
                EqvInst = ground(Uniq, none_or_default_func),
                inst_to_pieces(Info, MaybeInline, EqvInst, Suffix,
                    Pieces, !Expansions)
            ;
                Audience = uod_developer(TVarSet),
                InstVarSet = Info ^ imi_inst_varset,
                TypePieces = type_to_pieces(TVarSet, InstVarSet,
                    print_name_and_num, do_not_add_quotes, [], Type),
                UniqStr = inst_uniqueness(Uniq, "shared"),
                (
                    MaybeInline = multi_line_pieces,
                    Pieces = [fixed("typed_ground(" ++ UniqStr ++ ",")] ++
                        [nl_indent_delta(1)] ++ TypePieces ++
                        [nl_indent_delta(-1), fixed(")") | Suffix]
                ;
                    MaybeInline = inline_pieces,
                    Pieces = [fixed("typed_ground(" ++ UniqStr ++ ",")] ++
                        TypePieces ++ [suffix(")") | Suffix]
                )
            )
        ;
            ( InstName = unify_inst(_, _, _, _)
            ; InstName = merge_inst(_, _)
            ; InstName = ground_inst(_, _, _, _)
            ; InstName = any_inst(_, _, _, _)
            ; InstName = shared_inst(_)
            ; InstName = mostly_uniq_inst(_)
            ),
            ModuleInfo = Info ^ imi_module_info,
            % We need to lookup InstName0, NOT InstName. The reason
            % is explained in the big comment in error_msg_inst.
            inst_lookup_debug(ModuleInfo, InstName, EqvInst),
            ( if
                EqvInst = defined_inst(EqvInstName),
                EqvInstName = user_inst(EqvSymName, EqvArgInsts),
                is_unknown_or_missing_user_inst_name(EqvInstName)
            then
                NameInfo = Info ^ imi_named_insts := dont_expand_named_insts,
                sym_name_to_min_qual_string(Info, EqvSymName, EqvSymNameStr),
                name_and_arg_insts_to_pieces(NameInfo, MaybeInline,
                    EqvSymNameStr, EqvArgInsts, Suffix, Pieces,
                    !.Expansions, _)
            else
                Audience = Info ^ imi_audience,
                compiler_key_inst_name_to_dollar_string(InstName, InstNameStr),
                (
                    Audience = uod_user,
                    ( if EqvInst = defined_inst(InstName) then
                        Pieces = [fixed(InstNameStr) | Suffix]
                    else
                        record_internal_inst_name(InstName, InstNameStr,
                            InstNumPieces, !Expansions),
                        inst_to_pieces(Info, MaybeInline, EqvInst, Suffix,
                            EqvPieces, !Expansions),
                        (
                            MaybeInline = multi_line_pieces,
                            Pieces = InstNumPieces ++
                                [nl, words("which expands to"),
                                nl_indent_delta(1) | EqvPieces] ++
                                [nl_indent_delta(-1) | Suffix]
                        ;
                            MaybeInline = inline_pieces,
                            Pieces = InstNumPieces ++
                                [words("which expands to"),
                            prefix("<") | EqvPieces] ++ [suffix(">") | Suffix]
                        )
                    )
                ;
                    Audience = uod_developer(_TVarSet),
                    (
                        (
                            InstName = unify_inst(Live, Real,
                                SubInstA, SubInstB),
                            UnifyOrMerge = "unify",
                            InitialArgs =
                                [suffix(is_live_to_str(Live) ++ ","),
                                fixed(unify_is_real_to_str(Real) ++ ",")]
                        ;
                            InstName = merge_inst(SubInstA, SubInstB),
                            UnifyOrMerge = "merge",
                            InitialArgs = []
                        ),
                        SubSuffixA = [],
                        SubSuffixB = [],
                        inst_to_pieces(Info, MaybeInline, SubInstA, SubSuffixA,
                            SubInstPiecesA, !Expansions),
                        inst_to_pieces(Info, MaybeInline, SubInstB, SubSuffixB,
                            SubInstPiecesB, !Expansions),
                        InstNamePieces = [fixed(UnifyOrMerge ++ "(")] ++
                            InitialArgs ++ [nl_indent_delta(1)] ++
                            SubInstPiecesA ++ [suffix(","), nl] ++
                            SubInstPiecesB ++ [nl_indent_delta(-1), fixed(")")]
                    ;
                        (
                            InstName = ground_inst(SubInstName, Uniq,
                                Live, Real),
                            GroundOrAny = "ground",
                            UniqStr = inst_uniqueness(Uniq, "shared")
                        ;
                            InstName = any_inst(SubInstName, Uniq, Live, Real),
                            GroundOrAny = "any",
                            UniqStr = any_inst_uniqueness(Uniq)
                        ),
                        SubSuffix = [],
                        inst_name_to_pieces(Info, MaybeInline, SubInstName,
                            SubSuffix, SubInstNamePieces, !Expansions),
                        InstNamePieces =
                            [fixed(GroundOrAny ++ "(" ++ UniqStr ++ ","),
                            fixed(is_live_to_str(Live) ++ ","),
                            fixed(unify_is_real_to_str(Real) ++ ","),
                            nl_indent_delta(1)] ++ SubInstNamePieces ++
                            [nl_indent_delta(-1), fixed(")")]
                    ;
                        (
                            InstName = shared_inst(SubInstName),
                            SorMU = "shared"
                        ;
                            InstName = mostly_uniq_inst(SubInstName),
                            SorMU = "mostly_uniq"
                        ),
                        SubSuffix = [],
                        inst_name_to_pieces(Info, MaybeInline, SubInstName,
                            SubSuffix, SubInstNamePieces, !Expansions),
                        InstNamePieces = [fixed(SorMU ++ "("),
                            nl_indent_delta(1)] ++ SubInstNamePieces ++
                            [nl_indent_delta(-1), fixed(")")]
                    ),
                    Pieces = InstNamePieces
                )
            )
        )
    ).

:- inst compiler_key_inst_name for inst_name/0
    --->    unify_inst(ground, ground, ground, ground)
    ;       merge_inst(ground, ground)
    ;       ground_inst(ground, ground, ground, ground)
    ;       any_inst(ground, ground, ground, ground)
    ;       shared_inst(ground)
    ;       mostly_uniq_inst(ground).

:- pred compiler_key_inst_name_to_dollar_string(
    inst_name::in(compiler_key_inst_name), string::out) is det.

compiler_key_inst_name_to_dollar_string(InstName, InstNameStr) :-
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
    ).

:- pred sym_name_to_min_qual_string(inst_msg_info::in,
    sym_name::in, string::out) is det.

sym_name_to_min_qual_string(Info, SymName, SymNameStr) :-
    (
        SymName = qualified(ModuleName, BaseName),
        ModuleInfo = Info ^ imi_module_info,
        module_info_get_name(ModuleInfo, CurModuleName),
        ( if
            ( ModuleName = CurModuleName
            ; ModuleName = mercury_public_builtin_module
            )
        then
            SymNameStr = BaseName
        else
            SymNameStr = sym_name_to_string(SymName)
        )
    ;
        SymName = unqualified(BaseName),
        SymNameStr = BaseName
    ).

%---------------------------------------------------------------------------%

:- pred have_we_expanded_inst_name(expansions_info::in, inst_name::in,
    list(format_piece)::out) is semidet.

have_we_expanded_inst_name(Expansions, InstName, PastPieces) :-
    Expansions = expansions_info(ExpansionsMap, _),
    map.search(ExpansionsMap, InstName, PastPieces).

:- pred record_user_inst_name(inst_name::in, list(format_piece)::in,
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
    list(format_piece)::out,
    expansions_info::in, expansions_info::out) is det.

record_internal_inst_name(InstName, InstNameStr, InstNumPieces, !Expansions) :-
    !.Expansions = expansions_info(ExpansionsMap0, ExpansionsCounter0),
    counter.allocate(InstNum, ExpansionsCounter0, ExpansionsCounter),
    string.format("internal %s #%d", [s(InstNameStr), i(InstNum)],
        InstNameNumStr),
    InstNumPieces = [fixed(InstNameNumStr)],
    map.det_insert(InstName, InstNumPieces, ExpansionsMap0, ExpansionsMap),
    !:Expansions = expansions_info(ExpansionsMap, ExpansionsCounter).

%---------------------------------------------------------------------------%

:- pred pred_inst_info_to_pieces(inst_msg_info, maybe_inline_pieces,
    string, uniqueness, pred_inst_info, list(format_piece),
    expansions_info, expansions_info).
:- mode pred_inst_info_to_pieces(in, in(multi_line_pieces),
    in, in, in, out, in, out) is det.
:- mode pred_inst_info_to_pieces(in, in(inline_pieces),
    in, in, in, out, in, out) is det.

pred_inst_info_to_pieces(Info, MaybeInline, AnyPrefix, Uniq, PredInstInfo,
        Pieces, !Expansions) :-
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
        BoundName = mercury_uniqueness_to_string(Uniq, "ground"),
        UniqPieces = [fixed("/*"), fixed(BoundName), fixed("*/")]
    ),
    modes_to_pieces(Info, MaybeInline, ArgModes, ArgModesPieces, !Expansions),
    IsDetStr = "is " ++ mercury_det_to_string(Det),
    % XXX Should we print each argument mode on a separate line
    % with multi_line_pieces?
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
            RealArgModePieces, RetModePieces),
        % XXX Should we put parentheses around RetModePieces?
        % If it prints as "InitInst >> FinalInst", then the parentheses
        % could make the output easier to read, but if it prints as a
        % simple name, they would probably be just in the way.
        (
            RealArgModePieces = [],
            ModesDetPieces =
                [fixed("(" ++ AnyPrefix ++ "func =") | RetModePieces] ++
                [fixed(IsDetStr), suffix(")")]
        ;
            RealArgModePieces = [_ | _],
            JoinedRealArgModePieces =
                strict_component_lists_to_pieces(RealArgModePieces),
            (
                MaybeInline = multi_line_pieces,
                ModesDetPieces =
                    [fixed("(" ++ AnyPrefix ++ "func("), nl_indent_delta(1) |
                    JoinedRealArgModePieces] ++
                    [nl_indent_delta(-1), fixed(") =") | RetModePieces] ++
                    [fixed(IsDetStr ++ ")")]
            ;
                MaybeInline = inline_pieces,
                ModesDetPieces =
                    [prefix("(" ++ AnyPrefix ++ "func(") |
                    JoinedRealArgModePieces] ++
                    [suffix(") =") | RetModePieces] ++
                    [fixed(IsDetStr ++ ")")]
            )
        )
    ),
    Pieces = UniqPieces ++ ModesDetPieces.

%---------------------%

:- pred modes_to_pieces(inst_msg_info, maybe_inline_pieces,
    list(mer_mode), list(list(format_piece)),
    expansions_info, expansions_info).
:- mode modes_to_pieces(in, in(multi_line_pieces),
    in, out, in, out) is det.
:- mode modes_to_pieces(in, in(inline_pieces),
    in, out, in, out) is det.

modes_to_pieces(_Info, _, [], [], !Expansions).
modes_to_pieces(Info, MaybeInline, [HeadMode | TailModes],
        [HeadPieces | TailPieces], !Expansions) :-
    mode_to_pieces(Info, MaybeInline, HeadMode, HeadPieces, !Expansions),
    modes_to_pieces(Info, MaybeInline, TailModes, TailPieces, !Expansions).

%---------------------%

:- pred mode_to_pieces(inst_msg_info, maybe_inline_pieces,
    mer_mode, list(format_piece), expansions_info, expansions_info).
:- mode mode_to_pieces(in, in(multi_line_pieces), in, out, in, out) is det.
:- mode mode_to_pieces(in, in(inline_pieces), in, out, in, out) is det.

mode_to_pieces(Info, MaybeInline, Mode0, Pieces, !Expansions) :-
    strip_typed_insts_from_mode(Mode0, Mode1),
    (
        Mode1 = from_to_mode(FromInst1, ToInst1),
        insts_to_mode(FromInst1, ToInst1, Mode),
        (
            Mode = from_to_mode(FromInst, ToInst),
            inst_to_pieces(Info, MaybeInline, FromInst, [], FromPieces,
                !Expansions),
            inst_to_pieces(Info, MaybeInline, ToInst, [], ToPieces,
                !Expansions),
            Pieces = FromPieces ++ [fixed(">>") | ToPieces]
        ;
            Mode = user_defined_mode(ModeName, ArgInsts),
            user_defined_mode_to_pieces(Info, MaybeInline, ModeName, ArgInsts,
                Pieces, !Expansions)
        )
    ;
        Mode1 = user_defined_mode(ModeName, ArgInsts),
        user_defined_mode_to_pieces(Info, MaybeInline, ModeName, ArgInsts,
            Pieces, !Expansions)
    ).

%---------------------%

:- pred user_defined_mode_to_pieces(inst_msg_info, maybe_inline_pieces,
    sym_name, list(mer_inst), list(format_piece),
    expansions_info, expansions_info).
:- mode user_defined_mode_to_pieces(in, in(multi_line_pieces),
    in, in, out, in, out) is det.
:- mode user_defined_mode_to_pieces(in, in(inline_pieces),
    in, in, out, in, out) is det.

user_defined_mode_to_pieces(Info, MaybeInline, ModeName, ArgInsts, Pieces,
        !Expansions) :-
    BaseModeName = unqualify_name(ModeName),
    (
        ArgInsts = [],
        Pieces = [fixed(BaseModeName)]
    ;
        ArgInsts = [_ | _],
        arg_insts_to_pieces(Info, MaybeInline, ArgInsts, ArgInstPieces,
            !Expansions),
        Pieces =
            [prefix(BaseModeName ++ "(") |
                strict_component_lists_to_pieces(ArgInstPieces)] ++
                [suffix(")")]
    ).

%---------------------%

:- pred arg_insts_to_pieces(inst_msg_info, maybe_inline_pieces,
    list(mer_inst), list(list(format_piece)),
    expansions_info, expansions_info).
:- mode arg_insts_to_pieces(in, in(multi_line_pieces),
    in, out, in, out) is det.
:- mode arg_insts_to_pieces(in, in(inline_pieces),
    in, out, in, out) is det.

arg_insts_to_pieces(_Info, _, [], [], !Expansions).
arg_insts_to_pieces(Info, MaybeInline, [HeadArgInst | TailArgInsts],
        [HeadPieces | TailPieces], !Expansions) :-
    inst_to_pieces(Info, MaybeInline, HeadArgInst, [], HeadPieces,
        !Expansions),
    arg_insts_to_pieces(Info, MaybeInline, TailArgInsts, TailPieces,
        !Expansions).

%---------------------%

:- pred name_and_arg_insts_to_pieces(inst_msg_info, maybe_inline_pieces,
    string, list(mer_inst), list(format_piece), list(format_piece),
    expansions_info, expansions_info).
:- mode name_and_arg_insts_to_pieces(in, in(multi_line_pieces),
    in, in, in, out, in, out) is det.
:- mode name_and_arg_insts_to_pieces(in, in(inline_pieces),
    in, in, in, out, in, out) is det.

name_and_arg_insts_to_pieces(Info, MaybeInline, Name, ArgInsts, Suffix, Pieces,
        !Expansions) :-
    (
        ArgInsts = [],
        Pieces = [fixed(Name) | Suffix]
    ;
        ArgInsts = [HeadArgInst | TailArgInsts],
        insts_to_pieces(Info, MaybeInline, HeadArgInst, TailArgInsts,
            [], ArgPieces, !Expansions),
        ( if summarize_a_few_arg_insts(ArgPieces, 4, ArgSummary) then
            Pieces = [fixed(Name ++ "(" ++ ArgSummary ++ ")") | Suffix]
        else
            (
                MaybeInline = multi_line_pieces,
                Pieces = [fixed(Name), suffix("("), nl_indent_delta(1)] ++
                    ArgPieces ++ [nl_indent_delta(-1), fixed(")") | Suffix]
            ;
                MaybeInline = inline_pieces,
                Pieces = [prefix(Name ++ "(") | ArgPieces] ++
                    [suffix(")") | Suffix]
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred summarize_a_few_arg_insts(list(format_piece)::in,
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
