%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: livemap.m.
% Main author: zs.
%
% This module builds up a map that gives the set of live lvals at each label.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.livemap.
:- interface.

:- import_module ll_backend.llds.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type livemap == map(label, lvalset).
:- type lvalset == set(lval).

    % Given a list of instructions defining a procedure, return a map
    % giving the set of live non-field lvals at each label.
    %
    % We can compute this set only if the procedure contains no C code.
    %
:- pred build_livemap(list(instruction)::in, maybe(livemap)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.opt_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

build_livemap(Instrs, MaybeLivemap) :-
    % The method we follow is a backward scan of the instruction list,
    % keeping track of the set of live lvals as we go. We update this set
    % at each instruction. When we get to a label, we know that this set
    % of lvals is live at that label.
    %
    % At instructions that can branch away, every lval that is live at
    % any possible target is live before that instruction. Since some
    % branches may be backward branches, we may not have seen the branch
    % target when we process the branch. Therefore we have to repeat the scan,
    % this time with more knowledge about more labels, until we get to
    % a fixpoint.

    map.init(Livemap0),
    list.reverse(Instrs, BackInstrs),
    build_livemap_fixpoint(BackInstrs, Livemap0, 0, MaybeLivemap).

:- pred build_livemap_fixpoint(list(instruction)::in, livemap::in, int::in,
    maybe(livemap)::out) is det.

build_livemap_fixpoint(BackInstrs, Livemap0, CurIteration, MaybeLivemap) :-
    set.init(Livevals0),
    livemap_do_build(BackInstrs, Livevals0, no, ContainsBadUserCode,
        Livemap0, Livemap1),
    (
        ContainsBadUserCode = yes,
        MaybeLivemap = no
    ;
        ContainsBadUserCode = no,
        ( if livemap.equal_livemaps(Livemap0, Livemap1) then
            MaybeLivemap = yes(Livemap1)
        else
            ( if CurIteration < livemap_iteration_limit then
                build_livemap_fixpoint(BackInstrs, Livemap1, CurIteration + 1,
                    MaybeLivemap)
            else
                MaybeLivemap = no
            )
        )
    ).

    % Check whether the two livemaps agree on the set of live lvals
    % at every label. They must agree on the set of labels as well.
    % This is important. Livemap1 will be empty in the first call,
    % so agreement only on the set of labels in Livemap1 is useless.
    % The domain of Livemap2 should always be every label in the procedure.
    % as should the domain of Livemap1 in every call after the first.
    %
:- pred equal_livemaps(livemap::in, livemap::in) is semidet.

equal_livemaps(Livemap1, Livemap2) :-
    map.keys(Livemap1, Labels),
    map.keys(Livemap2, Labels),
    livemap.equal_livemaps_keys(Labels, Livemap1, Livemap2).

:- pred equal_livemaps_keys(list(label)::in, livemap::in, livemap::in)
    is semidet.

equal_livemaps_keys([], _Livemap1, _Livemap2).
equal_livemaps_keys([Label | Labels], Livemap1, Livemap2) :-
    map.lookup(Livemap1, Label, Liveset1),
    map.lookup(Livemap2, Label, Liveset2),
    set.equal(Liveset1, Liveset2),
    equal_livemaps_keys(Labels, Livemap1, Livemap2).

    % Don't iterate the map building process more than this many times.
    %
:- func livemap_iteration_limit = int.

livemap_iteration_limit = 5.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Build up a map of what lvals are live at each label.
    % The input instruction sequence is reversed.
    %
:- pred livemap_do_build(list(instruction)::in, lvalset::in,
    bool::in, bool::out, livemap::in, livemap::out) is det.

livemap_do_build([], _, !ContainsBadUserCode, !Livemap).
livemap_do_build([Instr0 | Instrs0], Livevals0,
        !ContainsBadUserCode, !Livemap) :-
    livemap_do_build_instr(Instr0, Instrs0, Instrs1,
        Livevals0, Livevals1, !ContainsBadUserCode, !Livemap),
    livemap_do_build(Instrs1, Livevals1, !ContainsBadUserCode, !Livemap).

:- pred livemap_do_build_instr(instruction::in, list(instruction)::in,
    list(instruction)::out, lvalset::in, lvalset::out,
    bool::in, bool::out, livemap::in, livemap::out) is det.

livemap_do_build_instr(Instr0, !Instrs, !Livevals, !ContainsBadUserCode,
        !Livemap) :-
    Instr0 = llds_instr(Uinstr0, _),
    (
        Uinstr0 = comment(_)
    ;
        Uinstr0 = livevals(_),
        unexpected($pred, "livevals found in backward scan in build_livemap")
    ;
        Uinstr0 = block(_, _, _),
        unexpected($pred, "block found in backward scan")
    ;
        ( Uinstr0 = assign(Lval, Rval)
        ; Uinstr0 = keep_assign(Lval, Rval)
        ),

        % Make dead the variable assigned, but make any variables
        % needed to access it live. Make the variables in the assigned
        % expression live as well.
        % The deletion has to be done first. If the assigned-to lval
        % appears on the right hand side as well as the left, then we
        % want make_live to put it back into the liveval set.

        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals([Rval | Rvals], !Livevals)
    ;
        Uinstr0 = lc_create_loop_control(_NumSlots, Lval),
        set.delete(Lval, !Livevals)
    ;
        Uinstr0 = lc_wait_free_slot(LCRval, LCSLval, _InternalLabel),
        set.delete(LCSLval, !Livevals),
        opt_util.lval_access_rvals(LCSLval, LCSAccessRvals),
        livemap.make_live_in_rvals([LCRval | LCSAccessRvals], !Livevals)
    ;
        Uinstr0 = lc_spawn_off(LCRval, LCSRval, Child),
        livemap.make_live_in_rvals([LCRval, LCSRval], !Livevals),
        % Everything that is live at Child is live before this instruction.
        livemap_insert_label_livevals(!.Livemap, Child, !Livevals)
    ;
        Uinstr0 = lc_join_and_terminate(LCRval, LCSRval),
        % Since this instruction terminates its context, nothing is live
        % before it except its arguments and their components.
        livemap.make_live_in_rvals([LCRval, LCSRval], set.init, !:Livevals)
    ;
        Uinstr0 = llcall(_, _, _, _, _, _),
        livemap.look_for_livevals(!Instrs, !Livevals, "call", yes, _)
    ;
        Uinstr0 = mkframe(_, _)
    ;
        Uinstr0 = label(Label),
        map.set(Label, !.Livevals, !Livemap)
    ;
        Uinstr0 = goto(CodeAddr),
        LivevalsNeeded = opt_util.livevals_addr(CodeAddr),
        livemap.look_for_livevals(!Instrs, !Livevals, "goto",
            LivevalsNeeded, Found),
        (
            Found = yes
        ;
            Found = no,
            (
                CodeAddr = code_label(Label),
                livemap_insert_label_livevals(!.Livemap, Label,
                    set.init, !:Livevals)
            ;
                ( CodeAddr = do_redo
                ; CodeAddr = do_fail
                ; CodeAddr = do_not_reached
                )
            ;
                ( CodeAddr = code_imported_proc(_)
                ; CodeAddr = code_succip
                ; CodeAddr = do_succeed(_)
                ; CodeAddr = do_trace_redo_fail_shallow
                ; CodeAddr = do_trace_redo_fail_deep
                ; CodeAddr = do_call_closure(_)
                ; CodeAddr = do_call_class_method(_)
                ),
                unexpected($pred, "unknown code_addr type")
            )
        ),
        livemap_special_code_addr(CodeAddr, MaybeSpecial),
        (
            MaybeSpecial = yes(Special),
            set.insert(Special, !Livevals)
        ;
            MaybeSpecial = no
        )
    ;
        Uinstr0 = computed_goto(Rval, MaybeLabels),
        livemap.make_live_in_rvals([Rval], set.init, !:Livevals),
        list.foldl(livemap_insert_maybe_label_livevals(!.Livemap), MaybeLabels,
            !Livevals)
    ;
        Uinstr0 = if_val(Rval, CodeAddr),
        Livevals0 = !.Livevals,
        livemap.look_for_livevals(!Instrs, !Livevals, "if_val", no, Found),
        (
            Found = yes,
            % This if_val was put here by middle_rec.
            % We must make sure that the locations mentioned
            % in the livevals annotation become live,
            % since they will be needed at CodeAddr.
            % The locations in Livevals0 may be needed
            % in the fall-through continuation.
            set.union(Livevals0, !Livevals)
        ;
            Found = no,
            livemap.make_live_in_rvals([Rval], !Livevals),
            ( if CodeAddr = code_label(Label) then
                livemap_insert_label_livevals(!.Livemap, Label, !Livevals)
            else
                true
            )
        ),
        livemap_special_code_addr(CodeAddr, MaybeSpecial),
        (
            MaybeSpecial = yes(Special),
            set.insert(Special, !Livevals)
        ;
            MaybeSpecial = no
        )
    ;
        Uinstr0 = save_maxfr(Lval),
        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals(Rvals, !Livevals)
    ;
        Uinstr0 = restore_maxfr(Lval),
        livemap.make_live_in_rval(lval(Lval), !Livevals)
    ;
        Uinstr0 = incr_hp(Lval, _, _, SizeRval, _, _, MaybeRegionRval,
            MaybeReuse),

        % Make dead the variable assigned, but make any variables
        % needed to access it live. Make the variables in the size
        % expression live as well.
        % The use of the size rval occurs after the assignment to lval,
        % but the two should never have any variables in common. This is
        % why doing the deletion first works.

        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals(Rvals, !Livevals),
        livemap.make_live_in_rval(SizeRval, !Livevals),
        (
            MaybeRegionRval = no
        ;
            MaybeRegionRval = yes(RegionRval),
            livemap.make_live_in_rval(RegionRval, !Livevals)
        ),
        (
            MaybeReuse = no_llds_reuse
        ;
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            livemap.make_live_in_rval(ReuseRval, !Livevals),
            (
                MaybeFlagLval = no
            ;
                MaybeFlagLval = yes(FlagLval),
                set.delete(FlagLval, !Livevals),
                opt_util.lval_access_rvals(FlagLval, FlagRvals),
                livemap.make_live_in_rvals(FlagRvals, !Livevals)
            )
        )
    ;
        Uinstr0 = mark_hp(Lval),
        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals(Rvals, !Livevals)
    ;
        Uinstr0 = restore_hp(Rval),
        livemap.make_live_in_rvals([Rval], !Livevals)
    ;
        Uinstr0 = free_heap(Rval),
        livemap.make_live_in_rvals([Rval], !Livevals)
    ;
        Uinstr0 = push_region_frame(_RegionStackId, _EmbeddedStackFrame)
    ;
        Uinstr0 = region_fill_frame(_FillOp, _EmbeddedStackFrame, IdRval,
            NumLval, AddrLval),
        livemap.make_live_in_rval(IdRval, !Livevals),
        % The instruction takes the current values in NumLval and AddrLval
        % as inputs, and then updates those values. This means that they are
        % live on entry to the instruction, and will stay that way afterward.
        livemap.make_live_in_rval(lval(NumLval), !Livevals),
        livemap.make_live_in_rval(lval(AddrLval), !Livevals)
    ;
        Uinstr0 = region_set_fixed_slot(_SetOp, _EmbeddedStackFrame,
            ValueRval),
        livemap.make_live_in_rval(ValueRval, !Livevals)
    ;
        Uinstr0 = use_and_maybe_pop_region_frame(_UseOp, _EmbeddedStackFrame)
        % XXX We should make all stackvars or framevars in _EmbeddedStackFrame
        % live, to prevent the compiler from optimizing away assignments to
        % them. However, at the moment all such assignments are done via
        % region_fill_frame and region_set_fixed_slot instructions, which
        % we currently do not ever optimize away, so recording the stack slots
        % as live would be redundant.
    ;
        Uinstr0 = store_ticket(Lval),
        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals(Rvals, !Livevals)
    ;
        Uinstr0 = reset_ticket(Rval, _Reason),
        livemap.make_live_in_rval(Rval, !Livevals)
    ;
        Uinstr0 = discard_ticket
    ;
        Uinstr0 = prune_ticket
    ;
        Uinstr0 = mark_ticket_stack(Lval),
        set.delete(Lval, !Livevals),
        opt_util.lval_access_rvals(Lval, Rvals),
        livemap.make_live_in_rvals(Rvals, !Livevals)
    ;
        Uinstr0 = prune_tickets_to(Rval),
        livemap.make_live_in_rval(Rval, !Livevals)
    ;
        Uinstr0 = incr_sp(_, _, _)
    ;
        Uinstr0 = decr_sp(_)
    ;
        Uinstr0 = decr_sp_and_return(_),
        % These instructions should be generated only *after* any optimizations
        % that need livemaps have been run for the last time.
        unexpected($pred, "decr_sp_and_return")
    ;
        Uinstr0 = init_sync_term(_, _, _)
    ;
        Uinstr0 = fork_new_child(_, _)
    ;
        Uinstr0 = join_and_continue(_, _)
    ;
        Uinstr0 = arbitrary_c_code(AffectsLiveness, LiveLvalInfo, Code),
        build_live_lval_info(AffectsLiveness, LiveLvalInfo, Code,
            !Livevals, !ContainsBadUserCode)
    ;
        Uinstr0 = foreign_proc_code(_, Components, _, _, _, _, _, _, _, _),
        build_livemap_foreign_proc_components(Components,
            !Livevals, !ContainsBadUserCode)
    ).

:- pred build_livemap_foreign_proc_components(list(foreign_proc_component)::in,
    lvalset::in, lvalset::out, bool::in, bool::out) is det.

build_livemap_foreign_proc_components([], !Livevals, !ContainsBadUserCode).
build_livemap_foreign_proc_components([Component | Components],
        !Livevals, !ContainsBadUserCode) :-
    (
        Component = foreign_proc_inputs(Inputs),
        build_livemap_foreign_proc_inputs(Inputs, !Livevals)
    ;
        Component = foreign_proc_outputs(_)
    ;
        Component = foreign_proc_user_code(_, AffectsLiveness, Code),
        (
            AffectsLiveness = proc_affects_liveness,
            !:ContainsBadUserCode = yes
        ;
            AffectsLiveness = proc_default_affects_liveness,
            ( if Code = "" then
                true
            else
                % We should take the contents of the Code into account here.
                % For now, we just assume the worst.
                !:ContainsBadUserCode = yes
            )
        ;
            AffectsLiveness = proc_does_not_affect_liveness
        )
    ;
        Component = foreign_proc_raw_code(_Context, AffectsLiveness,
            LiveLvalInfo, Code),
        build_live_lval_info(AffectsLiveness, LiveLvalInfo, Code,
            !Livevals, !ContainsBadUserCode)
    ;
        Component = foreign_proc_fail_to(_)
    ;
        Component = foreign_proc_alloc_id(_)
    ;
        Component = foreign_proc_noop
    ),
    build_livemap_foreign_proc_components(Components,
        !Livevals, !ContainsBadUserCode).

:- pred build_live_lval_info(proc_affects_liveness::in, c_code_live_lvals::in,
    string::in, lvalset::in, lvalset::out, bool::in, bool::out) is det.

build_live_lval_info(AffectsLiveness, LiveLvalInfo, Code,
        !Livevals, !ContainsBadUserCode) :-
    (
        AffectsLiveness = proc_affects_liveness,
        !:ContainsBadUserCode = yes
    ;
        AffectsLiveness = proc_default_affects_liveness,
        ( if Code = "" then
            true
        else
            % We should take the contents of the Code into account here.
            % For now, we just assume the worst.
            !:ContainsBadUserCode = yes
        )
    ;
        AffectsLiveness = proc_does_not_affect_liveness,
        (
            LiveLvalInfo = no_live_lvals_info,
            !:ContainsBadUserCode = yes
        ;
            LiveLvalInfo = live_lvals_info(LiveLvalSet),
            set.to_sorted_list(LiveLvalSet, LiveLvals),
            livemap_insert_proper_livevals(LiveLvals, !Livevals)
        )
    ).

:- pred build_livemap_foreign_proc_inputs(list(foreign_proc_input)::in,
    lvalset::in, lvalset::out) is det.

build_livemap_foreign_proc_inputs([], !Livevals).
build_livemap_foreign_proc_inputs([Input | Inputs], !Livevals) :-
    Input = foreign_proc_input(_, _, _, _, Rval, _, _),
    ( if Rval = lval(Lval) then
        livemap_insert_proper_liveval(Lval, !Livevals)
    else
        true
    ),
    build_livemap_foreign_proc_inputs(Inputs, !Livevals).

:- pred look_for_livevals(list(instruction)::in,
    list(instruction)::out, lvalset::in, lvalset::out, string::in,
    bool::in, bool::out) is det.

look_for_livevals(Instrs0, Instrs, !Livevals, Site, Compulsory, Found) :-
    opt_util.skip_comments(Instrs0, Instrs1),
    ( if Instrs1 = [llds_instr(livevals(Livevals1), _) | Instrs2] then
        livemap_filter_livevals(Livevals1, !:Livevals),
        Instrs = Instrs2,
        Found = yes
    else
        (
            Compulsory = yes,
            unexpected($pred, Site ++ " not preceded by livevals")
        ;
            Compulsory = no,
            Instrs = Instrs1,
            Found = no
        )
    ).

    % What lval (if any) is consulted when we branch to a code address?
    %
:- pred livemap_special_code_addr(code_addr::in, maybe(lval)::out) is det.

livemap_special_code_addr(CodeAddr, MaybeSpecial) :-
    (
        ( CodeAddr = code_label(_)
        ; CodeAddr = code_imported_proc(_)
        ; CodeAddr = do_trace_redo_fail_shallow
        ; CodeAddr = do_trace_redo_fail_deep
        ; CodeAddr = do_fail
        ; CodeAddr = do_call_closure(_)
        ; CodeAddr = do_call_class_method(_)
        ; CodeAddr = do_not_reached
        ),
        MaybeSpecial = no
    ;
        CodeAddr = code_succip,
        MaybeSpecial = yes(succip)
    ;
        CodeAddr = do_succeed(_),
        MaybeSpecial = yes(succip_slot(lval(curfr)))
    ;
        CodeAddr = do_redo,
        MaybeSpecial = yes(redoip_slot(lval(maxfr)))
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred make_live_in_rvals(list(rval)::in, lvalset::in, lvalset::out) is det.

make_live_in_rvals([], !Live).
make_live_in_rvals([Rval | Rvals], !Live) :-
    make_live_in_rval(Rval, !Live),
    make_live_in_rvals(Rvals, !Live).

    % Set all lvals found in this rval to live, with the exception of fields,
    % since they are treated specially (the later stages consider them
    % to be live even if they are not explicitly in the live set).
    %
:- pred make_live_in_rval(rval::in, lvalset::in, lvalset::out) is det.

make_live_in_rval(Rval, !Live) :-
    (
        Rval = lval(Lval),
        % XXX Maybe we should treat mem_refs the same way as field refs.
        ( if Lval = field(_, _, _) then
            true
        else
            set.insert(Lval, !Live)
        ),
        opt_util.lval_access_rvals(Lval, AccessRvals),
        make_live_in_rvals(AccessRvals, !Live)
    ;
        ( Rval = mkword_hole(_)
        ; Rval = const(_)
        )
    ;
        ( Rval = mkword(_, SubRval)
        ; Rval = cast(_, SubRval)
        ; Rval = unop(_, SubRval)
        ),
        make_live_in_rval(SubRval, !Live)
    ;
        Rval = binop(_, SubRvalA, SubRvalB),
        make_live_in_rval(SubRvalA, !Live),
        make_live_in_rval(SubRvalB, !Live)
    ;
        Rval = var(_),
        unexpected($pred, "var rval should not propagate to the optimizer")
    ;
        Rval = mem_addr(MemRef),
        make_live_in_mem_ref(MemRef, !Live)
    ).

:- pred make_live_in_mem_ref(mem_ref::in, lvalset::in, lvalset::out) is det.

make_live_in_mem_ref(MemRef, !Live) :-
    (
        MemRef = stackvar_ref(Rval),
        make_live_in_rval(Rval, !Live)
    ;
        MemRef = framevar_ref(Rval),
        make_live_in_rval(Rval, !Live)
    ;
        MemRef = heap_ref(CellPtrRval, _MaybeTag, FieldNumRval),
        make_live_in_rval(CellPtrRval, !Live),
        make_live_in_rval(FieldNumRval, !Live)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred livemap_filter_livevals(lvalset::in, lvalset::out) is det.

livemap_filter_livevals(Livevals0, Livevals) :-
    set.to_sorted_list(Livevals0, Livelist),
    set.init(Livevals1),
    livemap_insert_proper_livevals(Livelist, Livevals1, Livevals).

:- pred livemap_insert_maybe_label_livevals(livemap::in, maybe(label)::in,
    lvalset::in, lvalset::out) is det.

livemap_insert_maybe_label_livevals(Livemap, MaybeLabel, !Livevals) :-
    (
        MaybeLabel = yes(Label),
        livemap_insert_label_livevals(Livemap, Label, !Livevals)
    ;
        MaybeLabel = no
    ).

:- pred livemap_insert_label_livevals(livemap::in, label::in,
    lvalset::in, lvalset::out) is det.

livemap_insert_label_livevals(Livemap, Label, !Livevals) :-
    ( if map.search(Livemap, Label, LabelLivevals) then
        set.to_sorted_list(LabelLivevals, Livelist),
        livemap_insert_proper_livevals(Livelist, !Livevals)
    else
        true
    ).

:- pred livemap_insert_proper_livevals(list(lval)::in, lvalset::in,
    lvalset::out) is det.

livemap_insert_proper_livevals([], !Livevals).
livemap_insert_proper_livevals([Live | Livelist], !Livevals) :-
    livemap_insert_proper_liveval(Live, !Livevals),
    livemap_insert_proper_livevals(Livelist, !Livevals).

    % Don't insert references to locations on the heap.
    %
:- pred livemap_insert_proper_liveval(lval::in, lvalset::in, lvalset::out)
    is det.

livemap_insert_proper_liveval(Live, !Livevals) :-
    ( if Live = field(_, _, _) then
        true
    else
        set.insert(Live, !Livevals)
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.livemap.
%-----------------------------------------------------------------------------%
