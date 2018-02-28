%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_elim_nested.m.
% Main author: fjh.
%
% This module is an MLDS-to-MLDS transformation that has two functions:
%
% 1 Eliminating nested functions.
%
% 2 Putting local variables that might contain pointers into structs, and
%   chaining these structs together, for use with accurate garbage collection.
%
% The two transformations are quite similar, so they are both handled by
% the same code; a flag is passed to say which transformation should be done.
%
% The word "environment" (as in "environment struct" or "environment pointer")
% is used to refer to both the environment structs used when eliminating
% nested functions and also to the frame structs used for accurate GC.
%
% XXX Would it be possible to do both in a single pass?
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% (1) eliminating nested functions
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Note that this module does not attempt to handle arbitrary MLDS as input;
% it will only work with the output of the current MLDS code generator.
% In particular, it assumes that local variables in nested functions can be
% hoisted into the outermost function's environment. That is not true
% in general (e.g. if the nested functions are recursive), but it is true
% for the code that ml_code_gen generates.
%
% As well as eliminating nested functions, this transformation also has
% the effect of fixing up the dangling `env_ptr' references that ml_code_gen.m
% leaves in the code.
%
%---------------------------------------------------------------------------%
% TRANSFORMATION SUMMARY
%---------------------------------------------------------------------------%
%
% We transform code of the form e.g.
%
%   <OuterRet>
%   outer(<OuterArgs>) {
%       <OuterLocals>
%
%       <Inner1Ret>
%       inner(<Inner1Args>, void *env_ptr_arg) {
%           <Inner1Locals>
%
%           <NestedInnerRet>
%           nested_inner(<NestedInnerArgs>, void *env_ptr_arg)
%           {
%               <NestedInnerLocals>
%
%               <NestedInnerCode>
%           }
%
%           <Inner1Code>
%       }
%
%       <Inner2Ret>
%       inner(<Inner2Args>, void *env_ptr_arg) {
%           <Inner2Locals>
%
%           <Inner2Code>
%       }
%
%       <OuterCode>
%   }
%
% into
%
%   struct OuterLocals_struct {
%       <OuterArgs>
%       <OuterLocals>
%       <Inner1Locals>
%   };
%
%   <NestedInnerRet>
%   nested_inner(<NestedInnerArgs>, void *env_ptr_arg) {
%       OuterLocals *env_ptr = env_ptr_arg;
%       <NestedInnerLocals>
%
%       <NestedInnerCode'>
%   }
%
%   <Inner1Ret>
%   inner(<Inner1Args>, void *env_ptr_arg) {
%       OuterLocals *env_ptr = env_ptr_arg;
%
%       <Inner1Code'>
%   }
%
%   <Inner2Ret>
%   inner(<Inner2Args>, void *env_ptr_arg) {
%       OuterLocals *env_ptr = env_ptr_arg;
%       <Inner2Locals>
%
%       <Inner2Code'>
%   }
%
%   <OuterRet>
%   outer(<OuterArgs>) {
%       OuterLocals env;
%       OuterLocals *env_ptr = &env;
%
%       env_ptr-><OuterArgs> = <OuterArgs>;
%       <OuterCode'>
%   }
%
% where <Inner1Code'>, <Inner2Code'> and <NestedInnerCode'> are the
% same as <Inner1Code>, <Inner2Code> and <NestedInnerCode> (respectively)
% except that any references to a local variable <Var> declared in outer()
% are replaced with `env_ptr -> <Var>', and likewise <OuterCode'> is
% the same as <OuterCode> with references to local variables replaced with
% `env_ptr->foo'. In the latter case it could (depending on how smart the C
% compiler is) potentially be more efficient to generate `env.foo', but
% currently we don't do that.
%
% Actually the description above is slightly over-simplified: not all local
% variables need to be put in the environment struct. Only those local
% variables which are referenced by nested functions need to be
% put in the environment struct. Also, if none of the nested functions
% refer to the locals in the outer function, we don't need to create
% an environment struct at all, we just need to hoist the definitions
% of the nested functions out to the top level.
%
% The `env_ptr' variables generated here serve as definitions for
% the (previously dangling) references to such variables that
% ml_code_gen puts in calls to the nested functions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% (2) accurate GC
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% SUMMARY
%
% This is an MLDS-to-MLDS transformation that transforms the MLDS code
% to add the information needed to do accurate GC when compiling to C.
%
% Basically what we do is to put all local variables that might contain
% pointers in structs, with one struct for each stack frame, and chain
% these structs together. At GC time, we traverse the chain of structs.
% This allows us to accurately scan the C stack.
%
% This is described in more detail in the following paper:
%
%   Fergus Henderson <fjh@cs.mu.oz.au>,
%   "Accurate garbage collection in an uncooperative environment".
%   International Symposium on Memory Management, Berlin, Germany, 2002.
%
% In theory, accurate GC is now fully implemented, i.e. it should support
% the whole Mercury language, modulo the caveats below.
%
% TODO:
% (The "I" below is fjh).
%
% - XXX Need to test the GC tracing code for type class methods.
%   This code in theory ought to work, I think, but it has not really been
%   tested.
%
% - XXX The garbage collector should resize the heap if/when it fills up.
%   We should allocate a large amount of virtual memory for each heap,
%   but we should collect when we have allocated a small part of it.
%
% - Heap reclamation on failure is not yet supported.
%   One difficulty is that when resetting the heap, we need to also reset
%   all the local variables which might point to reclaimed garbage, otherwise
%   the collector might try to trace through them, which can result in an error
%   since the data pointed to isn't of the right type because it has been
%   overwritten.
%
% - The garbage collector should collect the solutions heap and the global heap
%   as well as the ordinary heap.
%
%   Note that this is currently not an issue, since currently we don't use
%   these heaps, because we don't support heap reclamation on failure or
%   tabling (respectively).
%
%   Actually I think GC of these heaps should almost work already, or would
%   once we start using these heaps, because we never allocate on the
%   solutions heap or the global heap directly, instead we swap heaps to make
%   the heap that we want to allocate on the main heap. Then if that heap
%   runs out of space, we will invoke a garbage collection, and everything
%   should work fine. However, there are a couple of problems.
%
%   First, GC will swap the to-space heap and the from-space heap. So if the
%   different heaps are different sizes, we may end up with the to-space heap
%   being too small (e.g. because it was originally the solutions heap).
%   To fix that, we can just allocate large total sizes for all the heaps;
%   see the point above about heap resizing.
%
%   Second, for GC of the global heap to work, the runtime routines which
%   allocate stuff on that heap need to be modified to support GC.
%   In particular, MR_deep_copy() and MR_make_long_lived() and its callers
%   need be modified so that they are safe for GC (i.e. they must record
%   all parameters and locals that point to the heap on the GC's shadow stack),
%   and MR_deep_copy() needs to call MR_GC_check() before each heap allocation.
%
% - XXX We need to handle `pragma foreign_export'.
%
%   The C interface in general is a bit problematic for GC. But for code which
%   does not call back to Mercury, the way we currently handle it is fairly
%   safe even if the C code uses pointers to the Mercury heap or allocates
%   on the Mercury heap, because such code will not invoke the GC. So the worst
%   that can go wrong is a heap overflow. Provided that the C code does not
%   allocate too much (more than MR_heap_margin_size), it won't overflow the
%   heap, and the heap will get GC'd next time you call some Mercury code
%   which does a heap allocation. Of course you may run into problems if
%   there is a loop that calls C code which allocates on the Mercury heap,
%   and the loop contains no intervening calls to Mercury code that allocates
%   heap space (and hence calls MR_GC_check()).
%
%   But if Mercury code calls C code which calls back to Mercury code, and
%   the C code uses pointers to the Mercury heap, then there could be
%   serious problems (i.e. dangling pointers). Even if you just use `pragma
%   foreign_export' to export a procedure and `pragma foreign_proc' to import
%   it back again, there may be trouble. The code generated for the exported
%   functions can include calls to MR_MAYBE_BOX_FOREIGN_TYPE, which may
%   allocate heap; we ought to register the frame and call MR_GC_check()
%   before each call to MR_MAYBE_BOX_FOREIGN_TYPE, but currently we don't.
%
%   Even if that was solved, there is still the issue of what to do about
%   any heap pointers held by user-written C code; we need to provide an API
%   for registering pointers on the stack. (MR_agc_add_root() only works
%   for globals, really, since there's no MR_agc_remove_root()).
%
% Various optional features of Mercury are not yet supported, e.g.
%
% - `--high-level-data' (fixup_newobj_in_atomic_statement
%   gets the types wrong; see comment in ml_code_util.m)
%
% - trailing
%
% - tabling
%
% - multithreading
%
% There are also some things that could be done to improve efficiency, e.g.
%
% - optimize away temporary variables
%
% - put stack_chain and/or heap pointer in global register variables
%
% - move termination conditions (check for base case) outside of stack frame
%   setup & GC check where possible
%
%---------------------------------------------------------------------------%
%
% DETAILED DESCRIPTION
%
% For each function, we generate a struct for that function.
% Each such struct starts with a sub-struct containing a couple of
% fixed fields, which allow the GC to traverse the chain:
%
%   struct <function_name>_frame {
%       struct MR_StackChain fixed_fields;
%       ...
%   };
%
% The fixed fields are as follows:
%
%   struct MR_StackChain {
%       struct MR_StackChain *prev;
%       void (*trace)(void *this_frame);
%   };
%
% Actually, rather than using a nested structure, we just put these fields
% directly in the <function_name>_frame struct. (This turned out to be
% a little easier.)
%
% The prev field holds a link to the entry for this function's caller.
% The trace field is the address of a function to trace everything pointed
% to by this stack frame.
%
% To ensure that we don't try to traverse uninitialized fields,
% we zero-initialize each struct before inserting it into the chain.
%
% We need to keep a link to the topmost frame on the stack. There are two
% possible ways that we could handle this. One way is to pass it down
% as an parameter. Each function would get an extra parameter `stack_chain'
% which points to the caller's struct. An alternative approach is to just
% have a global variable `stack_chain' that points to the top of the stack.
% We need extra code to set this pointer when entering and returning from
% functions. To make this approach thread-safe, the variable would actually
% need to be thread-local rather than global. This approach would probably
% work best if the variable is a GNU C global register variable, which would
% make it both efficient and thread-safe.
% XXX Currently, for simplicity, we are using a global variable.
%
% At each allocation, we do a call to MR_GC_check(), which checks for heap
% exhaustion, and if necessary calls MR_garbage_collect() in
% runtime/mercury_accurate_gc.c to do the collection. The calls to
% MR_GC_check() are inserted by compiler/mlds_to_c.m.
%
% As an optimization, we ought to not bother allocating a struct for functions
% that don't have any variables that might contain pointers. We also ought
% to not bother allocating a struct for leaf functions that don't contain
% any functions calls or memory allocations.
% XXX These optimizations are not yet implemented!
%
%---------------------------------------------------------------------------%
%
% EXAMPLE
%
% If we have a function
%
%   RetType
%   foo(Arg1Type arg1, Arg2Type arg2, ...)
%   {
%       Local1Type local1;
%       Local2Type local2;
%       ...
%       local1 = MR_new_object(...);
%       ...
%       bar(arg1, arg2, local1, &local2);
%       ...
%   }
%
% where say Arg1Type and Local1Type might contain pointers,
% but Arg2Type and Local2Type don't, then we would transform it as follows:
%
%   struct foo_frame {
%       MR_StackChain fixed_fields;
%       Arg1Type arg1;
%       Local1Type local1;
%       ...
%   };
%
%   static void
%   foo_trace(void *this_frame) {
%       struct foo_frame *frame = (struct foo_frame *)this_frame;
%
%       ... code to construct TypeInfo for type of arg1 ...
%       mercury__private_builtin__gc_trace_1_p_0(
%           <TypeInfo for type of arg1>, &frame->arg1);
%
%       ... code to construct TypeInfo for type of local1 ...
%       mercury__private_builtin__gc_trace_1_p_0(
%           <TypeInfo for type of local1>, &frame->local1);
%
%       ...
%   }
%
%   RetType
%   foo(Arg1Type arg1, Arg2Type arg2, ...)
%   {
%       struct foo_frame this_frame;
%       Local2Type local2;
%
%       this_frame.fixed_fields.prev = stack_chain;
%       this_frame.fixed_fields.trace = foo_trace;
%       this_frame.arg1 = arg1;
%       this_frame.local1 = NULL;
%       stack_chain = &this_frame;
%
%       ...
%       this_frame.local1 = MR_new_object(...);
%       ...
%       bar(this_frame.arg1, arg2, this_frame.local1, &local2);
%       ...
%       stack_chain = stack_chain->prev;
%   }
%
% Alternatively, if we were passing stack_chain as an argument,
% rather than treating it as a global variable, then the generated
% code for foo() would look like this:
%
%   RetType
%   foo(struct MR_StackChain *stack_chain,
%       Arg1Type arg1, Arg2Type arg2, ...)
%   {
%       struct foo_frame this_frame;
%       Local2Type local2;
%
%       this_frame.fixed_fields.prev = stack_chain;
%       this_frame.fixed_fields.trace = foo_trace;
%       this_frame.arg1 = arg1;
%       this_frame.local1 = NULL;
%
%       ...
%       this_frame.local1 = MR_new_object(&this_frame, ...);
%       ...
%       bar(&this_frame, this_frame.arg1, arg2,
%           this_frame.local1, &local2);
%       ...
%       /* no need to explicitly unchain the stack frame here */
%   }
%
% Currently, rather than initializing the fields of `this_frame'
% using a sequence of assignment statements, we actually just use
% an initializer:
%       struct foo_frame this_frame = { stack_chain };
% This implicitly zeros out the remaining fields.
% Only the non-null fields, i.e. the arguments and the trace
% field, need to be explicitly assigned using assignment statements.
%
% The code in the Mercury runtime to traverse the stack frames would
% look something like this:
%
%   void
%   MR_traverse_stack(struct MR_StackChain *stack_chain)
%   {
%       while (stack_chain != NULL) {
%           (*stack_chain->trace)(stack_chain);
%           stack_chain = stack_chain->prev;
%       }
%   }
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_elim_nested.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds.

%---------------------------------------------------------------------------%

:- type action
    --->    hoist_nested_funcs
            % Eliminate nested functions

    ;       chain_gc_stack_frames.
            % Add shadow stack for supporting accurate GC.

:- inst hoist for action/0
    --->    hoist_nested_funcs.
:- inst chain for action/0
    --->    chain_gc_stack_frames.

    % Process the whole MLDS, performing the indicated action.
    %
:- pred ml_elim_nested(action, globals, mlds_target_lang, mlds, mlds).
:- mode ml_elim_nested(in(hoist), in, in, in, out) is det.
:- mode ml_elim_nested(in(chain), in, in, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

ml_elim_nested(Action, Globals, Target, MLDS0, MLDS) :-
    MLDS0 = mlds(ModuleName, Imports, GlobalData0, TypeDefns0,
        TableStructDefns, ProcDefns0, InitPreds, FinalPreds,
        ForeignCode, ExportedEnums),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_global_data_get_closure_wrapper_func_defns(GlobalData0,
        WrapperFuncsCord),
    ProcDefns1 = ProcDefns0 ++ cord.to_list(WrapperFuncsCord),
    ml_elim_nested_defns_in_funcs(Action, MLDS_ModuleName, Globals, Target,
        ProcDefns1, cord.init, ProcDefnsCord, cord.init, EnvTypeDefnsCord),
    ProcDefns = cord.to_list(ProcDefnsCord),
    % The (flattened forms of) the closure wrapper functions are in ProcDefns;
    % don't include them in the MLDS twice.
    ml_global_data_set_closure_wrapper_func_defns(cord.init,
        GlobalData0, GlobalData),
    EnvTypeDefns = cord.to_list(EnvTypeDefnsCord),
    TypeDefns = TypeDefns0 ++ EnvTypeDefns,
    MLDS = mlds(ModuleName, Imports, GlobalData, TypeDefns,
        TableStructDefns, ProcDefns, InitPreds, FinalPreds,
        ForeignCode, ExportedEnums).

:- pred ml_elim_nested_defns_in_funcs(action, mlds_module_name,
    globals, mlds_target_lang, list(mlds_function_defn),
    cord(mlds_function_defn), cord(mlds_function_defn),
    cord(mlds_class_defn), cord(mlds_class_defn)).
:- mode ml_elim_nested_defns_in_funcs(in(hoist), in, in, in, in,
    in, out, in, out) is det.
:- mode ml_elim_nested_defns_in_funcs(in(chain), in, in, in, in,
    in, out, in, out) is det.

ml_elim_nested_defns_in_funcs(_, _, _, _, [], !FuncDefnsCord, !ClassDefnsCord).
ml_elim_nested_defns_in_funcs(Action, ModuleName, Globals, Target,
        [FuncDefn | FuncDefns], !FuncDefnsCord, !ClassDefnsCord) :-
    FuncDefn = mlds_function_defn(Name, _Context, _Flags,
        _PredProcId, _Params0, _Body0, _EnvVarNames, _MaybeRequiretailrecInfo),
    % Don't add GC tracing code to the gc_trace/1 primitive!
    % (Doing so would just slow things down unnecessarily.)
    % And since it is implemented as a foreign proc, it has no
    % nested definitions to flatten out either.
    ( if
        Name = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _),
        FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
        ProcLabel = mlds_proc_label(PredLabel, _ProcId),
        PredLabel = mlds_user_pred_label(_, _, "gc_trace", 1, _, _),
        PrivateBuiltin = mercury_private_builtin_module,
        ModuleName = mercury_module_name_to_mlds(PrivateBuiltin)
    then
        !:FuncDefnsCord = cord.snoc(!.FuncDefnsCord, FuncDefn)
    else
        ml_elim_nested_defns_in_func(Action, ModuleName, Globals, Target,
            FuncDefn, !FuncDefnsCord, !ClassDefnsCord)
    ),
    ml_elim_nested_defns_in_funcs(Action, ModuleName, Globals, Target,
        FuncDefns, !FuncDefnsCord, !ClassDefnsCord).

    % Either eliminate nested functions:
    % Hoist out any nested function occurring in a single mlds_defn.
    % Return a list of mlds_defns that contains no nested functions,
    % by adding them to the tail of !DefnsCord, in their desired order.
    %
    % Or handle accurate GC: put all variables that might contain pointers
    % in structs and chain these structs together into a "shadow stack".
    % Extract out the code to trace these variables, putting it in a function
    % whose address is stored in the shadow stack frame.
    %
:- pred ml_elim_nested_defns_in_func(action, mlds_module_name,
    globals, mlds_target_lang, mlds_function_defn,
    cord(mlds_function_defn), cord(mlds_function_defn),
    cord(mlds_class_defn), cord(mlds_class_defn)).
:- mode ml_elim_nested_defns_in_func(in(hoist), in, in, in, in,
    in, out, in, out) is det.
:- mode ml_elim_nested_defns_in_func(in(chain), in, in, in, in,
    in, out, in, out) is det.

ml_elim_nested_defns_in_func(Action, ModuleName, Globals, Target, FuncDefn0,
        !FuncDefnsCord, !ClassDefnsCord) :-
    FuncDefn0 = mlds_function_defn(Name, Context, Flags, PredProcId,
        Params0, Body0, EnvVarNames, MaybeRequiretailrecInfo),
    (
        Body0 = body_external,
        !:FuncDefnsCord = cord.snoc(!.FuncDefnsCord, FuncDefn0)
    ;
        Body0 = body_defined_here(FuncBody0),
        EnvName = ml_env_name(Name, Action),
        EnvClassId = ml_create_env_class_id(EnvName, ModuleName, Globals),
        EnvPtrTypeName = ml_make_env_ptr_type(EnvClassId),

        % Traverse the function body, finding (and removing) any nested
        % functions, and fixing up any references to the arguments or to local
        % variables or local static constants that need to be put in the
        % environment structure (e.g. because they occur in nested functions,
        % or to make them visible to the garbage collector)
        %
        % Also, for accurate GC, add code to save and restore the stack chain
        % pointer at any `try_commit' statements.

        ElimInfo0 = elim_info_init(EnvClassId, EnvPtrTypeName, Target),
        Params0 = mlds_func_params(Arguments0, RetValues),
        ml_maybe_add_args(Action, Arguments0, FuncBody0, ModuleName,
            Context, ElimInfo0, ElimInfo1),
        flatten_statement(Action, FuncBody0, FuncBody1, ElimInfo1, ElimInfo2),
        fixup_gc_statements(Action, ElimInfo2, ElimInfo),
        elim_info_finish(ElimInfo, NestedFuncs0, Locals),

        (
            NestedFuncs0 = [],
            % When hoisting nested functions, if there were no nested
            % functions, we have nothing to do.
            % Likewise, when doing accurate GC, if there were no local
            % variables (or arguments) that contained pointers, then we don't
            % need to chain a stack frame for this function.
            FuncBody = FuncBody1
        ;
            NestedFuncs0 = [_ | _],
            % Create a struct to hold the local variables, and initialize
            % the environment pointers for both the containing function
            % and the nested functions. Also generate the GC tracing function,
            % if Action = chain_gc_stack_frames.
            ml_create_env(Action, EnvName, EnvClassId, Locals, Context,
                ModuleName, Name, Globals, EnvTypeDefn, EnvDefns, InitEnv,
                GCTraceFuncDefns),
            list.map_foldl(
                ml_insert_init_env(Action, EnvClassId),
                    NestedFuncs0, NestedFuncs,
                    have_not_inserted_env, InsertedEnv),

            % Hoist out the nested functions.
            !:FuncDefnsCord = !.FuncDefnsCord ++
                cord.from_list(GCTraceFuncDefns) ++
                cord.from_list(NestedFuncs),

            % When hoisting nested functions, it is possible that none of the
            % nested functions reference the arguments or locals of the parent
            % function. In that case, there is no need to create an
            % environment, we just need to flatten the functions.
            %
            % Note that we don't generate the env_ptr_args in this module
            % (instead they are generated when the nested functions are
            % generated). This means that we don't avoid generating these
            % arguments. This is not really a big problem, since the code
            % that generates these arguments needs them.
            ( if
                Action = hoist_nested_funcs,
                InsertedEnv = have_not_inserted_env
            then
                FuncBody = FuncBody1
            else
                !:ClassDefnsCord = cord.snoc(!.ClassDefnsCord, EnvTypeDefn),

                % If the function's arguments are referenced by nested
                % functions, or (for accurate GC) may contain pointers,
                % then we need to copy them to local variables in the
                % environment structure.
                ml_maybe_copy_args(Action, ElimInfo, Arguments0, FuncBody0,
                    EnvClassId, EnvPtrTypeName, Context,
                    _ArgsToCopy, CodeToCopyArgs),

                % Insert code to unlink this stack frame before doing any tail
                % calls or returning from the function, either explicitly
                % or implicitly.
                %
                % Add unlink statements before any explicit returns or tail
                % calls.
                (
                    Action = hoist_nested_funcs,
                    FuncBody2 = FuncBody1
                ;
                    Action = chain_gc_stack_frames,
                    add_unchain_stack_to_stmt(Action, FuncBody1, FuncBody2,
                        ElimInfo, _ElimInfo)
                ),
                % Add a final unlink statement at the end of the function,
                % if needed. This is only needed if the function has no
                % return values -- if there is a return value, then the
                % function must exit with an explicit return statement.
                ( if
                    Action = chain_gc_stack_frames,
                    RetValues = []
                then
                    UnchainFrame = [ml_gen_unchain_frame(Context, ElimInfo)]
                else
                    UnchainFrame = []
                ),

                % Insert the definition and initialization of the environment
                % struct variable at the start of the top-level function's
                % body, and append the final unlink statement (if any)
                % at the end.
                % XXX MLDS_DEFN
                FuncBody = ml_gen_block(EnvDefns, [],
                    InitEnv ++ CodeToCopyArgs ++ [FuncBody2] ++ UnchainFrame,
                    Context)
            )
        ),
        (
            Action = chain_gc_stack_frames,
            % This pass will have put the GC tracing code for the arguments
            % in the GC tracing function. So we don't need the GC tracing code
            % annotation on the arguments anymore. We delete them here, because
            % otherwise the `#if 0 ... #endif' blocks output for the
            % annotations clutter up the generated C files.
            Arguments = list.map(strip_gc_statement, Arguments0)
        ;
            Action = hoist_nested_funcs,
            Arguments = Arguments0
        ),
        Params = mlds_func_params(Arguments, RetValues),
        FuncDefn = mlds_function_defn(Name, Context, Flags,
            PredProcId, Params, body_defined_here(FuncBody),
            EnvVarNames, MaybeRequiretailrecInfo),
        !:FuncDefnsCord = cord.snoc(!.FuncDefnsCord, FuncDefn)
    ).

:- func strip_gc_statement(mlds_argument) = mlds_argument.

strip_gc_statement(Argument0) = Argument :-
    Argument0 = mlds_argument(Name, Type, _GCStmt),
    Argument = mlds_argument(Name, Type, gc_no_stmt).

    % Add any arguments which are used in nested functions
    % to the ei_local_vars field in the elim_info.
    %
:- pred ml_maybe_add_args(action, list(mlds_argument), mlds_stmt,
    mlds_module_name, prog_context, elim_info, elim_info).
:- mode ml_maybe_add_args(in(hoist), in, in, in, in, in, out) is det.
:- mode ml_maybe_add_args(in(chain), in, in, in, in, in, out) is det.

ml_maybe_add_args(_, [], _, _, _, !Info).
ml_maybe_add_args(Action, [Arg | Args], FuncBody, ModuleName, Context,
        !Info) :-
    Arg = mlds_argument(VarName, _Type, GCStmt),
    ( if ml_should_add_local_var(Action, VarName, GCStmt, [], [FuncBody]) then
        ml_conv_arg_to_var(Context, Arg, ArgToCopy),
        elim_info_add_local_var(ArgToCopy, !Info)
    else
        true
    ),
    ml_maybe_add_args(Action, Args, FuncBody, ModuleName, Context, !Info).

    % Generate code to copy any arguments which are used in nested functions
    % to the environment struct.
    %
:- pred ml_maybe_copy_args(action, elim_info, list(mlds_argument), mlds_stmt,
    mlds_class_id, mlds_type, prog_context,
    list(mlds_local_var_defn), list(mlds_stmt)).
:- mode ml_maybe_copy_args(in(hoist), in, in, in, in, in, in, out, out) is det.
:- mode ml_maybe_copy_args(in(chain), in, in, in, in, in, in, out, out) is det.

ml_maybe_copy_args(_, _, [], _, _, _, _, [], []).
ml_maybe_copy_args(Action, Info, [Arg | Args], FuncBody, ClassId,
        EnvPtrTypeName, Context, ArgsToCopy, CodeToCopyArgs) :-
    ml_maybe_copy_args(Action, Info, Args, FuncBody, ClassId,
        EnvPtrTypeName, Context, ArgsToCopyTail, CodeToCopyArgsTail),
    Arg = mlds_argument(VarName, FieldType, GCStmt),
    ( if ml_should_add_local_var(Action, VarName, GCStmt, [], [FuncBody]) then
        ml_conv_arg_to_var(Context, Arg, ArgToCopy),

        % Generate code to copy this arg to the environment struct:
        %   env_ptr->foo = foo;
        %
        Target = elim_info_get_target_lang(Info),
        EnvModuleName = ml_env_module_name(Target, ClassId),
        FieldName = ml_field_named(
            qual_field_var_name(EnvModuleName, type_qual,
                fvn_env_field_from_local_var(VarName)),
            EnvPtrTypeName),
        Tag = yes(0),
        EnvPtrVarName = env_ptr_var(Action),
        EnvPtr = ml_lval(ml_local_var(EnvPtrVarName, EnvPtrTypeName)),
        EnvArgLval = ml_field(Tag, EnvPtr, FieldName, FieldType,
            EnvPtrTypeName),
        ArgRval = ml_lval(ml_local_var(VarName, FieldType)),
        AssignToEnv = assign(EnvArgLval, ArgRval),
        CodeToCopyArg = ml_stmt_atomic(AssignToEnv, Context),

        ArgsToCopy = [ArgToCopy | ArgsToCopyTail],
        CodeToCopyArgs = [CodeToCopyArg | CodeToCopyArgsTail]
    else
        ArgsToCopy = ArgsToCopyTail,
        CodeToCopyArgs = CodeToCopyArgsTail
    ).

    % Create the environment struct "type".
    %
:- func ml_create_env_class_id(mlds_class_name, mlds_module_name, globals) =
    mlds_class_id.

ml_create_env_class_id(EnvClassName, ModuleName, Globals) = ClassId :-
    % If we are allocating it on the heap, then we need to use a class type
    % rather than a struct (value type). This is needed for verifiable code
    % on the IL back-end.
    globals.lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
    (
        OnHeap = yes,
        EnvClassKind = mlds_class
    ;
        OnHeap = no,
        EnvClassKind = mlds_struct
    ),
    ClassName = qual_class_name(ModuleName, module_qual, EnvClassName),
    ClassId = mlds_class_id(ClassName, 0, EnvClassKind).

    % Create the environment struct type, the declaration of the environment
    % variable, and the declaration and initializer for the environment
    % pointer variable:
    %
    %   struct <EnvClassName> {
    %       <LocalVars>
    %   };
    %   struct <EnvClassName> env;
    %   struct <EnvClassName> *env_ptr;
    %   env_ptr = &env;
    %
    % For accurate GC, we do something similar, but with a few differences:
    %
    %   struct <EnvClassName> {
    %       /* these fixed fields match `struct MR_StackChain' */
    %       void *prev;
    %       void (*trace)(...);
    %       <LocalVars>
    %   };
    %   struct <EnvClassName> env = { stack_chain, foo_trace };
    %   struct <EnvClassName> *env_ptr;
    %   env_ptr = &env;
    %   stack_chain = env_ptr;
    %
:- pred ml_create_env(action::in, mlds_class_name::in, mlds_class_id::in,
    list(mlds_local_var_defn)::in, prog_context::in, mlds_module_name::in,
    mlds_function_name::in, globals::in, mlds_class_defn::out,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    list(mlds_function_defn)::out) is det.

ml_create_env(Action, EnvClassName, EnvClassId, LocalVars, Context,
        ModuleName, FuncName, Globals, EnvClassDefn, EnvDefns, InitEnv,
        GCTraceFuncDefns) :-
    % Generate the following type:
    %
    %   struct <EnvClassName> {
    %     #ifdef ACCURATE_GC
    %       /* these fixed fields match `struct MR_StackChain' */
    %       void *prev;
    %       void (*trace)(...);
    %     #endif
    %       <LocalVars>
    %   };
    %
    % If we are allocating it on the heap, then we need to use a class type
    % rather than a struct (value type). This is needed for verifiable code
    % on the IL back-end.
    % XXX Which doesn't exist anymore. However, the Java and C# backends
    % also set put_nondet_env_on_heap to true.
    globals.lookup_bool_option(Globals, put_nondet_env_on_heap, OnHeap),
    (
        OnHeap = no,
        EnvClassKind = mlds_struct,
        Inherits = inherits_nothing
    ;
        OnHeap = yes,
        EnvClassKind = mlds_class,
        Inherits = inherits_generic_env_ptr_type
    ),
    EnvClassFlags =
        mlds_class_decl_flags(class_private, overridable, modifiable),
    Fields0 = list.map(convert_local_to_field, LocalVars),

    % Extract the GC tracing code from the fields.
    list.map3(extract_gc_statements, Fields0, Fields1,
        GC_InitStmtLists, GC_TraceStmtLists),
    GC_StmtLists = GC_InitStmtLists ++ GC_TraceStmtLists,
    GC_Stmts = list.condense(GC_StmtLists),

    (
        Action = chain_gc_stack_frames,
        ml_chain_stack_frames(ModuleName, FuncName, Context,
            GC_Stmts, EnvClassId, Fields1, Fields, EnvInitializer,
            LinkStackChain, GCTraceFuncDefns),
        GCStmtEnv = gc_no_stmt
    ;
        Action = hoist_nested_funcs,
        (
            GC_Stmts = [],
            GCStmtEnv = gc_no_stmt
        ;
            GC_Stmts = [_ | _],
            GC_Block = ml_gen_block([], [], GC_Stmts, Context),
            GCStmtEnv = gc_trace_code(GC_Block)
        ),
        Fields = Fields1,
        EnvInitializer = no_initializer,
        LinkStackChain = [],
        GCTraceFuncDefns = []
    ),

    Imports = [],
    Interfaces = [],
    TypeParams = [],
    Ctors = [],
    EnvClassDefn = mlds_class_defn(EnvClassName, 0, Context,
        EnvClassFlags, EnvClassKind, Imports, Inherits, Interfaces,
        TypeParams, Fields, [], [], Ctors),

    % Generate the following variable declaration:
    %
    %   struct <EnvTypeName> env; // = { ... }
    %
    EnvVarName = env_var(Action),
    EnvTypeName = mlds_class_type(EnvClassId),
    EnvVarDecl = mlds_local_var_defn(EnvVarName, Context,
        EnvTypeName, EnvInitializer, GCStmtEnv),

    % Declare the `env_ptr' var, and initialize the `env_ptr' with the
    % address of `env'.

    % Generate code to initialize the environment pointer, either by
    % allocating an object on the heap, or by taking the address of
    % the struct we put on the stack.
    (
        OnHeap = yes,
        EnvVarAddr = ml_lval(ml_local_var(EnvVarName, EnvTypeName)),
        % OnHeap should be "yes" only on for the IL backend, for which
        % the value of MayUseAtomic is immaterial.
        % XXX The comment on the option lookup setting the value of OnHeap
        % says OnHeap may be "yes" on current backends as well.
        MayUseAtomic = may_not_use_atomic_alloc,
        MaybeAllocId = no,
        NewObj = [
            ml_stmt_atomic(
                new_object(ml_local_var(EnvVarName, EnvTypeName), no, no,
                    EnvTypeName, no, no, [], MayUseAtomic, MaybeAllocId),
                Context)
        ]
    ;
        OnHeap = no,
        EnvVarAddr = ml_mem_addr(ml_local_var(EnvVarName, EnvTypeName)),
        NewObj = []
    ),
    ml_init_env(Action, EnvClassId, EnvVarAddr, Context,
        EnvPtrVarDecl, InitEnv0),
    EnvDefns = [EnvVarDecl, EnvPtrVarDecl],
    InitEnv = NewObj ++ [InitEnv0] ++ LinkStackChain.

:- pred ml_chain_stack_frames(mlds_module_name::in,
    mlds_function_name::in, prog_context::in, list(mlds_stmt)::in,
    mlds_class_id::in,
    list(mlds_field_var_defn)::in, list(mlds_field_var_defn)::out,
    mlds_initializer::out, list(mlds_stmt)::out,
    list(mlds_function_defn)::out) is det.

ml_chain_stack_frames(ModuleName, FuncName, Context, GCTraceStmts,
        EnvClassId, Fields0, Fields, EnvInitializer,
        LinkStackChain, GCTraceFuncDefns) :-
    % Generate code to declare and initialize the environment pointer
    % for the GC trace function from that function's `this_frame' parameter:
    %
    %   struct foo_frame *frame_ptr;
    %   frame_ptr = (struct foo_frame *) this_frame;
    %
    ThisFrameName = lvn_comp_var(lvnc_this_frame),
    ThisFrameRval = ml_lval(ml_local_var(ThisFrameName, mlds_generic_type)),
    CastThisFrameRval = ml_unop(
        cast(mlds_ptr_type(mlds_class_type(EnvClassId))),
        ThisFrameRval),
    ml_init_env(chain_gc_stack_frames, EnvClassId, CastThisFrameRval,
        Context, FramePtrDecl, InitFramePtr),

    % Put the environment pointer declaration and initialization
    % and the GC tracing code in a function:
    %
    %   void foo_trace(void *this_frame) {
    %       struct foo_frame *frame_ptr;
    %       frame_ptr = (struct foo_frame *) this_frame;
    %       <GCTraceStmts>
    %   }
    %
    gen_gc_trace_func(ModuleName, FuncName, FramePtrDecl,
        [InitFramePtr | GCTraceStmts], Context,
        GCTraceFuncLabel, GCTraceFuncSignature,
        GCTraceFuncParams, GCTraceFuncDefn),
    GCTraceFuncDefns = [GCTraceFuncDefn],

    % Insert the fixed fields in the struct <EnvClassName>:
    %
    %   void *prev;
    %   void (*trace)(...);
    %
    PrevFieldName = fvn_prev,
    PrevFieldFlags = ml_gen_public_field_decl_flags,
    PrevFieldType = ml_stack_chain_type,
    PrevFieldDecl = mlds_field_var_defn(PrevFieldName, Context,
        PrevFieldFlags, PrevFieldType, no_initializer, gc_no_stmt),

    TraceFieldName = fvn_trace,
    TraceFieldFlags = ml_gen_public_field_decl_flags,
    TraceFieldType = mlds_func_type(GCTraceFuncParams),
    TraceFieldDecl = mlds_field_var_defn(TraceFieldName, Context,
        TraceFieldFlags, TraceFieldType, no_initializer, gc_no_stmt),

    Fields = [PrevFieldDecl, TraceFieldDecl | Fields0],

    % Set the initializer so that the `prev' field is initialized to the global
    % stack chain, and the `trace' field is initialized to the address of
    % the GC tracing function:
    %
    %   ... = { stack_chain, foo_trace };
    %
    % Since there no values for the remaining fields in the initializer,
    % this means the remaining fields will get initialized to zero
    % (C99 6.7.8 #21).
    %
    % XXX This uses a non-const initializer, which is a feature that is only
    % supported in C99 and GNU C; it won't work in C89. We should just generate
    % a bunch of assignments to all the fields, rather than relying on
    % initializers like this.
    %
    StackChain = ml_stack_chain_var,
    EnvInitializer = init_struct(mlds_class_type(EnvClassId), [
        init_obj(ml_lval(StackChain)),
        init_obj(ml_const(mlconst_code_addr(
            mlds_code_addr(GCTraceFuncLabel, GCTraceFuncSignature))))
    ]),

    % Generate code to set the global stack chain
    % to point to the current environment:
    %
    %    stack_chain = frame_ptr;
    %
    EnvPtrTypeName = ml_make_env_ptr_type(EnvClassId),
    EnvPtr = ml_lval(
        ml_local_var(lvn_comp_var(lvnc_frame_ptr),
            EnvPtrTypeName)),
    AssignToStackChain = assign(StackChain, EnvPtr),
    LinkStackChain = [ml_stmt_atomic(AssignToStackChain, Context)].

:- pred gen_gc_trace_func(mlds_module_name::in, mlds_function_name::in,
    mlds_local_var_defn::in, list(mlds_stmt)::in, prog_context::in,
    qual_func_label::out, mlds_func_signature::out,
    mlds_func_params::out, mlds_function_defn::out) is det.

gen_gc_trace_func(PredModule, FuncName, FramePointerDefn, GCTraceStmts,
        Context, QualGCTraceFuncLabel, Signature, FuncParams,
        GCTraceFuncDefn) :-
    % Compute the signature of the GC tracing function.
    ArgVarName = lvn_comp_var(lvnc_this_frame),
    ArgType = mlds_generic_type,
    Argument = mlds_argument(ArgVarName, ArgType, gc_no_stmt),
    FuncParams = mlds_func_params([Argument], []),
    Signature = mlds_get_func_signature(FuncParams),

    % Compute the name of the GC tracing function.
    (
        FuncName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        (
            MaybeAux = proc_func,
            GCTraceMaybeAux = gc_trace_for_proc_func
        ;
            MaybeAux = proc_aux_func(SeqNum),
            GCTraceMaybeAux = gc_trace_for_proc_aux_func(SeqNum)
        ;
            ( MaybeAux = gc_trace_for_proc_func
            ; MaybeAux = gc_trace_for_proc_aux_func(_)
            ),
            unexpected($pred, "gc trace func for gc trace func")
        ),
        GCTraceFuncLabel = mlds_func_label(ProcLabel, GCTraceMaybeAux),
        QualGCTraceFuncLabel = qual_func_label(PredModule, GCTraceFuncLabel),
        GCTracePlainFuncName = mlds_plain_func_name(GCTraceFuncLabel, PredId),
        GCTraceFuncName = mlds_function_name(GCTracePlainFuncName)
    ;
        FuncName = mlds_function_export(_),
        % XXX I (zs) think that this abort is wrong for two reasons.
        % First, a function with an exported name is still a function,
        % so the text of the abort message is misleading. Second,
        % I see no reasoning that guarantees that our caller will
        % never call here with an exported function.
        unexpected($pred, "not a function")
    ),

    % Construct the function definition.
    Stmt = ml_stmt_block([FramePointerDefn], [], GCTraceStmts, Context),
    DeclFlags = mlds_function_decl_flags(func_private, one_copy),
    MaybePredProcId = no,
    EnvVarNames = set.init,
    GCTraceFuncDefn = mlds_function_defn(GCTraceFuncName,
        Context, DeclFlags, MaybePredProcId, FuncParams,
        body_defined_here(Stmt), EnvVarNames, no).

:- pred extract_gc_statements(
    mlds_field_var_defn::in, mlds_field_var_defn::out,
    list(mlds_stmt)::out, list(mlds_stmt)::out) is det.

extract_gc_statements(FieldVarDefn0, FieldVarDefn,
        GCInitStmts, GCTraceStmts) :-
    FieldVarDefn0 = mlds_field_var_defn(Name, Context, Flags, Type,
        Init, GCStmt),
    (
        GCStmt = gc_trace_code(GCTraceStmt),
        FieldVarDefn = mlds_field_var_defn(Name, Context, Flags, Type,
            Init, gc_no_stmt),
        GCInitStmts = [],
        GCTraceStmts = [GCTraceStmt]
    ;
        GCStmt = gc_initialiser(GCInitStmt),
        FieldVarDefn = mlds_field_var_defn(Name, Context, Flags, Type,
            Init, gc_no_stmt),
        GCInitStmts = [GCInitStmt],
        GCTraceStmts = []
    ;
        GCStmt = gc_no_stmt,
        FieldVarDefn = FieldVarDefn0,
        GCInitStmts = [],
        GCTraceStmts = []
    ).

    % When converting local variables into fields of the environment struct,
    % we need to change `local' access into something else, since `local'
    % is only supposed to be used for entities that are local to a function
    % or block, not for fields. Currently we change it to `public'.
    % (Perhaps changing it to `default' might be better?)
    % XXX MLDS_DEFN
    % Since we no longer distinguish between local variables and field
    % variables using the *flags* of the old, replaced-by-now mlds_data_defn
    % structure, we should set fields to public only if actually *want*
    % them to be public.
    %
:- func convert_local_to_field(mlds_local_var_defn) = mlds_field_var_defn.

convert_local_to_field(LocalVarDefn) = FieldVarDefn :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context, Type,
        Init, GcStmt),
    FieldVarName = fvn_env_field_from_local_var(LocalVarName),
    Flags = mlds_field_var_decl_flags(per_instance, modifiable),
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, Flags, Type,
        Init, GcStmt).

:- type inserted_env
    --->    have_not_inserted_env
    ;       have_inserted_env.

    % ml_insert_init_env:
    %
    % If the definition is a nested function definition, and its body makes
    % use of the environment pointer (`env_ptr'), then insert code to declare
    % and initialize the environment pointer.
    %
    % We transform code of the form
    %   <Ret>
    %   <Func>(<Args>) {
    %       <Body>
    %   }
    %
    % to
    %
    %   <Ret>
    %   <Func>(<Args>) {
    %       struct <EnvClassName> *env_ptr;
    %       env_ptr = (<EnvClassName> *) env_ptr_arg;
    %       <Body>
    %   }
    %
    % If we perform this transformation, set !:InsertedEnv to
    % have_inserted_env, otherwise leave it unchanged.
    %
:- pred ml_insert_init_env(action::in, mlds_class_id::in,
    mlds_function_defn::in, mlds_function_defn::out,
    inserted_env::in, inserted_env::out) is det.

ml_insert_init_env(Action, ClassId, FunctionDefn0, FunctionDefn,
        !InsertedEnv) :-
    FunctionDefn0 = mlds_function_defn(Name, Context, Flags, PredProcId,
        Params, Body, EnvVarNames, MaybeRequiretailrecInfo),
    ( if
        Body = body_defined_here(FuncBody0),
        EnvPtrVar = lvn_comp_var(lvnc_env_ptr),
        statement_contains_var(FuncBody0, EnvPtrVar) = yes
    then
        EnvPtrVal = ml_lval(
            ml_local_var(lvn_comp_var(lvnc_env_ptr_arg),
                mlds_generic_env_ptr_type)),
        EnvPtrVarType = ml_make_env_ptr_type(ClassId),

        % Insert a cast, to downcast from mlds_generic_env_ptr_type to the
        % specific environment type for this procedure.
        CastEnvPtrVal = ml_unop(cast(EnvPtrVarType), EnvPtrVal),

        ml_init_env(Action, ClassId, CastEnvPtrVal, Context,
            EnvPtrDefn, InitEnvPtr),
        FuncBody = ml_stmt_block([EnvPtrDefn], [],
            [InitEnvPtr, FuncBody0], Context),
        FunctionDefn = mlds_function_defn(Name, Context, Flags,
            PredProcId, Params, body_defined_here(FuncBody),
            EnvVarNames, MaybeRequiretailrecInfo),
        !:InsertedEnv = have_inserted_env
    else
        FunctionDefn = FunctionDefn0
    ).

:- func ml_make_env_ptr_type(mlds_class_id) = mlds_type.

ml_make_env_ptr_type(EnvClassId) = EnvPtrType :-
    EnvPtrType = mlds_ptr_type(mlds_class_type(EnvClassId)).

    % Create the environment pointer and initialize it:
    %
    %   struct <EnvClassName> *env_ptr;
    %   env_ptr = <EnvPtrVal>;
    %
:- pred ml_init_env(action::in, mlds_class_id::in, mlds_rval::in,
    prog_context::in, mlds_local_var_defn::out, mlds_stmt::out) is det.

ml_init_env(Action, EnvClassId, EnvPtrVal, Context,
        EnvPtrVarDecl, InitEnvPtr) :-
    % Generate the following variable declaration:
    %
    %   <EnvClassId> *env_ptr;
    %
    EnvPtrVarName = env_ptr_var(Action),
    EnvPtrVarType = ml_make_env_ptr_type(EnvClassId),
    % The env_ptr never needs to be traced by the GC, since the environment
    % that it points to will always be on the stack, not into the heap.
    GCStmt = gc_no_stmt,
    EnvPtrVarDecl = mlds_local_var_defn(EnvPtrVarName, Context,
        EnvPtrVarType, no_initializer, GCStmt),

    % Generate the following statement:
    %
    %   env_ptr = <EnvPtrVal>;
    %
    % (note that the caller of this routine is responsible
    % for inserting a cast in <EnvPtrVal> if needed).
    %
    AssignEnvPtr =
        assign(ml_local_var(EnvPtrVarName, EnvPtrVarType), EnvPtrVal),
    InitEnvPtr = ml_stmt_atomic(AssignEnvPtr, Context).

    % Given the declaration for a function parameter, produce a declaration
    % for a corresponding local variable or environment struct field.
    % We need to do this so as to include function parameter in the
    % environment struct.
    %
:- pred ml_conv_arg_to_var(prog_context::in, mlds_argument::in,
    mlds_local_var_defn::out) is det.

ml_conv_arg_to_var(Context, Arg, LocalVarDefn) :-
    Arg = mlds_argument(VarName, Type, GCStmt),
    LocalVarDefn = mlds_local_var_defn(VarName, Context, Type,
        no_initializer, GCStmt).

:- func ml_stack_chain_var = mlds_lval.

ml_stack_chain_var = StackChain :-
    % XXX When we had module qualified variables, this code said that
    % StackChain was defined in mercury_private_builtin_module.
    % That looks wrong to me (zs), but it may be of interest to whoever
    % wants to work on accurate gc in the future.
    StackChain = ml_local_var(lvn_comp_var(lvnc_stack_chain),
        ml_stack_chain_type).

    % The type of the `stack_chain' pointer, i.e. `void *'.
    %
:- func ml_stack_chain_type = mlds_type.

ml_stack_chain_type = mlds_generic_env_ptr_type.

%---------------------------------------------------------------------------%
%
% This code does some name mangling.
% It essentially duplicates the functionality in mlds_output_name.
%
% Doing name mangling here is probably a bad idea; it might be better
% to change the MLDS data structure to allow structured type names, so that
% we don't have to do any name mangling at this point.

    % Compute the name to use for the environment struct
    % for the specified function.
    %
:- func ml_env_name(mlds_function_name, action) = mlds_class_name.

ml_env_name(FunctionName, Action) = ClassName :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        PredLabelStr = ml_pred_label_name(PredLabel),
        proc_id_to_int(ProcId, ModeNum),
        Base = env_name_base(Action),
        MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        ClassName = string.format("%s_%d%s_%s",
            [s(PredLabelStr), i(ModeNum), s(MaybeAuxSuffix), s(Base)])
    ;
        FunctionName = mlds_function_export(_),
        % XXX I (zs) think that this abort here is a bug, and that
        % our caller can legitimately give us an exported function.
        unexpected($pred, "expected function, got export")
    ).

:- func env_name_base(action) = string.

env_name_base(chain_gc_stack_frames) = "frame".
env_name_base(hoist_nested_funcs) = "env".

:- func env_var(action) = mlds_local_var_name.

env_var(chain_gc_stack_frames) = lvn_comp_var(lvnc_frame).
env_var(hoist_nested_funcs) = lvn_comp_var(lvnc_env).

:- func env_ptr_var(action) = mlds_local_var_name.

env_ptr_var(chain_gc_stack_frames) = lvn_comp_var(lvnc_frame_ptr).
env_ptr_var(hoist_nested_funcs) = lvn_comp_var(lvnc_env_ptr).

:- func ml_pred_label_name(mlds_pred_label) = string.

ml_pred_label_name(mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
        Name, Arity, _CodeModel, _NonOutputFunc)) = LabelName :-
    ( PredOrFunc = pf_predicate, Suffix = "p"
    ; PredOrFunc = pf_function, Suffix = "f"
    ),
    (
        MaybeDefiningModule = yes(DefiningModule),
        ModuleNameString = ml_module_name_string(DefiningModule),
        string.format("%s_%d_%s_in__%s",
            [s(Name), i(Arity), s(Suffix), s(ModuleNameString)], LabelName)
    ;
        MaybeDefiningModule = no,
        string.format("%s_%d_%s",
            [s(Name), i(Arity), s(Suffix)], LabelName)
    ).
ml_pred_label_name(mlds_special_pred_label(PredName, MaybeTypeModule,
        TypeName, TypeArity)) = LabelName :-
    (
        MaybeTypeModule = yes(TypeModule),
        TypeModuleString = ml_module_name_string(TypeModule),
        string.format("%s__%s__%s_%d",
            [s(PredName), s(TypeModuleString), s(TypeName), i(TypeArity)],
            LabelName)
    ;
        MaybeTypeModule = no,
        string.format("%s__%s_%d",
            [s(PredName), s(TypeName), i(TypeArity)], LabelName)
    ).

:- func ml_module_name_string(mercury_module_name) = string.

ml_module_name_string(ModuleName) = sym_name_to_string_sep(ModuleName, "__").

%---------------------------------------------------------------------------%

% flatten_function_body:
% flatten_maybe_statement:
% flatten_gc_statement:
% flatten_statements:
% flatten_statement:
%
% Recursively process the statement(s),
% - calling fixup_var on every use of a variable inside them,
% - calling flatten_nested_local_var_defns for every list of local variable
%   definitions they contain, and
% - calling flatten_nested_function_defns for every list of local function
%   definitions they contain.
%
% Also, for Action = chain_gc_stack_frames, add code to save and restore
% the stack chain pointer at any `try_commit' statements.

:- pred flatten_function_body(action, mlds_function_body, mlds_function_body,
    elim_info, elim_info).
:- mode flatten_function_body(in(hoist), in, out, in, out) is det.
:- mode flatten_function_body(in(chain), in, out, in, out) is det.

flatten_function_body(Action, Body0, Body, !Info) :-
    (
        Body0 = body_external,
        Body = Body0
    ;
        Body0 = body_defined_here(Stmt0),
        flatten_statement(Action, Stmt0, Stmt, !Info),
        Body = body_defined_here(Stmt)
    ).

:- pred flatten_maybe_statement(action, maybe(mlds_stmt), maybe(mlds_stmt),
    elim_info, elim_info).
:- mode flatten_maybe_statement(in(hoist), in, out, in, out) is det.
:- mode flatten_maybe_statement(in(chain), in, out, in, out) is det.

flatten_maybe_statement(_, no, no, !Info).
flatten_maybe_statement(Action, yes(Stmt0), yes(Stmt), !Info) :-
    flatten_statement(Action, Stmt0, Stmt, !Info).

:- pred flatten_gc_statement(action, mlds_gc_statement, mlds_gc_statement,
    elim_info, elim_info).
:- mode flatten_gc_statement(in(hoist), in, out, in, out) is det.
:- mode flatten_gc_statement(in(chain), in, out, in, out) is det.

flatten_gc_statement(Action, GCStmt0, GCStmt, !Info) :-
    (
        GCStmt0 = gc_no_stmt,
        GCStmt = gc_no_stmt
    ;
        GCStmt0 = gc_trace_code(Stmt0),
        flatten_statement(Action, Stmt0, Stmt, !Info),
        GCStmt = gc_trace_code(Stmt)
    ;
        GCStmt0 = gc_initialiser(Stmt0),
        flatten_statement(Action, Stmt0, Stmt, !Info),
        GCStmt = gc_initialiser(Stmt)
    ).

:- pred flatten_statements(action, list(mlds_stmt), list(mlds_stmt),
    elim_info, elim_info).
:- mode flatten_statements(in(hoist), in, out, in, out) is det.
:- mode flatten_statements(in(chain), in, out, in, out) is det.

flatten_statements(_, [], [], !Info).
flatten_statements(Action, [Stmt0 | Stmts0], [Stmt | Stmts], !Info) :-
    flatten_statement(Action, Stmt0, Stmt, !Info),
    flatten_statements(Action, Stmts0, Stmts, !Info).

:- pred flatten_statement(action, mlds_stmt, mlds_stmt, elim_info, elim_info).
:- mode flatten_statement(in(hoist), in, out, in, out) is det.
:- mode flatten_statement(in(chain), in, out, in, out) is det.

flatten_statement(Action, Stmt0, Stmt, !Info) :-
    (
        Stmt0 = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts0, Context),
        flatten_nested_local_var_defns(Action, LocalVarDefns0, LocalVarDefns,
            FuncDefns0, SubStmts0, InitStmts, !Info),
        flatten_nested_function_defns(Action, FuncDefns0, FuncDefns, !Info),
        flatten_statements(Action,
            InitStmts ++ SubStmts0, SubStmts, !Info),
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval0, SubStmt0, Context),
        fixup_rval(Action, !.Info, Rval0, Rval),
        flatten_statement(Action, SubStmt0, SubStmt, !Info),
        Stmt = ml_stmt_while(Kind, Rval, SubStmt, Context)
    ;
        Stmt0 = ml_stmt_if_then_else(Cond0, Then0, MaybeElse0, Context),
        fixup_rval(Action, !.Info, Cond0, Cond),
        flatten_statement(Action, Then0, Then, !Info),
        flatten_maybe_statement(Action, MaybeElse0, MaybeElse, !Info),
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse, Context)
    ;
        Stmt0 = ml_stmt_switch(Type, Val0, Range, Cases0, Default0, Context),
        fixup_rval(Action, !.Info, Val0, Val),
        flatten_cases(Action, Cases0, Cases, !Info),
        flatten_default(Action, Default0, Default, !Info),
        Stmt = ml_stmt_switch(Type, Val, Range, Cases, Default, Context)
    ;
        Stmt0 = ml_stmt_label(_, _Context),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_goto(_, _Context),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_computed_goto(Rval0, Labels, Context),
        fixup_rval(Action, !.Info, Rval0, Rval),
        Stmt = ml_stmt_computed_goto(Rval, Labels, Context)
    ;
        Stmt0 = ml_stmt_call(Sig, Func0, Args0, RetLvals0, TailCall, Context),
        fixup_rval(Action, !.Info, Func0, Func),
        fixup_rvals(Action, !.Info, Args0, Args),
        fixup_lvals(Action, !.Info, RetLvals0, RetLvals),
        Stmt = ml_stmt_call(Sig, Func, Args, RetLvals, TailCall, Context)
    ;
        Stmt0 = ml_stmt_return(Rvals0, Context),
        fixup_rvals(Action, !.Info, Rvals0, Rvals),
        Stmt = ml_stmt_return(Rvals, Context)
    ;
        Stmt0 = ml_stmt_do_commit(Ref0, Context),
        fixup_rval(Action, !.Info, Ref0, Ref),
        Stmt = ml_stmt_do_commit(Ref, Context)
    ;
        Stmt0 = ml_stmt_try_commit(Ref0, BodyStmt0, HandlerStmt0, Context),
        fixup_lval(Action, !.Info, Ref0, Ref),
        flatten_statement(Action, BodyStmt0, BodyStmt, !Info),
        flatten_statement(Action, HandlerStmt0, HandlerStmt1, !Info),
        Stmt1 = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt1, Context),
        (
            Action = chain_gc_stack_frames,
            save_and_restore_stack_chain(Stmt1, Stmt, !Info)
        ;
            Action = hoist_nested_funcs,
            Stmt = Stmt1
        )
    ;
        Stmt0 = ml_stmt_atomic(AtomicStmt0, Context),
        fixup_atomic_stmt(Action, !.Info, AtomicStmt0, AtomicStmt),
        Stmt = ml_stmt_atomic(AtomicStmt, Context)
    ).

:- pred flatten_cases(action, list(mlds_switch_case), list(mlds_switch_case),
    elim_info, elim_info).
:- mode flatten_cases(in(hoist), in, out, in, out) is det.
:- mode flatten_cases(in(chain), in, out, in, out) is det.

flatten_cases(_, [], [], !Info).
flatten_cases(Action, [Case0 | Cases0], [Case | Cases], !Info) :-
    flatten_case(Action, Case0, Case, !Info),
    flatten_cases(Action, Cases0, Cases, !Info).

:- pred flatten_case(action, mlds_switch_case, mlds_switch_case,
    elim_info, elim_info).
:- mode flatten_case(in(hoist), in, out, in, out) is det.
:- mode flatten_case(in(chain), in, out, in, out) is det.

flatten_case(Action, Case0, Case, !Info) :-
    Case0 = mlds_switch_case(FirstCond0, LaterConds0, Stmt0),
    fixup_case_cond(Action, !.Info, FirstCond0, FirstCond),
    fixup_case_conds(Action, !.Info, LaterConds0, LaterConds),
    flatten_statement(Action, Stmt0, Stmt, !Info),
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- pred flatten_default(action, mlds_switch_default, mlds_switch_default,
    elim_info, elim_info).
:- mode flatten_default(in(hoist), in, out, in, out) is det.
:- mode flatten_default(in(chain), in, out, in, out) is det.

flatten_default(Action, Default0, Default, !Info) :-
    (
        Default0 = default_is_unreachable,
        Default = default_is_unreachable
    ;
        Default0 = default_do_nothing,
        Default = default_do_nothing
    ;
        Default0 = default_case(Stmt0),
        flatten_statement(Action, Stmt0, Stmt, !Info),
        Default = default_case(Stmt)
    ).

%---------------------------------------------------------------------------%

    % Add code to save/restore the stack chain pointer. This means converting
    %
    %   try {
    %       Stmt
    %   } commit {
    %       Handler
    %   }
    %
    % into
    %
    %   {
    %       void *saved_stack_chain;
    %       try {
    %           saved_stack_chain = stack_chain;
    %           Stmt
    %       } commit {
    %           stack_chain = saved_stack_chain;
    %           Handler
    %       }
    %   }
    %
:- inst try_commit for mlds_stmt/0
    --->    ml_stmt_try_commit(ground, ground, ground, ground).

:- pred save_and_restore_stack_chain(mlds_stmt::in(try_commit),
    mlds_stmt::out, elim_info::in, elim_info::out) is det.

save_and_restore_stack_chain(Stmt0, Stmt, !ElimInfo) :-
    elim_info_allocate_saved_stack_chain_id(Id, !ElimInfo),

    Stmt0 = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt0, Context),
    BodyContext = get_mlds_stmt_context(BodyStmt0),
    HandlerContext = get_mlds_stmt_context(HandlerStmt0),
    SavedVarDefn = gen_saved_stack_chain_var(Id, BodyContext),
    gen_save_and_restore_of_stack_chain_var(Id, BodyContext,
        SaveStmt, RestoreStmt),
    BodyStmt =
        ml_stmt_block([], [], [SaveStmt, BodyStmt0], BodyContext),
    HandlerStmt =
        ml_stmt_block([], [], [RestoreStmt, HandlerStmt0], HandlerContext),
    TryCommit = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, Context),
    Stmt = ml_stmt_block([SavedVarDefn], [], [TryCommit], Context).

%---------------------------------------------------------------------------%

% flatten_nested_function_defns:
% flatten_nested_function_defn:
% flatten_nested_local_var_defns:
% flatten_nested_local_var_defn:
%
% Hoist out nested function definitions, and any local variables that need
% to go in the environment struct (e.g. because they are referenced by
% nested functions), storing them both in the elim_info. Convert initializers
% for local variables that need to go in the environment struct into assignment
% statements. Return the remaining (non-hoisted) definitions, the list of
% assignment statements, and the updated elim_info.

:- pred flatten_nested_function_defns(action,
    list(mlds_function_defn), list(mlds_function_defn), elim_info, elim_info).
:- mode flatten_nested_function_defns(in(hoist), in, out, in, out) is det.
:- mode flatten_nested_function_defns(in(chain), in, out, in, out) is det.

flatten_nested_function_defns(_, [], [], !Info).
flatten_nested_function_defns(Action,
        [HeadFuncDefn0 | TailFuncDefns0], FuncDefns, !Info) :-
    flatten_nested_function_defn(Action, HeadFuncDefn0, HeadFuncDefns,
        !Info),
    flatten_nested_function_defns(Action, TailFuncDefns0, TailFuncDefns,
        !Info),
    FuncDefns = HeadFuncDefns ++ TailFuncDefns.

:- pred flatten_nested_function_defn(action,
    mlds_function_defn, list(mlds_function_defn), elim_info, elim_info).
:- mode flatten_nested_function_defn(in(hoist), in, out, in, out) is det.
:- mode flatten_nested_function_defn(in(chain), in, out, in, out) is det.

flatten_nested_function_defn(Action, FuncDefn0, FuncDefns, !Info) :-
    FuncDefn0 = mlds_function_defn(Name, Context, Flags0, PredProcId, Params,
        FuncBody0, EnvVarNames, MaybeRequiretailrecInfo),
    % Recursively flatten the nested function.
    flatten_function_body(Action, FuncBody0, FuncBody, !Info),

    % Mark the function as private / one_copy, rather than as
    % local / per_instance, if we are about to hoist it out to the
    % top level.
    (
        Action = hoist_nested_funcs,
        Flags = mlds_function_decl_flags(func_private, one_copy)
    ;
        Action = chain_gc_stack_frames,
        Flags = Flags0
    ),
    FuncDefn = mlds_function_defn(Name, Context, Flags, PredProcId, Params,
        FuncBody, EnvVarNames, MaybeRequiretailrecInfo),
    (
        Action = hoist_nested_funcs,
        % Note that we assume that we can safely hoist stuff inside nested
        % functions into the containing function. If that wasn't the case,
        % we would need code something like this:
        % LocalVars = elim_info_get_local_vars(ElimInfo),
        % OuterVars0 = elim_info_get_outer_vars(ElimInfo),
        % OuterVars = [LocalVars | OuterVars0],
        % FlattenedDefns = ml_elim_nested_defns(ModuleName,
        %   OuterVars, Defn0),
        % list.foldl(elim_info_add_nested_func, FlattenedDefns),

        % Strip out the now flattened nested function, and store it
        % in the elim_info.
        elim_info_add_nested_func(FuncDefn, !Info),
        FuncDefns = []
    ;
        Action = chain_gc_stack_frames,
        FuncDefns = [FuncDefn]
    ).

:- pred flatten_nested_local_var_defns(action,
    list(mlds_local_var_defn), list(mlds_local_var_defn),
    list(mlds_function_defn), list(mlds_stmt), list(mlds_stmt),
    elim_info, elim_info).
:- mode flatten_nested_local_var_defns(in(hoist), in, out, in, in, out,
    in, out) is det.
:- mode flatten_nested_local_var_defns(in(chain), in, out, in, in, out,
    in, out) is det.

flatten_nested_local_var_defns(_, [], [], _, _, [], !Info).
flatten_nested_local_var_defns(Action,
        [HeadLocalVarDefn0 | TailLocalVarDefns0], LocalVarDefns, FuncDefns,
        Stmts, InitStmts, !Info) :-
    flatten_nested_local_var_defn(Action,
        HeadLocalVarDefn0, TailLocalVarDefns0, HeadLocalVarDefns,
        FuncDefns, Stmts, HeadInitStmts, !Info),
    flatten_nested_local_var_defns(Action,
        TailLocalVarDefns0, TailLocalVarDefns,
        FuncDefns, Stmts, TailInitStmts, !Info),
    LocalVarDefns = HeadLocalVarDefns ++ TailLocalVarDefns,
    InitStmts = HeadInitStmts ++ TailInitStmts.

:- pred flatten_nested_local_var_defn(action,
    mlds_local_var_defn, list(mlds_local_var_defn), list(mlds_local_var_defn),
    list(mlds_function_defn), list(mlds_stmt), list(mlds_stmt),
    elim_info, elim_info).
:- mode flatten_nested_local_var_defn(in(hoist), in, in, out, in, in, out,
    in, out) is det.
:- mode flatten_nested_local_var_defn(in(chain), in, in, out, in, in, out,
    in, out) is det.

flatten_nested_local_var_defn(Action, HeadLocalVarDefn0, _TailLocalVarDefns0,
        HeadLocalVarDefns, FuncDefns, Stmts, InitStmts, !Info) :-
    HeadLocalVarDefn0 = mlds_local_var_defn(LocalVarName, Context,
        Type, Init0, GCStmt),
    % For local variable definitions, if they are referenced by any nested
    % functions, then strip them out and store them in the elim_info.
    ( if
        % Hoist ordinary local variables.
        ml_should_add_local_var(Action, LocalVarName, GCStmt,
            FuncDefns, Stmts)
    then
        % We need to strip out the initializer (if any) and convert it
        % into an assignment statement, since this local variable
        % is going to become a field, and fields can't have initializers.
        (
            Init0 = init_obj(Rval),
            Init = no_initializer,
            HeadLocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
                Type, Init, GCStmt),
            % XXX BUG! Converting the initializer to an assignment doesn't
            % work, because it doesn't handle the case when initializers in
            % _TailLocalVarDefns0 reference this variable.
            VarLval = ml_local_var(LocalVarName, Type),
            InitStmt = ml_stmt_atomic(assign(VarLval, Rval), Context),
            InitStmts = [InitStmt]
        ;
            Init0 = init_struct(_, _),
            unexpected($pred, "init_struct")
        ;
            Init0 = init_array(_),
            % XXX We do generate init_array initializers for some local
            % variables.
            unexpected($pred, "init_array")
        ;
            Init0 = no_initializer,
            HeadLocalVarDefn = HeadLocalVarDefn0,
            InitStmts = []
        ),
        elim_info_add_local_var(HeadLocalVarDefn, !Info),
        HeadLocalVarDefns = []
    else
        fixup_initializer(Action, !.Info, Init0, Init),
        HeadLocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
            Type, Init, GCStmt),
        HeadLocalVarDefns = [HeadLocalVarDefn],
        InitStmts = []
    ).

    % Succeed iff we should add the definition of this variable to the
    % ei_local_vars field of the elim_info, meaning that it should be added
    % to the environment struct (if it is a variable) or hoisted out to the
    % top level (if it is a static const).
    %
:- pred ml_should_add_local_var(action, mlds_local_var_name,
    mlds_gc_statement, list(mlds_function_defn), list(mlds_stmt)).
:- mode ml_should_add_local_var(in(hoist), in, in, in, in) is semidet.
:- mode ml_should_add_local_var(in(chain), in, in, in, in) is semidet.

ml_should_add_local_var(Action, VarName, GCStmt, FuncDefns, FollowingStmts) :-
    (
        Action = chain_gc_stack_frames,
        ( GCStmt = gc_trace_code(_)
        ; GCStmt = gc_initialiser(_)
        )
    ;
        Action = hoist_nested_funcs,
        ml_need_to_hoist(VarName, FuncDefns, FollowingStmts)
    ).

    % This checks for a nested function definition.
    %
    % XXX Do we need to check for references from the GCStmt fields here?
    %
    % XXX This algorithm is quadratic. For a block with N defs, each of which
    % is referenced in a later definition, we do N^2 tests.
    %
:- pred ml_need_to_hoist(mlds_local_var_name::in,
    list(mlds_function_defn)::in, list(mlds_stmt)::in) is semidet.

ml_need_to_hoist(VarName, FuncDefns, FollowingStmts) :-
    Filter = ml_need_to_hoist_defn(VarName),
    (
        list.find_first_match(Filter, FuncDefns, _)
    ;
        statements_contains_matching_defn(Filter, FollowingStmts)
    ).

:- pred ml_need_to_hoist_defn(mlds_local_var_name::in, mlds_function_defn::in)
    is semidet.

ml_need_to_hoist_defn(QualVarName, FuncDefn) :-
    function_defn_contains_var(FuncDefn, QualVarName) = yes.

%---------------------------------------------------------------------------%

% fixup_initializers:
% fixup_initializer:
% fixup_atomic_stmt:
% fixup_case_conds:
% fixup_case_cond:
% fixup_target_code_components:
% fixup_target_code_component:
% fixup_trail_op:
% fixup_typed_rvals:
% fixup_rvals:
% fixup_rval:
% fixup_lvals:
% fixup_lval:
%
% Recursively process the specified construct, calling fixup_var on
% every variable inside it.

:- pred fixup_initializers(action, elim_info,
    list(mlds_initializer), list(mlds_initializer)).
:- mode fixup_initializers(in(hoist), in, in, out) is det.
:- mode fixup_initializers(in(chain), in, in, out) is det.

fixup_initializers(_, _, [], []).
fixup_initializers(Action, Info,
        [Initializer0 | Initializers0], [Initializer | Initializers]) :-
    fixup_initializer(Action, Info, Initializer0, Initializer),
    fixup_initializers(Action, Info, Initializers0, Initializers).

:- pred fixup_initializer(action, elim_info,
    mlds_initializer, mlds_initializer).
:- mode fixup_initializer(in(hoist), in, in, out) is det.
:- mode fixup_initializer(in(chain), in, in, out) is det.

fixup_initializer(Action, Info, Initializer0, Initializer) :-
    (
        Initializer0 = no_initializer,
        Initializer = Initializer0
    ;
        Initializer0 = init_obj(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Initializer = init_obj(Rval)
    ;
        Initializer0 = init_struct(Type, Members0),
        fixup_initializers(Action, Info, Members0, Members),
        Initializer = init_struct(Type, Members)
    ;
        Initializer0 = init_array(Elements0),
        fixup_initializers(Action, Info, Elements0, Elements),
        Initializer = init_array(Elements)
    ).

:- pred fixup_atomic_stmt(action, elim_info,
    mlds_atomic_statement, mlds_atomic_statement).
:- mode fixup_atomic_stmt(in(hoist), in, in, out) is det.
:- mode fixup_atomic_stmt(in(chain), in, in, out) is det.

fixup_atomic_stmt(Action, Info, Atomic0, Atomic) :-
    (
        ( Atomic0 = comment(_)
        ; Atomic0 = gc_check
        ),
        Atomic = Atomic0
    ;
        Atomic0 = assign(Lval0, Rval0),
        fixup_lval(Action, Info, Lval0, Lval),
        fixup_rval(Action, Info, Rval0, Rval),
        Atomic = assign(Lval, Rval)
    ;
        Atomic0 = assign_if_in_heap(Lval0, Rval0),
        fixup_lval(Action, Info, Lval0, Lval),
        fixup_rval(Action, Info, Rval0, Rval),
        Atomic = assign_if_in_heap(Lval, Rval)
    ;
        Atomic0 = delete_object(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Atomic = delete_object(Rval)
    ;
        Atomic0 = new_object(Target0, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, ArgRvalsTypes0, MayUseAtomic,
            MaybeAllocId),
        fixup_lval(Action, Info, Target0, Target),
        fixup_typed_rvals(Action, Info, ArgRvalsTypes0, ArgRvalsTypes),
        Atomic = new_object(Target, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, ArgRvalsTypes, MayUseAtomic,
            MaybeAllocId)
    ;
        Atomic0 = mark_hp(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Atomic = mark_hp(Lval)
    ;
        Atomic0 = restore_hp(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Atomic = restore_hp(Rval)
    ;
        Atomic0 = trail_op(TrailOp0),
        fixup_trail_op(Action, Info, TrailOp0, TrailOp),
        Atomic = trail_op(TrailOp)
    ;
        Atomic0 = inline_target_code(Lang, Components0),
        fixup_target_code_components(Action, Info, Components0, Components),
        Atomic = inline_target_code(Lang, Components)
    ;
        Atomic0 = outline_foreign_proc(Lang, Vs, Lvals0, Code),
        fixup_lvals(Action, Info, Lvals0, Lvals),
        Atomic = outline_foreign_proc(Lang, Vs, Lvals, Code)
    ).

:- pred fixup_case_conds(action, elim_info,
    list(mlds_case_match_cond), list(mlds_case_match_cond)).
:- mode fixup_case_conds(in(hoist), in, in, out) is det.
:- mode fixup_case_conds(in(chain), in, in, out) is det.

fixup_case_conds(_, _, [], []).
fixup_case_conds(Action, Info, [Cond0 | Conds0], [Cond | Conds]) :-
    fixup_case_cond(Action, Info, Cond0, Cond),
    fixup_case_conds(Action, Info, Conds0, Conds).

:- pred fixup_case_cond(action, elim_info,
    mlds_case_match_cond, mlds_case_match_cond).
:- mode fixup_case_cond(in(hoist), in, in, out) is det.
:- mode fixup_case_cond(in(chain), in, in, out) is det.

fixup_case_cond(Action, Info, Cond0, Cond) :-
    (
        Cond0 = match_value(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Cond = match_value(Rval)
    ;
        Cond0 = match_range(Low0, High0),
        fixup_rval(Action, Info, Low0, Low),
        fixup_rval(Action, Info, High0, High),
        Cond = match_range(Low, High)
    ).

:- pred fixup_target_code_components(action, elim_info,
    list(target_code_component), list(target_code_component)).
:- mode fixup_target_code_components(in(hoist), in, in, out) is det.
:- mode fixup_target_code_components(in(chain), in, in, out) is det.

fixup_target_code_components(_, _, [], []).
fixup_target_code_components(Action, Info,
        [Component0 | Components0], [Component | Components]) :-
    fixup_target_code_component(Action, Info, Component0, Component),
    fixup_target_code_components(Action, Info, Components0, Components).

:- pred fixup_target_code_component(action, elim_info,
    target_code_component, target_code_component).
:- mode fixup_target_code_component(in(hoist), in, in, out) is det.
:- mode fixup_target_code_component(in(chain), in, in, out) is det.

fixup_target_code_component(Action, Info, Component0, Component) :-
    (
        ( Component0 = raw_target_code(_Code)
        ; Component0 = user_target_code(_Code, _Context)
        ; Component0 = target_code_type(_Type)
        ; Component0 = target_code_function_name(_Name)
        ; Component0 = target_code_alloc_id(_AllocId)
        ),
        Component = Component0
    ;
        Component0 = target_code_input(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Component = target_code_input(Rval)
    ;
        Component0 = target_code_output(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Component = target_code_output(Lval)
    ).

:- pred fixup_trail_op(action, elim_info, trail_op, trail_op).
:- mode fixup_trail_op(in(hoist), in, in, out) is det.
:- mode fixup_trail_op(in(chain), in, in, out) is det.

fixup_trail_op(Action, Info, Op0, Op) :-
    (
        Op0 = store_ticket(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Op = store_ticket(Lval)
    ;
        Op0 = reset_ticket(Rval0, Reason),
        fixup_rval(Action, Info, Rval0, Rval),
        Op = reset_ticket(Rval, Reason)
    ;
        ( Op0 = discard_ticket
        ; Op0 = prune_ticket
        ),
        Op = Op0
    ;
        Op0 = mark_ticket_stack(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Op = mark_ticket_stack(Lval)
    ;
        Op0 = prune_tickets_to(Rval0),
        fixup_rval(Action, Info, Rval0, Rval),
        Op = prune_tickets_to(Rval)
    ).

:- pred fixup_typed_rvals(action, elim_info,
    list(mlds_typed_rval), list(mlds_typed_rval)).
:- mode fixup_typed_rvals(in(hoist), in, in, out) is det.
:- mode fixup_typed_rvals(in(chain), in, in, out) is det.

fixup_typed_rvals(_, _, [], []).
fixup_typed_rvals(Action, Info,
        [TypedRval0 | TypedRvals0], [TypedRval | TypedRvals]) :-
    TypedRval0 = ml_typed_rval(Rval0, Type),
    fixup_rval(Action, Info, Rval0, Rval),
    TypedRval = ml_typed_rval(Rval, Type),
    fixup_typed_rvals(Action, Info, TypedRvals0, TypedRvals).

:- pred fixup_rvals(action, elim_info, list(mlds_rval), list(mlds_rval)).
:- mode fixup_rvals(in(hoist), in, in, out) is det.
:- mode fixup_rvals(in(chain), in, in, out) is det.

fixup_rvals(_, _, [], []).
fixup_rvals(Action, Info, [Rval0 | Rvals0], [Rval | Rvals]) :-
    fixup_rval(Action, Info, Rval0, Rval),
    fixup_rvals(Action, Info, Rvals0, Rvals).

:- pred fixup_rval(action, elim_info, mlds_rval, mlds_rval).
:- mode fixup_rval(in(hoist), in, in, out) is det.
:- mode fixup_rval(in(chain), in, in, out) is det.

fixup_rval(Action, Info, Rval0, Rval) :-
    (
        Rval0 = ml_lval(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Rval = ml_lval(Lval)
    ;
        Rval0 = ml_mem_addr(Lval0),
        fixup_lval(Action, Info, Lval0, Lval),
        Rval = ml_mem_addr(Lval)
    ;
        Rval0 = ml_mkword(Tag, BaseRval0),
        fixup_rval(Action, Info, BaseRval0, BaseRval),
        Rval = ml_mkword(Tag, BaseRval)
    ;
        Rval0 = ml_unop(UnOp, XRval0),
        fixup_rval(Action, Info, XRval0, XRval),
        Rval = ml_unop(UnOp, XRval)
    ;
        Rval0 = ml_binop(BinOp, XRval0, YRval0),
        fixup_rval(Action, Info, XRval0, XRval),
        fixup_rval(Action, Info, YRval0, YRval),
        Rval = ml_binop(BinOp, XRval, YRval)
    ;
        Rval0 = ml_vector_common_row_addr(VectorCommon, RowRval0),
        fixup_rval(Action, Info, RowRval0, RowRval),
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval)
    ;
        ( Rval0 = ml_const(_)
        ; Rval0 = ml_scalar_common(_)
        ; Rval0 = ml_scalar_common_addr(_)
        ; Rval0 = ml_self(_)
        ),
        Rval = Rval0
    ).

:- pred fixup_lvals(action, elim_info, list(mlds_lval), list(mlds_lval)).
:- mode fixup_lvals(in(hoist), in, in, out) is det.
:- mode fixup_lvals(in(chain), in, in, out) is det.

fixup_lvals(_, _, [], []).
fixup_lvals(Action, Info, [X0 | Xs0], [X | Xs]) :-
    fixup_lval(Action, Info, X0, X),
    fixup_lvals(Action, Info, Xs0, Xs).

:- pred fixup_lval(action, elim_info, mlds_lval, mlds_lval).
:- mode fixup_lval(in(hoist), in, in, out) is det.
:- mode fixup_lval(in(chain), in, in, out) is det.

fixup_lval(Action, Info, Lval0, Lval) :-
    (
        Lval0 = ml_field(MaybeTag, Rval0, FieldId, FieldType, PtrType),
        fixup_rval(Action, Info, Rval0, Rval),
        Lval = ml_field(MaybeTag, Rval, FieldId, FieldType, PtrType)
    ;
        Lval0 = ml_mem_ref(Rval0, Type),
        fixup_rval(Action, Info, Rval0, Rval),
        Lval = ml_mem_ref(Rval, Type)
    ;
        ( Lval0 = ml_global_var(_, _)
        ; Lval0 = ml_target_global_var_ref(_)
        ),
        Lval = Lval0
    ;
        Lval0 = ml_local_var(Var0, VarType),
        fixup_var(Action, Info, Var0, VarType, Lval)
    ).

% fixup_gc_statements:
%
% Process the trace code in the locals that have been hoisted to the stack
% frame structure so that the code correctly refers to any variables that
% have been pulled out. It assumes the locals don't actually change during
% the process. I think this should be safe. (schmidt)

:- pred fixup_gc_statements(action, elim_info, elim_info).
:- mode fixup_gc_statements(in(hoist), in, out) is det.
:- mode fixup_gc_statements(in(chain), in, out) is det.

fixup_gc_statements(Action, !Info) :-
    % We must preserve the order for the Java backend, otherwise the generated
    % code may contain closure_layout vectors that reference typevar vectors
    % which are defined later.
    LocalsCord0 = elim_info_get_local_vars(!.Info),
    cord.map_foldl(fixup_gc_statements_defn(Action),
        LocalsCord0, LocalsCord, !Info),
    elim_info_set_local_vars(LocalsCord, !Info).

:- pred fixup_gc_statements_defn(action,
    mlds_local_var_defn, mlds_local_var_defn, elim_info, elim_info).
% We need this predicate to have a single mode for cord.map_foldl.
:- mode fixup_gc_statements_defn(in, in, out, in, out) is det.

fixup_gc_statements_defn(Action, Defn0, Defn, !Info) :-
    Defn0 = mlds_local_var_defn(Name, Context, Type, Init, GCStmt0),
    (
        Action = hoist_nested_funcs,
        flatten_gc_statement(Action, GCStmt0, GCStmt, !Info)
    ;
        Action = chain_gc_stack_frames,
        flatten_gc_statement(Action, GCStmt0, GCStmt, !Info)
    ),
    Defn = mlds_local_var_defn(Name, Context, Type, Init, GCStmt).

%---------------------------------------------------------------------------%

    % Change up any references to local vars in the containing function
    % to go via the environment pointer.
    %
:- pred fixup_var(action, elim_info, mlds_local_var_name, mlds_type,
    mlds_lval).
:- mode fixup_var(in(hoist), in, in, in, out) is det.
:- mode fixup_var(in(chain), in, in, in, out) is det.

fixup_var(Action, Info, ThisVarName, ThisVarType, Lval) :-
    Locals = elim_info_get_local_vars(Info),
    ClassId = elim_info_get_env_type_name(Info),
    EnvPtrVarType = elim_info_get_env_ptr_type_name(Info),
    Target = elim_info_get_target_lang(Info),
    ( if
        % Check for references to local variables that are used by
        % nested functions, and replace them with `env_ptr->foo'.
        DefnIsThisVar =
            ( pred(Defn::in) is semidet :-
                Defn ^ mlvd_name = ThisVarName
            ),
        cord.find_first_match(DefnIsThisVar, Locals, ThisVarDefn)
    then
        FieldType = ThisVarDefn ^ mlvd_type,
        EnvPtr = ml_lval(ml_local_var(env_ptr_var(Action), EnvPtrVarType)),
        EnvModuleName = ml_env_module_name(Target, ClassId),
        FieldName = ml_field_named(
            qual_field_var_name(EnvModuleName, type_qual,
                fvn_env_field_from_local_var(ThisVarName)),
            EnvPtrVarType),
        Tag = yes(0),
        Lval = ml_field(Tag, EnvPtr, FieldName, FieldType, EnvPtrVarType)
    else if
        % Check for references to the env_ptr itself.
        % For those, the code generator will have left the type as
        % mlds_unknown_type, and we need to fill it in here.
        Action = hoist_nested_funcs,
        ThisVarName = lvn_comp_var(lvnc_env_ptr),
        ThisVarType = mlds_unknown_type
    then
        Lval = ml_local_var(ThisVarName, EnvPtrVarType)
    else
        % Leave everything else unchanged.
        Lval = ml_local_var(ThisVarName, ThisVarType)
    ).

% The following code is what we would have to use if we couldn't
% just hoist all local variables out to the outermost function.
%   ( if
%       % Check for references to local variables
%       % that are used by nested functions,
%       % and replace them with `(&env)->foo'.
%       % (The MLDS doesn't have any representation
%       % for `env.foo'.)
%       %
%       list.member(Var, Locals),
%       Var = mlds_defn(data(var(ThisVarName)), _, _, _)
%   then
%       Env = var(qual(ModuleName, module_qual, "env")),
%       FieldName = named_field(ThisVar),
%       Tag = yes(0),
%       Lval = field(Tag, mem_addr(Env), FieldName)
%   else if
%       % Check for references to variables in the
%       % containing function(s), and replace them
%       % with envptr->foo, envptr->envptr->foo, etc.
%       % depending on the depth of nesting.
%       %
%       outervar_member(ThisVarName, OuterVars, 1, Depth)
%   then
%       EnvPtrName = qual(ModuleName, module_qual, "env_ptr"),
%       EnvPtr = lval(var(EnvPtrName)),
%       Lval = make_envptr_ref(Depth, EnvPtr, EnvPtrName, ThisVar)
%   else
%       %
%       % leave everything else unchanged
%       %
%       Lval = var(ThisVar, ThisVarType)
%   ).
%
%   % check if the specified variable is contained in the
%   % outervars, and if so, return the depth of nesting
%   %
% :- pred outervar_member(mlds_var_name::in, outervars::in, int::in, int::out)
%   is semidet.
%
% outervar_member(ThisVarName, [OuterVars | OtherOuterVars], Depth0, Depth) :-
%   ( if
%       list.member(Var, OuterVars),
%       Var = mlds_defn(data(var(ThisVarName)), _, _, _)
%   then
%       Depth = Depth0
%   else
%       outervar_member(ThisVarName, OtherOuterVars, Depth0 + 1, Depth)
%   ).
%
%   % Produce a reference to a variable via `Depth' levels
%   % of `envptr->' indirections.
%   %
% :- func make_envptr_ref(int, mlds_rval, mlds_var, mlds_var) = lval.
%
% make_envptr_ref(Depth, CurEnvPtr, EnvPtrVar, Var) = Lval :-
%   ( if Depth = 1 then
%       Tag = yes(0),
%       Lval = field(Tag, CurEnvPtr, named_field(Var))
%   else
%       Tag = yes(0),
%       NewEnvPtr = lval(field(Tag, CurEnvPtr, named_field(EnvPtrVar))),
%       Lval = make_envptr_ref(Depth - 1, NewEnvPtr, EnvPtrVar, Var)
%   ).

:- func ml_env_module_name(mlds_target_lang, mlds_class_id) = mlds_module_name.

ml_env_module_name(Target, ClassId) = EnvModuleName :-
    ClassId = mlds_class_id(ClassModuleName, Arity, _Kind),
    ClassModuleName = qual_class_name(ClassModule, QualKind, ClassName),
    EnvModuleName = mlds_append_class_qualifier(Target, ClassModule,
        QualKind, ClassName, Arity).

%---------------------------------------------------------------------------%
%
% Succeed if the specified construct contains a definition for which the
% given filter predicate succeeds.
%

:- pred statements_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet),
    list(mlds_stmt)::in) is semidet.

statements_contains_matching_defn(Filter, [Stmt | Stmts]) :-
    (
        statement_contains_matching_defn(Filter, Stmt)
    ;
        statements_contains_matching_defn(Filter, Stmts)
    ).

:- pred statement_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet),
    mlds_stmt::in) is semidet.

statement_contains_matching_defn(Filter, Stmt) :-
    require_complete_switch [Stmt]
    (
        Stmt = ml_stmt_block(_LocalVarDefns, FuncDefns, SubStmts, _Context),
        % Local var definitions contain no other definitions.
        ( function_defns_contains_matching_defn(Filter, FuncDefns)
        ; statements_contains_matching_defn(Filter, SubStmts)
        )
    ;
        Stmt = ml_stmt_while(_Kind, _Rval, SubStmt, _Context),
        statement_contains_matching_defn(Filter, SubStmt)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, SubThen, MaybeSubElse, _Context),
        (
            statement_contains_matching_defn(Filter, SubThen)
        ;
            MaybeSubElse = yes(SubElse),
            statement_contains_matching_defn(Filter, SubElse)
        )
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, SubCases, SubDefault,
            _Context),
        ( cases_contains_matching_defn(Filter, SubCases)
        ; default_contains_matching_defn(Filter, SubDefault)
        )
    ;
        Stmt = ml_stmt_try_commit(_Ref, SubStmt, SubHandler, _Context),
        ( statement_contains_matching_defn(Filter, SubStmt)
        ; statement_contains_matching_defn(Filter, SubHandler)
        )
    ;
        ( Stmt = ml_stmt_label(_Label, _Context)
        ; Stmt = ml_stmt_goto(_Target, _Context)
        ; Stmt = ml_stmt_computed_goto(_Rval, _Labels, _Context)
        ; Stmt = ml_stmt_call(_Sig, _Func, _Args, _RetLvals, _TailCall,
            _Context)
        ; Stmt = ml_stmt_return(_Rvals, _Context)
        ; Stmt = ml_stmt_do_commit(_Ref, _Context)
        ; Stmt = ml_stmt_atomic(_AtomicStmt, _Context)
        ),
        fail
    ).

:- pred cases_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet),
    list(mlds_switch_case)::in) is semidet.

cases_contains_matching_defn(Filter, [Case | Cases]) :-
    (
        case_contains_matching_defn(Filter, Case)
    ;
        cases_contains_matching_defn(Filter, Cases)
    ).

:- pred case_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet),
    mlds_switch_case::in) is semidet.

case_contains_matching_defn(Filter, Case) :-
    Case = mlds_switch_case(_FirstMatchCond, _LaterMatchConds, Stmt),
    statement_contains_matching_defn(Filter, Stmt).

:- pred default_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet), mlds_switch_default::in)
    is semidet.

% default_contains_matching_defn(_, default_do_nothing) :- fail.
% default_contains_matching_defn(_, default_is_unreachable) :- fail.
default_contains_matching_defn(Filter, default_case(Stmt)) :-
    statement_contains_matching_defn(Filter, Stmt).

:- pred function_defns_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet),
    list(mlds_function_defn)::in) is semidet.

function_defns_contains_matching_defn(Filter, [FuncDefn | FuncDefns]) :-
    (
        Filter(FuncDefn)
    ;
        function_defn_contains_matching_defn(Filter, FuncDefn)
    ;
        function_defns_contains_matching_defn(Filter, FuncDefns)
    ).

:- pred function_defn_contains_matching_defn(
    pred(mlds_function_defn)::in(pred(in) is semidet), mlds_function_defn::in)
    is semidet.

function_defn_contains_matching_defn(Filter, FuncDefn) :-
    FuncDefn = mlds_function_defn(_Name, _Ctxt, _Flags, _PredProcId, _Params,
        FunctionBody, _EnvVarNames, _MaybeRequiretailrecInfo),
    FunctionBody = body_defined_here(Stmt),
    statement_contains_matching_defn(Filter, Stmt).

%---------------------------------------------------------------------------%

    % Add code to unlink the stack chain before any explicit returns or
    % tail calls.
    %
:- pred add_unchain_stack_to_maybe_statement(action,
    maybe(mlds_stmt), maybe(mlds_stmt), elim_info, elim_info).
% :- mode add_unchain_stack_to_maybe_statement(in(hoist), in, out, in, out)
%     is det.
:- mode add_unchain_stack_to_maybe_statement(in(chain), in, out, in, out)
    is det.

add_unchain_stack_to_maybe_statement(_, no, no, !Info).
add_unchain_stack_to_maybe_statement(Action, yes(Stmt0), yes(Stmt), !Info) :-
    add_unchain_stack_to_stmt(Action, Stmt0, Stmt, !Info).

:- pred add_unchain_stack_to_stmts(action,
    list(mlds_stmt), list(mlds_stmt), elim_info, elim_info).
% :- mode add_unchain_stack_to_stmts(in(hoist), in, out, in, out) is det.
:- mode add_unchain_stack_to_stmts(in(chain), in, out, in, out) is det.

add_unchain_stack_to_stmts(_, [], [], !Info).
add_unchain_stack_to_stmts(Action, [Stmt0 | Stmts0], [Stmt | Stmts], !Info) :-
    add_unchain_stack_to_stmt(Action, Stmt0, Stmt, !Info),
    add_unchain_stack_to_stmts(Action, Stmts0, Stmts, !Info).

:- pred add_unchain_stack_to_stmt(action,
    mlds_stmt, mlds_stmt, elim_info, elim_info).
% :- mode add_unchain_stack_to_stmt(in(hoist), in, out, in, out) is det.
:- mode add_unchain_stack_to_stmt(in(chain), in, out, in, out) is det.

add_unchain_stack_to_stmt(Action, Stmt0, Stmt, !Info) :-
    (
        Stmt0 = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts0, Context),
        add_unchain_stack_to_stmts(Action, SubStmts0, SubStmts, !Info),
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval, SubStmt0, Context),
        add_unchain_stack_to_stmt(Action, SubStmt0, SubStmt, !Info),
        Stmt = ml_stmt_while(Kind, Rval, SubStmt, Context)
    ;
        Stmt0 = ml_stmt_if_then_else(Cond, Then0, MaybeElse0, Context),
        add_unchain_stack_to_stmt(Action, Then0, Then, !Info),
        add_unchain_stack_to_maybe_statement(Action, MaybeElse0, MaybeElse,
            !Info),
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse, Context)
    ;
        Stmt0 = ml_stmt_switch(Type, Val, Range, Cases0, Default0, Context),
        add_unchain_stack_to_cases(Action, Cases0, Cases, !Info),
        add_unchain_stack_to_default(Action, Default0, Default, !Info),
        Stmt = ml_stmt_switch(Type, Val, Range, Cases, Default, Context)
    ;
        Stmt0 = ml_stmt_call(_Sig, _Func, _Args, RetLvals, CallKind, Context),
        add_unchain_stack_to_call(Stmt0, RetLvals, CallKind, Context,
            Stmt, !Info)
    ;
        Stmt0 = ml_stmt_return(_Rvals, Context),
        Stmt = prepend_unchain_frame(Stmt0, Context, !.Info)
    ;
        Stmt0 = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt0, Context),
        add_unchain_stack_to_stmt(Action, BodyStmt0, BodyStmt, !Info),
        add_unchain_stack_to_stmt(Action, HandlerStmt0, HandlerStmt, !Info),
        Stmt = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, Context)
    ;
        ( Stmt0 = ml_stmt_label(_Label, _Context)
        ; Stmt0 = ml_stmt_goto(_Target, _Context)
        ; Stmt0 = ml_stmt_computed_goto(_Rval, _Labels, _Context)
        ; Stmt0 = ml_stmt_do_commit(_Ref, _Context)
        ; Stmt0 = ml_stmt_atomic(_AtomicStmt0, _Context)
        ),
        Stmt = Stmt0
    ).

:- pred add_unchain_stack_to_call(mlds_stmt::in, list(mlds_lval)::in,
    ml_call_kind::in, prog_context::in, mlds_stmt::out,
    elim_info::in, elim_info::out) is det.

add_unchain_stack_to_call(Stmt0, RetLvals, CallKind, Context, Stmt, !Info) :-
    (
        CallKind = no_return_call,
        % For no-return calls, we just unchain the stack
        % frame before the call.
        Stmt = prepend_unchain_frame(Stmt0, Context, !.Info)
    ;
        CallKind = tail_call,
        % For tail calls, we unchain the stack frame before the call,
        % and then we insert a return statement after the call.
        % The return statement is needed ensure that the code doesn't
        % fall through (past the tail call) and then try to unchain
        % the already-unchained stack frame.
        UnchainFrame = ml_gen_unchain_frame(Context, !.Info),
        RetRvals = list.map(func(Rval) = ml_lval(Rval), RetLvals),
        RetStmt = ml_stmt_return(RetRvals, Context),
        Stmt = ml_stmt_block([], [], [UnchainFrame, Stmt0, RetStmt], Context)
    ;
        CallKind = ordinary_call,
        Stmt = Stmt0
    ).

:- pred add_unchain_stack_to_cases(action,
    list(mlds_switch_case), list(mlds_switch_case), elim_info, elim_info).
% :- mode add_unchain_stack_to_cases(in(hoist), in, out, in, out) is det.
:- mode add_unchain_stack_to_cases(in(chain), in, out, in, out) is det.

add_unchain_stack_to_cases(_, [], [], !Info).
add_unchain_stack_to_cases(Action, [Case0 | Cases0], [Case | Cases], !Info) :-
    add_unchain_stack_to_case(Action, Case0, Case, !Info),
    add_unchain_stack_to_cases(Action, Cases0, Cases, !Info).

:- pred add_unchain_stack_to_case(action,
    mlds_switch_case, mlds_switch_case, elim_info, elim_info).
% :- mode add_unchain_stack_to_case(in(hoist), in, out, in, out) is det.
:- mode add_unchain_stack_to_case(in(chain), in, out, in, out) is det.

add_unchain_stack_to_case(Action, Case0, Case, !Info) :-
    Case0 = mlds_switch_case(FirstCond0, LaterConds0, Stmt0),
    fixup_case_cond(Action, !.Info, FirstCond0, FirstCond),
    fixup_case_conds(Action, !.Info, LaterConds0, LaterConds),
    add_unchain_stack_to_stmt(Action, Stmt0, Stmt, !Info),
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- pred add_unchain_stack_to_default(action,
    mlds_switch_default, mlds_switch_default, elim_info, elim_info).
% :- mode add_unchain_stack_to_default(in(hoist), in, out, in, out) is det.
:- mode add_unchain_stack_to_default(in(chain), in, out, in, out) is det.

add_unchain_stack_to_default(Action, Default0, Default, !Info) :-
    (
        Default0 = default_is_unreachable,
        Default = default_is_unreachable
    ;
        Default0 = default_do_nothing,
        Default = default_do_nothing
    ;
        Default0 = default_case(Stmt0),
        add_unchain_stack_to_stmt(Action, Stmt0, Stmt, !Info),
        Default = default_case(Stmt)
    ).

:- func prepend_unchain_frame(mlds_stmt, prog_context, elim_info) = mlds_stmt.

prepend_unchain_frame(Stmt0, Context, ElimInfo) = Stmt :-
    UnchainFrame = ml_gen_unchain_frame(Context, ElimInfo),
    Stmt = ml_stmt_block([], [], [UnchainFrame, Stmt0], Context).

:- func ml_gen_unchain_frame(prog_context, elim_info) = mlds_stmt.

ml_gen_unchain_frame(Context, ElimInfo) = UnchainFrame :-
    EnvPtrTypeName = elim_info_get_env_ptr_type_name(ElimInfo),

    % Generate code to remove this frame from the stack chain:
    %
    %   stack_chain = stack_chain->prev;
    %
    % Actually, it is not quite as simple as that. The global `stack_chain'
    % has type `void *', rather than `MR_StackChain *', and the MLDS has
    % no way of representing the `struct MR_StackChain' type (which we would
    % need to cast it to) or of accessing an unqualified field name
    % like `prev' (rather than `modulename__prev').
    %
    % So we do this in a slightly lower-level fashion, using a field offset
    % rather than a field name:
    %
    %   stack_chain = MR_hl_field(stack_chain, 0);

    StackChain = ml_stack_chain_var,
    Tag = yes(0),
    PrevFieldId = ml_field_offset(ml_const(mlconst_int(0))),
    PrevFieldType = mlds_generic_type,
    PrevFieldRval = ml_lval(ml_field(Tag, ml_lval(StackChain), PrevFieldId,
        PrevFieldType, EnvPtrTypeName)),
    Assignment = assign(StackChain, PrevFieldRval),
    UnchainFrame = ml_stmt_atomic(Assignment, Context).

    % Generate a local variable declaration to hold the saved stack chain
    % pointer:
    %
    %   void *saved_stack_chain;
    %
:- func gen_saved_stack_chain_var(int, prog_context) = mlds_local_var_defn.

gen_saved_stack_chain_var(Id, Context) = Defn :-
    Name = lvn_comp_var(lvnc_saved_stack_chain(Id)),
    Type = ml_stack_chain_type,
    Initializer = no_initializer,
    % The saved stack chain never needs to be traced by the GC,
    % since it will always point to the stack, not into the heap.
    GCStmt = gc_no_stmt,
    Defn = mlds_local_var_defn(Name, Context, Type, Initializer, GCStmt).

    % Generate two statements, one to save the stack chain pointer:
    %
    %   saved_stack_chain = stack_chain;
    %
    % and one to restore it:
    %
    %   stack_chain = saved_stack_chain;
    %
:- pred gen_save_and_restore_of_stack_chain_var(int::in, prog_context::in,
    mlds_stmt::out, mlds_stmt::out) is det.

gen_save_and_restore_of_stack_chain_var(Id, Context, SaveStmt, RestoreStmt) :-
    SavedStackChain = ml_local_var(lvn_comp_var(lvnc_saved_stack_chain(Id)),
        ml_stack_chain_type),
    SaveAssignment = assign(SavedStackChain, ml_lval(ml_stack_chain_var)),
    SaveStmt = ml_stmt_atomic(SaveAssignment, Context),
    RestoreAssignment = assign(ml_stack_chain_var, ml_lval(SavedStackChain)),
    RestoreStmt = ml_stmt_atomic(RestoreAssignment, Context).

%---------------------------------------------------------------------------%

%
% The elim_info type holds information that we use or accumulate
% as we traverse through the function body.
%

    % The lists of local variables for each of the containing functions,
    % innermost first.
:- type outervars == list(list(mlds_local_var_defn)).

:- type elim_info
    --->    elim_info(
                % The lists of local variables for each of the containing
                % functions, innermost first.
                % XXX this is not used.
                % It would be needed if we want to handle arbitrary nesting.
                % Currently we assume that any variables can safely be hoisted
                % to the outermost function, so this field is not needed.
                % outer_vars                    :: outervars,

                % The nested function definitions that we must hoist out.
                ei_nested_funcs                 :: cord(mlds_function_defn),

                % The local variables that we must put in the
                % environment structure.
                ei_local_vars                   :: cord(mlds_local_var_defn),

                % The "type" of the introduced environment struct.
                ei_env_type_name                :: mlds_class_id,

                % Type of the introduced environment struct pointer.
                % This might not just be just a pointer to the env_type_name
                % (in the IL backend we don't necessarily use a pointer).
                ei_env_ptr_type_name            :: mlds_type,

                % A counter used to number the local variables
                % used to save the stack chain
                ei_saved_stack_chain_counter    :: counter,

                ei_target_lang                  :: mlds_target_lang
            ).

:- func elim_info_init(mlds_class_id, mlds_type,
    mlds_target_lang) = elim_info.

elim_info_init(EnvClassId, EnvPtrTypeName, Target) =
    elim_info(cord.init, cord.init, EnvClassId, EnvPtrTypeName,
        counter.init(0), Target).

:- func elim_info_get_local_vars(elim_info) = cord(mlds_local_var_defn).
:- func elim_info_get_env_type_name(elim_info) = mlds_class_id.
:- func elim_info_get_env_ptr_type_name(elim_info) = mlds_type.
:- func elim_info_get_target_lang(elim_info) = mlds_target_lang.

:- pred elim_info_set_local_vars(cord(mlds_local_var_defn)::in,
    elim_info::in, elim_info::out) is det.

elim_info_get_local_vars(ElimInfo) = X :-
    X = ElimInfo ^ ei_local_vars.
elim_info_get_env_type_name(ElimInfo) = X :-
    X = ElimInfo ^ ei_env_type_name.
elim_info_get_env_ptr_type_name(ElimInfo) = X :-
    X = ElimInfo ^ ei_env_ptr_type_name.
elim_info_get_target_lang(ElimInfo) = X :-
    X = ElimInfo ^ ei_target_lang.

elim_info_set_local_vars(X, !ElimInfo) :-
    !ElimInfo ^ ei_local_vars := X.

:- pred elim_info_add_nested_func(mlds_function_defn::in,
    elim_info::in, elim_info::out) is det.

elim_info_add_nested_func(NestedFunc, !ElimInfo) :-
    NestedFuncs0 = !.ElimInfo ^ ei_nested_funcs,
    NestedFuncs = cord.snoc(NestedFuncs0, NestedFunc),
    !ElimInfo ^ ei_nested_funcs := NestedFuncs.

:- pred elim_info_add_local_var(mlds_local_var_defn::in,
    elim_info::in, elim_info::out) is det.

elim_info_add_local_var(LocalVar, !ElimInfo) :-
    LocalVars0 = !.ElimInfo ^ ei_local_vars,
    LocalVars = cord.snoc(LocalVars0, LocalVar),
    !ElimInfo ^ ei_local_vars := LocalVars.

:- pred elim_info_allocate_saved_stack_chain_id(int::out,
    elim_info::in, elim_info::out) is det.

elim_info_allocate_saved_stack_chain_id(Id, !ElimInfo) :-
    Counter0 = !.ElimInfo ^ ei_saved_stack_chain_counter,
    counter.allocate(Id, Counter0, Counter),
    !ElimInfo ^ ei_saved_stack_chain_counter := Counter.

:- pred elim_info_finish(elim_info::in,
    list(mlds_function_defn)::out, list(mlds_local_var_defn)::out) is det.

elim_info_finish(ElimInfo, NestedFuncs, LocalVars) :-
    NestedFuncs = cord.to_list(ElimInfo ^ ei_nested_funcs),
    LocalVars = cord.to_list(ElimInfo ^ ei_local_vars).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_elim_nested.
%---------------------------------------------------------------------------%
