%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2000,2002-2003, 2005-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: tr_store.m. 
% Main author: fjh.
% Stability: medium.
%
% This file provides facilities for manipulating mutable backtrackable stores.
% This is a backtrackable version of the standard library module `store.m';
% the interface and implementation are almost identical to store.m,
% the only difference is that destructive updates are recorded on a trail
% so that updates can be undone on backtracking.
%
% See store.m for documentation, and for the definition of the types
% `store', `generic_mutvar', and `generic_ref'.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module tr_store.
:- interface.
:- import_module store.

%---------------------------------------------------------------------------%
%
% mutvars
%

    % create a new mutable variable,
    % initialized with the specified value
    %
:- pred new_mutvar(T::in, generic_mutvar(T, S)::out,
    store(S)::mdi, store(S)::muo) is det.

    % copy_mutvar(Mutvar, Copy, S0, S)
    % is equivalent to
    %   get_mutvar(Mutvar, Value, S0, S1),
    %   new_mutvar(Value, Copy,   S1, S )
    %
:- pred copy_mutvar(generic_mutvar(T, S)::in,
    generic_mutvar(T, S)::out, store(S)::mdi, store(S)::muo) is det.

    % lookup the value stored in a given mutable variable
    %
:- pred get_mutvar(generic_mutvar(T, S)::in, T::out,
    store(S)::mdi, store(S)::muo) is det.

    % replace the value stored in a given mutable variable
    %
:- pred set_mutvar(generic_mutvar(T, S)::in, T::in,
    store(S)::mdi, store(S)::muo) is det.

%---------------------------------------------------------------------------%
%
% references
%

    % new_ref(Val, Ref):    
    %   /* In C: Ref = malloc(...); *Ref = Val; */
    % Given a value of any type `T', insert a copy of the term
    % into the store and return a new reference to that term.
    % (This does not actually perform a copy, it just returns a view
    % of the representation of that value.
    % It does however allocate one cell to hold the reference;
    % you can use new_arg_ref to avoid that.)
    %
:- pred new_ref(T::mdi, generic_ref(T, S)::out,
    store(S)::mdi, store(S)::muo) is det.

    % ref_functor(Ref, Functor, Arity):
    % Given a reference to a term, return the functor and arity
    % of that term.
    %
:- pred ref_functor(generic_ref(T, S)::in, string::out, int::out,
    store(S)::mdi, store(S)::muo) is det.

    % arg_ref(Ref, ArgNum, ArgRef):      
    %   /* Psuedo-C code: ArgRef = &Ref[ArgNum]; */
    % Given a reference to a term, return a reference to
    % the specified argument (field) of that term
    % (argument numbers start from zero).
    % It is an error if the argument number is out of range,
    % or if the argument reference has the wrong type.
    %
:- pred arg_ref(generic_ref(T, S)::in, int::in,
    generic_ref(ArgT, S)::out, store(S)::mdi, store(S)::muo) is det.

    % new_arg_ref(Val, ArgNum, ArgRef):
    %   /* Psuedo-C code: ArgRef = &Val[ArgNum]; */
    % Equivalent to `new_ref(Val, Ref), arg_ref(Ref, ArgNum, ArgRef)',
    % except that it is more efficient.
    % It is an error if the argument number is out of range,
    % or if the argument reference has the wrong type.
    %
:- pred new_arg_ref(T::mdi, int::in, generic_ref(ArgT, S)::out,
    store(S)::mdi, store(S)::muo) is det.

    % set_ref(Ref, ValueRef):
    %   /* Pseudo-C code: *Ref = *ValueRef; */
    % Given a reference to a term (Ref), 
    % a reference to another term (ValueRef),
    % update the store so that the term referred to by Ref
    % is replaced with the term referenced by ValueRef.
    %
:- pred set_ref(generic_ref(T, S)::in, generic_ref(T, S)::in,
    store(S)::mdi, store(S)::muo) is det.

    % set_ref_value(Ref, Value):
    %   /* Pseudo-C code: *Ref = Value; */
    % Given a reference to a term (Ref), and a value (Value),
    % update the store so that the term referred to by Ref
    % is replaced with Value.
    %
:- pred set_ref_value(generic_ref(T, S)::in, T::mdi,
    store(S)::mdi, store(S)::muo) is det.

    % Given a reference to a term, return that term.
    % NOTE: this requires making a copy, so this pred may
    % be inefficient if used to return large terms; it
    % is most efficient with atomic terms.
    %
:- pred copy_ref_value(generic_ref(T, S)::in, T::uo,
    store(S)::mdi, store(S)::muo) is det.

    % Same as above, but without making a copy.
    % Destroys the store.
    %
:- pred extract_ref_value(store(S)::mdi, generic_ref(T, S)::in, T::out) is det.

%---------------------------------------------------------------------------%
%
% Nasty performance hacks
%
% WARNING: use of these procedures is dangerous!
% Use them only only as a last resort, only if performance
% is critical, and only if profiling shows that using the
% safe versions is a bottleneck.
%
% These procedures may vanish in some future version of Mercury.

    % `unsafe_arg_ref' is the same as `arg_ref',
    % and `unsafe_new_arg_ref' is the same as `new_arg_ref'
    % except that they doesn't check for errors,
    % and they don't work for `no_tag' types (types with
    % exactly one functor which has exactly one argument),
    % and they don't work for types with >4 functors.
    % If the argument number is out of range,
    % or if the argument reference has the wrong type,
    % or if the argument is a `no_tag' type,
    % then the behaviour is undefined, and probably harmful.
    %
:- pred unsafe_arg_ref(generic_ref(T, S)::in, int::in,
    generic_ref(ArgT, S)::out, store(S)::mdi, store(S)::muo) is det.

:- pred unsafe_new_arg_ref(T::mdi, int::in,
    generic_ref(ArgT, S)::out, store(S)::mdi, store(S)::muo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.

:- pragma require_feature_set([trailing]).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_mutvar(Val::in, Mutvar::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    MR_incr_hp(Mutvar, 1);
    * (MR_Word *) Mutvar = Val;
    S = S0;
").

copy_mutvar(Mutvar, Copy, !S) :-
    tr_store.get_mutvar(Mutvar, Val, !S),
    tr_store.new_mutvar(Val, Copy, !S).

:- pragma foreign_proc("C",
    get_mutvar(Mutvar::in, Val::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    Val = * (MR_Word *) Mutvar;
    S = S0;
").

:- pragma foreign_proc("C",
    set_mutvar(Mutvar::in, Val::in, S0::mdi, S::muo),
    [promise_pure,  will_not_call_mercury],
"
    MR_trail_current_value((MR_Word *) Mutvar);
    * (MR_Word *) Mutvar = Val;
    S = S0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_ref(Val::mdi, Ref::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    MR_incr_hp(Ref, 1);
    * (MR_Word *) Ref = Val;
    S = S0;
").

copy_ref_value(Ref, Val) -->
    /* XXX need to deep-copy non-atomic types */
    unsafe_ref_value(Ref, Val).

    % unsafe_ref_value extracts the value that a reference refers
    % to, without making a copy; it is unsafe because the store
    % could later be modified, changing the returned value.
    %
:- pred unsafe_ref_value(generic_ref(T, S)::in, T::uo,
    store(S)::mdi, store(S)::muo) is det.
:- pragma foreign_proc("C",
    unsafe_ref_value(Ref::in, Val::uo, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    Val = * (MR_Word *) Ref;
    S = S0;
").

ref_functor(Ref, Functor, Arity, !S) :-
    unsafe_ref_value(Ref, Val, !S),
    functor(Val, canonicalize, Functor, Arity).

:- pragma foreign_decl("C", "#include ""mercury_deconstruct.h""").

:- pragma foreign_proc("C",
    arg_ref(Ref::in, ArgNum::in, ArgRef::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    MR_TypeInfo arg_type_info;
    MR_Word* arg_ref;
    const MR_DuArgLocn* arg_locn;

    MR_save_transient_registers();

    if (!MR_arg((MR_TypeInfo) TypeInfo_for_T, (MR_Word *) Ref, ArgNum,
        &arg_type_info, &arg_ref, &arg_locn, MR_NONCANON_ALLOW))
    {
        MR_fatal_error(
        ""tr_store.arg_ref: argument number out of range"");
    }

    if (MR_compare_type_info(arg_type_info,
        (MR_TypeInfo) TypeInfo_for_ArgT) != MR_COMPARE_EQUAL)
    {
        MR_fatal_error(""tr_store.arg_ref: argument has wrong type"");
    }

    if (arg_locn != NULL && arg_locn->MR_arg_bits != 0) {
        MR_fatal_error(
            ""tr_store.arg_ref: argument has packed representation"");
    }

    MR_restore_transient_registers();

    ArgRef = (MR_Word) arg_ref;
    S = S0;
").

:- pragma foreign_proc("C",
    new_arg_ref(Val::mdi, ArgNum::in, ArgRef::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    MR_TypeInfo arg_type_info;
    MR_Word* arg_ref;
    const MR_DuArgLocn* arg_locn;

    MR_save_transient_registers();

    if (!MR_arg((MR_TypeInfo) TypeInfo_for_T, (MR_Word *) &Val, ArgNum,
        &arg_type_info, &arg_ref, &arg_locn, MR_NONCANON_ALLOW))
    {
        MR_fatal_error(
            ""tr_store.new_arg_ref: argument number out of range"");
    }

    if (MR_compare_type_info(arg_type_info,
        (MR_TypeInfo) TypeInfo_for_ArgT) != MR_COMPARE_EQUAL)
    {
        MR_fatal_error(
            ""tr_store.new_arg_ref: argument has wrong type"");
    }

    MR_restore_transient_registers();

    if (arg_locn != NULL && arg_locn->MR_arg_bits != 0) {
        MR_incr_hp(ArgRef, 1);
        * (MR_Word *) ArgRef = MR_arg_value(arg_ref, arg_locn);
    } else if (arg_ref == &Val) {
        // For no_tag types, the argument may have the same address as the
        // term. Since the term (Val) is currently on the C stack, we can't
        // return a pointer to it; so if that is the case, then we need
        // to copy it to the heap before returning.
        MR_incr_hp(ArgRef, 1);
        * (MR_Word *) ArgRef = Val;
    } else {
        ArgRef = (MR_Word) arg_ref;
    }
    S = S0;
").

:- pragma foreign_proc("C",
    set_ref(Ref::in, ValRef::in, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    MR_trail_current_value((MR_Word *) Ref);
    * (MR_Word *) Ref = * (MR_Word *) ValRef;
    S = S0;
").

:- pragma foreign_proc("C",
    set_ref_value(Ref::in, Val::mdi, S0::mdi, S::muo),
    [promise_pure,  will_not_call_mercury],
"
    MR_trail_current_value((MR_Word *) Ref);
    * (MR_Word *) Ref = Val;
    S = S0;
").

:- pragma foreign_proc("C",
    extract_ref_value(_S::mdi, Ref::in, Val::out),
    [promise_pure, will_not_call_mercury],
"
    Val = * (MR_Word *) Ref;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_arg_ref(Ref::in, Arg::in, ArgRef::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    // unsafe - does not check type & arity, won't handle no_tag types.
    MR_Word *Ptr = (MR_Word *) MR_strip_tag(Ref);
    ArgRef = (MR_Word) &Ptr[Arg];
    S = S0;
").

:- pragma foreign_proc("C",
    unsafe_new_arg_ref(Val::mdi, Arg::in, ArgRef::out, S0::mdi, S::muo),
    [promise_pure, will_not_call_mercury],
"
    // unsafe - does not check type & arity, won't handle no_tag types.
    MR_Word *Ptr = (MR_Word *) MR_strip_tag(Val);
    ArgRef = (MR_Word) &Ptr[Arg];
    S = S0;
").

%---------------------------------------------------------------------------%
:- end_module tr_store.
%---------------------------------------------------------------------------%
