%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pickle.m
% Main authors: petdr, wangp.
%
% This file contains routines to serialise arbitrary data structures into some
% unspecified binary format which can be restored quickly.
%
% We don't preserve sharing in the pickled data structure.  This would be
% possible but would introduce slowdowns in both the pickling and unpickling
% processes.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module libs.pickle.
:- interface.

:- import_module io.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%

    % A type which holds custom pickling predicates.
    %
:- type picklers.

:- type pickler_pred == pred(picklers, univ, io, io).
:- inst pickler_pred == (pred(in, in, di, uo) is det).

    % Initialize the custom pickling predicates.
    %
:- func init_picklers = picklers.

    % For the type described by the type_ctor_desc, add the supplied custom
    % pickle predicate to the set of registered pickle predicates.
    %
:- pred register_pickler(type_ctor_desc::in, pickler_pred::in(pickler_pred),
    picklers::in, picklers::out) is det.

    % Serialise an arbitrary data structure into the current binary output
    % stream, using the picklers given to override the default pickling method.
    %
    % Existential, foreign and higher-order types are not supported
    % generically.  Register custom handlers to handle those types.
    %
:- pred pickle(picklers::in, T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % A type which holds the custom unpickling predicates.
    %
:- type unpicklers.

:- type unpickle_handle.

:- type unpickle_state.

:- type unpickler_pred == pred(unpicklers, unpickle_handle, type_desc, univ,
                            unpickle_state, unpickle_state).
:- inst unpickler_pred == (pred(in, in, in, out, di, uo) is det).

    % Initialize the custom unpickling predicates.
    %
:- func init_unpicklers = unpicklers.

    % For the type described by the type_ctor_desc, add the supplied custom
    % unpickle predicate to the set of registered unpickle predicates.
    %
:- pred register_unpickler(type_ctor_desc::in,
    unpickler_pred::in(unpickler_pred), unpicklers::in, unpicklers::out)
    is det.

    % Get a pickled type back out from the file, using the unpicklers to
    % override the default unpickling method.
    %
:- pred unpickle_from_file(unpicklers::in, string::in, io.res(T)::out,
    io::di, io::uo) is det.

    % Unpickle a single value.
    %
:- pred unpickle(unpicklers::in, unpickle_handle::in, T::out,
    unpickle_state::di, unpickle_state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module bool.
:- import_module char.
:- import_module construct.
:- import_module deconstruct.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type picklers
    --->    picklers(
                map(type_ctor_desc, pickler_pred)
            ).

:- type unpicklers
    --->    unpicklers(
                map(type_ctor_desc, unpickler_pred)
            ).

:- type unpickle_handle == bitmap.
:- type unpickle_state  == int.         % offset into bitmap

:- type get_byte_out_of_range
    --->    get_byte_out_of_range(string).

%-----------------------------------------------------------------------------%
%
% Pickling
%

init_picklers = picklers(map.init).

register_pickler(TypeCtorDesc, Pickle, Pickles0, Pickles) :-
    Pickles0 = picklers(Map0),
    map.det_insert(TypeCtorDesc, Pickle, Map0, Map),
    Pickles = picklers(Map).

pickle(Pickles, T, !IO) :-
    ( if
        dynamic_cast(T, String)
    then
        pickle_string(String, !IO)
    else if
        dynamic_cast(T, Int)
    then
        pickle_int32(Int, !IO)
    else if
        dynamic_cast(T, Float)
    then
        pickle_float(Float, !IO)
    else if
        dynamic_cast(T, Char)
    then
        pickle_char(Char, !IO)
    else if
        TypeDesc = type_of(T),
        TypeCtorDesc = type_ctor(TypeDesc),
        user_defined_pickler(Pickles, TypeCtorDesc, Pickle)
    then
        Pickle(Pickles, univ(T), !IO)
    else
        deconstruct.functor(T, do_not_allow, Functor, Arity),
        pickle_string(Functor, !IO),
        pickle_int32(Arity, !IO),
        pickle_args(Pickles, 0, Arity, T, !IO)
    ).

:- pred pickle_args(picklers::in, int::in, int::in, T::in, io::di, io::uo)
    is det.

pickle_args(Pickles, N, Arity, T, !IO) :-
    ( if N = Arity then
        true
    else
        ( if deconstruct.arg(T, do_not_allow, N, Arg) then
            pickle(Pickles, Arg, !IO),
            pickle_args(Pickles, N + 1, Arity, T, !IO)
        else
            unexpected($pred, "unable to deconstruct arg")
        )
    ).

:- pred user_defined_pickler(picklers::in, type_ctor_desc::in,
    pickler_pred::out(pickler_pred)) is semidet.

user_defined_pickler(picklers(Pickles), TypeCtorDesc, Pickle) :-
    map.search(Pickles, TypeCtorDesc, Pickle0),
    pickler_inst_cast(Pickle0, Pickle).

:- pred pickler_inst_cast(pickler_pred::in, pickler_pred::out(pickler_pred))
    is det.

:- pragma foreign_proc("C",
    pickler_inst_cast(A::in, B::out(pickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

:- pragma foreign_proc("Java",
    pickler_inst_cast(A::in, B::out(pickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

:- pragma foreign_proc("Erlang",
    pickler_inst_cast(A::in, B::out(pickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A
").

:- pragma foreign_proc("C#",
    pickler_inst_cast(A::in, B::out(pickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

%-----------------------------------------------------------------------------%
%
% Unpickling
%

init_unpicklers = unpicklers(map.init).

register_unpickler(TypeCtorDesc, Unpickle, Unpicklers0, Unpicklers) :-
    Unpicklers0 = unpicklers(Map0),
    map.det_insert(TypeCtorDesc, Unpickle, Map0, Map),
    Unpicklers = unpicklers(Map).

unpickle_from_file(Unpicklers, FileName, Result, !IO) :-
    io.see_binary(FileName, SeeResult, !IO),
    (
        SeeResult = ok,
        % Perform unpickling from an intermediate memory buffer, as it seems to
        % be faster.
        io.read_binary_file_as_bitmap(ReadResult, !IO),
        io.seen_binary(!IO),
        (
            ReadResult = ok(Bitmap),
            promise_equivalent_solutions [TryResult] (
                try((pred(T0::out) is det :-
                    unpickle(Unpicklers, Bitmap, T0, 0, _State)
                ), TryResult)
            ),
            (
                TryResult = succeeded(T),
                Result = ok(T)
            ;
                TryResult = exception(Excp),
                ( if univ_to_type(Excp, get_byte_out_of_range(Msg)) then
                    Result = error(io.make_io_error(Msg))
                else
                    rethrow(TryResult)
                )
            )
        ;
            ReadResult = error(Error),
            Result = error(Error)
        )
    ;
        SeeResult = error(Error),
        Result = error(Error)
    ).

unpickle(Unpicklers, Handle, T, !State) :-
    unpickle_2(Unpicklers, Handle, type_of(T), Univ, !State),
    det_univ_to_type(Univ, T).

:- pred unpickle_2(unpicklers::in, unpickle_handle::in,
    type_desc::in, univ::out, unpickle_state::di, unpickle_state::uo)
    is det.

unpickle_2(Unpicklers, Handle, TypeDesc, Univ, !State) :-
    ( if
        TypeDesc = type_of(_ : string)
    then
        unpickle_string(Handle, String, !State),
        Univ = univ(String)
    else if
        TypeDesc = type_of(_ : int)
    then
        unpickle_int32(Handle, Int, !State),
        Univ = univ(Int)
    else if
        TypeDesc = type_of(_ : float)
    then
        unpickle_float(Handle, Float, !State),
        Univ = univ(Float)
    else if
        TypeDesc = type_of(_ : character)
    then
        unpickle_char(Handle, Char, !State),
        Univ = univ(Char)
    else if
        user_defined_unpickler(Unpicklers, type_ctor(TypeDesc), Unpickle)
    then
        Unpickle(Unpicklers, Handle, TypeDesc, Univ, !State)
    else
        unpickle_string(Handle, Functor, !State),
        unpickle_int32(Handle, Arity, !State),
        ( if
            ( if Functor = "{}" then
                IsTuple = yes,
                type_ctor_and_args(TypeDesc, _, ArgTypes),
                N = 0
            else
                IsTuple = no,
                % XXX consider tabling this call
                find_functor(TypeDesc, Functor, Arity, N, ArgTypes)
            )
        then
            list.map_foldl(unpickle_2(Unpicklers, Handle), ArgTypes, ArgUnivs,
                !State),
            (
                IsTuple = yes,
                Univ = construct_tuple(ArgUnivs)
            ;
                IsTuple = no,
                ( if Univ0 = construct(TypeDesc, N, ArgUnivs) then
                    Univ = Univ0
                else
                    unexpected($pred, "unable to construct")
                )
            )
        else
            unexpected($pred, "unable to unpickle")
        )
    ).

:- pred user_defined_unpickler(unpicklers::in, type_ctor_desc::in,
    unpickler_pred::out(unpickler_pred)) is semidet.

user_defined_unpickler(unpicklers(Unpicklers), TypeCtorDesc, Unpickle) :-
    map.search(Unpicklers, TypeCtorDesc, Unpickle0),
    unpickler_inst_cast(Unpickle0, Unpickle).

:- pred unpickler_inst_cast(unpickler_pred::in,
    unpickler_pred::out(unpickler_pred)) is det.

:- pragma foreign_proc("C",
    unpickler_inst_cast(A::in, B::out(unpickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

:- pragma foreign_proc("Java",
    unpickler_inst_cast(A::in, B::out(unpickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

:- pragma foreign_proc("Erlang",
    unpickler_inst_cast(A::in, B::out(unpickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A
").

:- pragma foreign_proc("C#",
    unpickler_inst_cast(A::in, B::out(unpickler_pred)),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

%-----------------------------------------------------------------------------%
%
% Basic types picklers/unpicklers
%

:- pred pickle_int32(int::in, io::di, io::uo) is det.

pickle_int32(Int, !IO) :-
    A = (Int >> 24) /\ 0xff,
    B = (Int >> 16) /\ 0xff,
    C = (Int >>  8) /\ 0xff,
    D = (Int >>  0) /\ 0xff,
    io.write_byte(A, !IO),
    io.write_byte(B, !IO),
    io.write_byte(C, !IO),
    io.write_byte(D, !IO).

:- pred unpickle_int32(unpickle_handle::in, int::out,
    unpickle_state::di, unpickle_state::uo) is det.

unpickle_int32(Handle, Int, !State) :-
    get_byte(Handle, A, !State),
    get_byte(Handle, B, !State),
    get_byte(Handle, C, !State),
    get_byte(Handle, D, !State),
    Int0 = (A `unchecked_left_shift` 24)
        \/ (B `unchecked_left_shift` 16)
        \/ (C `unchecked_left_shift`  8)
        \/ (D `unchecked_left_shift`  0),
    Int = sign_extend_32(Int0).

:- func sign_extend_32(int) = int.

sign_extend_32(X) = R :-
    % http://graphics.stanford.edu/~seander/bithacks.html#FixedSignExtend
    Mask = 1 `unchecked_left_shift` 31,
    R = (X `xor` Mask) - Mask.

:- pred pickle_char(char::in, io::di, io::uo) is det.

pickle_char(Char, !IO) :-
    % XXX handle non-ASCII characters
    char.to_int(Char, Int),
    io.write_byte(Int, !IO).

:- pred unpickle_char(unpickle_handle::in, char::out,
    unpickle_state::di, unpickle_state::uo) is det.

unpickle_char(Handle, Char, !State) :-
    get_byte(Handle, Byte, !State),
    char.det_from_int(Byte, Char).

:- pred pickle_string(string::in, io::di, io::uo) is det.

pickle_string(String, !IO) :-
    Length = string.length(String),
    pickle_int32(Length, !IO),
    string.foldl(pickle_char, String, !IO).

:- pred unpickle_string(unpickle_handle::in, string::uo,
    unpickle_state::di, unpickle_state::uo) is det.

unpickle_string(Handle, String, !State) :-
    unpickle_int32(Handle, Length, !State),
    allocate_string(Length, String0),
    unpickle_string_2(Handle, 0, Length, String0, String, !State).

:- pred unpickle_string_2(unpickle_handle::in, int::in, int::in,
    string::di, string::uo, unpickle_state::di, unpickle_state::uo) is det.

unpickle_string_2(Handle, Index, Length, !String, !State) :-
    ( if Index = Length then
        true
    else
        unpickle_char(Handle, Char, !State),
        local_unsafe_set_char(Char, Index, !String),
        unpickle_string_2(Handle, Index + 1, Length, !String, !State)
    ).

:- pred allocate_string(int::in, string::uo) is det.

:- pragma foreign_proc("C",
    allocate_string(Length::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_allocate_aligned_string_msg(Str, Length, MR_ALLOC_ID);
    Str[Length] = '\\0';
").

:- pragma no_determinism_warning(allocate_string/2).
allocate_string(_, _) :-
    sorry($file, $pred).

    % string.unsafe_set_char is disabled in the standard library so we need our
    % own copy.
:- pred local_unsafe_set_char(char::in, int::in, string::di, string::uo)
    is det.

:- pragma foreign_proc("C",
    local_unsafe_set_char(Chr::in, Index::in, Str0::di, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str0[Index] = Chr;
    Str = Str0;
").

:- pragma no_determinism_warning(local_unsafe_set_char/4).
local_unsafe_set_char(_, _, _, _) :-
    sorry($file, $pred).

:- pred pickle_float(float::in, io::di, io::uo) is det.

pickle_float(Float, !IO) :-
    reinterpret_float_as_ints(Float, A, B),
    % We always write floats using 64 bits.  Single precision floats are not
    % the default and the compiler hardly uses floats anyhow.
    pickle_int32(A, !IO),
    pickle_int32(B, !IO).

:- pred reinterpret_float_as_ints(float::in, int::out, int::out)
    is det.

:- pragma foreign_proc("C",
    reinterpret_float_as_ints(Flt::in, A::out, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (sizeof(MR_Float) == sizeof(float)) {
        MR_uint_least32_t   *p;

        p = (MR_uint_least32_t *) &Flt;
        A = *p;
        B = 0;
    } else {
        MR_uint_least64_t   *p;

        p = (MR_uint_least64_t *) &Flt;
        A = (*p >> 32) & 0xffffffff;
        B = (*p >>  0) & 0xffffffff;
    }
").

:- pragma no_determinism_warning(reinterpret_float_as_ints/3).
reinterpret_float_as_ints(_, _, _) :-
    sorry($file, $pred).

:- pred unpickle_float(unpickle_handle::in, float::out,
    unpickle_state::di, unpickle_state::uo) is det.

unpickle_float(Handle, Float, !State) :-
    unpickle_int32(Handle, A, !State),
    unpickle_int32(Handle, B, !State),
    reinterpret_ints_as_float(A, B, Float).

:- pred reinterpret_ints_as_float(int::in, int::in, float::out) is det.

:- pragma foreign_proc("C",
    reinterpret_ints_as_float(A::in, B::in, Flt::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (sizeof(MR_Float) == sizeof(float)) {
        MR_Float    *p;

        p = (MR_Float *) &A;
        Flt = *p;
        (void) B;
    } else {
        MR_uint_least64_t   tmp;
        MR_Float            *p;

        tmp = (((MR_uint_least64_t) A) << 32) |
            (((MR_uint_least64_t) B) & 0xffffffff);
        p = (MR_Float *) &tmp;
        Flt = *p;
    }
").

:- pragma no_determinism_warning(reinterpret_ints_as_float/3).
reinterpret_ints_as_float(_, _, _) :-
    sorry($file, $pred).

:- pred get_byte(unpickle_handle::in, int::out,
    unpickle_state::di, unpickle_state::uo) is det.

get_byte(Bitmap, Byte, Index, Index + 1) :-
    ( if bitmap.byte_in_range(Bitmap, Index) then
        Byte = Bitmap ^ unsafe_byte(Index)
    else
        Msg = "byte " ++ string.from_int(Index) ++ " is out of range",
        throw(get_byte_out_of_range(Msg))
    ).

%-----------------------------------------------------------------------------%
:- end_module libs.pickle.
%-----------------------------------------------------------------------------%
