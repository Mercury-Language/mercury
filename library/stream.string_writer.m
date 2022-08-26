%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: stream.string_writer.m.
% Authors: trd, fjh, stayl
%
% Predicates to write to streams that accept strings.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module stream.string_writer.
:- interface.

:- import_module char.
:- import_module deconstruct.
:- import_module io.
:- import_module list.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

:- pred put_int(Stream::in, int::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_uint(Stream::in, uint::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_int8(Stream::in, int8::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_uint8(Stream::in, uint8::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_int16(Stream::in, int16::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_uint16(Stream::in, uint16::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_int32(Stream::in, int32::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_uint32(Stream::in, uint32::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_int64(Stream::in, int64::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_uint64(Stream::in, uint64::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_float(Stream::in, float::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_char(Stream::in, char::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % A version of io.format that works for arbitrary string writers.
    %
:- pred format(Stream::in, string::in, list(poly_type)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

:- pred nl(Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % print/4 writes its second argument to the string writer stream specified
    % in its first argument. In all cases, the argument to output can be of
    % any type. It is output in a format that is intended to be human readable.
    %
    % If the argument is just a single string or character, it will be printed
    % out exactly as is (unquoted). If the argument is of type integer (i.e.
    % an arbitrary precision integer), then its decimal representation will be
    % printed. If the argument is of type univ, then the value stored in the
    % the univ will be printed out, but not the type. If the argument is of
    % type date_time, it will be printed out in the same form as the string
    % returned by the function date_to_string/1. If the argument is of type
    % duration, it will be printed out in the same form as the string
    % returned by the function duration_to_string/1.
    %
    % print/5 is the same as print/4 except that it allows the caller to
    % specify how non-canonical types should be handled. print/4 implicitly
    % specifies `canonicalize' as the method for handling non-canonical types.
    % This means that for higher-order types, or types with user-defined
    % equality axioms, or types defined using the foreign language interface
    % (i.e. pragma foreign_type), the text output will only describe the type
    % that is being printed, not the value.
    %
    % print_cc/4 is the same as print/4 except that it specifies
    % `include_details_cc' rather than `canonicalize'. This means that it will
    % print the details of non-canonical types. However, it has determinism
    % `cc_multi'.
    %
    % Note that even if `include_details_cc' is specified, some implementations
    % may not be able to print all the details for higher-order types or types
    % defined using the foreign language interface.
    %
:- pred print(Stream::in, T::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pred print_cc(Stream::in, T::in, State::di, State::uo) is cc_multi
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pred print(Stream, deconstruct.noncanon_handling, T, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode print(in, in(do_not_allow), in, di, uo) is det.
:- mode print(in, in(canonicalize), in, di, uo) is det.
:- mode print(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode print(in, in, in, di, uo) is cc_multi.

    % write/4 writes its second argument to the string writer stream specified
    % in its first argument. In all cases, the argument to output may be of
    % any type. The argument is written in a format that is intended to be
    % valid Mercury syntax whenever possible.
    %
    % Strings and characters are always printed out in quotes, using backslash
    % escapes if necessary and backslash or octal escapes for all characters
    % for which char.is_control/1 is true. For higher-order types, or for types
    % defined using the foreign language interface (pragma foreign_type), the
    % text output will only describe the type that is being printed, not the
    % value, and the result may not be parsable by `read'. For the types
    % containing existential quantifiers, the type `type_desc' and closure
    % types, the result may not be parsable by `read', either. But in all
    % other cases the format used is standard Mercury syntax, and if you append
    % a period and newline (".\n"), then the results can be read in again using
    % `read'.
    %
    % write/5 is the same as write/4 except that it allows the caller to
    % specify how non-canonical types should be handled. write_cc/4 is the
    % same as write/4 except that it specifies `include_details_cc' rather than
    % `canonicalize'.
    %
:- pred write(Stream::in, T::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pred write_cc(Stream::in, T::in, State::di, State::uo) is cc_multi
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pred write(Stream, deconstruct.noncanon_handling, T, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write(in, in(do_not_allow), in, di, uo) is det.
:- mode write(in, in(canonicalize), in, di, uo) is det.
:- mode write(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write(in, in, in, di, uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- import_module ops.

%
% For use by browser/browse.m.
%

% Predicates for writing out univs.

:- pred write_univ(Stream::in, univ::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pred write_univ(Stream, deconstruct.noncanon_handling,
    univ, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_univ(in, in(do_not_allow), in, di, uo) is det.
:- mode write_univ(in, in(canonicalize), in, di, uo) is det.
:- mode write_univ(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_univ(in, in, in, di, uo) is cc_multi.

%
% For use by term_io.m.
%

:- pred maybe_write_paren(Stream::in, char::in, ops.priority::in,
    ops.priority::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- pragma type_spec(pred(maybe_write_paren/6),
    (Stream = io.output_stream, State = io.state)).

:- pragma type_spec(pred(write/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(write/5),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(write_univ/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(write_univ/5),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_int/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_uint/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_int8/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_uint8/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_int16/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_uint16/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_int32/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_uint32/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_int64/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_uint64/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_float/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(put_char/4),
    (Stream = io.output_stream, State = io.state)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module calendar.
:- import_module int.
:- import_module integer.
:- import_module io.stream_db.
:- import_module require.
:- import_module rtti_implementation.
:- import_module term_io.
:- import_module type_desc.
:- import_module version_array.

put_int(Stream, Int, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_int(IOStream, Int, unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.int_to_string(Int), !State)
    ).

put_uint(Stream, UInt, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_uint(IOStream, UInt,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.uint_to_string(UInt), !State)
    ).

put_int8(Stream, Int8, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_int8(IOStream, Int8,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.int8_to_string(Int8), !State)
    ).

put_uint8(Stream, UInt8, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_uint8(IOStream, UInt8,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.uint8_to_string(UInt8), !State)
    ).

put_int16(Stream, Int16, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_int16(IOStream, Int16,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.int16_to_string(Int16), !State)
    ).

put_uint16(Stream, UInt16, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_uint16(IOStream, UInt16,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.uint16_to_string(UInt16), !State)
    ).

put_int32(Stream, Int32, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_int32(IOStream, Int32,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.int32_to_string(Int32), !State)
    ).

put_uint32(Stream, UInt32, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_uint32(IOStream, UInt32,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.uint32_to_string(UInt32), !State)
    ).

put_int64(Stream, Int64, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_int64(IOStream, Int64,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.int64_to_string(Int64), !State)
    ).

put_uint64(Stream, UInt64, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_uint64(IOStream, UInt64,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.uint64_to_string(UInt64), !State)
    ).

put_float(Stream, Float, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_float(IOStream, Float,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.float_to_string(Float), !State)
    ).

put_char(Stream, Char, !State) :-
    ( if
        % Handle the common I/O case more efficiently.
        dynamic_cast(!.State, IOState0),
        dynamic_cast(Stream, IOStream)
    then
        io.write_char(IOStream, Char,
            unsafe_promise_unique(IOState0), IOState),
        ( if dynamic_cast(IOState, !:State) then
            !:State = unsafe_promise_unique(!.State)
        else
            error($pred, "unexpected type error")
        )
    else
        put(Stream, string.char_to_string(Char), !State)
    ).

format(Stream, FormatString, Arguments, !State) :-
    string.format(FormatString, Arguments, String),
    put(Stream, String, !State).

nl(Stream, !State) :-
    put(Stream, "\n", !State).

%---------------------------------------------------------------------------%
%
% Various different versions of print.
%

    % The builtin types within the builtin.m module whose printing
    % we may want to specialize.
    %
:- type builtin_type_in_builtin
    --->    type_builtin_string
    ;       type_builtin_character
    ;       type_builtin_float
    ;       type_builtin_int
    ;       type_builtin_int8
    ;       type_builtin_int16
    ;       type_builtin_int32
    ;       type_builtin_int64
    ;       type_builtin_uint
    ;       type_builtin_uint8
    ;       type_builtin_uint16
    ;       type_builtin_uint32
    ;       type_builtin_uint64
    ;       type_builtin_c_ptr.

print(Stream, Term, !State) :-
    print(Stream, canonicalize, Term, !State).

print_cc(Stream, Term, !State) :-
    print(Stream, include_details_cc, Term, !State).

print(Stream, NonCanon, Term, !State) :-
    % In the general case, we call print_quoted below, which is itself
    % intended to call write in most cases. (For now, it calls write
    % in all cases). The purpose of this predicate is to special-case
    % the handling several types, which we do for either of two purposes.
    %
    % The purpose that applies to most of the types handled below
    % is to print things in a more user-friendly fashion than write can.
    % The write predicate is required to output terms in a form that is
    % round-trippable, which means when the string that we write out
    % is read back in, the result should be *exactly* the term we wrote out.
    % For the all the integral types except int itself, this means write
    % has to append a suffix (u, i8, u8 etc), which print does not append.
    % For strings and characters, write adds quotes, while print does not.
    % And so on.
    %
    % The other purpose is speed. We expect that handling ints and floats
    % should be a bit faster with the special case code than the general code.
    % Since they occur often enough in many workloads, optimizing them
    % is a good idea.

    TypeDesc = type_of(Term),
    TypeCtorDesc = type_ctor(TypeDesc),
    type_ctor_name_and_arity(TypeCtorDesc,
        TypeCtorModuleName, TypeCtorName, _TypeCtorArity),
    ( if
        ( TypeCtorModuleName = "builtin"
        ; TypeCtorModuleName = "integer"
        ; TypeCtorModuleName = "univ"
        ; TypeCtorModuleName = "calendar"
        )
    then
        % The code for each type we handle here calls the code for
        % the generic case if the dynamic cast to the expected type fails.
        % The reason why do that is that we want to do the right thing
        % in two very unlikely but possible cases: that we add new type
        % constructors to these modules with the same names but different
        % arities, and that the user links into their programs their own
        % modules with the same names but different contents as the
        % standard library modules. The latter can't happen with builtin.m
        % and private_builtin.m, since those are always implicitly imported
        % into every module, but can happen with the others.

        (
            TypeCtorModuleName = "builtin",
            ( if
                ( TypeCtorName = "string",      TB = type_builtin_string
                ; TypeCtorName = "character",   TB = type_builtin_character
                ; TypeCtorName = "float",       TB = type_builtin_float
                ; TypeCtorName = "int",         TB = type_builtin_int
                ; TypeCtorName = "int8",        TB = type_builtin_int8
                ; TypeCtorName = "int16",       TB = type_builtin_int16
                ; TypeCtorName = "int32",       TB = type_builtin_int32
                ; TypeCtorName = "int64",       TB = type_builtin_int64
                ; TypeCtorName = "uint",        TB = type_builtin_uint
                ; TypeCtorName = "uint8",       TB = type_builtin_uint8
                ; TypeCtorName = "uint16",      TB = type_builtin_uint16
                ; TypeCtorName = "uint32",      TB = type_builtin_uint32
                ; TypeCtorName = "uint64",      TB = type_builtin_uint64
                )
            then
                (
                    TB = type_builtin_string,
                    ( if dynamic_cast(Term, String : string) then
                        put(Stream, String, !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_character,
                    ( if dynamic_cast(Term, Char : char) then
                        put(Stream, Char, !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_float,
                    ( if dynamic_cast(Term, Float : float) then
                        put(Stream, string.float_to_string(Float), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_int,
                    ( if dynamic_cast(Term, Int : int) then
                        put(Stream, int_to_string(Int), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_int8,
                    ( if dynamic_cast(Term, Int8 : int8) then
                        put(Stream, int8_to_string(Int8), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_int16,
                    ( if dynamic_cast(Term, Int16 : int16) then
                        put(Stream, int16_to_string(Int16), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_int32,
                    ( if dynamic_cast(Term, Int32 : int32) then
                        put(Stream, int32_to_string(Int32), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_int64,
                    ( if dynamic_cast(Term, Int64 : int64) then
                        put(Stream, int64_to_string(Int64), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_uint,
                    ( if dynamic_cast(Term, UInt : uint) then
                        put(Stream, uint_to_string(UInt), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_uint8,
                    ( if dynamic_cast(Term, UInt8 : uint8) then
                        put(Stream, uint8_to_string(UInt8), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_uint16,
                    ( if dynamic_cast(Term, UInt16 : uint16) then
                        put(Stream, uint16_to_string(UInt16), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_uint32,
                    ( if dynamic_cast(Term, UInt32 : uint32) then
                        put(Stream, uint32_to_string(UInt32), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                ;
                    TB = type_builtin_uint64,
                    ( if dynamic_cast(Term, UInt64 : uint64) then
                        put(Stream, uint64_to_string(UInt64), !State)
                    else
                        print_quoted(Stream, NonCanon, Term, !State)
                    )
                )
            else
                print_quoted(Stream, NonCanon, Term, !State)
            )
        ;
            TypeCtorModuleName = "integer",
            ( if dynamic_cast(Term, BigInt) then
                put(Stream, integer.to_string(BigInt), !State)
            else
                print_quoted(Stream, NonCanon, Term, !State)
            )
        ;
            TypeCtorModuleName = "univ",
            ( if dynamic_cast(Term, OrigUniv) then
                write_univ(Stream, OrigUniv, !State)
            else
                print_quoted(Stream, NonCanon, Term, !State)
            )
        ;
            TypeCtorModuleName = "calendar",
            ( if dynamic_cast(Term, DateTime) then
                put(Stream, date_to_string(DateTime), !State)
            else if dynamic_cast(Term, Duration) then
                put(Stream, duration_to_string(Duration), !State)
            else
                print_quoted(Stream, NonCanon, Term, !State)
            )
        )
    else
        print_quoted(Stream, NonCanon, Term, !State)
    ).

:- pred print_quoted(Stream, deconstruct.noncanon_handling, T, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode print_quoted(in, in(do_not_allow), in, di, uo) is det.
:- mode print_quoted(in, in(canonicalize), in, di, uo) is det.
:- mode print_quoted(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode print_quoted(in, in, in, di, uo) is cc_multi.

print_quoted(Stream, NonCanon, Term, !State) :-
    write(Stream, NonCanon, Term, !State).
% When we have runtime type classes membership tests, then instead
% of write(Term), we will want to do something like
%   ( if univ_to_type_class(Univ, Portrayable) then
%       portray(Stream, Portrayable, !State)
%   else
%       ... code like write, but which prints the arguments
%       using print_quoted, rather than write ...
%   )

%---------------------------------------------------------------------------%
%
% Various different versions of write.
%

write(Stream, X, !State) :-
    write(Stream, canonicalize, X, !State).

write_cc(Stream, X, !State) :-
    write(Stream, include_details_cc, X, !State).

write(Stream, NonCanon, Term, !State) :-
    type_to_univ(Term, Univ),
    do_write_univ(Stream, NonCanon, Univ, !State).

%---------------------------------------------------------------------------%
%
% Various different versions of write_univ.
%

write_univ(Stream, Univ, !State) :-
    do_write_univ(Stream, canonicalize, Univ, !State).

write_univ(Stream, NonCanon, Univ, !State) :-
    do_write_univ(Stream, NonCanon, Univ, !State).

:- pred do_write_univ(Stream, deconstruct.noncanon_handling, univ,
    State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode do_write_univ(in, in(do_not_allow), in, di, uo) is det.
:- mode do_write_univ(in, in(canonicalize), in, di, uo) is det.
:- mode do_write_univ(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode do_write_univ(in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(do_write_univ/5),
    (Stream = io.output_stream, State = io.state)).

do_write_univ(Stream, NonCanon, Univ, !State) :-
    do_write_univ_prio(Stream, NonCanon, Univ,
        ops.mercury_op_table_max_priority + 1, !State).

:- pred do_write_univ_prio(Stream, deconstruct.noncanon_handling, univ,
    ops.priority, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode do_write_univ_prio(in, in(do_not_allow), in, in, di, uo) is det.
:- mode do_write_univ_prio(in, in(canonicalize), in, in, di, uo) is det.
:- mode do_write_univ_prio(in, in(include_details_cc), in, in, di, uo)
    is cc_multi.
:- mode do_write_univ_prio(in, in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(do_write_univ_prio/6),
    (Stream = io.output_stream, State = io.state)).

    % We only use the io.stream_db we read impurely when we have
    % the io.state.
:- pragma promise_pure(pred(do_write_univ_prio/6)).
do_write_univ_prio(Stream, NonCanon, Univ, Priority, !State) :-
    % We need to special-case a whole bunch of builtin types.
    TypeDesc = univ_type(Univ),
    type_ctor_and_args(TypeDesc, TypeCtorDesc, ArgTypeDescs),
    type_ctor_name_and_arity(TypeCtorDesc,
        TypeCtorModuleName, TypeCtorName, _TypeCtorArity),
    ( if
        ( TypeCtorModuleName = "builtin"
        ; TypeCtorModuleName = "private_builtin"
        ; TypeCtorModuleName = "bitmap"
        ; TypeCtorModuleName = "type_desc"
        ; TypeCtorModuleName = "io"
        ; TypeCtorModuleName = "array"
        ; TypeCtorModuleName = "version_array"
        )
    then
        % The code for each type we handle here calls the code for
        % the generic case if the dynamic cast to the expected type fails.
        % The comment in print above explains why we do this.
        (
            TypeCtorModuleName = "builtin",
            ( if
                ( TypeCtorName = "string",      TB = type_builtin_string
                ; TypeCtorName = "character",   TB = type_builtin_character
                ; TypeCtorName = "float",       TB = type_builtin_float
                ; TypeCtorName = "int",         TB = type_builtin_int
                ; TypeCtorName = "int8",        TB = type_builtin_int8
                ; TypeCtorName = "int16",       TB = type_builtin_int16
                ; TypeCtorName = "int32",       TB = type_builtin_int32
                ; TypeCtorName = "int64",       TB = type_builtin_int64
                ; TypeCtorName = "uint",        TB = type_builtin_uint
                ; TypeCtorName = "uint8",       TB = type_builtin_uint8
                ; TypeCtorName = "uint16",      TB = type_builtin_uint16
                ; TypeCtorName = "uint32",      TB = type_builtin_uint32
                ; TypeCtorName = "uint64",      TB = type_builtin_uint64
                ; TypeCtorName = "c_pointer",   TB = type_builtin_c_ptr
                )
            then
                (
                    TB = type_builtin_string,
                    ( if univ_to_type(Univ, String) then
                        term_io.quote_string(Stream, String, !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_character,
                    ( if univ_to_type(Univ, Char) then
                        term_io.quote_char(Stream, Char, !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_float,
                    ( if univ_to_type(Univ, Float) then
                        put_float(Stream, Float, !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_int,
                    ( if univ_to_type(Univ, Int) then
                        put_int(Stream, Int, !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_int8,
                    ( if univ_to_type(Univ, Int8) then
                        put_int8(Stream, Int8, !State),
                        put(Stream, "i8", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_int16,
                    ( if univ_to_type(Univ, Int16) then
                        put_int16(Stream, Int16, !State),
                        put(Stream, "i16", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_int32,
                    ( if univ_to_type(Univ, Int32) then
                        put_int32(Stream, Int32, !State),
                        put(Stream, "i32", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_int64,
                    ( if univ_to_type(Univ, Int64) then
                        put_int64(Stream, Int64, !State),
                        put(Stream, "i64", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_uint,
                    ( if univ_to_type(Univ, UInt) then
                        put_uint(Stream, UInt, !State),
                        put_char(Stream, 'u', !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_uint8,
                    ( if univ_to_type(Univ, UInt8) then
                        put_uint8(Stream, UInt8, !State),
                        put(Stream, "u8", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_uint16,
                    ( if univ_to_type(Univ, UInt16) then
                        put_uint16(Stream, UInt16, !State),
                        put(Stream, "u16", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_uint32,
                    ( if univ_to_type(Univ, UInt32) then
                        put_uint32(Stream, UInt32, !State),
                        put(Stream, "u32", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_uint64,
                    ( if univ_to_type(Univ, UInt64) then
                        put_uint64(Stream, UInt64, !State),
                        put(Stream, "u64", !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                ;
                    TB = type_builtin_c_ptr,
                    ( if univ_to_type(Univ, C_Pointer) then
                        write_c_pointer(Stream, C_Pointer, !State)
                    else
                        write_ordinary_term(Stream, NonCanon, Univ, Priority,
                            !State)
                    )
                )
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "bitmap",
            ( if univ_to_type(Univ, Bitmap) then
                % Bitmaps are converted to strings of hex digits.
                put_char(Stream, '"', !State),
                put(Stream, bitmap.to_string(Bitmap), !State),
                put_char(Stream, '"', !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "type_desc",
            ( if univ_to_type(Univ, UnivTypeDesc) then
                write_type_desc(Stream, UnivTypeDesc, !State)
            else if univ_to_type(Univ, UnivTypeCtorDesc) then
                write_type_ctor_desc(Stream, UnivTypeCtorDesc, !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "io",
            ( if
                impure get_stream_db_with_locking(StreamDB),
                StreamInfo = get_io_stream_info(StreamDB, univ_value(Univ))
            then
                type_to_univ(StreamInfo, StreamInfoUniv),
                do_write_univ_prio(Stream, NonCanon, StreamInfoUniv, Priority,
                    !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "array",
            ( if
                % Check if the type is array.array/1. We can't just use
                % univ_to_type here since array.array/1 is a polymorphic type.
                TypeCtorName = "array",
                ArgTypeDescs = [ElemType]
            then
                % Now that we know the element type, we can constrain the type
                % of the variable Array so that we can use det_univ_to_type.
                has_type(Elem, ElemType),
                same_array_elem_type(Array, Elem),
                det_univ_to_type(Univ, Array),
                write_array(Stream, Array, !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "version_array",
            ( if
                TypeCtorName = "version_array",
                ArgTypeDescs = [ElemType]
            then
                has_type(Elem, ElemType),
                same_version_array_elem_type(VersionArray, Elem),
                det_univ_to_type(Univ, VersionArray),
                write_version_array(Stream, VersionArray, !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
            )
        ;
            TypeCtorModuleName = "private_builtin",
            ( if
                % Check if the type is private_builtin.type_info/1.
                % See the comments above for array.array/1.
                TypeCtorName = "type_info",
                ArgTypeDescs = [ElemType]
            then
                has_type(Elem, ElemType),
                same_private_builtin_type(PrivateBuiltinTypeInfo, Elem),
                det_univ_to_type(Univ, PrivateBuiltinTypeInfo),
                write_private_builtin_type_info(Stream,
                    PrivateBuiltinTypeInfo, !State)
            else
                write_ordinary_term(Stream, NonCanon, Univ, Priority,
                    !State)
            )
        )
    else
        write_ordinary_term(Stream, NonCanon, Univ, Priority, !State)
    ).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

:- pred same_version_array_elem_type(version_array(T)::unused, T::unused)
    is det.

same_version_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin.type_info::unused,
    T::unused) is det.

same_private_builtin_type(_, _).

:- pred write_ordinary_term(Stream, deconstruct.noncanon_handling, univ,
    ops.priority, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_ordinary_term(in, in(do_not_allow), in, in, di, uo) is det.
:- mode write_ordinary_term(in, in(canonicalize), in, in, di, uo) is det.
:- mode write_ordinary_term(in, in(include_details_cc), in, in, di, uo)
    is cc_multi.
:- mode write_ordinary_term(in, in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(write_ordinary_term/6),
    (Stream = io.output_stream, State = io.state)).

write_ordinary_term(Stream, NonCanon, Univ, Priority, !State) :-
    univ_value(Univ) = Term,
    deconstruct.deconstruct(Term, NonCanon, Functor, _Arity, Args),
    ( if
        Functor = "[|]",
        Args = [ListHead, ListTail]
    then
        put(Stream, '[', !State),
        write_arg(Stream, NonCanon, ListHead, !State),
        write_list_tail(Stream, NonCanon, ListTail, !State),
        put(Stream, ']', !State)
    else if
        Functor = "[]",
        Args = []
    then
        put(Stream, "[]", !State)
    else if
        Functor = "{}",
        Args = [BracedHead | BracedTail]
    then
        (
            BracedTail = [],
            put(Stream, "{ ", !State),
            do_write_univ(Stream, NonCanon, BracedHead, !State),
            put(Stream, " }", !State)
        ;
            BracedTail = [_ | _],
            put(Stream, '{', !State),
            write_arg(Stream, NonCanon, BracedHead, !State),
            write_term_args(Stream, NonCanon, BracedTail, !State),
            put(Stream, '}', !State)
        )
    else if
        ops.lookup_op_infos(ops.init_mercury_op_table, Functor,
            FirstOpInfo, OtherOpInfos)
    then
        select_op_info_and_print(Stream, NonCanon, FirstOpInfo, OtherOpInfos,
            Priority, Functor, Args, !State)
    else
        write_functor_and_args(Stream, NonCanon, Functor, Args, !State)
    ).

:- pred select_op_info_and_print(Stream, deconstruct.noncanon_handling,
    op_info, list(op_info), ops.priority, string, list(univ), State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode select_op_info_and_print(in, in(do_not_allow), in, in, in, in, in,
    di, uo) is det.
:- mode select_op_info_and_print(in, in(canonicalize), in, in, in, in, in,
    di, uo) is det.
:- mode select_op_info_and_print(in, in(include_details_cc), in, in, in, in,
    in, di, uo) is cc_multi.
:- mode select_op_info_and_print(in, in, in, in, in, in, in,
    di, uo) is cc_multi.
:- pragma type_spec(pred(select_op_info_and_print/9),
    (Stream = io.output_stream, State = io.state)).

select_op_info_and_print(Stream, NonCanon, OpInfo, OtherOpInfos, Priority,
        Functor, Args, !State) :-
    OpInfo = op_info(OpClass, _),
    (
        OpClass = prefix(_OpAssoc),
        ( if Args = [Arg] then
            OpInfo = op_info(_, OpPriority),
            maybe_write_paren(Stream, '(', Priority, OpPriority, !State),
            term_io.quote_atom(Stream, Functor, !State),
            put(Stream, " ", !State),
            OpClass = prefix(OpAssoc),
            adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
            do_write_univ_prio(Stream, NonCanon, Arg, NewPriority, !State),
            maybe_write_paren(Stream, ')', Priority, OpPriority, !State)
        else
            select_remaining_op_info_and_print(Stream, NonCanon, OtherOpInfos,
                Priority, Functor, Args, !State)
        )
    ;
        OpClass = postfix(_OpAssoc),
        ( if Args = [PostfixArg] then
            OpInfo = op_info(_, OpPriority),
            maybe_write_paren(Stream, '(', Priority, OpPriority, !State),
            OpClass = postfix(OpAssoc),
            adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
            do_write_univ_prio(Stream, NonCanon, PostfixArg,
                NewPriority, !State),
            put(Stream, " ", !State),
            term_io.quote_atom(Stream, Functor, !State),
            maybe_write_paren(Stream, ')', Priority, OpPriority, !State)
        else
            select_remaining_op_info_and_print(Stream, NonCanon, OtherOpInfos,
                Priority, Functor, Args, !State)
        )
    ;
        OpClass = infix(_LeftAssoc, _RightAssoc),
        ( if Args = [Arg1, Arg2] then
            OpInfo = op_info(_, OpPriority),
            maybe_write_paren(Stream, '(', Priority, OpPriority, !State),
            OpClass = infix(LeftAssoc, _),
            adjust_priority_for_assoc(OpPriority, LeftAssoc, LeftPriority),
            do_write_univ_prio(Stream, NonCanon, Arg1, LeftPriority, !State),
            ( if Functor = "," then
                put(Stream, ", ", !State)
            else
                put(Stream, " ", !State),
                term_io.quote_atom(Stream, Functor, !State),
                put(Stream, " ", !State)
            ),
            OpClass = infix(_, RightAssoc),
            adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
            do_write_univ_prio(Stream, NonCanon, Arg2, RightPriority, !State),
            maybe_write_paren(Stream, ')', Priority, OpPriority, !State)
        else
            select_remaining_op_info_and_print(Stream, NonCanon, OtherOpInfos,
                Priority, Functor, Args, !State)
        )
    ;
        OpClass = binary_prefix(_FirstAssoc, _SecondAssoc),
        ( if Args = [Arg1, Arg2] then
            OpInfo = op_info(_, OpPriority),
            maybe_write_paren(Stream, '(', Priority, OpPriority, !State),
            term_io.quote_atom(Stream, Functor, !State),
            put(Stream, " ", !State),
            OpClass = binary_prefix(FirstAssoc, _),
            adjust_priority_for_assoc(OpPriority, FirstAssoc, FirstPriority),
            do_write_univ_prio(Stream, NonCanon, Arg1, FirstPriority, !State),
            put(Stream, " ", !State),
            OpClass = binary_prefix(_, SecondAssoc),
            adjust_priority_for_assoc(OpPriority, SecondAssoc,
                SecondPriority),
            do_write_univ_prio(Stream, NonCanon, Arg2, SecondPriority, !State),
            maybe_write_paren(Stream, ')', Priority, OpPriority, !State)
        else
            select_remaining_op_info_and_print(Stream, NonCanon, OtherOpInfos,
                Priority, Functor, Args, !State)
        )
    ).

:- pred select_remaining_op_info_and_print(Stream,
    deconstruct.noncanon_handling, list(op_info), ops.priority, string,
    list(univ), State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode select_remaining_op_info_and_print(in, in(do_not_allow), in, in,
    in, in, di, uo) is det.
:- mode select_remaining_op_info_and_print(in, in(canonicalize), in, in,
    in, in, di, uo) is det.
:- mode select_remaining_op_info_and_print(in, in(include_details_cc), in, in,
    in, in, di, uo) is cc_multi.
:- mode select_remaining_op_info_and_print(in, in, in, in,
    in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(select_remaining_op_info_and_print/8),
    (Stream = io.output_stream, State = io.state)).

select_remaining_op_info_and_print(Stream, NonCanon,
        [FirstOpInfo | MoreOpInfos], Priority, Functor, Args, !State) :-
    select_op_info_and_print(Stream, NonCanon, FirstOpInfo, MoreOpInfos,
        Priority, Functor, Args, !State).
select_remaining_op_info_and_print(Stream, NonCanon, [],
        Priority, Functor, Args, !State) :-
    ( if
        Args = [],
        Priority =< ops.mercury_op_table_max_priority
    then
        put(Stream, '(', !State),
        term_io.quote_atom(Stream, Functor, !State),
        put(Stream, ')', !State)
    else
        write_functor_and_args(Stream, NonCanon, Functor, Args, !State)
    ).

:- pred write_functor_and_args(Stream, deconstruct.noncanon_handling, string,
    list(univ), State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_functor_and_args(in, in(do_not_allow), in, in, di, uo) is det.
:- mode write_functor_and_args(in, in(canonicalize), in, in, di, uo) is det.
:- mode write_functor_and_args(in, in(include_details_cc), in, in, di, uo)
    is cc_multi.
:- mode write_functor_and_args(in, in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(write_functor_and_args/6),
    (Stream = io.output_stream, State = io.state)).

:- pragma inline(pred(write_functor_and_args/6)).

write_functor_and_args(Stream, NonCanon, Functor, Args, !State) :-
    term_io.quote_atom_agt(Stream, Functor,
        maybe_adjacent_to_graphic_token, !State),
    (
        Args = [X | Xs],
        put(Stream, '(', !State),
        write_arg(Stream, NonCanon, X, !State),
        write_term_args(Stream, NonCanon, Xs, !State),
        put(Stream, ')', !State)
    ;
        Args = []
    ).

:- pragma inline(pred(maybe_write_paren/6)).

maybe_write_paren(Stream, String, Priority, OpPriority, !State) :-
    ( if OpPriority > Priority then
        put(Stream, String, !State)
    else
        true
    ).

:- pred write_list_tail(Stream, deconstruct.noncanon_handling, univ,
    State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_list_tail(in, in(do_not_allow), in, di, uo) is det.
:- mode write_list_tail(in, in(canonicalize), in, di, uo) is det.
:- mode write_list_tail(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_list_tail(in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(write_list_tail/5),
    (Stream = io.output_stream, State = io.state)).

write_list_tail(Stream, NonCanon, Univ, !State) :-
    Term = univ_value(Univ),
    deconstruct.deconstruct(Term, NonCanon, Functor, _Arity, Args),
    ( if
        Functor = "[|]",
        Args = [ListHead, ListTail]
    then
        put(Stream, ", ", !State),
        write_arg(Stream, NonCanon, ListHead, !State),
        write_list_tail(Stream, NonCanon, ListTail, !State)
    else if
        Functor = "[]",
        Args = []
    then
        true
    else
        put(Stream, " | ", !State),
        do_write_univ(Stream, NonCanon, Univ, !State)
    ).

    % Write the remaining arguments.
    %
:- pred write_term_args(Stream, deconstruct.noncanon_handling, list(univ),
    State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_term_args(in, in(do_not_allow), in, di, uo) is det.
:- mode write_term_args(in, in(canonicalize), in, di, uo) is det.
:- mode write_term_args(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_term_args(in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(write_term_args/5),
    (Stream = io.output_stream, State = io.state)).

write_term_args(_Stream, _, [], !State).
write_term_args(Stream, NonCanon, [X | Xs], !State) :-
    put(Stream, ", ", !State),
    write_arg(Stream, NonCanon, X, !State),
    write_term_args(Stream, NonCanon, Xs, !State).

:- pred write_arg(Stream, deconstruct.noncanon_handling, univ, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode write_arg(in, in(do_not_allow), in, di, uo) is det.
:- mode write_arg(in, in(canonicalize), in, di, uo) is det.
:- mode write_arg(in, in(include_details_cc), in, di, uo) is cc_multi.
:- mode write_arg(in, in, in, di, uo) is cc_multi.
:- pragma type_spec(pred(write_arg/5),
    (Stream = io.output_stream, State = io.state)).

write_arg(Stream, NonCanon, X, !State) :-
    arg_priority(ArgPriority, !State),
    do_write_univ_prio(Stream, NonCanon, X, ArgPriority, !State).

:- pred arg_priority(int::out, State::di, State::uo) is det.

% arg_priority(ArgPriority, !State) :-
%   ( ops.lookup_infix_op(ops.init_mercury_op_table, ",", Priority, _, _) ->
%       ArgPriority = Priority
%   ;
%       error("arg_priority: can't find the priority of `,'")
%   ).
%
% We could implement this as above, but it's more efficient to just
% hard-code it.
arg_priority(1000, !State).

%---------------------------------------------------------------------------%

:- pred write_type_desc(Stream::in, type_desc::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_type_desc(Stream, TypeDesc, !State) :-
    put(Stream, type_name(TypeDesc), !State).

:- pred write_type_ctor_desc(Stream::in, type_ctor_desc::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

write_type_ctor_desc(Stream, TypeCtorDesc, !State) :-
    type_ctor_name_and_arity(TypeCtorDesc, ModuleName, Name, Arity0),
    ( if
        ModuleName = "builtin",
        Name = "func"
    then
        % The type ctor that we call `builtin:func/N' takes N + 1
        % type parameters: N arguments plus one return value.
        % So we need to subtract one from the arity here.
        Arity = Arity0 - 1
    else
        Arity = Arity0
    ),
    ( if ModuleName = "builtin" then
        format(Stream, "%s/%d", [s(Name), i(Arity)], !State)
    else
        format(Stream, "%s.%s/%d", [s(ModuleName), s(Name), i(Arity)], !State)
    ).

:- pred write_c_pointer(Stream::in, c_pointer::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_c_pointer(Stream, C_Pointer, !State) :-
    put(Stream, c_pointer_to_string(C_Pointer), !State).

:- pred write_array(Stream::in, array(T)::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- pragma type_spec(pred(write_array/4),
    (Stream = io.output_stream, State = io.state)).

write_array(Stream, Array, !State) :-
    put(Stream, "array(", !State),
    array.to_list(Array, List),
    write(Stream, List, !State),
    put(Stream, ")", !State).

:- pred write_version_array(Stream::in, version_array(T)::in,
    State::di, State::uo) is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- pragma type_spec(pred(write_version_array/4),
    (Stream = io.output_stream, State = io.state)).

write_version_array(Stream, VersionArray, !State) :-
    put(Stream, "version_array(", !State),
    List = version_array.to_list(VersionArray),
    write(Stream, List, !State),
    put(Stream, ")", !State).

:- pred write_private_builtin_type_info(Stream::in,
    private_builtin.type_info::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

write_private_builtin_type_info(Stream, PrivateBuiltinTypeInfo, !State) :-
    private_builtin.unsafe_type_cast(PrivateBuiltinTypeInfo, TypeInfo),
    type_info_to_type_desc(TypeInfo, TypeDesc),
    write_type_desc(Stream, TypeDesc, !State).

%---------------------------------------------------------------------------%
:- end_module stream.string_writer.
%---------------------------------------------------------------------------%
