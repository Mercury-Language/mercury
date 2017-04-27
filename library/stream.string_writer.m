%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

:- pred put_float(Stream::in, float::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

:- pred put_char(Stream::in, char::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % A version of io.format that works for arbitrary string writers.
    %
:- pred format(Stream::in, string::in, list(string.poly_type)::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

:- pred nl(Stream::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

    % print/4 writes its second argument to the string writer stream specified
    % in its first argument.  In all cases, the argument to output can be of
    % any type.  It is output in a format that is intended to be human
    % readable.
    %
    % If the argument is just a single string or character, it will be printed
    % out exactly as is (unquoted).  If the argument is of type integer (i.e.
    % an arbitrary precision integer), then its decimal representation will be
    % printed.  If the argument is of type univ, then the value stored in the
    % the univ will be printed out, but not the type.  If the argument is of
    % type date_time, it will be printed out in the same form as the string
    % returned by the function date_to_string/1.  If the argument is of type
    % duration, it will be printed out in the same form as the string
    % returned by the function duration_to_string/1.
    %
    % print/5 is the same as print/4 except that it allows the caller to
    % specify how non-canonical types should be handled.  print/4 implicitly
    % specifies `canonicalize' as the method for handling non-canonical types.
    % This means that for higher-order types, or types with user-defined
    % equality axioms, or types defined using the foreign language interface
    % (i.e. pragma foreign_type), the text output will only describe the type
    % that is being printed, not the value.
    %
    % print_cc/4 is the same as print/4 except that it specifies
    % `include_details_cc' rather than `canonicalize'.  This means that it will
    % print the details of non-canonical types.  However, it has determinism
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
    % in its first argument.  In all cases, the argument to output may be of
    % any type.  The argument is written in a format that is intended to be
    % valid Mercury syntax whenever possible.
    %
    % Strings and characters are always printed out in quotes, using backslash
    % escapes if necessary.  For higher-order types, or for types defined using
    % the foreign language interface (pragma foreign_type), the text output
    % will only describe the type that is being printed, not the value, and the
    % result may not be parsable by `read'.  For the types containing
    % existential quantifiers, the type `type_desc' and closure types, the
    % result may not be parsable by `read', either.  But in all other cases the
    % format used is standard Mercury syntax, and if you append a period and
    % newline (".\n"), then the results can be read in again using `read'.
    %
    % write/5 is the same as write/4 except that it allows the caller to
    % specify how non-canonical types should be handled.  write_cc/4 is the
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
:- pragma type_spec(maybe_write_paren/6,
    (Stream = io.output_stream, State = io.state)).

:- pragma type_spec(write/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(write/5,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(write_univ/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(write_univ/5,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(put_int/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(put_uint/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(put_float/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(put_char/4,
            (Stream = io.output_stream, State = io.state)).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module calendar.
:- import_module int.
:- import_module integer.
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
            error("stream.string_writer.put_int: unexpected type error")
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
            error("stream.string_writer.put_uint: unexpected type error")
        )
    else
        put(Stream, string.uint_to_string(UInt), !State)
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
            error("stream.string_writer.put_float: unexpected type error")
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
            error("stream.string_writer.put_char: unexpected type error")
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
% Various different versions of print
%

print(Stream, Term, !State) :-
    print(Stream, canonicalize, Term, !State).

print_cc(Stream, Term, !State) :-
    print(Stream, include_details_cc, Term, !State).

print(Stream, NonCanon, Term, !State) :-
    % `string', `char', `uint' and `univ' are special cases for print
    ( if dynamic_cast(Term, String : string) then
        put(Stream, String, !State)
    else if dynamic_cast(Term, Char : char) then
        put(Stream, Char, !State)
    else if dynamic_cast(Term, UInt : uint) then
        put(Stream, uint_to_string(UInt), !State)
    else if dynamic_cast(Term, OrigUniv) then
        write_univ(Stream, OrigUniv, !State)
    else if dynamic_cast(Term, BigInt) then
        put(Stream, integer.to_string(BigInt), !State)
    else if dynamic_cast(Term, DateTime) then
        put(Stream, date_to_string(DateTime), !State)
    else if dynamic_cast(Term, Duration) then
        put(Stream, duration_to_string(Duration), !State)
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
% Various different versions of write
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
% Various different versions of write_univ
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
:- pragma type_spec(do_write_univ/5,
            (Stream = io.output_stream, State = io.state)).

do_write_univ(Stream, NonCanon, Univ, !State) :-
    do_write_univ_prio(Stream, NonCanon, Univ,
        ops.mercury_max_priority(ops.init_mercury_op_table) + 1, !State).

:- pred do_write_univ_prio(Stream, deconstruct.noncanon_handling, univ,
    ops.priority, State, State)
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- mode do_write_univ_prio(in, in(do_not_allow), in, in, di, uo) is det.
:- mode do_write_univ_prio(in, in(canonicalize), in, in, di, uo) is det.
:- mode do_write_univ_prio(in, in(include_details_cc), in, in, di, uo)
    is cc_multi.
:- mode do_write_univ_prio(in, in, in, in, di, uo) is cc_multi.
:- pragma type_spec(do_write_univ_prio/6,
            (Stream = io.output_stream, State = io.state)).

    % We only use the io.stream_db we read impurely when we have
    % the io.state.
:- pragma promise_pure(do_write_univ_prio/6).

do_write_univ_prio(Stream, NonCanon, Univ, Priority, !State) :-
    % We need to special-case the builtin types:
    %   int, uint, char, float, string
    %   type_info, univ, c_pointer, array
    %   and private_builtin.type_info
    %
    ( if univ_to_type(Univ, String) then
        term_io.quote_string(Stream, String, !State)
    else if univ_to_type(Univ, Char) then
        term_io.quote_char(Stream, Char, !State)
    else if univ_to_type(Univ, Int) then
        put_int(Stream, Int, !State)
    else if univ_to_type(Univ, UInt) then
        put_uint(Stream, UInt, !State),
        put_char(Stream, 'u', !State)
    else if univ_to_type(Univ, Float) then
        put_float(Stream, Float, !State)
    else if univ_to_type(Univ, Bitmap) then
        % Bitmaps are converted to strings of hex digits.
        put_char(Stream, '"', !State),
        put(Stream, bitmap.to_string(Bitmap), !State),
        put_char(Stream, '"', !State)
    else if univ_to_type(Univ, TypeDesc) then
        write_type_desc(Stream, TypeDesc, !State)
    else if univ_to_type(Univ, TypeCtorDesc) then
        write_type_ctor_desc(Stream, TypeCtorDesc, !State)
    else if univ_to_type(Univ, C_Pointer) then
        write_c_pointer(Stream, C_Pointer, !State)
    else if
        impure io.get_stream_db_with_locking(StreamDB),
        StreamInfo = get_io_stream_info(StreamDB, univ_value(Univ))
    then
        type_to_univ(StreamInfo, StreamInfoUniv),
        do_write_univ_prio(Stream, NonCanon, StreamInfoUniv, Priority,
            !.State, !:State)
    else if
        % Check if the type is array.array/1. We can't just use univ_to_type
        % here since array.array/1 is a polymorphic type.
        %
        % The calls to type_ctor_name and type_ctor_module_name are not really
        % necessary -- we could use univ_to_type in the condition instead
        % of det_univ_to_type in the body. However, this way of doing things
        % is probably more efficient in the common case when the thing being
        % printed is *not* of type array.array/1.
        %
        % The ordering of the tests here (arity, then name, then module name,
        % rather than the reverse) is also chosen for efficiency, to find
        % failure cheaply in the common cases, rather than for readability.
        %
        type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "array",
        type_ctor_module_name(TypeCtor) = "array"
    then
        % Now that we know the element type, we can constrain the type
        % of the variable `Array' so that we can use det_univ_to_type.

        has_type(Elem, ElemType),
        same_array_elem_type(Array, Elem),
        det_univ_to_type(Univ, Array),
        write_array(Stream, Array, !State)
    else if
        type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "version_array",
        type_ctor_module_name(TypeCtor) = "version_array"
    then
        has_type(Elem, ElemType),
        same_version_array_elem_type(VersionArray, Elem),
        det_univ_to_type(Univ, VersionArray),
        write_version_array(Stream, VersionArray, !State)
    else if
        % Check if the type is private_builtin.type_info/1.
        % See the comments above for array.array/1.

        type_ctor_and_args(univ_type(Univ), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "type_info",
        type_ctor_module_name(TypeCtor) = "private_builtin"
    then
        has_type(Elem, ElemType),
        same_private_builtin_type(PrivateBuiltinTypeInfo, Elem),
        det_univ_to_type(Univ, PrivateBuiltinTypeInfo),
        write_private_builtin_type_info(Stream, PrivateBuiltinTypeInfo, !State)
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
:- pragma type_spec(write_ordinary_term/6,
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
:- pragma type_spec(select_op_info_and_print/9,
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
:- mode select_remaining_op_info_and_print(in, in(do_not_allow), in, in, in,
    in, di, uo) is det.
:- mode select_remaining_op_info_and_print(in, in(canonicalize), in, in, in,
    in, di, uo) is det.
:- mode select_remaining_op_info_and_print(in(include_details_cc), in, in, in,
    in, in, di, uo) is cc_multi.
:- mode select_remaining_op_info_and_print(in, in, in, in, in, in, di, uo)
    is cc_multi.
:- pragma type_spec(select_remaining_op_info_and_print/8,
    (Stream = io.output_stream, State = io.state)).

select_remaining_op_info_and_print(Stream, NonCanon,
        [FirstOpInfo | MoreOpInfos], Priority, Functor, Args, !State) :-
    select_op_info_and_print(Stream, NonCanon, FirstOpInfo, MoreOpInfos,
        Priority, Functor, Args, !State).
select_remaining_op_info_and_print(Stream, NonCanon, [],
        Priority, Functor, Args, !State) :-
    ( if
        Args = [],
        Priority =< ops.mercury_max_priority(ops.init_mercury_op_table)
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
:- pragma type_spec(write_functor_and_args/6,
    (Stream = io.output_stream, State = io.state)).

:- pragma inline(write_functor_and_args/6).

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

:- pragma inline(maybe_write_paren/6).

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
:- pragma type_spec(write_list_tail/5,
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
:- pragma type_spec(write_term_args/5,
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
:- pragma type_spec(write_arg/5,
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
:- pragma type_spec(write_array/4,
    (Stream = io.output_stream, State = io.state)).

write_array(Stream, Array, !State) :-
    put(Stream, "array(", !State),
    array.to_list(Array, List),
    write(Stream, List, !State),
    put(Stream, ")", !State).

:- pred write_version_array(Stream::in, version_array(T)::in,
    State::di, State::uo) is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- pragma type_spec(write_version_array/4,
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
