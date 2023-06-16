%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a problem in rotd-2008-01-15 and before.
% In those versions of Mercury the MLDS->C code generator was generating
% invalid C for static ground terms when the terms contained
% no_tag types whose arguments were floats.  The problem was that the code
% generator was trying to insert calls to MR_box_float() into static
% initializers rather than emitting a separate global containing the
% float constant and taking the address of that global.  (This problem was
% independent of whether --high-level-data was enabled or not.)
% The problem did not occur with the LLDS backend because it handles
% static ground terms differently.
%
% This module also exercises creating static ground terms with other sorts
% of no_tag type as well.
%

:- module static_no_tag.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- type no_tag_float   ---> no_tag_float(float).
:- type no_tag_int     ---> no_tag_int(int).
:- type no_tag_string  ---> no_tag_string(string).
:- type no_tag_char    ---> no_tag_char(char).
:- type no_tag_poly(T) ---> no_tag_poly(T).

:- type float_pair ---> float_pair(float, float).

:- func get_no_tag_floats = list(no_tag_float).
:- func get_no_tag_ints = list(no_tag_int).
:- func get_no_tag_strings = list(no_tag_string).
:- func get_no_tag_poly_float = list(no_tag_poly(float)).
:- func get_float_list = list(float).
:- func get_list_float_pair = list(float_pair).

:- implementation.

main(!IO) :-
    io.write_string("get_no_tag_floats = ", !IO),
    io.write_line(get_no_tag_floats, !IO),
    io.write_string("get_no_tag_ints = ", !IO),
    io.write_line(get_no_tag_ints, !IO),
    io.write_string("get_no_tag_strings = ", !IO),
    io.write_line(get_no_tag_strings, !IO),
    io.write_string("get_no_tag_chars = ", !IO),
    io.write_line(get_no_tag_chars, !IO),
    io.write_string("get_no_tag_poly_float = ", !IO),
    io.write_line(get_no_tag_poly_float, !IO),
    io.write_string("get_float_list = ", !IO),
    io.write_line(get_float_list, !IO),
    io.write_string("get_list_float_pair = ", !IO),
    io.write_line(get_list_float_pair, !IO).

:- pragma no_inline(get_no_tag_floats/0).
get_no_tag_floats = [no_tag_float(3.0), no_tag_float(4.0)].

:- pragma no_inline(get_no_tag_ints/0).
get_no_tag_ints = [no_tag_int(777), no_tag_int(888)].

:- pragma no_inline(get_no_tag_strings/0).
get_no_tag_strings = [no_tag_string("foo"), no_tag_string("bar"),
    no_tag_string("baz")].

:- pragma no_inline(get_no_tag_chars/0).
:- func get_no_tag_chars = list(no_tag_char).
get_no_tag_chars = [no_tag_char('a'), no_tag_char('b'), no_tag_char('c')].

:- pragma no_inline(get_no_tag_poly_float/0).
get_no_tag_poly_float = [no_tag_poly(5.5), no_tag_poly(6.6), no_tag_poly(7.7)].

:- pragma no_inline(get_float_list/0).
get_float_list = [1.1, 2.2, 3.3, 4.4].

:- pragma no_inline(get_list_float_pair/0).
get_list_float_pair = [float_pair(6.6, 6.6), float_pair(7.7, 7.7)].
