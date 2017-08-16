%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: percent_encoding.m.
% Main author: wangp.
%
% This module performs percent-encoding.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.percent_encoding.
:- interface.

    % Apply percent-encoding to a path segment.
    %
:- func percent_encode_path_segment(string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

percent_encode_path_segment(S0) = S :-
    ( if string.all_match(unreserved_char_in_path_segment, S0) then
        S = S0
    else
        string.to_utf8_code_unit_list(S0, Octets),
        list.foldr(percent_encode_octet, Octets, [], Encoded),
        string.from_char_list(Encoded, S)
    ).

:- pred percent_encode_octet(int::in, list(char)::in, list(char)::out) is det.

percent_encode_octet(Octet, Encoded0, Encoded) :-
    ( if
        Octet =< 0x7f,
        char.from_int(Octet, Char),
        unreserved_char_in_path_segment(Char)
    then
        Encoded = [Char | Encoded0]
    else
        octet_to_hex_chars(Octet, Hi, Lo),
        Encoded = ['%', Hi, Lo | Encoded0]
    ).

:- pred octet_to_hex_chars(int::in, char::out, char::out) is det.

octet_to_hex_chars(I, Hi, Lo) :-
    Int_Hi = (I /\ 0xf0) `unchecked_right_shift` 4,
    Int_Lo = (I /\ 0x0f),
    Hi = char.det_int_to_hex_digit(Int_Hi),
    Lo = char.det_int_to_hex_digit(Int_Lo).

:- pred unreserved_char_in_path_segment(char::in) is semidet.

unreserved_char_in_path_segment(C) :-
    % These characters are in the reserved set but have no reserved purpose in
    % path segments.
    ( C = ('!')
    ; C = ('*')
    ; C = ('''')
    ; C = ('(')
    ; C = (')')
    ; C = (';')
    ; C = (':')
    ; C = ('@')
    ; C = ('&')
    ; C = ('=')
    ; C = ('+')
    ; C = ('$')
    ; C = (',')

    % These characters are in the unreserved set.
    ; C = ('-')
    ; C = ('_')
    ; C = ('.')
    ; C = ('~')
    ; C = ('0')
    ; C = ('1')
    ; C = ('2')
    ; C = ('3')
    ; C = ('4')
    ; C = ('5')
    ; C = ('6')
    ; C = ('7')
    ; C = ('8')
    ; C = ('9')
    ; C = ('A')
    ; C = ('B')
    ; C = ('C')
    ; C = ('D')
    ; C = ('E')
    ; C = ('F')
    ; C = ('G')
    ; C = ('H')
    ; C = ('I')
    ; C = ('J')
    ; C = ('K')
    ; C = ('L')
    ; C = ('M')
    ; C = ('N')
    ; C = ('O')
    ; C = ('P')
    ; C = ('Q')
    ; C = ('R')
    ; C = ('S')
    ; C = ('T')
    ; C = ('U')
    ; C = ('V')
    ; C = ('W')
    ; C = ('X')
    ; C = ('Y')
    ; C = ('Z')
    ; C = ('a')
    ; C = ('b')
    ; C = ('c')
    ; C = ('d')
    ; C = ('e')
    ; C = ('f')
    ; C = ('g')
    ; C = ('h')
    ; C = ('i')
    ; C = ('j')
    ; C = ('k')
    ; C = ('l')
    ; C = ('m')
    ; C = ('n')
    ; C = ('o')
    ; C = ('p')
    ; C = ('q')
    ; C = ('r')
    ; C = ('s')
    ; C = ('t')
    ; C = ('u')
    ; C = ('v')
    ; C = ('w')
    ; C = ('x')
    ; C = ('y')
    ; C = ('z')
    ).

%---------------------------------------------------------------------------%
:- end_module mdb.percent_encoding.
%---------------------------------------------------------------------------%
