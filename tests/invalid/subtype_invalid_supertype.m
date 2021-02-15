%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_invalid_supertype.
:- interface.

:- type s1_1 =< int ---> s.
:- type s1_2 =< uint ---> s.
:- type s1_3 =< int8 ---> s.
:- type s1_4 =< uint8 ---> s.
:- type s1_5 =< int16 ---> s.
:- type s1_6 =< uint16 ---> s.
:- type s1_7 =< int32 ---> s.
:- type s1_8 =< uint32 ---> s.
:- type s1_9 =< int64 ---> s.
:- type s1_10 =< uint64 ---> s.
:- type s1_11 =< float ---> s.
:- type s1_12 =< string ---> s.
:- type s1_13 =< character ---> s.

:- type s2 =< {}
    --->    s.

:- type s3 =< (func(int) = int)
    --->    s.

:- type s4 =< (pred)
    --->    s.

:- type s5 =< undefined
    --->    s.

:- type s6 =< t(1, 2)
    --->    s.
