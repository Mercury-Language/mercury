%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test warnings for unsigned comparisons against zero that are always true
% or always false.
%
%---------------------------------------------------------------------------%

:- module unsigned_zero_cmp.
:- interface.

:- pred test_uint(uint::in) is semidet.
:- pred test_uint8(uint8::in) is semidet.
:- pred test_uint16(uint16::in) is semidet.
:- pred test_uint32(uint32::in) is semidet.
:- pred test_uint64(uint64::in) is semidet.

:- implementation.

:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

test_uint(X) :-
   X < 0u,
   0u > X,
   0u =< X,
   X >= 0u.

test_uint8(X) :-
   X < 0u8,
   0u8 > X,
   0u8 =< X,
   X >= 0u8.

test_uint16(X) :-
   X < 0u16,
   0u16 > X,
   0u16 =< X,
   X >= 0u16.

test_uint32(X) :-
   X < 0u32,
   0u32 > X,
   0u32 =< X,
   X >= 0u32.

test_uint64(X) :-
   X < 0u64,
   0u64 > X,
   0u64 =< X,
   X >= 0u64.
