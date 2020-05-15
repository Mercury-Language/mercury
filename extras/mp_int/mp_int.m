%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: mp_int.m.
% Main author: Matthias Güdemann.
% Stability: low.
%
% This module implements a binding to libtomath.
%
% This library provides a portable ISO C implementation of multi precision
% integers. libtommath is in the public domain and its source code is available
% from https://github.com/libtom/libtommath.
%
% To use the provided binding, one needs the compiled library and the .h
% include files, see README.txt for the details.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mp_int.

:- interface.

:- type mp_int.

    % Addition.
    %
:- func '+'(mp_int, mp_int) = mp_int.

    % Subtraction.
    %
:- func '-'(mp_int, mp_int) = mp_int.

    % Unary minus.
    %
:- func '-'(mp_int) = mp_int.

    % Multiplication.
    %
:- func '*'(mp_int, mp_int) = mp_int.

    % Truncating integer division, e.g., (-10) // 3 = (-3).
    %
:- func '//'(mp_int, mp_int) = mp_int.

    % Remainder.
    % X rem Y = X - (X // Y) * Y
    %
:- func 'rem'(mp_int, mp_int) = mp_int.

    % Absolute value.
    %
:- func abs(mp_int) = mp_int.

    % Squaring.
    %
:- func square(mp_int) = mp_int.

    % Truncating integer division with remainder.
    %
:- pred divide_with_rem(mp_int::in, mp_int::in, mp_int::out, mp_int::out)
    is det.

    % Multiplication by 2.
    %
:- pred multiply_by_2(mp_int::in, mp_int::out) is det.

    % Division by 2.
    %
:- pred divide_by_2(mp_int::in, mp_int::out) is det.

    % Shift Left.
    %
:- func '<<'(mp_int, int) = mp_int.

    % Shift Right.
    %
:- func '>>'(mp_int, int) = mp_int.

    % is_zero(X) if X is 0.
    %
:- pred is_zero(mp_int::in) is semidet.

    % is_even(X) if X is even.
    %
:- pred is_even(mp_int::in) is semidet.

    % is_odd(X) if X is odd.
    %
:- pred is_odd(mp_int::in) is semidet.

    % is_negative(X) if X is negative.
    %
:- pred is_negative(mp_int::in) is semidet.

    % Greater than.
    %
:- pred '>'(mp_int::in, mp_int::in) is semidet.

    % Less than.
    %
:- pred '<'(mp_int::in, mp_int::in) is semidet.

    % Greater or equal.
    %
:- pred '>='(mp_int::in, mp_int::in) is semidet.

    % Less or equal.
    %
:- pred '=<'(mp_int::in, mp_int::in) is semidet.

    % Equal.
    %
:- pred equal(mp_int::in, mp_int::in) is semidet.

    % Exponentiation.
    % Throws exception `exception.domain_error` if Y is negative.
    %
:- func pow(mp_int, mp_int) = mp_int.

    % Convert mp_int to int.
    % Fails if not inside [min_int, max_int] interval.
    %
:- pred to_int(mp_int::in, int::out) is semidet.

    % As above, but throws exception if value is outside
    % [min_int, max_int] interval.
    %
:- func det_to_int(mp_int) = int.

    % to_base_string(Mp_Int, Base) = String:
    %
    % Convert mp_int to a string in given base.
    %
    % Base must be between 2 and 64, inclusive; if it is not, the predicate
    % will throw an exception.
    %
:- func to_base_string(mp_int, int) = string.

    % Convert mp_int to a string in base 10.
    %
:- func to_string(mp_int) = string.

    % from_base_string(String, Base, Mp_Int):
    %
    % Convert string in given base to mp_int.
    %
    % Base must be between 2 and 64, inclusive; fails if unsuccessful.
    %
:- pred from_base_string(string::in, int::in, mp_int::out) is semidet.

    % Convert string in base 10 to mp_int. Fails if unsuccessful.
    %
:- pred from_string(string::in, mp_int::out) is semidet.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_string(string) = mp_int.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_base_string(string, int) = mp_int.

    % Convert an int to an mp_int.
    %
:- func mp_int(int) = mp_int.

    % Square root of mp_int.
    %
    % sqrt(X, Sqrt) is true if Sqrt is the positive square root of X.
    % Fails if X is negative.
    %
:- pred sqrt(mp_int::in, mp_int::out) is semidet.

    % As above, but throws error in case of negative value.
    %
:- func det_sqrt(mp_int) = mp_int.

    % Bitwise or.
    %
:- func mp_int \/ mp_int = mp_int.

    % Bitwise and.
    %
:- func mp_int /\ mp_int = mp_int.

    % Bitwise xor.
    %
:- func mp_int `xor` mp_int = mp_int.

    % Bitwise complement.
    %
:- func \ mp_int = mp_int.

    % Greatest common divisor.
    %
:- func gcd(mp_int, mp_int) = mp_int.

    % Least common multiple.
    %
:- func lcm(mp_int, mp_int) = mp_int.

    % jacobi(A, N) = C:
    %
    % Computes Jacobi symbol.
    %
    % C = J(A, N) = L(A, P_1)^(i_1) * ... * L(A, P_k)^(i_k) where
    %
    % A = P_1^(i_1) * ... * P_k^(i_k) with P_j is prime, and
    %
    %            / 1, if a is a quadratic residue modulo p, and a \= 0 (mod p)
    % L(A, P) = | -1, if a is a quadratic non-residue modulo p
    %            \ 0, if a is a multiple of p
    %
:- func jacobi(mp_int, mp_int) = int.

    % invmod(A, B) = C:
    %
    % Modular inverse C = A^(-1) mod B
    %
:- func invmod(mp_int, mp_int) = mp_int.

    % exptmod(A, B, C, D):
    %
    % Modular exponentiation D = A^B mod C.
    %
:- func exptmod(mp_int, mp_int, mp_int) = mp_int.

    % Probabilistic primality test.
    %
:- pred is_prime(mp_int::in) is semidet.

    % Probabilistic primality test with given number of rounds. Probability of
    % reporting composite number for a prime is ca. (1/4)^(-#Rounds)
    %
:- pred is_prime(mp_int::in, int::in) is semidet.

    % Constant 0.
    %
:- func zero = mp_int.

    % Constant 1.
    %
:- func one = mp_int.

    % Constant 2.
    %
:- func two = mp_int.

    % Constant -1.
    %
:- func negative_one = mp_int.

    % Constant 10.
    %
:- func ten = mp_int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%
% foreign declarations
%---------------------------------------------------------------------------%

   % Type declaration for foreign type mp_int*.
   %
:- pragma foreign_type("C", mp_int, "mp_int*")
    where equality is equal, comparison is mp_cmp.
:- pragma foreign_decl("C",
                      "#include \"tommath.h\"").

    % We assume unsigned long long to be at least as big as MR_Integer.
    % This is currently required for the to_int predicates.
    %
:- pragma foreign_code("C", "
    MR_STATIC_ASSERT(mp_int, sizeof(unsigned long long) >= sizeof(MR_Integer));
").

    % Result type to signal success or failure of external functions.
    %
:- type mp_result_type --->
      mp_result_okay
    ; mp_result_out_of_mem
    ; mp_result_invalid_input.

    % mapping of libtommath results to Mercury enum.
:- pragma foreign_enum("C", mp_result_type/0,
                      [
                       mp_result_okay          - "MP_OKAY",
                       mp_result_out_of_mem    - "MP_MEM",
                       mp_result_invalid_input - "MP_VAL"
                      ]).

%---------------------------------------------------------------------------%
% initialisation code to create static mp_ints for often used constants
%---------------------------------------------------------------------------%

:- initialise mp_initialize/0.
:- impure pred mp_initialize is det.

:- pragma foreign_decl("C",
"
  extern mp_int MP_INT_constant_negative_one;
  extern mp_int MP_INT_constant_zero;
  extern mp_int MP_INT_constant_one;
  extern mp_int MP_INT_constant_two;
  extern mp_int MP_INT_constant_ten;
").

:- pragma foreign_code("C",
"
  mp_int MP_INT_constant_negative_one;
  mp_int MP_INT_constant_zero;
  mp_int MP_INT_constant_one;
  mp_int MP_INT_constant_two;
  mp_int MP_INT_constant_ten;
").

:- pragma foreign_proc("C",
                      mp_initialize,
                      [will_not_call_mercury, thread_safe],
"
  mp_init_set(&MP_INT_constant_negative_one, -1);
  mp_init_set(&MP_INT_constant_zero, 0);
  mp_init_set(&MP_INT_constant_one, 1);
  mp_init_set(&MP_INT_constant_two, 2);
  mp_init_set(&MP_INT_constant_ten, 10);
").

%---------------------------------------------------------------------------%
% module internal predicate declarations
%---------------------------------------------------------------------------%

:- pred mp_init(int::in, mp_int::out) is det.
mp_init(N, Res) :-
    mp_init(N, Result, Res0),
    ( Result = mp_result_okay ->
        Res = Res0
    ;
        error("could not initialize mp_int")
    ).

:- pred mp_init(int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_init(Value::in, Result::out, Mp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Mp_Int     = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(Mp_Int);
  if (Result == MP_OKAY)
    Result   = mp_set_long_long(Mp_Int, Value);
").

%---------------------------------------------------------------------------%
% basic arithmetic
%---------------------------------------------------------------------------%

:- pred mp_add(mp_int::in, mp_int::in, mp_int::out) is det.
mp_add(A, B, C) :-
    mp_add(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.mp_add: could not add"))
    ).

:- pred mp_add(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_add(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_add(A, B, C);
").

:- pred mp_sub(mp_int::in, mp_int::in, mp_int::out) is det.
mp_sub(A, B, C) :-
    mp_sub(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.mp_sub: could not subtract"))
    ).

:- pred mp_sub(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_sub(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_sub(A, B, C);
").

:- pred mp_neg(mp_int::in, mp_int::out) is det.
mp_neg(A, C) :-
    mp_neg(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.mp_neg: could not negate value"))
    ).

:- pred mp_neg(mp_int::in, mp_result_type::out, mp_int::out)is det.
:- pragma foreign_proc("C",
                      mp_neg(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_neg(A, C);
").

:- pred mp_abs(mp_int::in, mp_int::out) is det.
mp_abs(A, C) :-
    mp_abs(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error(
            "mp_int.mp_abs: could not compute absolute value"))
    ).

:- pred mp_abs(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_abs(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_abs(A, C);
").

abs(A) = Res :- mp_abs(A, Res).

:- pred mp_mul(mp_int::in, mp_int::in, mp_int::out) is det.
mp_mul(A, B, C) :-
    mp_mul(A, B, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.mp_mul: could not multiply"))
    ).

:- pred mp_mul(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_mul(A, B, C);
").

multiply_by_2(A, C) :-
    mp_mul_2(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.multiply_by_2: could not double value"))
    ).

:- pred mp_mul_2(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_mul_2(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(B);
  if (Result == MP_OKAY)
    Result   = mp_mul_2(A, B);
").

divide_by_2(A, C) :-
    mp_div_2(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.divide_by_2: could not halve value"))
    ).

:- pred mp_div_2(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_div_2(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(B);
  if (Result == MP_OKAY)
    Result   = mp_div_2(A, B);
").

divide_with_rem(A, B, Quot, Rem) :-
    ( is_zero(B) ->
        throw(domain_error("mp_int.quot_with_rem: division by zero"))
    ;
        mp_quot_rem(A, B, Result, Quot0, Rem0),
        (
          Result = mp_result_okay,
          Quot = Quot0,
          Rem = Rem0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(domain_error(
            "mp_int.quot_with_rem: could not compute quotient and remainder"))
        )
    ).

:- pred mp_quot_rem(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out,
                   mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_quot_rem(A::in, B::in, Result::out,
                                  Quot::out, Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Quot = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Rem = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result = mp_init(Quot);
  if (Result == MP_OKAY) {
    Result = mp_init(Rem);
    if (Result == MP_OKAY) {
      Result = mp_div(A, B, Quot, Rem);
    }
  }
").

rem(A, B) = Res :-
    ( is_zero(B) ->
        throw(domain_error("mp_int.rem: division by zero"))
    ;
        mp_rem(A, B, Result, Rem0),
        (
          Result = mp_result_okay,
          Res = Rem0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(domain_error(
                "mp_int.rem: could not compute remainder"))
        )
    ).

:- pred mp_rem(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_rem(A::in, B::in, Result::out, Rem::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Rem        = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(Rem);
  if (Result == MP_OKAY)
    Result   = mp_div(A, B, NULL, Rem);
").

:- func quotient(mp_int, mp_int) = mp_int.
quotient(A, B) = Res :-
    ( is_zero(B) ->
        throw(domain_error("mp_int.quotient: division by zero"))
    ;
        mp_quot(A, B, Result, Quot0),
        (
          Result = mp_result_okay,
          Res = Quot0
        ;
          Result = mp_result_out_of_mem,
          error("could not initialize mp_int")
        ;
          Result = mp_result_invalid_input,
          throw(domain_error(
                "mp_int.quotient: could not compute quotient"))
        )
    ).

:- pred mp_quot(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_quot(A::in, B::in, Result::out, Quot::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Quot       = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(Quot);
  if (Result == MP_OKAY)
    Result   = mp_div(A, B, Quot, NULL);
").

:- pred mp_square(mp_int::in, mp_int::out) is det.
mp_square(A, C) :-
    mp_square(A, Result, C0),
    (
      Result = mp_result_okay,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.mp_square: could not square"))
    ).

:- pred mp_square(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_square(A::in, Result::out, A_SQ::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  A_SQ       = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(A_SQ);
  if (Result == MP_OKAY)
    Result   = mp_sqr(A, A_SQ);
").

%---------------------------------------------------------------------------%
% conversion predicates
%---------------------------------------------------------------------------%

:- func min_int = mp_int.
min_int = mp_int(int.min_int).

:- func max_int = mp_int.
max_int = mp_int(int.max_int).

to_int(A, N) :-
    ( ( A >= min_int, A =< max_int) ->
        mp_to_long(A, N)
    ;
        fail
    ).

:- pred mp_to_long(mp_int::in, int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_to_long(A::in, N::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  N = mp_get_long_long(A);
").

det_to_int(A) = Res :-
    ( to_int(A, Res0) ->
        Res0 = Res
    ;
        throw(domain_error("mp_int.det_to_int: not in int range"))
    ).

to_base_string(A, Radix) = S :-
    mp_to_string(A, Radix, Result, S0),
    ( Result = mp_result_okay ->
        S = S0
    ;
        error("could not convert mp_int to string")
    ).

:- pred mp_to_string(mp_int::in, int::in, mp_result_type::out, string::out)
    is det.
:- pragma foreign_proc("C",
                      mp_to_string(A::in, Radix::in, Result::out, S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int length;
  Result     = mp_radix_size(A, Radix, &length);
  if (Result == MP_OKAY)
    {
      MR_allocate_aligned_string_msg(S, length, MR_ALLOC_ID);
      Result = mp_toradix(A, S, Radix);
    }
").

to_string(A) = to_base_string(A, 10).

from_base_string(S, Radix, A) :-
    mp_from_string(S, Radix, Result, A0),
    (
      Result = mp_result_okay,
      A = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      fail
    ).

:- pred mp_from_string(string::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_from_string(S::in, Radix::in, Result::out, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  A          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result = mp_init(A);
  if (Result == MP_OKAY)
    Result = mp_read_radix(A, S, Radix);
").

from_string(S, Res) :- from_base_string(S, 10, Res).

det_from_string(S) = Res :-
    ( from_base_string(S, 10, Res0) ->
        Res = Res0
    ;
        error("could not create mp_int from string")
    ).

det_from_base_string(S, Base) = Res :-
    ( from_base_string(S, Base, Res0) ->
        Res = Res0
    ;
        error("could not create mp_int from string")
    ).

mp_int(N) = Res :-
    ( N < 0 ->
        ( N = min_int ->
            % Avoid `-min_int' as it overflows.
            mp_init(-(min_int + 1), M),
            Res = -(M + one)
        ;
            mp_init(-N, M),
            Res = -M
        )
    ;
        mp_init(N, Res)
    ).

%---------------------------------------------------------------------------%
% bit shifting
%---------------------------------------------------------------------------%

A << N = Res :-
    mp_shift_left(A, N, Result, A0),
    (
      Result = mp_result_okay,
      Res = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.shift_left: could not shift"))
    ).

:- pred mp_shift_left(mp_int::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_shift_left(A::in, N::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init_copy(B, A);
  if (Result == MP_OKAY)
    Result   = mp_lshd(B, N);
").

A >> N = Res :-
    mp_shift_right(A, N, Result, A0),
    (
      Result = mp_result_okay,
      Res = A0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.shift_right: could not shift"))
    ).

:- pred mp_shift_right(mp_int::in, int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_shift_right(A::in, N::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init_copy(B, A);
  if (Result == MP_OKAY)
    mp_rshd(B, N);
").

%---------------------------------------------------------------------------%
% test predicates
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      is_zero(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_iszero(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_even(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_iseven(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_odd(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_isodd(A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_negative(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mp_isneg(A) ? MR_TRUE : MR_FALSE;
").

%---------------------------------------------------------------------------%
% comparison predicates
%---------------------------------------------------------------------------%

:- pred mp_cmp(comparison_result::uo, mp_int::in, mp_int::in) is det.

:- pragma foreign_proc("C",
                      mp_cmp(C::uo, A::in, B::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int result;
  result = mp_cmp(A, B);
  if (result == MP_LT)
    C = MR_COMPARE_LESS;
  else
    {
      if (result == MP_GT)
        C = MR_COMPARE_GREATER;
      else
        C = MR_COMPARE_EQUAL;
    }
").

A > B :- mp_cmp((>), A, B).

A < B :- mp_cmp((<), A, B).

A >= B :-
    mp_cmp(C, A, B),
    ( C = (>); C = (=)).

A =< B :-
    mp_cmp(C, A, B),
    ( C = (<); C = (=)).

equal(A, B) :- mp_cmp((=), A, B).

%---------------------------------------------------------------------------%

A + B = C       :- mp_add(A, B, C).
A - B = C       :- mp_sub(A, B, C).
-A = C          :- mp_neg(A, C).
A * B = C       :- mp_mul(A, B, C).
A // B          = quotient(A, B).
square(X) = Res :- mp_square(X, Res).

%---------------------------------------------------------------------------%
% higher level functions
%---------------------------------------------------------------------------%

pow(A, N) = Res :-
    ( is_zero(N) ->
        Res = one
    ; is_odd(N) ->
        Res = A * pow(A, N - one)
    ;
        divide_by_2(N, N0),
        SQ = pow(A, N0),
        mp_square(SQ, Res)
    ).

%---------------------------------------------------------------------------%
% number theoretic functions
%---------------------------------------------------------------------------%

gcd(A, B) = Res :-
    mp_gcd(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.gcd: could not compute gcd"))
    ).

:- pred mp_gcd(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_gcd(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_gcd(A, B, C);
").

lcm(A, B) = Res :-
    mp_lcm(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.lcm: could not compute lcm"))
    ).

:- pred mp_lcm(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_lcm(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_lcm(A, B, C);
").

jacobi(A, P) = Res :-
    mp_jacobi(A, P, Result, C0),
    ( Result = mp_result_okay  ->
        Res = C0
    ;
        throw(domain_error(
              "mp_int.jacobi: could not compute Jacobi symbol of mp_int"))
    ).

:- pred mp_jacobi(mp_int::in, mp_int::in, mp_result_type::out, int::out) is det.
:- pragma foreign_proc("C",
                      mp_jacobi(A::in, P::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int res;
  Result = mp_jacobi(A, P, &res);
  C      = res;
").

invmod(A, B) = Res :-
    mp_invmod(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      Res = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error(
            "mp_int.invmod: could not compute modular inverse"))
    ).

:- pred mp_invmod(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out)
    is det.
:- pragma foreign_proc("C",
                      mp_invmod(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_invmod(A, B, C);
").

exptmod(A, B, C) = Res :-
    mp_exptmod(A, B, C, Result, D0),
    (
      Result = mp_result_okay ,
      Res = D0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error(
            "mp_int.exptmod: could not compute modular exponentiation"))
    ).

:- pred mp_exptmod(mp_int::in, mp_int::in, mp_int::in, mp_result_type::out,
                  mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_exptmod(A::in, B::in, C::in, Result::out, D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(D);
  if (Result == MP_OKAY)
    Result   = mp_exptmod(A, B, C, D);
").

    % Default number of rounds for Miller-Rabin primality test.
    %
:- func miller_rabin_rounds = int.
miller_rabin_rounds = 40.

is_prime(A) :- is_prime(A, miller_rabin_rounds).

is_prime(A, Rounds) :-
    mp_is_prime(A, Rounds, Result, PResult),
    ( Result = mp_result_okay ->
        PResult = 1
    ;
        error("could not conduct Miller-Rabin primality test on mp_int")
    ).

:- pred mp_is_prime(mp_int::in, int::in, mp_result_type::out, int::out)
    is semidet.
:- pragma foreign_proc("C",
                      mp_is_prime(A::in, Rounds::in, Result::out, Value::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int result;
  Result = mp_prime_is_prime(A, Rounds, &result);
  Value    = result;
").

sqrt(A, Res) :-
    ( is_negative(A) ->
        fail
    ;
        mp_sqrt(A, Result, C0),
        ( Result = mp_result_okay  ->
            Res = C0
        ;
            error("could not initialize mp_int")
        )
    ).

:- pred mp_sqrt(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_sqrt(A::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_sqrt(A, C);
").

det_sqrt(A) = Res :-
    ( sqrt(A, Res0) ->
        Res = Res0
    ;
        throw(domain_error("mp_int.det_sqrt: argument negative"))
    ).

%---------------------------------------------------------------------------%
% bitwise operations
%---------------------------------------------------------------------------%

A /\ B = C :-
    mp_and(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int./\\: could not compute bitwise AND"))
    ).

:- pred mp_and(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_and(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_and(A, B, C);
").

A \/ B = C :-
    mp_or(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.\\/: could not compute bitwise OR"))
    ).

:- pred mp_or(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_or(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_or(A, B, C);
").

A `xor` B = C :-
    mp_xor(A, B, Result, C0),
    (
      Result = mp_result_okay ,
      C = C0
    ;
      Result = mp_result_out_of_mem,
      error("could not initialize mp_int")
    ;
      Result = mp_result_invalid_input,
      throw(domain_error("mp_int.xor: could not compute bitwise XOR"))
    ).

:- pred mp_xor(mp_int::in, mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_xor(A::in, B::in, Result::out, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result     = mp_init(C);
  if (Result == MP_OKAY)
    Result   = mp_xor(A, B, C);
").

\ X = Y :-
    mp_compl(X, Result, Y0),
    ( Result = mp_result_okay ->
        Y = Y0
    ;
        error("could not initialize mp_int")
    ).

:- pred mp_compl(mp_int::in, mp_result_type::out, mp_int::out) is det.
:- pragma foreign_proc("C",
                      mp_compl(A::in, Result::out, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int i;
  mp_digit tmpVal;
  B          = MR_GC_NEW_ATTRIB(mp_int, MR_ALLOC_ID);
  Result = mp_init_copy(B, A);
  if (Result == MP_OKAY)
    for(i = 0; i < USED(A); i++)
      {
        tmpVal = B->dp[i];
        B->dp[i] = (~tmpVal & MP_MASK);
      }
").

%---------------------------------------------------------------------------%
% often used constants
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    negative_one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &MP_INT_constant_negative_one;
"
).

:- pragma foreign_proc("C",
    zero = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &MP_INT_constant_zero;
"
).

:- pragma foreign_proc("C",
    one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &MP_INT_constant_one;
"
).

:- pragma foreign_proc("C",
    two = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &MP_INT_constant_two;
"
).

:- pragma foreign_proc("C",
    ten = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &MP_INT_constant_ten;
"
).

:- end_module mp_int.
