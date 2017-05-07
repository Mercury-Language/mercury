%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% File: gmp_int.m.
% Main author: Matthias Güdemann.
% Stability: low.
%
% This module implements a binding to GMP, a library providing a very
% efficient implementation of multi precision integers. GMP is licensed
% under LGPLv3; it is available from https://gmplib.org/
%
% To use the provided binding, see README or
% https://github.com/mgudemann/mercury_gmp_int for details.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module gmp_int.

:- interface.

:- type gmp_int.

    % Convert an int to an gmp_int.
    %
:- func gmp_int(int) = gmp_int.

    % Addition.
    %
:- func gmp_int + gmp_int = gmp_int.

    % Subtraction.
    %
:- func gmp_int - gmp_int = gmp_int.

    % Multiplication.
    %
:- func gmp_int * gmp_int = gmp_int.

    % Truncating integer division.
    %
:- func gmp_int // gmp_int = gmp_int.

    % Remainder.
    % X rem Y = X - (X // Y) * Y
    %
:- func 'rem'(gmp_int, gmp_int) = gmp_int.

    % Quotient and Remainder.
    % divide_with_rem(A, B, Quot, Rem) if A // B = Quot, A rem B = Rem
    %
:- pred divide_with_rem(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out)
    is det.

    % Flooring integer division.
    %
:- func 'div'(gmp_int, gmp_int) = gmp_int.

    % Modulo.
    % X mod Y = X - (X div Y) * Y
    %
:- func 'mod'(gmp_int, gmp_int) = gmp_int.

    % Divisor and Modulo.
    % divide_with_mod(A, B, Div, Mod) if A div B = Div, A mod B = Mod
    %
:- pred divide_with_mod(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out)
    is det.

    % Shift Left.
    %
:- func <<(gmp_int, int) = gmp_int.

    % Shift Right.
    %
    % This is implemented as truncating division with 2^N.
    %
:- func >>(gmp_int, int) = gmp_int.

    % Absolute value.
    %
:- func abs(gmp_int) = gmp_int.

    % Unary Negation
    %
:- func -(gmp_int) = gmp_int.

    % Equal.
    %
:- pred equal(gmp_int::in, gmp_int::in) is semidet.

    % Less than.
    %
:- pred '<'(gmp_int::in, gmp_int::in) is semidet.

    % Greater than.
    %
:- pred '>'(gmp_int::in, gmp_int::in) is semidet.

    % Less or equal.
    %
:- pred '=<'(gmp_int::in, gmp_int::in) is semidet.

    % Greater or equal.
    %
:- pred '>='(gmp_int::in, gmp_int::in) is semidet.

    % is_even(X) if X is even.
    %
:- pred is_even(gmp_int::in) is semidet.

    % is_odd(X) if X is odd.
    %
:- pred is_odd(gmp_int::in) is semidet.

    % is_negative(X) if X is negative.
    %
:- pred is_negative(gmp_int::in) is semidet.

    % is_zero(X) if X is zero.
    %
:- pred is_zero(gmp_int::in) is semidet.

    % is_positive(X) if X is positive
    %
:- pred is_positive(gmp_int::in) is semidet.

    % is_perfect_power(X) if there exist A, B > 1 with X = A^B
    %
:- pred is_perfect_power(gmp_int::in) is semidet.

    % is_perfect_square(X) if there exists A with A * A = X
    %
:- pred is_perfect_square(gmp_int::in) is semidet.

    % is_probab_prime(X) if X is prime with error probability p < 2^(-50)
    %
:- pred is_probab_prime(gmp_int::in) is semidet.

    % Greatest common divisor.
    %
:- func gcd(gmp_int, gmp_int) = gmp_int.

    % gcdext(A, B, S, T, G) is true if G = A*S + B*T
    %
    % Extended greatest common divisor.
    %
:- pred gcdext(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out,
    gmp_int::out) is det.

    % Least common multiple.
    %
:- func lcm(gmp_int, gmp_int) = gmp_int.

    % Square root of gmp_int.
    %
    % sqrt(X, Sqrt) is true if Sqrt is the positive square root of X.
    % Fails if X is negative.
    %
:- pred sqrt(gmp_int::in, gmp_int::out) is semidet.

    % As above, but throws error in case of negative value.
    %
:- func det_sqrt(gmp_int) = gmp_int.

    % nth root of gmp_int.
    %
    % nthroot(X, N, Root) is true if Root is the positive N-th root of X.
    % Fails if X is negative.
    %
:- pred nthroot(gmp_int::in, int::in, gmp_int::out) is semidet.

    % As above, but throws error in case of negative value.
    %
:- func det_nthroot(gmp_int, int) = gmp_int.

    % Computes Legendre symbol (see jacobi), fails if P is not prime.
    %
:- pred legendre(gmp_int::in, gmp_int::in, int::out) is semidet.

    % jacobi(A, N) = C:
    %
    % Computes Jacobi symbol J, using Legendre symbol L
    %
    % C = J(A, N) = L(A, P_1)^(i_1) * ... * L(A, P_k)^(i_k) where
    %
    % A = P_1^(i_1) * ... * P_k^(i_k) with P_j is prime, and
    %
    %            / 1, if a is a quadratic residue modulo p, and A \= 0 (mod P)
    % L(A, P) = | -1, if a is a quadratic non-residue modulo P
    %            \ 0, if a is a multiple of P
    %
:- func jacobi(gmp_int, gmp_int) = int.

    % invmod(A, B) = C:
    %
    % Modular inverse C = A^(-1) mod B
    %
:- func invmod(gmp_int, gmp_int) = gmp_int.

    % Exponentiation.
    % Throws exception `math.domain_error` if Y is negative.
    %
:- func pow(gmp_int, gmp_int) = gmp_int.

    % powm(A, B, C) = D:
    %
    % Modular exponentiation D = A^B mod C.
    %
:- func powm(gmp_int, gmp_int, gmp_int) = gmp_int.

    % Bitwise or.
    %
:- func gmp_int \/ gmp_int = gmp_int.

    % Bitwise and.
    %
:- func gmp_int /\ gmp_int = gmp_int.

    % Bitwise xor.
    %
:- func 'xor'(gmp_int, gmp_int) = gmp_int.

    % Bitwise complement.
    %
:- func \ gmp_int = gmp_int.

    % Convert gmp_int to int.
    % Fails if value does not fit into a signed long.
    %
:- pred to_int(gmp_int::in, int::out) is semidet.

    % As above, but throws an exception if value does not fit into a signed
    % long.
    %
:- func det_to_int(gmp_int) = int.

    % Convert string in base 10 to gmp_int. Fails if unsuccessful.
    %
:- pred from_string(string::in, gmp_int::out) is semidet.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_string(string) = gmp_int.

    % from_base_string(String, Base) = GMP_Int:
    %
    % Convert string in given base to gmp_int.
    %
    % Base must be 10 or 16, fails if unsuccessful.
    %
:- pred from_base_string(string::in, int::in, gmp_int::out) is semidet.

    % As above, throws exception instead of failing if unsuccessful.
    %
:- func det_from_base_string(string, int) = gmp_int.

    % Convert gmp_int to a string in base 10.
    %
:- func to_string(gmp_int) = string.

    % to_base_string(Gmp_Int, Base) = String:
    %
    % Convert gmp_int to a string in given base.
    %
    % Base must be between 2 and 62, inclusive; if it is not, the predicate
    % will throw an exception.
    %
:- func to_base_string(gmp_int, int) = string.

    % Constant -1.
    %
:- func negative_one = gmp_int.

    % Constant 0.
    %
:- func zero = gmp_int.

    % Constant 1.
    %
:- func one = gmp_int.

    % Constant 2.
    %
:- func two = gmp_int.

    % Constant 10.
    %
:- func ten = gmp_int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module math.
:- import_module require.

%---------------------------------------------------------------------------%
% foreign declarations
%---------------------------------------------------------------------------%

    % Type declaration for foreign type gmp_int*.
    %
    % Converting a gmp_int to MR_Integer with mpz_get_si requires that
    %   sizeof(MR_Integer) >= sizeof(long int)
    % Converting an MR_Integer to gmp_int with mpz_init_set_si requires that
    %   sizeof(long int) >= sizeof(MR_Integer)
    % Therefore we require that the two types have the same width.
    % On some 64-bit platforms, MR_Integer is 64-bits but long is 32-bits.
    % This module will need require changes to work on those platforms.
    %
:- pragma foreign_type("C", gmp_int, "mpz_t *", [can_pass_as_mercury_type])
    where equality is equal, comparison is cmp.
:- pragma foreign_decl("C",
"
#include <gmp.h>

MR_STATIC_ASSERT(gmp_int, sizeof(MR_Integer) == sizeof(long int));
#define MILLER_RABIN_ROUNDS 25
").

%---------------------------------------------------------------------------%
% initialisation code to create static gmp_ints for often used constants
% and to call Boehm-GC for memory alloc/free functions
%---------------------------------------------------------------------------%

:- initialise gmp_initialize/0.
:- impure pred gmp_initialize is det.

:- pragma foreign_decl("C", local,
"
#include <stdlib.h>

static void *
gmp_int_alloc_function(size_t size)
{
  return MR_GC_malloc(size);
}

static void *
gmp_int_realloc_function(void* ptr, size_t size, size_t new_size)
{
  return MR_GC_realloc(ptr, new_size);
}

static void
gmp_int_free_function(void* ptr, size_t size)
{
  GC_free(ptr);
}
").

:- pragma foreign_decl("C",
"
extern mpz_t GMP_INT_constant_negative_one;
extern mpz_t GMP_INT_constant_zero;
extern mpz_t GMP_INT_constant_one;
extern mpz_t GMP_INT_constant_two;
extern mpz_t GMP_INT_constant_ten;
").

:- pragma foreign_code("C",
"
mpz_t GMP_INT_constant_negative_one;
mpz_t GMP_INT_constant_zero;
mpz_t GMP_INT_constant_one;
mpz_t GMP_INT_constant_two;
mpz_t GMP_INT_constant_ten;
").

:- pragma foreign_proc("C",
                      gmp_initialize,
                      [will_not_call_mercury, thread_safe, may_not_duplicate],
"
  mp_set_memory_functions(&gmp_int_alloc_function,
                          &gmp_int_realloc_function,
                          &gmp_int_free_function);
  mpz_init_set_si(GMP_INT_constant_negative_one, -1);
  mpz_init_set_si(GMP_INT_constant_zero, 0);
  mpz_init_set_si(GMP_INT_constant_one, 1);
  mpz_init_set_si(GMP_INT_constant_two, 2);
  mpz_init_set_si(GMP_INT_constant_ten, 10);
").

%---------------------------------------------------------------------------%
% basic arithmetic
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      +(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_add(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      -(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_sub(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      *(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_mul(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      //(A::in, B::in) = (Q::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_tdiv_q(*Q, *A, *B);
").

:- pragma foreign_proc("C",
                      rem(A::in, B::in) = (Q::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_tdiv_r(*Q, *A, *B);
").

:- pragma foreign_proc("C",
                      divide_with_rem(A::in, B::in, Q::out, R::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  R  = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_init(*R);
  mpz_tdiv_qr(*Q, *R, *A, *B);
").

:- pragma foreign_proc("C",
                      div(A::in, B::in) = (D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_fdiv_q(*D, *A, *B);
").

:- pragma foreign_proc("C",
                      mod(A::in, B::in) = (D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_fdiv_r(*D, *A, *B);
").

:- pragma foreign_proc("C",
                      divide_with_mod(A::in, B::in, D::out, M::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  M = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_init(*M);
  mpz_fdiv_qr(*D, *M, *A, *B);
").

:- pragma foreign_proc("C",
                      >>(A::in, N::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_tdiv_q_2exp(*C, *A, N);
").

:- pragma foreign_proc("C",
                      abs(A::in) = (B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*B);
  mpz_abs(*B, *A);
").

:- pragma foreign_proc("C",
                      -(A::in) = (B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*B);
  mpz_neg(*B, *A);
").

:- pragma foreign_proc("C",
                      <<(A::in, N::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_mul_2exp(*C, *A, N);
").

%---------------------------------------------------------------------------%
% conversion predicates
%---------------------------------------------------------------------------%

to_int(A, N) :-
    fits_in_long(A),
    to_long(A, N).

det_to_int(A) = Res :-
    ( to_int(A, Res0) ->
        Res0 = Res
    ;
        throw(math.domain_error("gmp_int.det_to_int: not in int range"))
    ).

:- pred fits_in_long(gmp_int::in) is semidet.
:- pragma foreign_proc("C",
                      fits_in_long(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mpz_fits_slong_p(*A) ? MR_TRUE : MR_FALSE;
").

:- pred to_long(gmp_int::in, int::out) is det.
:- pragma foreign_proc("C",
                      to_long(A::in, N::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  N = mpz_get_si(*A);
").

:- pragma foreign_proc("C",
                      gmp_int(Value::in) = (Gmp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Gmp_Int = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init_set_si(*Gmp_Int, Value);
").

to_string(Value) = to_base_string(Value, 10).

to_base_string(A, R) = S :-
    ( ( R >= 2, R =< 62 ) ->
        S = gmp_to_base_string(A, R)
    ;
        throw(math.domain_error("gmp_int.to_base_string: base must be between 2 and 62"))
    ).

:- func gmp_to_base_string(gmp_int, int) = string.
:- pragma foreign_proc("C",
                      gmp_to_base_string(A::in, R::in) = (S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  S = mpz_get_str(NULL, R, *A);
").

from_string(S, Res) :-
    from_base_string(S, 10, Res).

det_from_string(Value) = det_from_base_string(Value, 10).

from_base_string(Value, Base, Res) :-
    (
      Base = 10
    ;
      Base = 16
    ),
    gmp_from_base_string(Value, Base, Res).

det_from_base_string(Value, Base) = Res :-
    ( ( Base = 10; Base = 16) ->
        gmp_from_base_string(Value, Base, Res)
    ;
        error("could not convert from string, base must be in {10, 16}")
    ).

:- pred gmp_from_base_string(string::in, int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_from_base_string(S::in, Base::in, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  A = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*A);
  if (Base == 10)
    gmp_sscanf(S, \"%Zd\", *A);
  else
    gmp_sscanf(S, \"%Zx\", *A);
").

%---------------------------------------------------------------------------%
% bitwise operators
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      /\(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_and(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      \/(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_ior(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      xor(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_xor(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      \(A::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_com(*C, *A);
").

%---------------------------------------------------------------------------%
% comparison predicates
%---------------------------------------------------------------------------%

equal(A, B) :- cmp((=), A, B).

:- pred cmp(comparison_result::uo, gmp_int::in, gmp_int::in) is det.
:- pragma foreign_proc("C",
                      cmp(Result::uo, A::in, B::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int res;
  res = mpz_cmp(*A, *B);
  if (res < 0)
    Result = MR_COMPARE_LESS;
  else if (res == 0)
    Result = MR_COMPARE_EQUAL;
  else
    Result = MR_COMPARE_GREATER;
").

A < B :- cmp((<), A, B).
A > B :- cmp((>), A, B).
A =< B :- \+ cmp((>), A, B).
A >= B :- \+ cmp((<), A, B).

%---------------------------------------------------------------------------%
% test predicates
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      is_even(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mpz_even_p(*A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_odd(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mpz_odd_p(*A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_negative(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == -1) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_zero(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == 0) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_positive(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == 1) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_perfect_power(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_perfect_power_p(*A) == 0) ? MR_FALSE : MR_TRUE;
").

:- pragma foreign_proc("C",
                      is_perfect_square(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_perfect_square_p(*A) == 0) ? MR_FALSE : MR_TRUE;
").

%---------------------------------------------------------------------------%
% various mathematical and number theoretic functions
%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
                      is_probab_prime(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int result;
  result = mpz_probab_prime_p(*A, MILLER_RABIN_ROUNDS);
  switch(result) {
  case 0:
    SUCCESS_INDICATOR = MR_FALSE;
    break;
  case 1:
  case 2:
    SUCCESS_INDICATOR = MR_TRUE;
    break;
  default:
    SUCCESS_INDICATOR = MR_FALSE;
    break;
  }
").

:- pragma foreign_proc("C",
                      gcd(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_gcd(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      gcdext(A::in, B::in, S::out, T::out, G::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  S = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  T = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  G = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*S);
  mpz_init(*T);
  mpz_init(*G);
  mpz_gcdext(*G, *S, *T, *A, *B);
").

:- pragma foreign_proc("C",
                      lcm(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_lcm(*C, *A, *B);
").

sqrt(A, Root) :-
    ( is_negative(A) ->
        fail
    ;
        gmp_sqrt(A, Root)
    ).

det_sqrt(A) = Res :-
    ( sqrt(A, Root) ->
        Res = Root
    ;
        throw(math.domain_error("gmp_int.det_sqrt: number must be non-negative"))
    ).

:- pred gmp_sqrt(gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_sqrt(A::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_sqrt(*C, *A);
").

nthroot(A, N, Res) :-
    ( is_negative(A) ->
        fail
    ;
        gmp_nthroot(A, N, Res)
    ).

det_nthroot(A, N) = Res :-
    ( nthroot(A, N, Root) ->
        Res = Root
    ;
        throw(math.domain_error("gmp_int.det_sqrt: number must be non-negative"))
    ).

:- pred gmp_nthroot(gmp_int::in, int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_nthroot(A::in, N::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_root(*C, *A, N);
").

legendre(A, B, Res) :-
    is_probab_prime(B),
    Res = jacobi(A, B).

:- pragma foreign_proc("C",
                      jacobi(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = mpz_jacobi(*A, *B);
").

pow(A, N) = Res :-
    ( is_negative(N) ->
        throw(math.domain_error("gmp_int.pow: cannot handle negative exponent"))
    ;
        Res = pow2(A, N)
    ).

:- func pow2(gmp_int, gmp_int) = gmp_int.
pow2(A, N) = Res :-
    ( is_zero(N) ->
        Res = one
    ; is_even(N) ->
        SQ = pow2(A, N // two),
        Res = SQ * SQ
    ;
        Res = A * pow2(A, N - one)
    ).

:- pragma foreign_proc("C",
                      powm(Base::in, Exp::in, Mod::in) = (Res::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Res);
  mpz_powm(*Res, *Base, *Exp, *Mod);
").

:- pragma foreign_proc("C",
                      invmod(Base::in, Mod::in) = (Res::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Res);
  mpz_invert(*Res, *Base, *Mod);
").

:- pragma foreign_proc("C",
    negative_one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &GMP_INT_constant_negative_one;
"
).

:- pragma foreign_proc("C",
    zero = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &GMP_INT_constant_zero;
"
).

:- pragma foreign_proc("C",
    one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &GMP_INT_constant_one;
"
).

:- pragma foreign_proc("C",
    two = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &GMP_INT_constant_two;
"
).

:- pragma foreign_proc("C",
    ten = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &GMP_INT_constant_ten;
"
).

:- end_module gmp_int.
