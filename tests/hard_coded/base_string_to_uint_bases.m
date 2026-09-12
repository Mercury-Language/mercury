%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check the boundary between the fast and checked paths in
% string.base_string_to_uint/3, for every base from 2 to 36.
%
% For each base, we compute:
%
% - SafeDigits, the largest number of digits D such that Base^D - 1 =<
%   max_uint.
%
% - The largest string the fast path can accept, namely SafeDigits copies
%   of the digit Base - 1. Converting it must yield Base^SafeDigits - 1.
%
% - The same string with one more digit, which must fail.
%
% - max_uint itself, which for most bases takes the checked path, and
%   max_uint + 1, which must fail.
%
% Since this test checks the results of the conversions rather than printing
% them, its expected output does not depend on the word size.
%
%---------------------------------------------------------------------------%

:- module base_string_to_uint_bases.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    int.fold_up(test_base, 2, 36, !IO).

:- pred test_base(int::in, io::di, io::uo) is det.

test_base(Base, !IO) :-
    SafeDigits = safe_digits(Base),
    MaxDigit = det_base_int_to_digit(Base, Base - 1),

    % The largest value the fast path can be given, and one digit more.
    LargestFastStr = string.duplicate_char(MaxDigit, SafeDigits),
    TooManyDigitsStr = string.duplicate_char(MaxDigit, SafeDigits + 1),
    LargestFast = integer.pow(integer(Base), integer(SafeDigits))
        - integer.one,

    % max_uint, and the value just outside the range.
    MaxUInt = integer.from_uint(max_uint),
    MaxUIntStr = integer.to_base_string(MaxUInt, Base),
    AboveMaxUIntStr = integer.to_base_string(MaxUInt + integer.one, Base),

    Checks = [
        % Description, string, expected result.
        succeeds("largest fast path value",
            LargestFastStr, LargestFast),
        fails("one digit more than the fast path allows",
            TooManyDigitsStr),
        succeeds("max_uint", MaxUIntStr, MaxUInt),
        fails("max_uint + 1", AboveMaxUIntStr)
    ],
    list.foldl(run_check(Base), Checks, !IO).

%---------------------------------------------------------------------------%

:- type check
    --->    succeeds(
                s_desc      :: string,
                s_string    :: string,
                s_expected  :: integer
            )
    ;       fails(
                f_desc      :: string,
                f_string    :: string
            ).

:- pred run_check(int::in, check::in, io::di, io::uo) is det.

run_check(Base, Check, !IO) :-
    (
        Check = succeeds(Desc, Str, Expected),
        ( if base_string_to_uint(Base, Str, UInt) then
            ( if integer.from_uint(UInt) = Expected then
                report_ok(Base, Desc, !IO)
            else
                report_bad(Base, Desc, Str,
                    "expected " ++ integer.to_string(Expected) ++
                    ", got " ++ integer.to_string(integer.from_uint(UInt)),
                    !IO)
            )
        else
            report_bad(Base, Desc, Str, "conversion failed", !IO)
        )
    ;
        Check = fails(Desc, Str),
        ( if base_string_to_uint(Base, Str, UInt) then
            report_bad(Base, Desc, Str,
                "conversion succeeded, yielding " ++
                integer.to_string(integer.from_uint(UInt)), !IO)
        else
            report_ok(Base, Desc, !IO)
        )
    ).

:- pred report_ok(int::in, string::in, io::di, io::uo) is det.

report_ok(Base, Desc, !IO) :-
    io.format("base %d: %s: ok\n", [i(Base), s(Desc)], !IO).

:- pred report_bad(int::in, string::in, string::in, string::in,
    io::di, io::uo) is det.

report_bad(Base, Desc, Str, Problem, !IO) :-
    io.format("base %d: %s: FAILED: \"%s\": %s\n",
        [i(Base), s(Desc), s(Str), s(Problem)], !IO).

%---------------------------------------------------------------------------%

    % safe_digits(Base) = SafeDigits:
    %
    % SafeDigits is the largest number of digits D such that
    % Base^D - 1 =< max_uint, i.e. the largest number of base Base digits
    % that a string can have while being guaranteed to denote a value
    % that fits in a uint.
    %
    % We compute this rather than reading it out of string.m, so that this
    % test cannot inherit an error from the tables there.
    %
:- func safe_digits(int) = int.

safe_digits(Base) = SafeDigits :-
    safe_digits_loop(integer(Base), integer.from_uint(max_uint),
        integer.one, 0, SafeDigits).

:- pred safe_digits_loop(integer::in, integer::in, integer::in, int::in,
    int::out) is det.

safe_digits_loop(Base, MaxUInt, Power0, Digits0, Digits) :-
    % Power0 is Base^Digits0. Add another digit if the largest value with
    % Digits0 + 1 digits, i.e. Base^(Digits0 + 1) - 1, still fits.
    Power1 = Power0 * Base,
    ( if Power1 - integer.one =< MaxUInt then
        safe_digits_loop(Base, MaxUInt, Power1, Digits0 + 1, Digits)
    else
        Digits = Digits0
    ).

%---------------------------------------------------------------------------%
:- end_module base_string_to_uint_bases.
%---------------------------------------------------------------------------%
