%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% According to the rules in the language reference manual,
% this example is not currently valid. But this behaviour
% is a bit unexpected for the programmer, so it would be
% nice if the language was changed so that it was valid.

% See <http://www.cs.mu.oz.au/research/mercury/mailing-lists/mercury-reviews/
% mercury-reviews.0312/0189.html> for more details.

% XXX Perhaps this test should not go in tests/valid,
% since it is not yet valid under the current Mercury language rules.

:- module field_detism.

:- interface.

:- type t
    --->    a
    ;       b(x :: int).

:- pred p(t::in, t::out) is det.

:- implementation.

:- import_module int.

p(a, a).
p(T0 @ b(N), T) :-
    T = T0 ^ x := N + 1.
