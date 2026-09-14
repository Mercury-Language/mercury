%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The first typo below used to generate this confusing error message:
%
% du_type_with_colon.m:010: In the first argument of function symbol `:' of
% du_type_with_colon.m:010:   the type `foo'/0:
% du_type_with_colon.m:010:   error: the type `foo_a'/0 is undefined.
% du_type_with_colon.m:010:   (Did you mean `foo'?)
% du_type_with_colon.m:011: In the second argument of function symbol `:' of
% du_type_with_colon.m:011:   the type `foo'/0:
% du_type_with_colon.m:011:   error: the type `foo_b'/0 is undefined.
% du_type_with_colon.m:011:   (Did you mean `foo'?)
%

:- module du_type_with_colon.

:- interface.

:- type foo
    --->    foo_a
    :       foo_b.      % This colon should be a SEMIcolon.

:- type bar
    --->    bar_a
    :       bar_b       % This colon should be a SEMIcolon.
    :       bar_c.      % This colon should be a SEMIcolon.

% Export something to avoid a warning we don't want to test.
:- type dummy
    --->    dummy.
