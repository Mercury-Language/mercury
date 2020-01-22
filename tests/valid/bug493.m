%---------------------------------------------------------------------------%
% vim: ts=2 sw=2 ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #493.
%
% The bug was a compiler abort in the MLDS code generator when processing
% the binding of Range in the parse predicate. The sequence of events
% leading to this abort was as follows.
%
% 1 The compiler inlines dash in parse_range.
% 2 The compiler inlines parse_range (with dash inlined) in parse.
% 3 The mark_static_terms pass marks both of the construction unifications
%   involved in Range = range(unicode(0x2d)) as being done statically.
% 4 The mark_static_terms pass marks the construction unification
%   Res = [Range] as also being done statically.
% 5 The pre-code-generation invocation of simplification runs the common
%   structure optimization on the body of parse, and replaces the code
%   that constructs Range with code that assigns the unnamed internal
%   variable representing the first element of Us0 to Range, exploiting
%   the coincidence that both are bound to unicode(0x2d).
% 6 When the MLDS code generator processes that assignment to Range,
%   it does not mark Range is being statically constructed, because
%   it isn't.
% 7 When the MLDS code generator processes the construction unification
%   that implements Res = [Range], it does so using the code path that
%   constructs Res statically, since mark_static_terms said that Res
%   should be constructed statically. However, this requires all the variables
%   on the right hand side of the construction unification to be in the
%   database of statically constructed terms, which (due to steps 5 and 6
%   above) it is not. This violation of this code path's precondition
%   causes the abort.
%
% The actual culprit is step 5. By replacing a "construct statically"
% construction unification of Range with an assignment, it broke all later
% "construct statically" construction unifications in which Range occurred
% on the right hand side.
%
%---------------------------------------------------------------------------%

:- module bug493.
:- interface.

:- import_module list.

:- type range
    --->    range(unicode).

:- type unicode
    --->    unicode(int).

:- pred parse(list(unicode)::in, list(unicode)::out, list(range)::out)
  is semidet.

:- implementation.

parse(Us0, Us, Res) :-
    parse_range(Us0, Us, Range),
    Res = [Range].

:- pred parse_range(list(unicode)::in, list(unicode)::out, range::out)
  is semidet.

parse_range(Us0, Us, Range) :-
    Us0 = [unicode(0x2d) | Us],
    Range = dash.

:- func dash = range.
:- pragma inline(dash/0).

dash = range(unicode(0x2d)).
