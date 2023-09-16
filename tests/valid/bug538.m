%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Author: Dirk Ziegemeyer <dirk@ziegemeyer.de>
%---------------------------------------------------------------------------%

:- module bug538.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % Html content categories modelled as subtypes (simplified)
    % 1. All html elements belong to the base type flow_content
    % 2. Phrasing_content is a subset of flow_content
    % 3. Nesting of <a> elements (hyperlinks) is invalid. Neither direct nor
    %    indirect nesting is allowed.
    %
:- type flow_content
    --->    p(list(phrasing_content))
    ;       div(list(flow_content))
    ;       a(list(flow_content_without_a))
    ;       text(string).

:- type flow_content_without_a =< flow_content
    --->    p(list(phrasing_content_without_a))
    ;       div(list(flow_content_without_a))
    ;       text(string).

:- type phrasing_content =< flow_content
    --->    a(list(phrasing_content_without_a))
    ;       text(string).

    % 1. Explicitly declared subtype of phrasing_content
    % 2. Implicit subtype of flow_content_without_a, because both share the
    %    same base type (flow_content) and the subtype only omits constructor
    %    definitions
    %
:- type phrasing_content_without_a =< phrasing_content
    --->    text(string).

%---------------------------------------------------------------------------%
