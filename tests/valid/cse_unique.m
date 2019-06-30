%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/
% Released by Transnat Games for testing purposes.
%---------------------------------------------------------------------------%
%
% This regression test used to cause this Mercury compiler abort:
% Software Error: check_hlds.cse_detection: predicate
% `check_hlds.cse_detection.detect_cse_in_proc'/4:
% Unexpected: mode check fails when repeated.
% This is Github issue #64.
%
% The cause was that the deconstruction of D in get_directions was pulled
% out of the switch on the first argument even though its inst contained
% unique components. Mode analysis can track uniqueness through the
% original form of the switch, but not through its transformed form,
% which is more complicated and has longer chains of unifications.
%
%---------------------------------------------------------------------------%

:- module cse_unique.

:- interface.

:- type direction
    --->    left
    ;       right.

:- type directions(T)
    --->    directions(on_left::T, on_right::T).

%---------------------------------------------------------------------------%

:- func get_directions(direction, directions(T)) = T.
:- mode get_directions(in, in) = (out) is det.
:- mode get_directions(in, di) = (uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

get_directions(left, D) = D ^ on_left.
get_directions(right, D) = D ^ on_right.

%---------------------------------------------------------------------------%
