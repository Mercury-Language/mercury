%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2007-10-19 and before would not emit an error if you referred to
% a type in an abstract instance declaration in the interface section,
% BUT that type was visible ONLY in the implementation section.
%
%---------------------------------------------------------------------------%

:- module instance_no_type.
:- interface.

:- import_module list.

:- type foo
        --->    foo.

:- typeclass tc(T) where [].
:- typeclass tc2(A, B) where [].

:- instance tc(int).                    % Builtin type, no error.
:- instance tc(foo).                    % Exported type, no error.
:- instance tc(list(T)) <= tc(T).       % Type imported in interface, no error.
:- instance tc(no_such_type).           % Non-exported type, ERROR.

% The compiler already detects the case when an imported type that is
% only visible in the interface is used in an abstract instance decl.

:- instance tc2(no_such_type, no_such_type2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type no_such_type  ---> no_such_type.
:- type no_such_type2 ---> no_such_type2.

:- instance tc(int) where [].
:- instance tc(foo) where [].
:- instance tc(list(T)) <= tc(T) where [].
:- instance tc(no_such_type) where [].

:- instance tc2(no_such_type, no_such_type2) where [].
