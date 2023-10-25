%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .err_exp{,2,3} files are for C, Java and C# respectively.
%---------------------------------------------------------------------------%

:- module exist_foreign_error.

:- interface.

    % Get the pseudo-typeinfo at the given index from the argument types.
    %
:- some [T] func get_pti_from_arg_types(string, int) = T.

:- implementation.

:- pragma foreign_proc("C",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [promise_pure],
"
    ArgTypeInfo = ArgTypes[Index];
").

:- pragma foreign_proc("Java",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [will_not_call_mercury, promise_pure],
"
    ArgTypeInfo = ArgTypes[Index];
").

:- pragma foreign_proc("C#",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [promise_pure],
"
    ArgTypeInfo = ArgTypes[Index];
").
