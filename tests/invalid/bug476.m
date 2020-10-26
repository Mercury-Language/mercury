%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test. Versions of the compiler before 2019 April 7
% used to get a compiler abort for this module. The reason was that
%
% - We first inserted the declaration of the predicate named "mark"
%   near the bottom of the file into the predicate table.
%
% - Later we tried to insert the declaration of the class method predicate
%   named "mark" near the top of the file into the predicate table.
%   This insertion failed, but it nevertheless returned a valid pred_proc_id
%   to the code handling the addition of the class methods to the HLDS.
%   This was the pred_proc_id resulting from the first, successful insertion.
%
% - The compiler expects every predicate that represents a class method
%   to have at least one universal typeclass constraint on it, this being
%   the constraint implicit in its nature as a typeclass method. The code
%   that checks typeclass constraints aborts if it does not find it.
%   In this case, it did not find it, since we added into the pred_info
%   we constructed it for the second, unsuccessful insertion, but this
%   never made it into the HLDS.
%

:- module bug476.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- typeclass input_stream(T) where [
    pred mark(T::in, int::in, io::di, io::uo) is det
].

%---------------------------------------------------------------------------%

:- type jinput_stream.

:- instance input_stream(jinput_stream).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", jinput_stream, "void *").
:- pragma foreign_type("C#", jinput_stream, "object").
:- pragma foreign_type("Java", jinput_stream, "java.io.InputStream").

%---------------------------------------------------------------------------%

:- instance input_stream(jinput_stream) where [
    pred(mark/4) is input_stream_mark
].

%---------------------------------------------------------------------------%

    % The name of this declaration is INCORRECT.
    % The bug was tickled by the coincidence between the name of this predicate
    % and the class method predicate above.
    %
:- pred mark(jinput_stream::in, int::in, io::di, io::uo) is det.

input_stream_mark(_, _, _) :-
    error("NYI mark for InputStream").

%---------------------------------------------------------------------------%
:- end_module bug476.
%---------------------------------------------------------------------------%
