%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module bug557.

:- interface.
:- import_module io.

:- type record
    --->    record(
                % This type is NOT ambiguous in the interface section
                % (because the only type named "ambiguous_type" available
                % in the interface is the one defined in this interface),
                % but it *would* be ambiguous in the implementation section
                % (because of the import of bug557_helper.ambiguous_type
                % there).
                %
                % Mantis bug #557 was caused by the fact that
                % module_qualify_type_ctor_checked_defn always module qualified
                % the checked definitions of types (such as record) as if
                % they were in the implementation section. This caused this
                % reference to ambiguous_type to set the "we found a type
                % that could not be uniquely qualified" flag. However,
                % module_qualify_type_ctor_checked_defn threw away the
                % error message generated for this, because it was about
                % to module qualify the original type definitions from which
                % the checked type definition was constructed, and it did not
                % want duplicate error messages. However, the qualification
                % of the original type definition (this one) did not find
                % any ambiguity, and thus did not generate an error message.
                % The flag caused compilation to stop without further message
                % when it reached the frontend_pass predicate.
                ambiguous_type  :: ambiguous_type
            ).

:- type ambiguous_type
    --->    a
    ;       b.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bug557_helper.

main(!IO) :-
    io.write_string("Successful compilation.\n", !IO).
