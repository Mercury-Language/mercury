%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% $ mmc -e superclass_bug.m
% Uncaught exception:
% Software Error: polymorphism.m: constraint not in constraint list

:- module superclass_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

% Types
:- type interface_pointer
    --->    interface_pointer(int).
:- type comobj
    --->    comobj(int).
:- type gc_descriptor
    --->    gc_descriptor(int).
:- type hresult
    --->    hresult(int).

%---------------------------------------------------------------------------%

:- typeclass iunknown(T) where [
    func get_interface_pointer(T) = interface_pointer,
    pred is_null(T::in) is semidet,
    pred release_instance(T::in) is det
].

:- typeclass idispatch(T) <= iunknown(T) where [
    pred fake_method(T::in, T::out) is det
].

:- typeclass ixmldomnode(IT) <= (idispatch(IT)) where [
    some [A, B, C] (func replacechild(T, A, U, B, C, IT, IT) = hresult
        => (ixmldomnode(A), ixmldomnode(B), ixmldomnode(C)))
        <= (ixmldomnode(T), ixmldomnode(U)),
    mode replacechild(in, out, in, out, out, in, out) = out is det
].

:- some [A, B, C] (
    func ixmldomnode_replacechild(T, A, U, B, C, comobj, comobj) = hresult
    => (ixmldomnode(A), ixmldomnode(B), ixmldomnode(C)))
    <= (ixmldomnode(T), ixmldomnode(U)).

:- mode ixmldomnode_replacechild(in, out, in, out, out, in, out) = out is det.

%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    io.write_string("Hello world.\n", !IO).

%---------------------------------------------------------------------------%

:- instance iunknown(comobj) where [
    get_interface_pointer(comobj(A)) = interface_pointer(A),
    (is_null(comobj(A)) :- A = 1),
    release_instance(comobj(_))

].
:- instance idispatch(comobj) where [
    fake_method(T, T)
].

:- instance ixmldomnode(comobj) where [
    func(replacechild/7) is ixmldomnode_replacechild
].

%---------------------------------------------------------------------------%

ixmldomnode_replacechild(T, A, U, B, C, InTypeVar, OutTypeVar) = HResult :-
    InTypeVarPtr = comobj_get_interface_pointer(InTypeVar),
    TPtr = get_interface_pointer(T),

        % XXX This was this line which caused the problem.
    UPtr = get_interface_pointer(U),

    HResult = ixmldomnode_replacechild_c_code(TPtr, APtr, UPtr, BPtr, CPtr,
        InTypeVarPtr, OutTypeVarPtr),
    A = comobj_create(APtr, gc_descriptor(0)),
    B = comobj_create(BPtr, gc_descriptor(0)),
    C = comobj_create(CPtr, gc_descriptor(0)),
    OutTypeVar = comobj_duplicate(InTypeVar, OutTypeVarPtr).

%---------------------------------------------------------------------------%

:- func ixmldomnode_replacechild_c_code(interface_pointer, interface_pointer,
    interface_pointer, interface_pointer, interface_pointer,
    interface_pointer, interface_pointer) = hresult.
:- mode ixmldomnode_replacechild_c_code(in, out, in, out, out, in, out) = out
    is det.

ixmldomnode_replacechild_c_code(A, A, B, B, A, C, C) = hresult(5).
:- pragma foreign_proc("C",
    ixmldomnode_replacechild_c_code(IntroducedIdlBug_1InPtr::in,
       IntroducedIdlBug_1OutPtr::out, IntroducedIdlBug_2InPtr::in,
       IntroducedIdlBug_2OutPtr::out, OutOldChildPtr::out,
       InTypeVarPtr::in, OutTypeVarPtr::out) = (HResult::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    OutOldChildPtr = (MR_Word) NULL;

    HResult = (MR_Word) NULL;

    IntroducedIdlBug_1OutPtr = IntroducedIdlBug_1InPtr;
    IntroducedIdlBug_2OutPtr = IntroducedIdlBug_2InPtr;
    OutTypeVarPtr = InTypeVarPtr;
").

%---------------------------------------------------------------------------%

:- func comobj_get_interface_pointer(comobj::in) =
    (interface_pointer::out) is det.

comobj_get_interface_pointer(comobj(A)) = interface_pointer(A).

:- func comobj_create(interface_pointer::in, gc_descriptor::in) =
    (comobj::out) is det.

comobj_create(interface_pointer(A), _) = comobj(A).

:- func comobj_duplicate(comobj::in, interface_pointer::in) =
    (comobj::out) is det.

comobj_duplicate(A, _) = A.

%---------------------------------------------------------------------------%
