:- module factt.
:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- pred example(int::in,int::out,int::out) is semidet.

:- pragma fact_table(example/3,"factt_examples").

:- pred show_examples(int::in,io__state::di, io__state::uo) is det.

main -->
        show_examples(1).

show_examples(Num) -->

        ({example(Num,Result1,Result2)} ->
            print(Num),print(" "),
            print(Result2),print(" "),
            print(Result1),nl;
            print("Example call failed."),nl
        ),

        {Num1 = Num + 1},

        ({Num1 < 51} ->
            show_examples(Num1);
            print("Finished"),nl
        ).
