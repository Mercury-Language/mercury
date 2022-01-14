// vim: ts=4 sw=4 et

// All Mercury generated C# code lives in the mercury namespace.
using mercury;
using System;

class CSharpMain {

    static void Main(string[] args)
    {
        // Initialise the C# version of Mercury's standard library.
        Console.WriteLine("CSharpMain: initialising Mercury standard library");
        library.ML_std_library_init();

        Console.WriteLine("CSharpMain: start main");
        runProgram(args);
        Console.WriteLine("CSharpMain: end main");

        // Once Main returns, any Mercury finalisers will be run.
    }

    private static void runProgram(string[] args)
    {
        // The mercury_lib class contains a static method corresponding to each
        // predicate or function that is foreign exported to C#.

        // Call a Mercury predicate that does some I/O.
        mercury_lib.WriteHello();

        // Call a Mercury function.
        Console.WriteLine("3^3 = " + mercury_lib.Cube(3));

        // Call a semidet Mercury predicate. This also illustrates the
        // use of a foreign exported enumeration in C#.
        Console.WriteLine("IsCitrus(LEMON) is " +
            (mercury_lib.IsCitrus(mercury_lib.LEMON) ? "true" : "false")); 
        Console.WriteLine("IsCitrus(APPLE) is " +
            (mercury_lib.IsCitrus(mercury_lib.APPLE) ? "true" : "false")); 

        // Build up Mercury list of ints in C# 
        // (See the section ``C# data passing conventions'' in the reference
        // manual for details.)
        list.List_1 mList = list.empty_list(); 
        mList = list.cons(1, mList);
        mList = list.cons(2, mList);
        mList = list.cons(3, mList);
       
        // At this point, mList is the Mercury list [3, 2, 1].
        // Pass it as argument to a Mercury predicate.  
        mercury_lib.PrintList(mList);
    }
}
