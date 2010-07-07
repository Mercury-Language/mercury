// This source file is hereby placed in the public domain.

package my_package;

import jmercury.list;
import jmercury.mercury_lib;
import static java.lang.System.out;

public class JavaMain {
    public static void java_main() {
        out.println("In java_main().");

        /*
        ** call the Java method foo_test(), which is the interface
        ** to the Mercury predicate foo/1 in mode
        **      :- mode foo(in) is semidet.
        */
        out.print("foo_test(42) returns ");
        out.println(mercury_lib.foo_test(42) ? "TRUE" : "FALSE");
        out.print("foo_test(43) returns ");
        out.println(mercury_lib.foo_test(43) ? "TRUE" : "FALSE");

        /*
        ** call the Java method one_foo(), which is the interface
        ** to the Mercury predicate foo/1 in mode
        **      :- mode foo(out) is cc_multi.
        */
        int value = mercury_lib.one_foo();
        out.println("one_foo() gives value = " + value);

        /*
        ** call the Java method foo_list(), which is the interface
        ** to the Mercury predicate foo/1 in mode
        **      :- mode foo(out) is multi.
        */
        list.List_1<Integer> lst = mercury_lib.foo_list();
        out.print("foo_list() = ");
        print_list(lst);

        /*
        ** call the Java methods bar(), bar_test(), and bar_inverse(),
        ** which are the interfaces to the Mercury function bar/1
        ** in the modes
        **      :- mode bar(in) = out is det.
        **      :- mode bar(out) = in is det.
        **      :- mode bar(in) = in is det.
        ** respectively.
        */
        out.println("bar(100) = " + mercury_lib.bar(100));
        out.print("bar_test(100, 101) returns ");
        out.println(mercury_lib.bar_test(100, 101) ? "TRUE" : "FALSE");
        out.print("bar_test(100, 200) returns ");
        out.println(mercury_lib.bar_test(100, 200) ? "TRUE" : "FALSE");
        value = mercury_lib.bar_inverse(101);
        out.println("bar_inverse(101) gives value = " + value);
        value = mercury_lib.bar_inverse(200);
        out.println("bar_inverse(200) gives value = " + value);

        jmercury.runtime.Ref<Integer> ref = new jmercury.runtime.Ref<Integer>();
        if (mercury_lib.baz(1, ref)) {
            out.println("baz(1, ref) returns TRUE with value = " + ref.val);
        } else {
            out.println("baz(100, ref) returns FALSE");
        }
        if (mercury_lib.baz(100, ref)) {
            out.println("baz(100, ref) returns TRUE with value = " + ref.val);
        } else {
            out.println("baz(100, ref) returns FALSE");
        }

        out.println("Returning from java_main()...");
    }

    static void print_list(list.List_1<Integer> lst) {
        if (list.is_empty(lst)) {
            out.println("[]");
        } else {
            out.print("[");
            out.print(list.det_head(lst));
            lst = list.det_tail(lst);
            while (!list.is_empty(lst)) {
                out.print(", " + list.det_head(lst));
                lst = list.det_tail(lst);
            }
            out.println("]");
        }
    }
}
