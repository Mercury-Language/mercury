//
// Copyright (C) 2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class list 
 { 

    public static class list_1
     {  public int data_tag;
    
        public static class f_nil_0 extends list_1
	 {   
	    public f_nil_0() 
	     {
                this.data_tag = 0;
	     }
	 
	 }

        public static class f_cons_2 extends list_1
	 {  
	    public java.lang.Object F1;
	    public list_1 F2;

	    public f_cons_2(java.lang.Object item, list_1 list)
	     {
		 this.data_tag = 1;
		 F1 = item;
		 F2 = list;
		 
             }
         }
        
	public list_1()
	 {
	 }

     }

    public static java.lang.Object foldl_4_p_0(
		    mercury.private_builtin.type_info_1 ignore1,
		    mercury.private_builtin.type_info_1 ignore2,
		    java.lang.Object[] methodptrstruct,
		    mercury.list.list_1 list,
		    java.lang.Object nothing)
     {
	mercury.runtime.MethodPtr methodptr = (mercury.runtime.MethodPtr) methodptrstruct[1];
	     int count;
	while(list.data_tag == 1)
	 {  methodptr.call___0_0(new java.lang.Object[] {null, ((mercury.list.list_1.f_cons_2) list).F1, null});
	    list = (list_1) ((mercury.list.list_1.f_cons_2) list).F2;
	 }
	     
       return null;
     }
 }

