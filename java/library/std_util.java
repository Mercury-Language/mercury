//
// Copyright (C) 2002-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//
//

package mercury;

public class std_util
 {
 
    public static boolean semidet_succeed_0_p_0()
     {
        return true;
     }

  public static mercury.list.list_1 cons_3_p_0(
    mercury.private_builtin.type_info_1 TypeInfo_for_T_6,
    java.lang.Object H_4,
    mercury.list.list_1 T_5)
  {
    {
      boolean succeeded = false;
      mercury.list.list_1 HeadVar__3_3 = null;

      {
        HeadVar__3_3 = new mercury.list.list_1.f_cons_2(H_4, T_5);
      }
      return HeadVar__3_3;
    }
  }

  public static class pair_2
  {
    public java.lang.Object F1 = null;
    public java.lang.Object F2 = null;

    public pair_2(
      java.lang.Object F1,
      java.lang.Object F2)
    {
      {
        ((mercury.std_util.pair_2) (this)).F1 = F1;
        ((mercury.std_util.pair_2) (this)).F2 = F2;
      }
    }
  }

 }

