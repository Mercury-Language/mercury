/*
** Machine registers mr0 - mr36 for the Intel 386 architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** At the moment we're only using the callee-save registers.
** We should modify this to optionally use the caller-save registers.
*/

#define NUM_REAL_REGS 3

reg 	Word	mr0 __asm__("bx");
reg	Word	mr1 __asm__("si");
reg	Word	mr2 __asm__("di");

#define save_registers()	((void)0)
#define restore_registers()	((void)0)

#define	mr3	unreal_reg_3
#define	mr4	unreal_reg_4
#define	mr5	unreal_reg_5
#define	mr6	unreal_reg_6
#define	mr7	unreal_reg_7
#define	mr8	unreal_reg_8
#define	mr9	unreal_reg_9
#define	mr10	unreal_reg_10
#define	mr11	unreal_reg_11
#define	mr12	unreal_reg_12
#define	mr13	unreal_reg_13
#define	mr14	unreal_reg_14
#define	mr15	unreal_reg_15
#define	mr16	unreal_reg_16
#define	mr17	unreal_reg_17
#define	mr18	unreal_reg_18
#define	mr19	unreal_reg_19
#define	mr20	unreal_reg_20
#define	mr21	unreal_reg_21
#define	mr22	unreal_reg_22
#define	mr23	unreal_reg_23
#define	mr24	unreal_reg_24
#define	mr25	unreal_reg_25
#define	mr26	unreal_reg_26
#define	mr27	unreal_reg_27
#define	mr28	unreal_reg_28
#define	mr29	unreal_reg_29
#define	mr30	unreal_reg_30
#define	mr31	unreal_reg_31
#define	mr32	unreal_reg_32
#define	mr33	unreal_reg_33
#define	mr34	unreal_reg_34
#define	mr35	unreal_reg_35
#define	mr36	unreal_reg_36
#define	mr37	unreal_reg_37
#define	mr38	unreal_reg_38
#define	mr39	unreal_reg_39
#define	mr40	unreal_reg_40
#define	mr41	unreal_reg_41
#define	mr42	unreal_reg_42
#define	mr43	unreal_reg_43
#define	mr44	unreal_reg_44
#define	mr45	unreal_reg_45
#define	mr46	unreal_reg_46
#define	mr47	unreal_reg_47
#define	mr48	unreal_reg_48
#define	mr49	unreal_reg_49
#define	mr50	unreal_reg_50
#define	mr51	unreal_reg_51
#define	mr52	unreal_reg_52
#define	mr53	unreal_reg_53
#define	mr54	unreal_reg_54
#define	mr55	unreal_reg_55
#define	mr56	unreal_reg_56
#define	mr57	unreal_reg_57
#define	mr58	unreal_reg_58
#define	mr59	unreal_reg_59
#define	mr60	unreal_reg_60
#define	mr61	unreal_reg_61
#define	mr62	unreal_reg_62
#define	mr63	unreal_reg_63
#define	mr64	unreal_reg_64
#define	mr65	unreal_reg_65
#define	mr66	unreal_reg_66
#define	mr67	unreal_reg_67
#define	mr68	unreal_reg_68
#define	mr69	unreal_reg_69
