#ifndef	MEMORY_H
#define	MEMORY_H

/* reserve MAX_RN unreal_regs, numbered from 0 to MAX_RN-1 */
extern	Word	unreal_reg_0;
extern	Word	unreal_reg_1;
extern	Word	unreal_reg_2;
extern	Word	unreal_reg_3;
extern	Word	unreal_reg_4;
extern	Word	unreal_reg_5;
extern	Word	unreal_reg_6;
extern	Word	unreal_reg_7;
extern	Word	unreal_reg_8;
extern	Word	unreal_reg_9;
extern	Word	unreal_reg_10;
extern	Word	unreal_reg_11;
extern	Word	unreal_reg_12;
extern	Word	unreal_reg_13;
extern	Word	unreal_reg_14;
extern	Word	unreal_reg_15;
extern	Word	unreal_reg_16;
extern	Word	unreal_reg_17;
extern	Word	unreal_reg_18;
extern	Word	unreal_reg_19;
extern	Word	unreal_reg_20;
extern	Word	unreal_reg_21;
extern	Word	unreal_reg_22;
extern	Word	unreal_reg_23;
extern	Word	unreal_reg_24;
extern	Word	unreal_reg_25;
extern	Word	unreal_reg_26;
extern	Word	unreal_reg_27;
extern	Word	unreal_reg_28;
extern	Word	unreal_reg_29;
extern	Word	unreal_reg_30;
extern	Word	unreal_reg_31;
extern	Word	unreal_reg_32;
extern	Word	unreal_reg_33;
extern	Word	unreal_reg_34;
extern	Word	unreal_reg_35;
extern	Word	unreal_reg_36;
extern	Word	unreal_reg_37;
extern	Word	unreal_reg_38;
extern	Word	unreal_reg_39;
extern	Word	unreal_reg_40;
extern	Word	unreal_reg_41;
extern	Word	unreal_reg_42;
extern	Word	unreal_reg_43;
extern	Word	unreal_reg_44;
extern	Word	unreal_reg_45;
extern	Word	unreal_reg_46;
extern	Word	unreal_reg_47;
extern	Word	unreal_reg_48;
extern	Word	unreal_reg_49;
extern	Word	unreal_reg_50;
extern	Word	unreal_reg_51;
extern	Word	unreal_reg_52;
extern	Word	unreal_reg_53;
extern	Word	unreal_reg_54;
extern	Word	unreal_reg_55;
extern	Word	unreal_reg_56;
extern	Word	unreal_reg_57;
extern	Word	unreal_reg_58;
extern	Word	unreal_reg_59;
extern	Word	unreal_reg_60;
extern	Word	unreal_reg_61;
extern	Word	unreal_reg_62;
extern	Word	unreal_reg_63;
extern	Word	unreal_reg_64;
extern	Word	unreal_reg_65;
extern	Word	unreal_reg_66;
extern	Word	unreal_reg_67;
extern	Word	unreal_reg_68;

/* both these are arrays of size MAX_RN */
extern	Word	*saved_regs;
extern	Word	*num_uses;

/* beginning of allocated areas */
extern	Word	*heap;
extern	Word	*detstack;
extern	Word	*nondstack;

/* beginning of used areas */
extern	Word	*heapmin;
extern	Word	*detstackmin;
extern	Word	*nondstackmin;

/* highest locations actually used */
extern	Word	*heapmax;
extern	Word	*detstackmax;
extern	Word	*nondstackmax;

/* end of allocated areas */
extern	Word	*heapend;
extern	Word	*detstackend;
extern	Word	*nondstackend;

/* beginning of redzones */
extern	caddr_t	heap_zone;
extern	caddr_t	detstack_zone;
extern	caddr_t	nondstack_zone;

extern	int	heap_zone_left;
extern	int	detstack_zone_left;
extern	int	nondstack_zone_left;

extern	void	init_memory(void);

#endif
