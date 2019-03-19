#ifndef __BOOLE_HEADER__
#define __BOOLE_HEADER__

#include "execute.h"
#include "local.h"

enum Boole_Index {
	Boole_1,
	Boole_2,
	Boole_And,
	Boole_AndC1,
	Boole_AndC2,
	Boole_C1,
	Boole_C2,
	Boole_Clr,
	Boole_Eqv,
	Boole_Ior,
	Boole_Nand,
	Boole_Nor,
	Boole_Orc1,
	Boole_Orc2,
	Boole_Set,
	Boole_Xor,
	Boole_Size
};

void logand_common(LocalRoot local, addr args, addr *ret);
void logandc1_common(LocalRoot local, addr a, addr b, addr *ret);
void logandc2_common(LocalRoot local, addr a, addr b, addr *ret);
void logeqv_common(LocalRoot local, addr args, addr *ret);
void logior_common(LocalRoot local, addr args, addr *ret);
void lognand_common(LocalRoot local, addr a, addr b, addr *ret);
void lognor_common(LocalRoot local, addr a, addr b, addr *ret);
void lognot_common(LocalRoot local, addr a, addr *ret);
void logorc1_common(LocalRoot local, addr a, addr b, addr *ret);
void logorc2_common(LocalRoot local, addr a, addr b, addr *ret);
void logxor_common(LocalRoot local, addr args, addr *ret);
void boole_common(LocalRoot local, addr op, addr a, addr b, addr *ret);
int logbitp_common(addr index, addr pos);
size_t logcount_common(addr pos);
int logtest_common(LocalRoot local, addr a, addr b);
void init_boole(void);

void deposit_field_common(LocalRoot local, addr *ret, addr a, addr spec, addr b);
void dpb_common(LocalRoot local, addr *ret, addr a, addr spec, addr b);
void ldb_common(LocalRoot local, addr *ret, addr spec, addr pos);
void setf_ldb(Execute ptr, addr args, addr env);
int ldb_test_common(addr spec, addr pos);
void mask_field_common(LocalRoot local, addr *ret, addr spec, addr pos);
void setf_mask_field(Execute ptr, addr args, addr env);

#endif

