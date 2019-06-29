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

_g void logand_common(LocalRoot local, addr args, addr *ret);
_g void logandc1_common(LocalRoot local, addr a, addr b, addr *ret);
_g void logandc2_common(LocalRoot local, addr a, addr b, addr *ret);
_g void logeqv_common(LocalRoot local, addr args, addr *ret);
_g void logior_common(LocalRoot local, addr args, addr *ret);
_g void lognand_common(LocalRoot local, addr a, addr b, addr *ret);
_g void lognor_common(LocalRoot local, addr a, addr b, addr *ret);
_g void lognot_common(LocalRoot local, addr a, addr *ret);
_g void logorc1_common(LocalRoot local, addr a, addr b, addr *ret);
_g void logorc2_common(LocalRoot local, addr a, addr b, addr *ret);
_g void logxor_common(LocalRoot local, addr args, addr *ret);
_g void boole_common(LocalRoot local, addr op, addr a, addr b, addr *ret);
_g int logbitp_common(addr index, addr pos);
_g size_t logcount_common(addr pos);
_g int logtest_common(LocalRoot local, addr a, addr b);
_g void init_boole(void);

_g void deposit_field_common(LocalRoot local, addr *ret, addr a, addr spec, addr b);
_g void dpb_common(LocalRoot local, addr *ret, addr a, addr spec, addr b);
_g void ldb_common(LocalRoot local, addr *ret, addr spec, addr pos);
_g void function_setf_ldb(Execute ptr, addr args, addr env);
_g int ldb_test_common(addr spec, addr pos);
_g void mask_field_common(LocalRoot local, addr *ret, addr spec, addr pos);
_g void function_setf_mask_field(Execute ptr, addr args, addr env);

#endif

