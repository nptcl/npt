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

_g int logand_common_(LocalRoot local, addr args, addr *ret);
_g int logandc1_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int logandc2_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int logeqv_common_(LocalRoot local, addr args, addr *ret);
_g int logior_common_(LocalRoot local, addr args, addr *ret);
_g int lognand_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int lognor_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int lognot_common_(LocalRoot local, addr a, addr *ret);
_g int logorc1_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int logorc2_common_(LocalRoot local, addr a, addr b, addr *ret);
_g int logxor_common_(LocalRoot local, addr args, addr *ret);
_g int boole_common_(LocalRoot local, addr op, addr a, addr b, addr *ret);
_g int logbitp_common_(addr index, addr pos, int *ret);
_g int logcount_common_(addr pos, size_t *ret);
_g int logtest_common_(LocalRoot local, addr a, addr b, int *ret);
_g void init_boole(void);

_g int deposit_field_common_(LocalRoot local, addr *ret, addr a, addr spec, addr b);
_g int dpb_common_(LocalRoot local, addr *ret, addr a, addr spec, addr b);
_g int ldb_common_(LocalRoot local, addr *ret, addr spec, addr pos);
_g int function_setf_ldb(Execute ptr, addr args, addr env);
_g int ldb_test_common_(addr spec, addr pos, int *ret);
_g int mask_field_common_(LocalRoot local, addr *ret, addr spec, addr pos);
_g int function_setf_mask_field(Execute ptr, addr args, addr env);

#endif

