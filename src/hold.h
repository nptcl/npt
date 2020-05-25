#ifndef __HOLD_HEADER__
#define __HOLD_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "local.h"

struct localhold {
	LocalRoot local;
	LocalStack stack;
	addr array;
};
typedef struct localhold *LocalHold;

_g void setgchold(addr pos, size_t index, addr value);
_g void gchold_local(LocalRoot local, addr *ret, size_t size);
_g void gchold_push_local(LocalRoot local, addr pos);
_g void gchold_pushva_local(LocalRoot local, ...);
_g void gchold_pushva_force_local(LocalRoot local, ...);
_g void gchold_push_special(Execute ptr, addr pos);
_g void gchold_pushva_special(Execute ptr, ...);

_g LocalHold localhold_local(LocalRoot local);
_g LocalHold localhold_local_push(LocalRoot local, addr pos);
_g LocalHold localhold_array(LocalRoot local, size_t size);
_g void localhold_push(LocalHold local, addr pos);
_g void localhold_pushva(LocalHold local, ...);
_g void localhold_pushva_force(LocalHold local, ...);
_g void localhold_end(LocalHold hold);
_g void localhold_set(LocalHold hold, size_t index, addr value);
_g void localhold_set_force(LocalHold hold, size_t index, addr value);

#define LocalHold_local(ptr) localhold_local((ptr)->local)
#define LocalHold_local_push(ptr, pos) localhold_local_push((ptr)->local, (pos))
#define LocalHold_array(ptr, size) localhold_array((ptr)->local, (size))

#endif

