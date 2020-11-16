#ifndef __HOLD_HEADER__
#define __HOLD_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "local.h"

#define setgchold _n(setgchold)
#define gchold_local _n(gchold_local)
#define gchold_push_local _n(gchold_push_local)
#define gchold_push_force_local _n(gchold_push_force_local)
#define gchold_pushva_local _n(gchold_pushva_local)
#define gchold_pushva_force_local _n(gchold_pushva_force_local)
#define gchold_push_special _n(gchold_push_special)
#define gchold_pushva_special _n(gchold_pushva_special)
#define localhold_local _n(localhold_local)
#define localhold_local_push _n(localhold_local_push)
#define localhold_array _n(localhold_array)
#define localhold_push _n(localhold_push)
#define localhold_pushva _n(localhold_pushva)
#define localhold_pushva_force _n(localhold_pushva_force)
#define localhold_end _n(localhold_end)
#define localhold_set _n(localhold_set)
#define localhold_set_force _n(localhold_set_force)
#define Hold_local _n(Hold_local)
#define hold_local _n(hold_local)
#define holdp _n(holdp)
#define hold_set _n(hold_set)
#define hold_set_null _n(hold_set_null)
#define hold_get _n(hold_get)
#define hold_value _n(hold_value)
#define holdv _n(holdv)

struct localhold {
	LocalRoot local;
	LocalStack stack;
	addr array;
};
typedef struct localhold *LocalHold;

void setgchold(addr pos, size_t index, addr value);
void gchold_local(LocalRoot local, addr *ret, size_t size);
void gchold_push_local(LocalRoot local, addr pos);
void gchold_push_force_local(LocalRoot local, addr pos);
void gchold_pushva_local(LocalRoot local, ...);
void gchold_pushva_force_local(LocalRoot local, ...);
void gchold_push_special(Execute ptr, addr pos);
void gchold_pushva_special(Execute ptr, ...);

LocalHold localhold_local(LocalRoot local);
LocalHold localhold_local_push(LocalRoot local, addr pos);
LocalHold localhold_array(LocalRoot local, size_t size);
void localhold_push(LocalHold local, addr pos);
void localhold_pushva(LocalHold local, ...);
void localhold_pushva_force(LocalHold local, ...);
void localhold_end(LocalHold hold);
void localhold_set(LocalHold hold, size_t index, addr value);
void localhold_set_force(LocalHold hold, size_t index, addr value);

#define LocalHold_local(ptr) localhold_local((ptr)->local)
#define LocalHold_local_push(ptr, pos) localhold_local_push((ptr)->local, (pos))
#define LocalHold_array(ptr, size) localhold_array((ptr)->local, (size))


/*
 *  hold object
 */
void Hold_local(addr *ret, addr value);
void hold_local(LocalRoot local, addr *ret, addr value);
int holdp(addr pos);
void hold_set(addr pos, addr value);
void hold_set_null(addr pos, addr value);
void hold_get(addr pos, addr *ret);
void hold_value(addr pos, addr *ret);
addr holdv(addr pos);

#endif

