#ifndef __LOCAL_HEADER__
#define __LOCAL_HEADER__

#include <stddef.h>
#include "typedef.h"

#define decrement_local _n(decrement_local)
#define lowlevel_local _n(lowlevel_local)
#define alloc_local _n(alloc_local)
#define make_local _n(make_local)
#define free_local _n(free_local)
#define push_local _n(push_local)
#define rollback_local _n(rollback_local)
#define local_cons _n(local_cons)
#define local_symbol _n(local_symbol)
#define local_array2_memory _n(local_array2_memory)
#define local_array4_memory _n(local_array4_memory)
#define local_body2_memory _n(local_body2_memory)
#define local_body4_memory _n(local_body4_memory)
#define local_smallsize_memory _n(local_smallsize_memory)
#define local_arraybody_memory _n(local_arraybody_memory)
#define local_array4_unbound_memory _n(local_array4_unbound_memory)
#define local_array _n(local_array)
#define local_body _n(local_body)
#define local_array8 _n(local_array8)
#define local_body8 _n(local_body8)
#define local_array2_debug _n(local_array2_debug)
#define local_array4_debug _n(local_array4_debug)
#define local_body2_debug _n(local_body2_debug)
#define local_body4_debug _n(local_body4_debug)
#define local_smallsize_debug _n(local_smallsize_debug)
#define local_arraybody_debug _n(local_arraybody_debug)
#define local_array4_unbound_debug _n(local_array4_unbound_debug)

#ifdef LISP_DEBUG
#define LocalCount		8
#else
#define LocalCount		32
#endif

#ifdef LISP_MEMORY_MALLOC
struct localmemory {
	struct localmemory *next;
	size_t count;
	void *point[LocalCount];
	size_t size[LocalCount];
};

struct localcell {
	struct localcell *next;
	size_t count;
	addr point[LocalCount];
};

struct localstack {
	struct localstack *stack;
	struct localmemory *mem;
	struct localcell *cell;
	size_t cellcount, memcount;
};

struct localroot {
	size_t size, now;
	struct localmemory *mem;
	struct localcell *cell;
	struct localstack *stack;
};

#else
struct localcell {
	struct localcell *next;
	size_t count;
	addr point[LocalCount];
};

struct localstack {
	struct localstack *stack;
	struct localcell *cell;
	size_t cellcount;
};

struct localroot {
	void *alloc;
	size_t size;
	addr tail, front;
	struct localcell *cell;
	struct localstack *stack;
};
#endif

typedef struct localroot *LocalRoot;
typedef struct localstack *LocalStack;

void *lowlevel_local(struct localroot *, size_t);
addr alloc_local(struct localroot *, size_t);
struct localroot *make_local(size_t);
void free_local(struct localroot *);
void push_local(struct localroot *, struct localstack **stack);
void rollback_local(struct localroot *, struct localstack *);

void local_cons(struct localroot *, addr *);
void local_symbol(struct localroot *, addr *);
void local_array2_memory(struct localroot *, addr *, enum LISPTYPE, byte16);
void local_array4_memory(struct localroot *, addr *, enum LISPTYPE, byte32);
void local_body2_memory(struct localroot *, addr *, enum LISPTYPE, byte16);
void local_body4_memory(struct localroot *, addr *, enum LISPTYPE, byte32);
void local_smallsize_memory(struct localroot *,
		addr *, enum LISPTYPE, byte, byte);
void local_arraybody_memory(struct localroot *,
		addr *, enum LISPTYPE, byte16, byte16);
void local_array4_unbound_memory(struct localroot *,
		addr *, enum LISPTYPE, byte32);
void local_array(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body(struct localroot *, addr *, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
void local_array8(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body8(struct localroot *, addr *, enum LISPTYPE, size_t);
#endif

#ifdef LISP_DEBUG
void local_array2_debug(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_array4_debug(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body2_debug(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body4_debug(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_smallsize_debug(struct localroot *, addr *, enum LISPTYPE, size_t, size_t);
void local_arraybody_debug(struct localroot *, addr *, enum LISPTYPE, size_t, size_t);
void local_array4_unbound_debug(struct localroot *, addr *, enum LISPTYPE, size_t);
#define local_array2 local_array2_debug
#define local_array4 local_array4_debug
#define local_body2 local_body2_debug
#define local_body4 local_body4_debug
#define local_smallsize local_smallsize_debug
#define local_arraybody local_arraybody_debug
#define local_array4_unbound local_array4_unbound_debug
#else
#define local_array2(m,r,t,a) local_array2_memory((m),(r),(t),(byte16)(a))
#define local_array4(m,r,t,a) local_array4_memory((m),(r),(t),(byte32)(a))
#define local_body2(m,r,t,b) local_body2_memory((m),(r),(t),(byte16)(b))
#define local_body4(m,r,t,b) local_body4_memory((m),(r),(t),(byte32)(b))
#define local_smallsize(m,r,t,a,b) \
	local_smallsize_memory((m),(r),(t),(byte)(a),(byte)(b))
#define local_arraybody(m,r,t,a,b) \
	local_arraybody_memory((m),(r),(t),(byte16)(a),(byte16)(b))
#define local_array4_unbound(m,r,t,a) \
	local_array4_unbound_memory((m),(r),(t),(byte32)(a))
#endif
#endif

