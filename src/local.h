#ifndef __LOCAL_HEADER__
#define __LOCAL_HEADER__

#include <stddef.h>
#include "typedef.h"

#define LocalCount		128
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

typedef struct localroot *LocalRoot;
typedef struct localstack *LocalStack;

struct localroot *make_local(size_t);
void free_local(struct localroot *);
addr lowlevel_unsafe(struct localroot *, size_t);
addr lowlevel_local(struct localroot *, size_t);
addr alloc_local(struct localroot *, size_t);
void unsafe_push_local(struct localroot *);
void push_local(struct localroot *, struct localstack **stack);
void unsafe_pop_local(struct localroot *);
void rollback_local(struct localroot *, struct localstack *);
int valid_local(struct localroot *, const void *);
int valid_memory(struct localroot *, const void *);
int valid_object(struct localroot *, addr);

addr localr_cons(struct localroot *);
addr localr_symbol(struct localroot *);
addr localr_array2_memory(struct localroot *, enum LISPTYPE, byte16);
addr localr_array4_memory(struct localroot *, enum LISPTYPE, byte32);
addr localr_body2_memory(struct localroot *, enum LISPTYPE, byte16);
addr localr_body4_memory(struct localroot *, enum LISPTYPE, byte32);
addr localr_smallsize_memory(struct localroot *, enum LISPTYPE, byte, byte);
addr localr_arraybody_memory(struct localroot *, enum LISPTYPE, byte16, byte16);
addr localr_array4_unbound_memory(struct localroot *, enum LISPTYPE, byte32);
addr localr_array(struct localroot *, enum LISPTYPE, size_t);
addr localr_body(struct localroot *, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
addr localr_array8(struct localroot *, enum LISPTYPE, size_t);
addr localr_body8(struct localroot *, enum LISPTYPE, size_t);
#endif

#ifdef LISP_DEBUG
addr localr_array2_debug(struct localroot *, enum LISPTYPE, size_t);
addr localr_array4_debug(struct localroot *, enum LISPTYPE, size_t);
addr localr_body2_debug(struct localroot *, enum LISPTYPE, size_t);
addr localr_body4_debug(struct localroot *, enum LISPTYPE, size_t);
addr localr_smallsize_debug(struct localroot *, enum LISPTYPE, size_t, size_t);
addr localr_arraybody_debug(struct localroot *, enum LISPTYPE, size_t, size_t);
addr localr_array4_unbound_debug(struct localroot *, enum LISPTYPE, size_t);
#define localr_array2 localr_array2_debug
#define localr_array4 localr_array4_debug
#define localr_body2 localr_body2_debug
#define localr_body4 localr_body4_debug
#define localr_smallsize localr_smallsize_debug
#define localr_arraybody localr_arraybody_debug
#define localr_array4_unbound localr_array4_unbound_debug
#else
#define localr_array2(m,t,a) localr_array2_memory((m),(t),(byte16)(a))
#define localr_array4(m,t,a) localr_array4_memory((m),(t),(byte32)(a))
#define localr_body2(m,t,b) localr_body2_memory((m),(t),(byte16)(b))
#define localr_body4(m,t,b) localr_body4_memory((m),(t),(byte32)(b))
#define localr_smallsize(m,t,a,b) \
	localr_smallsize_memory((m),(t),(byte)(a),(byte)(b))
#define localr_arraybody(m,t,a,b) \
	localr_arraybody_memory((m),(t),(byte16)(a),(byte16)(b))
#define localr_array4_unbound(m,t,a) \
	localr_array4_unbound_memory((m),(t),(byte32)(a))
#endif

void local_cons(struct localroot *, addr *);
void local_symbol(struct localroot *, addr *);
void local_array2_memory(struct localroot *, addr *, enum LISPTYPE, byte16);
void local_array4_memory(struct localroot *, addr *, enum LISPTYPE, byte32);
void local_body2_memory(struct localroot *, addr *, enum LISPTYPE, byte16);
void local_body4_memory(struct localroot *, addr *, enum LISPTYPE, byte32);
void local_smallsize_memory(struct localroot *, addr *, enum LISPTYPE, byte, byte);
void local_arraybody_memory(struct localroot *, addr *, enum LISPTYPE, byte16, byte16);
void local_array4_unbound_memory(struct localroot *, addr *, enum LISPTYPE, byte32);
void local_array(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body(struct localroot *, addr *, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
void local_array8(struct localroot *, addr *, enum LISPTYPE, size_t);
void local_body8(struct localroot *, addr *, enum LISPTYPE, size_t);
#endif
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

