#ifndef __OBJECT_HEADER__
#define __OBJECT_HEADER__

#include "execute.h"
#include "lisp.h"
#include "local.h"
#include "memory.h"
#include "unicode.h"

enum SYMBOL_INDEX {
	SYMBOL_INDEX_STACK    = 0,
	SYMBOL_INDEX_CDR      = 1, /* (cdr nil) -> nil */
	SYMBOL_INDEX_VALUE    = 2,
	SYMBOL_INDEX_FUNCTION = 3,
	SYMBOL_INDEX_NAME     = 4,
	SYMBOL_INDEX_PACKAGE  = 5,
	SYMBOL_INDEX_PLIST    = 6,
	SYMBOL_INDEX_INFO     = 7,
	SYMBOL_INDEX_SIZE     = 8  /* size must be a even number. */
};


/****************************************************************************
 *  low level macro
 ****************************************************************************/
#define GetCons_Low(x,l,r)			{ \
	addr *_getcons_pos = PtrArrayA2(x); \
	*(l) = _getcons_pos[0]; \
	*(r) = _getcons_pos[1]; \
}
#define GetCar_Low(x,v)				GetArrayA2(x, 0, v)
#define GetCdr_Low(x,v)				GetArrayA2(x, 1, v)
#define RefCar_Low(x)				RefArrayA2(x, 0)
#define RefCdr_Low(x)				RefArrayA2(x, 1)

#ifdef LISP_DEBUG
#define SetCons_Low(x,l,r)			{ \
	addr _pos = (x); \
	addr _left = (l); \
	addr _right = (r); \
	addr *_getcons_pos = PtrArrayA2(_pos); \
	CheckDynamic(_pos, _left); \
	CheckDynamic(_pos, _right); \
	_getcons_pos[0] = _left; \
	_getcons_pos[1] = _right; \
}
#else
#define SetCons_Low(x,l,r)			{ \
	addr *_getcons_pos = PtrArrayA2(x); \
	_getcons_pos[0] = (l); \
	_getcons_pos[1] = (r); \
}
#endif
#define SetCar_Low(x,v)				SetArrayA2(x,0,v)
#define SetCdr_Low(x,v)				SetArrayA2(x,1,v)
#define SetCar_force(x,v)			SetArrayA2_force(x,0,v)
#define SetCdr_force(x,v)			SetArrayA2_force(x,1,v)

#define PtrFixnum_Low(x)			((const fixnum *)PtrBodyB2(x))
#define RefFixnum_Low(x)			(*PtrFixnum_Low(x))
#define GetFixnum_Low(x,v)			GetvBodyB2((x),fixnum,(v))
#define SetFixnum_Low(x,v)			SetvBodyB2((x),fixnum,(v))

#define PtrIndex_Low(x)				((const size_t *)PtrBodyB2(x))
#define RefIndex_Low(x)				(*PtrIndex_Low(x))
#define GetIndex_Low(x,v)			GetvBodyB2((x),size_t,(v))
#define SetIndex_Low(x,v)			SetvBodyB2((x),size_t,(v))
#define IncIndex_Low(x,v)			IncvBodyB2((x),size_t,(v))
#define DecIndex_Low(x,v)			DecvBodyB2((x),size_t,(v))

#define PtrSingleFloat_Low(x)		((const single_float *)PtrBodyB2(x))
#define RefSingleFloat_Low(x)		(*PtrSingleFloat_Low(x))
#define GetSingleFloat_Low(x,v)		GetvBodyB2((x),single_float,(v))
#define SetSingleFloat_Low(x,v)		SetvBodyB2((x),single_float,(v))
#define PtrDoubleFloat_Low(x)		((const double_float *)PtrBodyB2(x))
#define RefDoubleFloat_Low(x)		(*PtrDoubleFloat_Low(x))
#define GetDoubleFloat_Low(x,v)		GetvBodyB2((x),double_float,(v))
#define SetDoubleFloat_Low(x,v)		SetvBodyB2((x),double_float,(v))
#define PtrLongFloat_Low(x)			((const long_float*)PtrBodyB2(x))
#define RefLongFloat_Low(x)			(*PtrLongFloat_Low(x))
#define GetLongFloat_Low(x,v)		GetvBodyB2((x),long_float,(v))
#define SetLongFloat_Low(x,v)		SetvBodyB2((x),long_float,(v))


/****************************************************************************
 *  function
 ****************************************************************************/
#ifdef LISP_DEBUG
#define GetCons(x,l,r)				getcons_unsafe(x,l,r)
#define GetCar(x,v)					getconscar_unsafe(x,v)
#define GetCdr(x,v)					getconscdr_unsafe(x,v)
#define RefCar(x)					refconscar_unsafe(x)
#define RefCdr(x)					refconscdr_unsafe(x)
#define SetCons(x,l,r)				setcons_unsafe(x,l,r)
#define SetCar(x,v)					setconscar_unsafe(x,v)
#define SetCdr(x,v)					setconscdr_unsafe(x,v)

#define RefFixnum(x)				(*ptrfixnum(x))
#define GetFixnum(x,v)				getfixnum(x,v)
#define SetFixnum(x,v)				setfixnum(x,v)

#define RefIndex(x)					(*ptrindex(x))
#define GetIndex(x,v)				getindex(x,v)
#define SetIndex(x,v)				setindex(x,v)
#define IncIndex(x,v)				incindex(x,v)
#define DecIndex(x,v)				decindex(x,v)

#define RefSingleFloat(x)			(*ptrsinglefloat(x))
#define GetSingleFloat(x,v)			getsinglefloat(x,v)
#define SetSingleFloat(x,v)			setsinglefloat(x,v)
#define RefDoubleFloat(x)			(*ptrdoublefloat(x))
#define GetDoubleFloat(x,v)			getdoublefloat(x,v)
#define SetDoubleFloat(x,v)			setdoublefloat(x,v)
#define RefLongFloat(x)				(*ptrlongfloat(x))
#define GetLongFloat(x,v)			getlongfloat(x,v)
#define SetLongFloat(x,v)			setlongfloat(x,v)

#else

#define GetCons(x,l,r)				GetCons_Low(x,l,r)
#define GetCar(x,v)					GetCar_Low(x,v)
#define GetCdr(x,v)					GetCdr_Low(x,v)
#define RefCar(x)					RefCar_Low(x)
#define RefCdr(x)					RefCdr_Low(x)
#define SetCons(x,l,r)				SetCons_Low(x,l,r)
#define SetCar(x,v)					SetCar_Low(x,v)
#define SetCdr(x,v)					SetCdr_Low(x,v)

#define RefFixed(x)					RefFixed_Low(x)
#define GetFixed(x,v)				GetFixed_Low(x,v)
#define SetFixed(x,v)				SetFixed_Low(x,v)
#define IncFixed(x,v)				IncFixed_Low(x,v)
#define DecFixed(x,v)				DecFixed_Low(x,v)

#define RefFixnum(x)				RefFixnum_Low(x)
#define GetFixnum(x,v)				GetFixnum_Low(x,v)
#define SetFixnum(x,v)				SetFixnum_Low(x,v)

#define RefIndex(x)					RefIndex_Low(x)
#define GetIndex(x,v)				GetIndex_Low(x,v)
#define SetIndex(x,v)				SetIndex_Low(x,v)
#define IncIndex(x,v)				IncIndex_Low(x,v)
#define DecIndex(x,v)				DecIndex_Low(x,v)

#define RefSingleFloat(x)			RefSingleFloat_Low(x)
#define GetSingleFloat(x,v)			GetSingleFloat_Low(x,v)
#define SetSingleFloat(x,v)			SetSingleFloat_Low(x,v)
#define RefDoubleFloat(x)			RefDoubleFloat_Low(x)
#define GetDoubleFloat(x,v)			GetDoubleFloat_Low(x,v)
#define SetDoubleFloat(x,v)			SetDoubleFloat_Low(x,v)
#define RefLongFloat(x)				RefLongFloat_Low(x)
#define GetLongFloat(x,v)			GetLongFloat_Low(x,v)
#define SetLongFloat(x,v)			SetLongFloat_Low(x,v)

#endif

/* alloc */
addr allocr_cons(LocalRoot);
addr allocr_symbol(LocalRoot);
addr allocr_array2_memory(LocalRoot, enum LISPTYPE, byte16);
addr allocr_array4_memory(LocalRoot, enum LISPTYPE, byte32);
addr allocr_body2_memory(LocalRoot, enum LISPTYPE, byte16);
addr allocr_body4_memory(LocalRoot, enum LISPTYPE, byte32);
addr allocr_smallsize_memory(LocalRoot, enum LISPTYPE, byte, byte);
addr allocr_arraybody_memory(LocalRoot, enum LISPTYPE, byte16, byte16);
addr allocr_array(LocalRoot, enum LISPTYPE, size_t);
addr allocr_body(LocalRoot, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
addr allocr_array8(LocalRoot, enum LISPTYPE, size_t);
addr allocr_body8(LocalRoot, enum LISPTYPE, size_t);
#endif

#ifdef LISP_DEBUG
addr allocr_array2_debug(LocalRoot, enum LISPTYPE, size_t);
addr allocr_array4_debug(LocalRoot, enum LISPTYPE, size_t);
addr allocr_body2_debug(LocalRoot, enum LISPTYPE, size_t);
addr allocr_body4_debug(LocalRoot, enum LISPTYPE, size_t);
addr allocr_smallsize_debug(LocalRoot, enum LISPTYPE, size_t, size_t);
addr allocr_arraybody_debug(LocalRoot, enum LISPTYPE, size_t, size_t);
#define allocr_array2 allocr_array2_debug
#define allocr_array4 allocr_array4_debug
#define allocr_body2 allocr_body2_debug
#define allocr_body4 allocr_body4_debug
#define allocr_smallsize allocr_smallsize_debug
#define allocr_arraybody allocr_arraybody_debug
#else
#define allocr_array2(m,t,a) allocr_array2_memory((m),(t),(byte16)(a))
#define allocr_array4(m,t,a) allocr_array4_memory((m),(t),(byte32)(a))
#define allocr_body2(m,t,b) allocr_body2_memory((m),(t),(byte16)(b))
#define allocr_body4(m,t,b) allocr_body4_memory((m),(t),(byte32)(b))
#define allocr_smallsize allocr_smallsize_memory((m),(t),(byte)(a),(byte)(b))
#define allocr_arraybody allocr_arraybody_memory((m),(t),(byte16)(a),(byte16)(b))
#endif

void alloc_cons(LocalRoot, addr *);
void alloc_symbol(LocalRoot, addr *);
void alloc_array2_memory(LocalRoot, addr *, enum LISPTYPE, byte16);
void alloc_array4_memory(LocalRoot, addr *, enum LISPTYPE, byte32);
void alloc_body2_memory(LocalRoot, addr *, enum LISPTYPE, byte16);
void alloc_body4_memory(LocalRoot, addr *, enum LISPTYPE, byte32);
void alloc_smallsize_memory(LocalRoot, addr *, enum LISPTYPE, byte, byte);
void alloc_arraybody_memory(LocalRoot, addr *, enum LISPTYPE, byte16, byte16);
void alloc_array(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_body(LocalRoot, addr *, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
void alloc_array8(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_body8(LocalRoot, addr *, enum LISPTYPE, size_t);
#endif

#ifdef LISP_DEBUG
void alloc_array2_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_array4_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_body2_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_body4_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
void alloc_smallsize_debug(LocalRoot, addr *, enum LISPTYPE, size_t, size_t);
void alloc_arraybody_debug(LocalRoot, addr *, enum LISPTYPE, size_t, size_t);
#define alloc_array2 alloc_array2_debug
#define alloc_array4 alloc_array4_debug
#define alloc_body2 alloc_body2_debug
#define alloc_body4 alloc_body4_debug
#define alloc_smallsize alloc_smallsize_debug
#define alloc_arraybody alloc_arraybody_debug
#else
#define alloc_array2(m,r,t,a) alloc_array2_memory((m),(r),(t),(byte16)(a))
#define alloc_array4(m,r,t,a) alloc_array4_memory((m),(r),(t),(byte32)(a))
#define alloc_body2(m,r,t,b) alloc_body2_memory((m),(r),(t),(byte16)(b))
#define alloc_body4(m,r,t,b) alloc_body4_memory((m),(r),(t),(byte32)(b))
#define alloc_smallsize(m,r,t,a,b) \
	alloc_smallsize_memory((m),(r),(t),(byte)(a),(byte)(b))
#define alloc_arraybody(m,r,t,a,b) \
	alloc_arraybody_memory((m),(r),(t),(byte16)(a),(byte16)(b))
#endif

/* init / free */
void build_object(void);

/* system t object */
void nil_heap(void);
void t_heap(void);

/* cons object */
void consnil_heap(addr *ret);
void conscar_heap(addr *ret, addr left);
void conscdr_heap(addr *ret, addr right);
void cons_heap(addr *ret, addr left, addr right);
void consnil_local(LocalRoot local, addr *ret);
void conscar_local(LocalRoot local, addr *ret, addr left);
void conscdr_local(LocalRoot local, addr *ret, addr right);
void cons_local(LocalRoot local, addr *ret, addr left, addr right);
void consnil_alloc(LocalRoot local, addr *ret);
void conscar_alloc(LocalRoot local, addr *ret, addr left);
void conscdr_alloc(LocalRoot local, addr *ret, addr right);
void cons_alloc(LocalRoot local, addr *ret, addr left, addr right);

addr consnil_heapr(void);
addr conscar_heapr(addr left);
addr conscdr_heapr(addr right);
addr cons_heapr(addr left, addr right);
addr consnil_localr(LocalRoot local);
addr conscar_localr(LocalRoot local, addr left);
addr conscdr_localr(LocalRoot local, addr right);
addr cons_localr(LocalRoot local, addr left, addr right);
addr consnil_allocr(LocalRoot local);
addr conscar_allocr(LocalRoot local, addr left);
addr conscdr_allocr(LocalRoot local, addr right);
addr cons_allocr(LocalRoot local, addr left, addr right);

addr refconscar_unsafe(addr pos);
addr refconscdr_unsafe(addr pos);
void getconscar_unsafe(addr pos, addr *ret);
void getconscdr_unsafe(addr pos, addr *ret);
void getcons_unsafe(addr pos, addr *left, addr *right);
void setconscar_unsafe(addr pos, addr value);
void setconscdr_unsafe(addr pos, addr value);
void setcons_unsafe(addr pos, addr left, addr right);
void setconscar_force(addr pos, addr value);
void setconscdr_force(addr pos, addr value);
void setcons_force(addr pos, addr left, addr right);

/* list */
int listp(addr pos);
int consp(addr pos);
int singlep(addr pos);
void list_alloc_stdarg(LocalRoot local, addr *ret, va_list args);
addr list_heapr(addr pos, ...);
addr list_localr(LocalRoot local, ...);
addr list_allocr(LocalRoot local, ...);
void list_heap(addr *ret, ...);
void list_local(LocalRoot local, addr *ret, ...);
void list_alloc(LocalRoot local, addr *ret, ...);

/* vector object */
addr vector2_heapr(size_t size);
addr vector2_localr(LocalRoot local, size_t size);
addr vector2_allocr(LocalRoot local, size_t size);
void vector2_heap(addr *ret, size_t size);
void vector2_local(LocalRoot local, addr *ret, size_t size);
void vector2_alloc(LocalRoot local, addr *ret, size_t size);

addr vector4_heapr(size_t size);
addr vector4_localr(LocalRoot local, size_t size);
addr vector4_allocr(LocalRoot local, size_t size);
void vector4_heap(addr *ret, size_t size);
void vector4_local(LocalRoot local, addr *ret, size_t size);
void vector4_alloc(LocalRoot local, addr *ret, size_t size);

addr vector_heapr(size_t size);
addr vector_localr(LocalRoot local, size_t size);
addr vector_allocr(LocalRoot local, size_t size);
void vector_heap(addr *ret, size_t size);
void vector_local(LocalRoot local, addr *ret, size_t size);
void vector_alloc(LocalRoot local, addr *ret, size_t size);

void copy_vector4_alloc(LocalRoot local, addr *ret, addr pos);
void copy_vector4_local(LocalRoot local, addr *ret, addr pos);
void copy_vector4_heap(addr *ret, addr pos);
void copy_vector_alloc(LocalRoot local, addr *ret, addr pos);
void copy_vector_local(LocalRoot local, addr *ret, addr pos);
void copy_vector_heap(addr *ret, addr pos);

#ifdef LISP_ARCH_64BIT
addr vector8_heapr(size_t size);
addr vector8_localr(LocalRoot local, size_t size);
addr vector8_allocr(LocalRoot local, size_t size);
void vector8_heap(addr *ret, size_t size);
void vector8_local(LocalRoot local, addr *ret, size_t size);
void vector8_alloc(LocalRoot local, addr *ret, size_t size);
#endif

/* fixnum object */
addr make_fixnum_heapr(fixnum value);
addr fixnum_heapr(fixnum value);
addr fixnum_localr(LocalRoot local, fixnum value);
addr fixnum_allocr(LocalRoot local, fixnum value);

void make_fixnum_heap(addr *ret, fixnum value);
void fixnum_heap(addr *ret, fixnum value);
void fixnum_local(LocalRoot local, addr *ret, fixnum value);
void fixnum_alloc(LocalRoot local, addr *ret, fixnum value);

#define fixnuma fixnum_allocr
#define fixnumh fixnum_heapr
#define fixnuml(v) fixnum_localr(Local_Thread, (v))

const fixnum *ptrfixnum(addr pos);
fixnum reffixnum(addr pos);
void getfixnum(addr pos, fixnum *ret);
void setfixnum(addr pos, fixnum value);
int fixnumequal(addr left, addr right);
int fixnumcompare(addr left, addr right);

/* index object */
addr index_heapr(size_t value);
addr index_localr(LocalRoot local, size_t value);
addr index_allocr(LocalRoot local, size_t value);
void index_heap(addr *ret, size_t value);
void index_local(LocalRoot local, addr *ret, size_t value);
void index_alloc(LocalRoot local, addr *ret, size_t value);

const size_t *ptrindex(addr pos);
size_t refindex(addr pos);
void getindex(addr pos, size_t *ret);
void setindex(addr pos, size_t value);
void incindex(addr pos, size_t value);
void decindex(addr pos, size_t value);

/* float */
addr single_float_heapr(single_float value);
addr single_float_localr(LocalRoot local, single_float value);
addr single_float_allocr(LocalRoot local, single_float value);
void single_float_heap(addr *ret, single_float value);
void single_float_local(LocalRoot local, addr *ret, single_float value);
void single_float_alloc(LocalRoot local, addr *ret, single_float value);
const single_float *ptrsinglefloat(addr pos);
single_float refsinglefloat(addr pos);
void getsinglefloat(addr pos, single_float *ret);
void setsinglefloat(addr pos, single_float value);

addr double_float_heapr(double_float value);
addr double_float_localr(LocalRoot local, double_float value);
addr double_float_allocr(LocalRoot local, double_float value);
void double_float_heap(addr *ret, double_float value);
void double_float_local(LocalRoot local, addr *ret, double_float value);
void double_float_alloc(LocalRoot local, addr *ret, double_float value);
const double_float *ptrdoublefloat(addr pos);
double_float refdoublefloat(addr pos);
void getdoublefloat(addr pos, double_float *ret);
void setdoublefloat(addr pos, double_float value);

addr long_float_heapr(long_float value);
addr long_float_localr(LocalRoot local, long_float value);
addr long_float_allocr(LocalRoot local, long_float value);
void long_float_heap(addr *ret, long_float value);
void long_float_local(LocalRoot local, addr *ret, long_float value);
void long_float_alloc(LocalRoot local, addr *ret, long_float value);
const long_float *ptrlongfloat(addr pos);
long_float reflongfloat(addr pos);
void getlongfloat(addr pos, long_float *ret);
void setlongfloat(addr pos, long_float value);

/* queue object */
addr queue_heapr(void);
addr queue_localr(LocalRoot local);
addr queue_allocr(LocalRoot local);
void queue_heap(addr *ret);
void queue_local(LocalRoot local, addr *ret);
void queue_alloc(LocalRoot local, addr *ret);
void pushqueue_heap(addr pos, addr insert);
void pushqueue_local(LocalRoot local, addr pos, addr insert);
void pushqueue_alloc(LocalRoot local, addr pos, addr insert);
void dotqueue(addr pos, addr right);
void clearqueue(addr pos);

addr rootqueuer(addr pos);
addr tailqueuer(addr pos);
void rootqueue(addr pos, addr *ret);
void tailqueue(addr pos, addr *ret);
int firstqueue(addr pos, addr *ret);
int lastqueue(addr pos, addr *ret);
int nthqueue(addr pos, size_t index, addr *ret);

#endif

