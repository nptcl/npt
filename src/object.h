#ifndef __OBJECT_HEADER__
#define __OBJECT_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "memory.h"

enum SYMBOL_INDEX {
	SYMBOL_INDEX_SPECIAL  = 0,
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
	CheckDynamic(_pos, _left); \
	CheckDynamic(_pos, _right); \
	SetCar_Low(_pos, _left); \
	SetCdr_Low(_pos, _right); \
}
#else
#define SetCons_Low(x,l,r)			{ \
	addr _pos = (x); \
	SetCar_Low(_pos, (l)); \
	SetCdr_Low(_pos, (r)); \
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
_g void alloc_cons(LocalRoot, addr *);
_g void alloc_symbol(LocalRoot, addr *);
_g void alloc_array2_memory(LocalRoot, addr *, enum LISPTYPE, byte16);
_g void alloc_array4_memory(LocalRoot, addr *, enum LISPTYPE, byte32);
_g void alloc_body2_memory(LocalRoot, addr *, enum LISPTYPE, byte16);
_g void alloc_body4_memory(LocalRoot, addr *, enum LISPTYPE, byte32);
_g void alloc_smallsize_memory(LocalRoot, addr *, enum LISPTYPE, byte, byte);
_g void alloc_arraybody_memory(LocalRoot, addr *, enum LISPTYPE, byte16, byte16);
_g void alloc_array(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_body(LocalRoot, addr *, enum LISPTYPE, size_t);
#ifdef LISP_ARCH_64BIT
_g void alloc_array8(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_body8(LocalRoot, addr *, enum LISPTYPE, size_t);
#endif

#ifdef LISP_DEBUG
_g void alloc_array2_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_array4_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_body2_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_body4_debug(LocalRoot, addr *, enum LISPTYPE, size_t);
_g void alloc_smallsize_debug(LocalRoot, addr *, enum LISPTYPE, size_t, size_t);
_g void alloc_arraybody_debug(LocalRoot, addr *, enum LISPTYPE, size_t, size_t);
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
_g void build_object(void);

/* nil t */
_g void nil_heap(void);
_g void t_heap(void);

/* cons */
_g void consnil_heap(addr *ret);
_g void conscar_heap(addr *ret, addr left);
_g void conscdr_heap(addr *ret, addr right);
_g void cons_heap(addr *ret, addr left, addr right);
_g void consnil_local(LocalRoot local, addr *ret);
_g void conscar_local(LocalRoot local, addr *ret, addr left);
_g void conscdr_local(LocalRoot local, addr *ret, addr right);
_g void cons_local(LocalRoot local, addr *ret, addr left, addr right);
_g void consnil_alloc(LocalRoot local, addr *ret);
_g void conscar_alloc(LocalRoot local, addr *ret, addr left);
_g void conscdr_alloc(LocalRoot local, addr *ret, addr right);
_g void cons_alloc(LocalRoot local, addr *ret, addr left, addr right);

_g addr refconscar_unsafe(addr pos);
_g addr refconscdr_unsafe(addr pos);
_g void getconscar_unsafe(addr pos, addr *ret);
_g void getconscdr_unsafe(addr pos, addr *ret);
_g void getcons_unsafe(addr pos, addr *left, addr *right);
_g void setconscar_unsafe(addr pos, addr value);
_g void setconscdr_unsafe(addr pos, addr value);
_g void setcons_unsafe(addr pos, addr left, addr right);
_g void setconscar_force(addr pos, addr value);
_g void setconscdr_force(addr pos, addr value);
_g void setcons_force(addr pos, addr left, addr right);

/* list */
_g int listp(addr pos);
_g int consp(addr pos);
_g int singlep(addr pos);

/* vector */
_g void vector2_heap(addr *ret, size_t size);
_g void vector2_local(LocalRoot local, addr *ret, size_t size);
_g void vector2_alloc(LocalRoot local, addr *ret, size_t size);
_g void vector4_heap(addr *ret, size_t size);
_g void vector4_local(LocalRoot local, addr *ret, size_t size);
_g void vector4_alloc(LocalRoot local, addr *ret, size_t size);
#ifdef LISP_ARCH_64BIT
_g void vector8_heap(addr *ret, size_t size);
_g void vector8_local(LocalRoot local, addr *ret, size_t size);
_g void vector8_alloc(LocalRoot local, addr *ret, size_t size);
#endif
_g void vector_heap(addr *ret, size_t size);
_g void vector_local(LocalRoot local, addr *ret, size_t size);
_g void vector_alloc(LocalRoot local, addr *ret, size_t size);
_g void copy_vector4_alloc(LocalRoot local, addr *ret, addr pos);
_g void copy_vector4_local(LocalRoot local, addr *ret, addr pos);
_g void copy_vector4_heap(addr *ret, addr pos);
_g void copy_vector_alloc(LocalRoot local, addr *ret, addr pos);
_g void copy_vector_local(LocalRoot local, addr *ret, addr pos);
_g void copy_vector_heap(addr *ret, addr pos);

/* fixnum */
_g void make_fixnum_heap(addr *ret, fixnum value);
_g void fixnum_heap(addr *ret, fixnum value);
_g void fixnum_local(LocalRoot local, addr *ret, fixnum value);
_g void fixnum_alloc(LocalRoot local, addr *ret, fixnum value);
_g addr fixnumh(fixnum value);
_g addr fixnuml(fixnum value);
_g addr fixnuma(LocalRoot local, fixnum value);
_g const fixnum *ptrfixnum(addr pos);
_g fixnum reffixnum(addr pos);
_g void getfixnum(addr pos, fixnum *ret);
_g void setfixnum(addr pos, fixnum value);
_g int fixnumequal(addr left, addr right);
_g int fixnumcompare(addr left, addr right);

/* index */
_g int indexp(addr pos);
_g void index_heap(addr *ret, size_t value);
_g void index_local(LocalRoot local, addr *ret, size_t value);
_g void index_alloc(LocalRoot local, addr *ret, size_t value);

_g const size_t *ptrindex(addr pos);
_g size_t refindex(addr pos);
_g void getindex(addr pos, size_t *ret);
_g void setindex(addr pos, size_t value);
_g void incindex(addr pos, size_t value);
_g void decindex(addr pos, size_t value);

/* float */
_g int single_float_p(addr value);
_g void single_float_heap(addr *ret, single_float value);
_g void single_float_local(LocalRoot local, addr *ret, single_float value);
_g void single_float_alloc(LocalRoot local, addr *ret, single_float value);
_g const single_float *ptrsinglefloat(addr pos);
_g single_float refsinglefloat(addr pos);
_g void getsinglefloat(addr pos, single_float *ret);
_g void setsinglefloat(addr pos, single_float value);

_g int double_float_p(addr value);
_g void double_float_heap(addr *ret, double_float value);
_g void double_float_local(LocalRoot local, addr *ret, double_float value);
_g void double_float_alloc(LocalRoot local, addr *ret, double_float value);
_g const double_float *ptrdoublefloat(addr pos);
_g double_float refdoublefloat(addr pos);
_g void getdoublefloat(addr pos, double_float *ret);
_g void setdoublefloat(addr pos, double_float value);

_g int long_float_p(addr value);
_g void long_float_heap(addr *ret, long_float value);
_g void long_float_local(LocalRoot local, addr *ret, long_float value);
_g void long_float_alloc(LocalRoot local, addr *ret, long_float value);
_g const long_float *ptrlongfloat(addr pos);
_g long_float reflongfloat(addr pos);
_g void getlongfloat(addr pos, long_float *ret);
_g void setlongfloat(addr pos, long_float value);

_g addr singleh(single_float value);
_g addr doubleh(double_float value);
_g addr longh(long_float value);

/* queue */
_g void queue_heap(addr *ret);
_g void queue_local(LocalRoot local, addr *ret);
_g void queue_alloc(LocalRoot local, addr *ret);
_g void pushqueue_heap(addr pos, addr insert);
_g void pushqueue_local(LocalRoot local, addr pos, addr insert);
_g void pushqueue_alloc(LocalRoot local, addr pos, addr insert);
_g void dotqueue(addr pos, addr right);
_g void clearqueue(addr pos);

_g void rootqueue(addr pos, addr *ret);
_g void tailqueue(addr pos, addr *ret);
_g int firstqueue(addr pos, addr *ret);
_g int lastqueue(addr pos, addr *ret);
_g int nthqueue(addr pos, size_t index, addr *ret);

#endif

