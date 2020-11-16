#ifndef __OBJECT_HEADER__
#define __OBJECT_HEADER__

#include "build.h"
#include "execute.h"
#include "local.h"
#include "memory.h"

#define alloc_cons _n(alloc_cons)
#define alloc_symbol _n(alloc_symbol)
#define alloc_array2_memory _n(alloc_array2_memory)
#define alloc_array4_memory _n(alloc_array4_memory)
#define alloc_body2_memory _n(alloc_body2_memory)
#define alloc_body4_memory _n(alloc_body4_memory)
#define alloc_smallsize_memory _n(alloc_smallsize_memory)
#define alloc_arraybody_memory _n(alloc_arraybody_memory)
#define alloc_array _n(alloc_array)
#define alloc_body _n(alloc_body)
#define alloc_array8 _n(alloc_array8)
#define alloc_body8 _n(alloc_body8)
#define alloc_array2_debug _n(alloc_array2_debug)
#define alloc_array4_debug _n(alloc_array4_debug)
#define alloc_body2_debug _n(alloc_body2_debug)
#define alloc_body4_debug _n(alloc_body4_debug)
#define alloc_smallsize_debug _n(alloc_smallsize_debug)
#define alloc_arraybody_debug _n(alloc_arraybody_debug)
#define build_object _n(build_object)
#define nil_heap _n(nil_heap)
#define t_heap _n(t_heap)
#define consnil_heap _n(consnil_heap)
#define conscar_heap _n(conscar_heap)
#define conscdr_heap _n(conscdr_heap)
#define cons_heap _n(cons_heap)
#define consnil_local _n(consnil_local)
#define conscar_local _n(conscar_local)
#define conscdr_local _n(conscdr_local)
#define cons_local _n(cons_local)
#define consnil_alloc _n(consnil_alloc)
#define conscar_alloc _n(conscar_alloc)
#define conscdr_alloc _n(conscdr_alloc)
#define cons_alloc _n(cons_alloc)
#define refconscar_unsafe _n(refconscar_unsafe)
#define refconscdr_unsafe _n(refconscdr_unsafe)
#define getconscar_unsafe _n(getconscar_unsafe)
#define getconscdr_unsafe _n(getconscdr_unsafe)
#define getcons_unsafe _n(getcons_unsafe)
#define setconscar_unsafe _n(setconscar_unsafe)
#define setconscdr_unsafe _n(setconscdr_unsafe)
#define setcons_unsafe _n(setcons_unsafe)
#define setconscar_force _n(setconscar_force)
#define setconscdr_force _n(setconscdr_force)
#define setcons_force _n(setcons_force)
#define listp _n(listp)
#define consp _n(consp)
#define singlep _n(singlep)
#define vector2_heap _n(vector2_heap)
#define vector2_local _n(vector2_local)
#define vector2_alloc _n(vector2_alloc)
#define vector4_heap _n(vector4_heap)
#define vector4_local _n(vector4_local)
#define vector4_alloc _n(vector4_alloc)
#define vector8_heap _n(vector8_heap)
#define vector8_local _n(vector8_local)
#define vector8_alloc _n(vector8_alloc)
#define vector_heap _n(vector_heap)
#define vector_local _n(vector_local)
#define vector_alloc _n(vector_alloc)
#define vector_type_heap _n(vector_type_heap)
#define copy_vector4_alloc _n(copy_vector4_alloc)
#define copy_vector4_local _n(copy_vector4_local)
#define copy_vector4_heap _n(copy_vector4_heap)
#define copy_vector_alloc _n(copy_vector_alloc)
#define copy_vector_local _n(copy_vector_local)
#define copy_vector_heap _n(copy_vector_heap)
#define fixnump _n(fixnump)
#define make_fixnum_heap _n(make_fixnum_heap)
#define fixnum_heap _n(fixnum_heap)
#define fixnum_local _n(fixnum_local)
#define fixnum_alloc _n(fixnum_alloc)
#define fixnumh _n(fixnumh)
#define fixnuml _n(fixnuml)
#define fixnuma _n(fixnuma)
#define ptrfixnum _n(ptrfixnum)
#define reffixnum _n(reffixnum)
#define getfixnum _n(getfixnum)
#define setfixnum _n(setfixnum)
#define fixnumequal _n(fixnumequal)
#define fixnumcompare _n(fixnumcompare)
#define indexp _n(indexp)
#define index_heap _n(index_heap)
#define index_local _n(index_local)
#define index_alloc _n(index_alloc)
#define ptrindex _n(ptrindex)
#define refindex _n(refindex)
#define getindex _n(getindex)
#define setindex _n(setindex)
#define incindex _n(incindex)
#define decindex _n(decindex)
#define single_float_p _n(single_float_p)
#define single_float_heap _n(single_float_heap)
#define single_float_local _n(single_float_local)
#define single_float_alloc _n(single_float_alloc)
#define ptrsinglefloat _n(ptrsinglefloat)
#define refsinglefloat _n(refsinglefloat)
#define getsinglefloat _n(getsinglefloat)
#define setsinglefloat _n(setsinglefloat)
#define double_float_p _n(double_float_p)
#define double_float_heap _n(double_float_heap)
#define double_float_local _n(double_float_local)
#define double_float_alloc _n(double_float_alloc)
#define ptrdoublefloat _n(ptrdoublefloat)
#define refdoublefloat _n(refdoublefloat)
#define getdoublefloat _n(getdoublefloat)
#define setdoublefloat _n(setdoublefloat)
#define long_float_p _n(long_float_p)
#define long_float_heap _n(long_float_heap)
#define long_float_local _n(long_float_local)
#define long_float_alloc _n(long_float_alloc)
#define ptrlongfloat _n(ptrlongfloat)
#define reflongfloat _n(reflongfloat)
#define getlongfloat _n(getlongfloat)
#define setlongfloat _n(setlongfloat)
#define singleh _n(singleh)
#define doubleh _n(doubleh)
#define longh _n(longh)
#define queue_heap _n(queue_heap)
#define queue_local _n(queue_local)
#define queue_alloc _n(queue_alloc)
#define pushqueue_heap _n(pushqueue_heap)
#define pushqueue_local _n(pushqueue_local)
#define pushqueue_alloc _n(pushqueue_alloc)
#define dotqueue _n(dotqueue)
#define clearqueue _n(clearqueue)
#define rootqueue _n(rootqueue)
#define tailqueue _n(tailqueue)
#define firstqueue _n(firstqueue)
#define lastqueue _n(lastqueue)
#define nthqueue _n(nthqueue)

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

/* nil t */
void nil_heap(void);
void t_heap(void);

/* cons */
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

/* vector */
void vector2_heap(addr *ret, size_t size);
void vector2_local(LocalRoot local, addr *ret, size_t size);
void vector2_alloc(LocalRoot local, addr *ret, size_t size);
void vector4_heap(addr *ret, size_t size);
void vector4_local(LocalRoot local, addr *ret, size_t size);
void vector4_alloc(LocalRoot local, addr *ret, size_t size);
#ifdef LISP_ARCH_64BIT
void vector8_heap(addr *ret, size_t size);
void vector8_local(LocalRoot local, addr *ret, size_t size);
void vector8_alloc(LocalRoot local, addr *ret, size_t size);
#endif
void vector_heap(addr *ret, size_t size);
void vector_local(LocalRoot local, addr *ret, size_t size);
void vector_alloc(LocalRoot local, addr *ret, size_t size);
void vector_type_heap(addr *ret, addr pos, size_t size);
void copy_vector4_alloc(LocalRoot local, addr *ret, addr pos);
void copy_vector4_local(LocalRoot local, addr *ret, addr pos);
void copy_vector4_heap(addr *ret, addr pos);
void copy_vector_alloc(LocalRoot local, addr *ret, addr pos);
void copy_vector_local(LocalRoot local, addr *ret, addr pos);
void copy_vector_heap(addr *ret, addr pos);

/* fixnum */
int fixnump(addr pos);
void make_fixnum_heap(addr *ret, fixnum value);
void fixnum_heap(addr *ret, fixnum value);
void fixnum_local(LocalRoot local, addr *ret, fixnum value);
void fixnum_alloc(LocalRoot local, addr *ret, fixnum value);
addr fixnumh(fixnum value);
addr fixnuml(fixnum value);
addr fixnuma(LocalRoot local, fixnum value);
const fixnum *ptrfixnum(addr pos);
fixnum reffixnum(addr pos);
void getfixnum(addr pos, fixnum *ret);
void setfixnum(addr pos, fixnum value);
int fixnumequal(addr left, addr right);
int fixnumcompare(addr left, addr right);

/* index */
int indexp(addr pos);
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
int single_float_p(addr value);
void single_float_heap(addr *ret, single_float value);
void single_float_local(LocalRoot local, addr *ret, single_float value);
void single_float_alloc(LocalRoot local, addr *ret, single_float value);
const single_float *ptrsinglefloat(addr pos);
single_float refsinglefloat(addr pos);
void getsinglefloat(addr pos, single_float *ret);
void setsinglefloat(addr pos, single_float value);

int double_float_p(addr value);
void double_float_heap(addr *ret, double_float value);
void double_float_local(LocalRoot local, addr *ret, double_float value);
void double_float_alloc(LocalRoot local, addr *ret, double_float value);
const double_float *ptrdoublefloat(addr pos);
double_float refdoublefloat(addr pos);
void getdoublefloat(addr pos, double_float *ret);
void setdoublefloat(addr pos, double_float value);

int long_float_p(addr value);
void long_float_heap(addr *ret, long_float value);
void long_float_local(LocalRoot local, addr *ret, long_float value);
void long_float_alloc(LocalRoot local, addr *ret, long_float value);
const long_float *ptrlongfloat(addr pos);
long_float reflongfloat(addr pos);
void getlongfloat(addr pos, long_float *ret);
void setlongfloat(addr pos, long_float value);

addr singleh(single_float value);
addr doubleh(double_float value);
addr longh(long_float value);

/* queue */
void queue_heap(addr *ret);
void queue_local(LocalRoot local, addr *ret);
void queue_alloc(LocalRoot local, addr *ret);
void pushqueue_heap(addr pos, addr insert);
void pushqueue_local(LocalRoot local, addr pos, addr insert);
void pushqueue_alloc(LocalRoot local, addr pos, addr insert);
void dotqueue(addr pos, addr right);
void clearqueue(addr pos);

void rootqueue(addr pos, addr *ret);
void tailqueue(addr pos, addr *ret);
int firstqueue(addr pos, addr *ret);
int lastqueue(addr pos, addr *ret);
int nthqueue(addr pos, size_t index, addr *ret);

#endif

