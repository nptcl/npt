#include <string.h>
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "define.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

#define FIXNUM_CACHE		1024

/*
 *  alloc
 */
void alloc_cons(LocalRoot local, addr *ret)
{
	if (local)
		local_cons(local, ret);
	else
		heap_cons(ret);
}

void alloc_symbol(LocalRoot local, addr *ret)
{
	if (local)
		local_symbol(local, ret);
	else
		heap_symbol(ret);
}

void alloc_array2_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte16 array)
{
	if (local)
		local_array2(local, ret, type, array);
	else
		heap_array2(ret, type, array);
}

void alloc_array4_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte32 array)
{
	if (local)
		local_array4(local, ret, type, array);
	else
		heap_array4(ret, type, array);
}

void alloc_body2_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte16 body)
{
	if (local)
		local_body2(local, ret, type, body);
	else
		heap_body2(ret, type, body);
}

void alloc_body4_memory(LocalRoot local, addr *ret, enum LISPTYPE type, byte32 body)
{
	if (local)
		local_body4(local, ret, type, body);
	else
		heap_body4(ret, type, body);
}

void alloc_smallsize_memory(LocalRoot local,
		addr *ret, enum LISPTYPE type, byte array, byte body)
{
	if (local)
		local_smallsize(local, ret, type, array, body);
	else
		heap_smallsize(ret, type, array, body);
}

void alloc_arraybody_memory(LocalRoot local,
		addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
{
	if (local)
		local_arraybody(local, ret, type, array, body);
	else
		heap_arraybody(ret, type, array, body);
}

void alloc_array(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	if (local)
		local_array(local, ret, type, array);
	else
		heap_array(ret, type, array);
}

void alloc_body(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	if (local)
		local_body(local, ret, type, body);
	else
		heap_body(ret, type, body);
}

#ifdef LISP_ARCH_64BIT
void alloc_array8(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	if (local)
		local_array8(local, ret, type, array);
	else
		heap_array8(ret, type, array);
}

void alloc_body8(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	if (local)
		local_body8(local, ret, type, body);
	else
		heap_body8(ret, type, body);
}
#endif

#ifdef LISP_DEBUG
void alloc_array2_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	alloc_array2_memory(local, ret, type, (byte16)array);
}
void alloc_array4_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	alloc_array4_memory(local, ret, type, (byte32)array);
}
void alloc_body2_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	alloc_body2_memory(local, ret, type, (byte16)body);
}
void alloc_body4_debug(LocalRoot local, addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	alloc_body4_memory(local, ret, type, (byte32)body);
}
void alloc_smallsize_debug(LocalRoot local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	alloc_smallsize_memory(local, ret, type, (byte)array, (byte)body);
}
void alloc_arraybody_debug(LocalRoot local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	alloc_arraybody_memory(local, ret, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  init/free
 */
void build_object(void)
{
	addr pos;

	/* fixnum cache */
	heap_array4(&pos, LISPSYSTEM_FIXNUM_CACHE, (FIXNUM_CACHE * 2) + 1);
	SetConstant(CONSTANT_FIXNUM_CACHE, pos);

	/* fixnum max/min */
	make_fixnum_heap(&pos, FIXNUM_MAX);
	SetConstant(CONSTANT_FIXNUM_MAX, pos);
	make_fixnum_heap(&pos, FIXNUM_MIN);
	SetConstant(CONSTANT_FIXNUM_MIN, pos);
}


/*
 *  system object
 */
void nil_heap(void)
{
	addr pos, name;

	Nil = Unbound;
	heap_symbol(&pos); /* Don't use symbol_heap. */
	Nil = pos;
	strvect_char_heap(&name, "NIL");
	SetArrayA2_force(pos, SYMBOL_INDEX_SPECIAL, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_CDR, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_VALUE, Nil);
	SetValueSymbol(pos, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_FUNCTION, Unbound);
	SetArrayA2_force(pos, SYMBOL_INDEX_NAME, name);
	SetArrayA2_force(pos, SYMBOL_INDEX_PACKAGE, Nil); /* "COMMON-LISP" */
	SetArrayA2_force(pos, SYMBOL_INDEX_PLIST, Nil);
	SetArrayA2_force(pos, SYMBOL_INDEX_INFO, Nil);
	SetType(pos, LISPTYPE_NIL);
	SetStatusValue(pos, LISPSTATUS_SYSTEM, 1);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetChain(pos, 0xFF);
}

void t_heap(void)
{
	addr pos, name;

	heap_symbol(&pos); /* Don't use symbol_heap. */
	T = pos;
	strvect_char_heap(&name, "T");
	SetValueSymbol(pos, T);
	SetArrayA2_force(pos, SYMBOL_INDEX_FUNCTION, Unbound);
	SetArrayA2_force(pos, SYMBOL_INDEX_NAME, name);
	SetArrayA2_force(pos, SYMBOL_INDEX_PACKAGE, Nil); /* "COMMON-LISP" */
	SetType(pos, LISPTYPE_T);
	SetStatusValue(pos, LISPSTATUS_SYSTEM, 1);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	SetChain(pos, 0xFF);
}

/* cons */
void consnil_heap(addr *ret)
{
	heap_cons(ret);
}
void conscar_heap(addr *ret, addr left)
{
	heap_cons(ret);
	SetCar_Low(*ret, left);
}
void conscdr_heap(addr *ret, addr right)
{
	heap_cons(ret);
	SetCdr_Low(*ret, right);
}
void cons_heap(addr *ret, addr left, addr right)
{
	heap_cons(ret);
	SetCons_Low(*ret, left, right);
}

void consnil_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
}
void conscar_local(LocalRoot local, addr *ret, addr left)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCar_Low(*ret, left);
}
void conscdr_local(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCdr_Low(*ret, right);
}
void cons_local(LocalRoot local, addr *ret, addr left, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCons_Low(*ret, left, right);
}

void consnil_alloc(LocalRoot local, addr *ret)
{
	if (local)
		consnil_local(local, ret);
	else
		consnil_heap(ret);
}
void conscar_alloc(LocalRoot local, addr *ret, addr left)
{
	if (local)
		conscar_local(local, ret, left);
	else
		conscar_heap(ret, left);
}
void conscdr_alloc(LocalRoot local, addr *ret, addr right)
{
	if (local)
		conscdr_local(local, ret, right);
	else
		conscdr_heap(ret, right);
}
void cons_alloc(LocalRoot local, addr *ret, addr left, addr right)
{
	if (local)
		cons_local(local, ret, left, right);
	else
		cons_heap(ret, left, right);
}

addr refconscar_unsafe(addr pos)
{
	Check(! IsList(pos), "type error");
	return RefCar_Low(pos);
}
addr refconscdr_unsafe(addr pos)
{
	Check(! IsList(pos), "type error");
	return RefCdr_Low(pos);
}
void getconscar_unsafe(addr pos, addr *ret)
{
	Check(! IsList(pos), "type error");
	GetCar_Low(pos, ret);
}
void getconscdr_unsafe(addr pos, addr *ret)
{
	Check(! IsList(pos), "type error");
	GetCdr_Low(pos, ret);
}
void getcons_unsafe(addr pos, addr *left, addr *right)
{
	Check(! IsList(pos), "type error");
	GetCons_Low(pos, left, right);
}
void setconscar_unsafe(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCar_Low(pos, value);
}
void setconscdr_unsafe(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCdr_Low(pos, value);
}
void setcons_unsafe(addr pos, addr left, addr right)
{
	Check(! IsCons(pos), "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCons_Low(pos, left, right);
}

void setconscar_force(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	SetCar_force(pos, value);
}
void setconscdr_force(addr pos, addr value)
{
	Check(! IsCons(pos), "type error");
	SetCdr_force(pos, value);
}
void setcons_force(addr pos, addr left, addr right)
{
	Check(! IsArray(pos), "type error");
	SetCar_force(pos, left);
	SetCdr_force(pos, right);
}

/* list */
int listp(addr pos)
{
	return IsList(pos);
}

int consp(addr pos)
{
	return GetType(pos) == LISPTYPE_CONS;
}

int singlep(addr pos)
{
	if (GetType(pos) == LISPTYPE_CONS) {
		GetCdr(pos, &pos);
		return pos == Nil;
	}

	return 0;
}

/* vector */
void vector2_heap(addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	heap_array2(ret, LISPTYPE_VECTOR, size);
}
void vector2_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFUL < size, "size error");
	local_array2(local, ret, LISPTYPE_VECTOR, size);
}
void vector2_alloc(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	if (local)
		vector2_local(local, ret, size);
	else
		vector2_heap(ret, size);
}

void vector4_heap(addr *ret, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	heap_array4(ret, LISPTYPE_VECTOR, size);
}
void vector4_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFFFFFUL < size, "size error");
	local_array4(local, ret, LISPTYPE_VECTOR, size);
}
void vector4_alloc(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	if (local)
		vector4_local(local, ret, size);
	else
		vector4_heap(ret, size);
}

#ifdef LISP_ARCH_64BIT
void vector8_heap(addr *ret, size_t size)
{
	heap_array8(ret, LISPTYPE_VECTOR, size);
}
void vector8_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array8(local, ret, LISPTYPE_VECTOR, size);
}
void vector8_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector8_local(local, ret, size);
	else
		vector8_heap(ret, size);
}
#endif

void vector_heap(addr *ret, size_t size)
{
	heap_array(ret, LISPTYPE_VECTOR, size);
}
void vector_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array(local, ret, LISPTYPE_VECTOR, size);
}
void vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector_local(local, ret, size);
	else
		vector_heap(ret, size);
}

void vector_type_heap(addr *ret, addr pos, size_t size)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			vector2_heap(ret, size);
			break;

		case LISPSIZE_ARRAY4:
			vector4_heap(ret, size);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			vector8_heap(ret, size);
			break;
#endif

		default:
			Abort("Invalid vector type.");
			return;
	}
}

/* copy vector */
void copy_vector4_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr array, one;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	LenArrayA4(pos, &size);
	vector4_local(local, &array, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &one);
		SetArrayA4(array, i, one);
	}
	*ret = pos;
}

void copy_vector4_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_vector4_alloc(local, ret, pos);
}

void copy_vector4_heap(addr *ret, addr pos)
{
	copy_vector4_alloc(NULL, ret, pos);
}

void copy_vector_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr array, one;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	lenarray(pos, &size);

	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			vector2_alloc(local, &array, size);
			break;

		case LISPSIZE_ARRAY4:
			vector4_alloc(local, &array, size);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			vector8_alloc(local, &array, size);
			break;
#endif
		default:
			Abort("size error");
			return;
	}

	for (i = 0; i < size; i++) {
		getarray(pos, i, &one);
		setarray(array, i, one);
	}
	*ret = array;
}

void copy_vector_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_vector_alloc(local, ret, pos);
}

void copy_vector_heap(addr *ret, addr pos)
{
	copy_vector_alloc(NULL, ret, pos);
}

/* fixnum */
int fixnump(addr pos)
{
	return GetType(pos) == LISPTYPE_FIXNUM;
}

void make_fixnum_heap(addr *ret, fixnum value)
{
	addr pos;

	heap_body2(&pos, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	*ret = pos;
}

#define fixnum_cache_p(v) (-FIXNUM_CACHE <= (v) && (v) <= FIXNUM_CACHE)

void fixnum_heap(addr *ret, fixnum value)
{
	addr cache, pos;
	size_t index;

	/* make object */
	if (! fixnum_cache_p(value)) {
		make_fixnum_heap(ret, value);
		return;
	}

	/* cache */
	index = value + FIXNUM_CACHE;
	GetConst(FIXNUM_CACHE, &cache);
	GetArrayA4(cache, index, &pos);

	/* cache hit */
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	/* add cache */
	make_fixnum_heap(&pos, value);
	SetArrayA4(cache, index, pos);
	*ret = pos;
}

void fixnum_local(LocalRoot local, addr *ret, fixnum value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(*ret, value);
}

void fixnum_alloc(LocalRoot local, addr *ret, fixnum value)
{
	if (local)
		fixnum_local(local, ret, value);
	else
		fixnum_heap(ret, value);
}

addr fixnumh(fixnum value)
{
	addr pos;
	fixnum_heap(&pos, value);
	return pos;
}

addr fixnuml(fixnum value)
{
	addr pos;
	fixnum_local(Local_Thread, &pos, value);
	return pos;
}

addr fixnuma(LocalRoot local, fixnum value)
{
	addr pos;
	fixnum_alloc(local, &pos, value);
	return pos;
}

const fixnum *ptrfixnum(addr pos)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	return PtrFixnum_Low(pos);
}
fixnum reffixnum(addr pos)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	return RefFixnum_Low(pos);
}
void getfixnum(addr pos, fixnum *ret)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	GetFixnum_Low(pos, ret);
}
void setfixnum(addr pos, fixnum value)
{
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFixnum_Low(pos, value);
}

int fixnumequal(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type error");
	return RefBodyB2(left, fixnum) == RefBodyB2(right, fixnum);
}

int fixnumcompare(addr left, addr right)
{
	fixnum value1, value2;

	Check(GetType(left) != LISPTYPE_FIXNUM, "type error");
	Check(GetType(right) != LISPTYPE_FIXNUM, "type error");
	GetFixnum_Low(left, &value1);
	GetFixnum_Low(right, &value2);
	if (value1 < value2) return -1;
	if (value1 > value2) return 1;

	return 0;
}

/* index */
int indexp(addr pos)
{
	return GetType(pos) == LISPTYPE_INDEX;
}

void index_heap(addr *ret, size_t value)
{
	heap_body2(ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}

void index_local(LocalRoot local, addr *ret, size_t value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}

void index_alloc(LocalRoot local, addr *ret, size_t value)
{
	if (local)
		index_local(local, ret, value);
	else
		index_heap(ret, value);
}

const size_t *ptrindex(addr pos)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	return PtrIndex_Low(pos);
}
size_t refindex(addr pos)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	return RefIndex_Low(pos);
}
void getindex(addr pos, size_t *ret)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	GetIndex_Low(pos, ret);
}
void setindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetIndex_Low(pos, value);
}
void incindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	IncIndex_Low(pos, value);
}
void decindex(addr pos, size_t value)
{
	Check(GetType(pos) != LISPTYPE_INDEX, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	DecIndex_Low(pos, value);
}

/* float */
int short_float_p(addr value)
{
	enum LISPTYPE type = GetType(value);
	return type == LISPTYPE_SHORT_FLOAT
		|| type == LISPTYPE_SINGLE_FLOAT;
}

int single_float_p(addr value)
{
	return GetType(value) == LISPTYPE_SINGLE_FLOAT;
}

void single_float_heap(addr *ret, single_float value)
{
	heap_body2(ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void single_float_local(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void single_float_alloc(LocalRoot local, addr *ret, single_float value)
{
	if (local)
		single_float_local(local, ret, value);
	else
		single_float_heap(ret, value);
}

const single_float *ptrsinglefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	return PtrSingleFloat_Low(pos);
}
single_float refsinglefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	return RefSingleFloat_Low(pos);
}
void getsinglefloat(addr pos, single_float *ret)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	GetSingleFloat_Low(pos, ret);
}
void setsinglefloat(addr pos, single_float value)
{
	Check(GetType(pos) != LISPTYPE_SINGLE_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSingleFloat_Low(pos, value);
}

int double_float_p(addr value)
{
	return GetType(value) == LISPTYPE_DOUBLE_FLOAT;
}

void double_float_heap(addr *ret, double_float value)
{
	heap_body2(ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void double_float_local(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void double_float_alloc(LocalRoot local, addr *ret, double_float value)
{
	if (local)
		double_float_local(local, ret, value);
	else
		double_float_heap(ret, value);
}

const double_float *ptrdoublefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	return PtrDoubleFloat_Low(pos);
}
double_float refdoublefloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	return RefDoubleFloat_Low(pos);
}
void getdoublefloat(addr pos, double_float *ret)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	GetDoubleFloat_Low(pos, ret);
}
void setdoublefloat(addr pos, double_float value)
{
	Check(GetType(pos) != LISPTYPE_DOUBLE_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDoubleFloat_Low(pos, value);
}

int long_float_p(addr value)
{
	return GetType(value) == LISPTYPE_LONG_FLOAT;
}

void long_float_heap(addr *ret, long_float value)
{
	heap_body2(ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void long_float_local(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}

void long_float_alloc(LocalRoot local, addr *ret, long_float value)
{
	if (local)
		long_float_local(local, ret, value);
	else
		long_float_heap(ret, value);
}

const long_float *ptrlongfloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	return PtrLongFloat_Low(pos);
}
long_float reflongfloat(addr pos)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	return RefLongFloat_Low(pos);
}
void getlongfloat(addr pos, long_float *ret)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	GetLongFloat_Low(pos, ret);
}
void setlongfloat(addr pos, long_float value)
{
	Check(GetType(pos) != LISPTYPE_LONG_FLOAT, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLongFloat_Low(pos, value);
}

/* inplace */
addr singleh(single_float value)
{
	addr pos;
	single_float_heap(&pos, value);
	return pos;
}

addr doubleh(double_float value)
{
	addr pos;
	double_float_heap(&pos, value);
	return pos;
}

addr longh(long_float value)
{
	addr pos;
	long_float_heap(&pos, value);
	return pos;
}


/* queue */
void queue_heap(addr *ret)
{
	consnil_heap(ret);
}
void queue_local(LocalRoot local, addr *ret)
{
	consnil_local(local, ret);
}
void queue_alloc(LocalRoot local, addr *ret)
{
	consnil_alloc(local, ret);
}

void pushqueue_alloc(LocalRoot local, addr pos, addr insert)
{
	addr right, make;

	GetCdr(pos, &right);
	consnil_alloc(local, &make);
	SetCar(make, insert);
	if (right == Nil) {
		SetCons(pos, make, make);
	}
	else {
		SetCdr(right, make);
		SetCdr(pos, make);
	}
}
void pushqueue_heap(addr pos, addr insert)
{
	pushqueue_alloc(NULL, pos, insert);
}
void pushqueue_local(LocalRoot local, addr pos, addr insert)
{
	Check(local == NULL, "localroot error");
	pushqueue_alloc(local, pos, insert);
}
void dotqueue(addr pos, addr right)
{
	addr cons;

	GetCdr(pos, &cons);
	if (cons == NULL) {
		Abort("dotqueue error");
		return;
	}
	SetCdr(cons, right);
}
void clearqueue(addr pos)
{
	SetCons(pos, Nil, Nil);
}

void rootqueue(addr pos, addr *ret)
{
	GetCar(pos, ret);
}
void tailqueue(addr pos, addr *ret)
{
	GetCdr(pos, ret);
}
int firstqueue(addr pos, addr *ret)
{
	addr cons;

	rootqueue(pos, &cons);
	if (cons == Nil) return 1;
	GetCar(cons, ret);

	return 0;
}
int lastqueue(addr pos, addr *ret)
{
	addr cons;

	tailqueue(pos, &cons);
	if (cons == Nil) return 1;
	GetCar(cons, ret);

	return 0;
}
int nthqueue(addr pos, size_t index, addr *ret)
{
	size_t i;
	addr right;

	GetCar(pos, &right);
	for (i = 0; i < index; i++) {
		GetCdr(right, &right);
		if (right == Nil) return 1;
	}
	GetCar(right, ret);

	return 0;
}

