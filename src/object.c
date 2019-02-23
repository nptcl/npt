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
#include "symbol.h"

#define FIXNUM_CACHE		1024

/*
 *  alloc
 */
addr allocr_cons(LocalRoot local)
{
	if (local)
		return localr_cons(local);
	else
		return heapr_cons();
}

addr allocr_symbol(LocalRoot local)
{
	if (local)
		return localr_symbol(local);
	else
		return heapr_symbol();
}

addr allocr_array2_memory(LocalRoot local, enum LISPTYPE type, byte16 array)
{
	if (local)
		return localr_array2(local, type, array);
	else
		return heapr_array2(type, array);
}

addr allocr_array4_memory(LocalRoot local, enum LISPTYPE type, byte32 array)
{
	if (local)
		return localr_array4(local, type, array);
	else
		return heapr_array4(type, array);
}

addr allocr_body2_memory(LocalRoot local, enum LISPTYPE type, byte16 body)
{
	if (local)
		return localr_body2(local, type, body);
	else
		return heapr_body2(type, body);
}

addr allocr_body4_memory(LocalRoot local, enum LISPTYPE type, byte32 body)
{
	if (local)
		return localr_body4(local, type, body);
	else
		return heapr_body4(type, body);
}

addr allocr_smallsize_memory(LocalRoot local,
		enum LISPTYPE type, byte array, byte body)
{
	if (local)
		return localr_smallsize(local, type, array, body);
	else
		return heapr_smallsize(type, array, body);
}

addr allocr_arraybody_memory(LocalRoot local,
		enum LISPTYPE type, byte16 array, byte16 body)
{
	if (local)
		return localr_arraybody(local, type, array, body);
	else
		return heapr_arraybody(type, array, body);
}

addr allocr_array(LocalRoot local, enum LISPTYPE type, size_t array)
{
	if (local)
		return localr_array(local, type, array);
	else
		return heapr_array(type, array);
}

addr allocr_body(LocalRoot local, enum LISPTYPE type, size_t body)
{
	if (local)
		return localr_body(local, type, body);
	else
		return heapr_body(type, body);
}

#ifdef LISP_ARCH_64BIT
addr allocr_array8(LocalRoot local, enum LISPTYPE type, size_t array)
{
	if (local)
		return localr_array8(local, type, array);
	else
		return heapr_array8(type, array);
}

addr allocr_body8(LocalRoot local, enum LISPTYPE type, size_t body)
{
	if (local)
		return localr_body8(local, type, body);
	else
		return heapr_body8(type, body);
}
#endif

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
addr allocr_array2_debug(LocalRoot local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	return allocr_array2_memory(local, type, (byte16)array);
}
addr allocr_array4_debug(LocalRoot local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return allocr_array4_memory(local, type, (byte32)array);
}
addr allocr_body2_debug(LocalRoot local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	return allocr_body2_memory(local, type, (byte16)body);
}
addr allocr_body4_debug(LocalRoot local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	return allocr_body4_memory(local, type, (byte32)body);
}
addr allocr_smallsize_debug(LocalRoot local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	return allocr_smallsize_memory(local, type, (byte)array, (byte)body);
}
addr allocr_arraybody_debug(LocalRoot local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	return allocr_arraybody_memory(local, type, (byte16)array, (byte16)body);
}

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
static void build_object_character(void)
{
	addr pos;

	/* character cache */
	heap_array4(&pos, LISPTYPE_SYSTEM, CHARACTER_CACHE);
	SetConstant(CONSTANT_CHARACTER_CACHE, pos);
}

static void build_object_fixnum(void)
{
	addr pos;

	/* fixnum cache */
	heap_array4(&pos, LISPTYPE_SYSTEM, (FIXNUM_CACHE * 2) + 1);
	SetConstant(CONSTANT_FIXNUM_CACHE, pos);

	/* fixnum max/min */
	make_fixnum_heap(&pos, FIXNUM_MAX);
	SetConstant(CONSTANT_FIXNUM_MAX, pos);
	make_fixnum_heap(&pos, FIXNUM_MIN);
	SetConstant(CONSTANT_FIXNUM_MIN, pos);
}

void build_object(void)
{
	build_object_character();
	build_object_fixnum();
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
	SetArrayA2_force(pos, SYMBOL_INDEX_STACK, Nil);
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
}

/* cons */
addr consnil_heapr(void)
{
	return heapr_cons();
}
void consnil_heap(addr *ret)
{
	heap_cons(ret);
}
addr conscar_heapr(addr left)
{
	addr pos;
	heap_cons(&pos);
	SetCar_Low(pos, left);
	return pos;
}
void conscar_heap(addr *ret, addr left)
{
	heap_cons(ret);
	SetCar_Low(*ret, left);
}
addr conscdr_heapr(addr right)
{
	addr pos;
	heap_cons(&pos);
	SetCdr_Low(pos, right);
	return pos;
}
void conscdr_heap(addr *ret, addr right)
{
	heap_cons(ret);
	SetCdr_Low(*ret, right);
}
addr cons_heapr(addr left, addr right)
{
	addr pos;
	heap_cons(&pos);
	SetCons_Low(pos, left, right);
	return pos;
}
void cons_heap(addr *ret, addr left, addr right)
{
	heap_cons(ret);
	SetCons_Low(*ret, left, right);
}

addr consnil_localr(LocalRoot local)
{
	Check(local == NULL, "local error");
	return localr_cons(local);
}
void consnil_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
}
addr conscar_localr(LocalRoot local, addr left)
{
	addr pos;
	Check(local == NULL, "local error");
	local_cons(local, &pos);
	SetCar_Low(pos, left);
	return pos;
}
void conscar_local(LocalRoot local, addr *ret, addr left)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCar_Low(*ret, left);
}
addr conscdr_localr(LocalRoot local, addr right)
{
	addr pos;
	Check(local == NULL, "local error");
	local_cons(local, &pos);
	SetCdr_Low(pos, right);
	return pos;
}
void conscdr_local(LocalRoot local, addr *ret, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCdr_Low(*ret, right);
}
addr cons_localr(LocalRoot local, addr left, addr right)
{
	addr pos;
	Check(local == NULL, "local error");
	local_cons(local, &pos);
	SetCons_Low(pos, left, right);
	return pos;
}
void cons_local(LocalRoot local, addr *ret, addr left, addr right)
{
	Check(local == NULL, "local error");
	local_cons(local, ret);
	SetCons_Low(*ret, left, right);
}

addr consnil_allocr(LocalRoot local)
{
	if (local)
		return consnil_localr(local);
	else
		return consnil_heapr();
}
void consnil_alloc(LocalRoot local, addr *ret)
{
	if (local)
		consnil_local(local, ret);
	else
		consnil_heap(ret);
}
addr conscar_allocr(LocalRoot local, addr left)
{
	if (local)
		return conscar_localr(local, left);
	else
		return conscar_heapr(left);
}
void conscar_alloc(LocalRoot local, addr *ret, addr left)
{
	if (local)
		conscar_local(local, ret, left);
	else
		conscar_heap(ret, left);
}
addr conscdr_allocr(LocalRoot local, addr right)
{
	if (local)
		return conscdr_localr(local, right);
	else
		return conscdr_heapr(right);
}
void conscdr_alloc(LocalRoot local, addr *ret, addr right)
{
	if (local)
		conscdr_local(local, ret, right);
	else
		conscdr_heap(ret, right);
}
addr cons_allocr(LocalRoot local, addr left, addr right)
{
	if (local)
		return cons_localr(local, left, right);
	else
		return cons_heapr(left, right);
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
	SetCons_Low(pos, left, right);
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

void list_alloc_stdarg(LocalRoot local, addr *ret, va_list args)
{
	addr left, right, next;

	left = va_arg(args, addr);
	if (left == NULL) {
		*ret = Nil;
		return;
	}
	conscar_alloc(local, &right, left);
	*ret = right;

	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL) break;
		conscar_alloc(local, &next, left);
		SetCdr(right, next);
		right = next;
	}
}

addr list_heapr(addr pos, ...)
{
	addr left, right, next;
	va_list args;

	if (pos == NULL)
		return Nil;
	conscar_heap(&right, pos);
	pos = right;

	va_start(args, pos);
	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL) break;
		conscar_heap(&next, left);
		SetCdr(right, next);
		right = next;
	}
	va_end(args);

	return pos;
}

addr list_localr(LocalRoot local, ...)
{
	addr pos;
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, local);
	list_alloc_stdarg(local, &pos, args);
	va_end(args);

	return pos;
}

addr list_allocr(LocalRoot local, ...)
{
	addr pos;
	va_list args;

	va_start(args, local);
	list_alloc_stdarg(local, &pos, args);
	va_end(args);

	return pos;
}

void list_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_alloc_stdarg(NULL, ret, args);
	va_end(args);
}

void list_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	list_alloc_stdarg(local, ret, args);
	va_end(args);
}

void list_alloc(LocalRoot local, addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	list_alloc_stdarg(local, ret, args);
	va_end(args);
}

/* vector */
addr vector2_heapr(size_t size)
{
	Check(0xFFFFUL < size, "size error");
	return heapr_array2(LISPTYPE_VECTOR, size);
}
void vector2_heap(addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	heap_array2(ret, LISPTYPE_VECTOR, size);
}
addr vector2_localr(LocalRoot local, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFUL < size, "size error");
	return localr_array2(local, LISPTYPE_VECTOR, size);
}
void vector2_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFUL < size, "size error");
	local_array2(local, ret, LISPTYPE_VECTOR, size);
}
addr vector2_allocr(LocalRoot local, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	if (local)
		return vector2_localr(local, size);
	else
		return vector2_heapr(size);
}
void vector2_alloc(LocalRoot local, addr *ret, size_t size)
{
	Check(0xFFFFUL < size, "size error");
	if (local)
		vector2_local(local, ret, size);
	else
		vector2_heap(ret, size);
}

addr vector4_heapr(size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	return heapr_array4(LISPTYPE_VECTOR, size);
}
void vector4_heap(addr *ret, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	heap_array4(ret, LISPTYPE_VECTOR, size);
}
addr vector4_localr(LocalRoot local, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	Check(local == NULL, "local error");
	return localr_array4(local, LISPTYPE_VECTOR, size);
}
void vector4_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	Check(0xFFFFFFFFUL < size, "size error");
	local_array4(local, ret, LISPTYPE_VECTOR, size);
}
addr vector4_allocr(LocalRoot local, size_t size)
{
	Check(0xFFFFFFFFUL < size, "size error");
	if (local)
		return vector4_localr(local, size);
	else
		return vector4_heapr(size);
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
addr vector8_heapr(size_t size)
{
	return heapr_array8(LISPTYPE_VECTOR, size);
}
void vector8_heap(addr *ret, size_t size)
{
	heap_array8(ret, LISPTYPE_VECTOR, size);
}
addr vector8_localr(LocalRoot local, size_t size)
{
	Check(local == NULL, "local error");
	return localr_array8(local, LISPTYPE_VECTOR, size);
}
void vector8_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array8(local, ret, LISPTYPE_VECTOR, size);
}
addr vector8_allocr(LocalRoot local, size_t size)
{
	if (local)
		return vector8_localr(local, size);
	else
		return vector8_heapr(size);
}
void vector8_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector8_local(local, ret, size);
	else
		vector8_heap(ret, size);
}
#endif

addr vector_heapr(size_t size)
{
	return heapr_array(LISPTYPE_VECTOR, size);
}
void vector_heap(addr *ret, size_t size)
{
	heap_array(ret, LISPTYPE_VECTOR, size);
}
addr vector_localr(LocalRoot local, size_t size)
{
	Check(local == NULL, "local error");
	return localr_array(local, LISPTYPE_VECTOR, size);
}
void vector_local(LocalRoot local, addr *ret, size_t size)
{
	Check(local == NULL, "local error");
	local_array(local, ret, LISPTYPE_VECTOR, size);
}
addr vector_allocr(LocalRoot local, size_t size)
{
	if (local)
		return vector_localr(local, size);
	else
		return vector_heapr(size);
}
void vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	if (local)
		vector_local(local, ret, size);
	else
		vector_heap(ret, size);
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
			fmte("size error", NULL);
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
addr make_fixnum_heapr(fixnum value)
{
	addr pos;

	heap_body2(&pos, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);

	return pos;
}
void make_fixnum_heap(addr *ret, fixnum value)
{
	*ret = make_fixnum_heapr(value);
}

addr fixnum_heapr(fixnum value)
{
	addr cache, pos;
	size_t index;

	if (-FIXNUM_CACHE <= value && value <= FIXNUM_CACHE) {
		index = value + FIXNUM_CACHE;
		GetConst(FIXNUM_CACHE, &cache);
		GetArrayA4(cache, index, &pos);
		if (pos != Nil) return pos;
		make_fixnum_heap(&pos, value);
		SetArrayA4(cache, index, pos);
		return pos;
	}
	return make_fixnum_heapr(value);
}
void fixnum_heap(addr *ret, fixnum value)
{
	*ret = fixnum_heapr(value);
}

addr fixnum_localr(LocalRoot local, fixnum value)
{
	addr pos;
	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(pos, value);
	return pos;
}
void fixnum_local(LocalRoot local, addr *ret, fixnum value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_FIXNUM, sizeof(value));
	SetFixnum_Low(*ret, value);
}

addr fixnum_allocr(LocalRoot local, fixnum value)
{
	if (local)
		return fixnum_localr(local, value);
	else
		return fixnum_heapr(value);
}
void fixnum_alloc(LocalRoot local, addr *ret, fixnum value)
{
	if (local)
		fixnum_local(local, ret, value);
	else
		fixnum_heap(ret, value);
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
addr index_heapr(size_t value)
{
	addr pos;
	heap_body2(&pos, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(pos, value);
	return pos;
}
void index_heap(addr *ret, size_t value)
{
	heap_body2(ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}
addr index_localr(LocalRoot local, size_t value)
{
	addr pos;
	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(pos, value);
	return pos;
}
void index_local(LocalRoot local, addr *ret, size_t value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_INDEX, sizeof(value));
	SetIndex_Low(*ret, value);
}
addr index_allocr(LocalRoot local, size_t value)
{
	if (local)
		return index_localr(local, value);
	else
		return index_heapr(value);
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
addr single_float_heapr(single_float value)
{
	addr pos;
	heap_body2(&pos, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void single_float_heap(addr *ret, single_float value)
{
	heap_body2(ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr single_float_localr(LocalRoot local, single_float value)
{
	addr pos;
	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void single_float_local(LocalRoot local, addr *ret, single_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_SINGLE_FLOAT, sizeof(value));
	SetSingleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr single_float_allocr(LocalRoot local, single_float value)
{
	if (local)
		return single_float_localr(local, value);
	else
		return single_float_heapr(value);
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

addr double_float_heapr(double_float value)
{
	addr pos;
	heap_body2(&pos, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void double_float_heap(addr *ret, double_float value)
{
	heap_body2(ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr double_float_localr(LocalRoot local, double_float value)
{
	addr pos;
	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void double_float_local(LocalRoot local, addr *ret, double_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_DOUBLE_FLOAT, sizeof(value));
	SetDoubleFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr double_float_allocr(LocalRoot local, double_float value)
{
	if (local)
		return double_float_localr(local, value);
	else
		return double_float_heapr(value);
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

addr long_float_heapr(long_float value)
{
	addr pos;
	heap_body2(&pos, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void long_float_heap(addr *ret, long_float value)
{
	heap_body2(ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr long_float_localr(LocalRoot local, long_float value)
{
	addr pos;
	Check(local == NULL, "local error");
	local_body2(local, &pos, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(pos, value);
	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	return pos;
}
void long_float_local(LocalRoot local, addr *ret, long_float value)
{
	Check(local == NULL, "local error");
	local_body2(local, ret, LISPTYPE_LONG_FLOAT, sizeof(value));
	SetLongFloat_Low(*ret, value);
	SetStatusValue(*ret, LISPSTATUS_READONLY, 1);
}
addr long_float_allocr(LocalRoot local, long_float value)
{
	if (local)
		return long_float_localr(local, value);
	else
		return long_float_heapr(value);
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

/* queue */
addr queue_heapr(void)
{
	return consnil_heapr();
}
addr queue_localr(LocalRoot local)
{
	return consnil_localr(local);
}
addr queue_allocr(LocalRoot local)
{
	return consnil_allocr(local);
}
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

addr rootqueuer(addr pos)
{
	return RefCar(pos);
}
addr tailqueuer(addr pos)
{
	return RefCdr(pos);
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

