#include "alloc.h"
#include "memory.h"
#include "thread.h"

/*
 *  type
 */
_g enum LISPTYPE gettype(addr pos)
{
	if (pos == Unbound) return LISPSYSTEM_UNBOUND;
	if (pos == Nil) return LISPTYPE_NIL;
	if (pos == T) return LISPTYPE_T;

	return GetType(pos);
}


/*
 *  size class
 */
static int size2_memory(addr pos)
{
	enum LISPSIZE x = GetStatusSize(pos);
	return x == LISPSIZE_ARRAY2 || x == LISPSIZE_BODY2 || x == LISPSIZE_SMALLSIZE;
}

_g size_t getobjectlength(addr pos)
{
	Check(pos == Unbound, "unbound error");
	Check(GetType(pos) == LISPSYSTEM_SPACE, "type space error");
	Check(GetType(pos) == LISPSYSTEM_SPACE1, "type space1 error");
	Check(GetType(pos) == LISPSYSTEM_RESERVED, "type reserved error");

	return size2_memory(pos)? *PtrValue2L(pos): *PtrValueL(pos);
}

_g size_t getmemorylength(addr pos)
{
	size_t size;

	Check(pos == Unbound, "unbound error");
	switch (GetType(pos)) {
		case LISPSYSTEM_SPACE1:
			GetSizeSpace1(pos, &size);
			break;

		case LISPSYSTEM_SPACE:
			GetSizeSpace(pos, &size);
			break;

		case LISPSYSTEM_RESERVED:
			GetSizeReserved(pos, &size);
			break;

		default:
			return getobjectlength(pos);
	}

	return size;
}

_g int valid_header(addr pos)
{
	return (unsigned)GetType(pos) < LISPSYSTEM_CHECK;
}


/*
 *  lenarray
 */
_g void lenarrayA2(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "type error");
	*ret = GetLenArrayA2(pos);
}
_g void lenarraySS(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = GetLenArraySS(pos);
}
_g void lenarrayA4(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "type error");
	*ret = GetLenArrayA4(pos);
}
_g void lenarrayAB(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = GetLenArrayAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g void lenarrayA8(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "type error");
	*ret = (size_t)GetLenArrayA8(pos);
}
#endif
_g void lenarray(addr pos, size_t *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			*ret = GetLenArrayA2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*ret = GetLenArraySS(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*ret = GetLenArrayAB(pos);
			break;

		case LISPSIZE_ARRAY4:
			*ret = GetLenArrayA4(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			*ret = (size_t)GetLenArrayA8(pos);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

_g size_t lenarrayA2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "type error");
	return GetLenArrayA2(pos);
}
_g size_t lenarraySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return GetLenArraySS(pos);
}
_g size_t lenarrayA4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "type error");
	return GetLenArrayA4(pos);
}
_g size_t lenarrayABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return GetLenArrayAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g size_t lenarrayA8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "type error");
	return (size_t)GetLenArrayA8(pos);
}
#endif
_g size_t lenarrayr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return GetLenArrayA2(pos);

		case LISPSIZE_SMALLSIZE:
			return GetLenArraySS(pos);

		case LISPSIZE_ARRAYBODY:
			return GetLenArrayAB(pos);

		case LISPSIZE_ARRAY4:
			return GetLenArrayA4(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return (size_t)GetLenArrayA8(pos);
#endif
		default:
			Abort("type error");
			break;
	}
	return 0;
}


/*
 *  lenbody
 */
_g void lenbodyB2(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	*ret = GetLenBodyB2(pos);
}
_g void lenbodySS(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = GetLenBodySS(pos);
}
_g void lenbodyB4(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	*ret = GetLenBodyB4(pos);
}
_g void lenbodyAB(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g void lenbodyB8(addr pos, size_t *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	*ret = GetLenBodyB8(pos);
}
#endif
_g void lenbody(addr pos, size_t *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			*ret = GetLenBodyB2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*ret = GetLenBodySS(pos);
			break;

		case LISPSIZE_BODY4:
			*ret = GetLenBodyB4(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*ret = GetLenBodyAB(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			*ret = (size_t)GetLenBodyB8(pos);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

_g size_t lenbodyB2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	return GetLenBodyB2(pos);
}
_g size_t lenbodySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return GetLenBodySS(pos);
}
_g size_t lenbodyB4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	return GetLenBodyB4(pos);
}
_g size_t lenbodyABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g size_t lenbodyB8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	return GetLenBodyB8(pos);
}
#endif
_g size_t lenbodyr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			return GetLenBodyB2(pos);

		case LISPSIZE_SMALLSIZE:
			return GetLenBodySS(pos);

		case LISPSIZE_BODY4:
			return GetLenBodyB4(pos);

		case LISPSIZE_ARRAYBODY:
			return GetLenBodyAB(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			return (size_t)GetLenBodyB8(pos);
#endif

		default:
			Abort("type error");
			break;
	}

	return 0;
}


/*
 *  posbody
 */
_g void posbodySSa(addr pos, size_t array, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	*ret = PtrBodySSa(pos, array);
}
_g void posbodyABa(addr pos, size_t array, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	*ret = PtrBodyABa(pos, array);
}
_g void posbodyB2(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	*ret = PtrBodyB2(pos);
}
_g void posbodySS(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	*ret = PtrBodySS(pos);
}
_g void posbodyB4(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	*ret = PtrBodyB4(pos);
}
_g void posbodyAB(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	*ret = PtrBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g void posbodyB8(addr pos, addr *ret)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	*ret = PtrBodyB8(pos);
}
#endif
_g void posbody(addr pos, addr *ret)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			PosBodyB2(pos, ret);
			break;

		case LISPSIZE_SMALLSIZE:
			PosBodySS(pos, ret);
			break;

		case LISPSIZE_BODY4:
			PosBodyB4(pos, ret);
			break;

		case LISPSIZE_ARRAYBODY:
			PosBodyAB(pos, ret);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			PosBodyB8(pos, ret);
			break;
#endif

		default:
			Abort("size error");
			break;
	}
}

_g addr posbodySSar(addr pos, size_t array)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	return PtrBodySSa(pos, array);
}
_g addr posbodyABar(addr pos, size_t array)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	return PtrBodyABa(pos, array);
}
_g addr posbodyB2r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "type error");
	return PtrBodyB2(pos);
}
_g addr posbodySSr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	return PtrBodySS(pos);
}
_g addr posbodyB4r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "type error");
	return PtrBodyB4(pos);
}
_g addr posbodyABr(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	return PtrBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g addr posbodyB8r(addr pos)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "type error");
	return PtrBodyB8(pos);
}
#endif
_g addr posbodyr(addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			return PtrBodyB2(pos);

		case LISPSIZE_SMALLSIZE:
			return PtrBodySS(pos);

		case LISPSIZE_BODY4:
			return PtrBodyB4(pos);

		case LISPSIZE_ARRAYBODY:
			return PtrBodyAB(pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			return PtrBodyB8(pos);
#endif

		default:
			Abort("size error");
			break;
	}

	return NULL;
}


/*
 *  posbodylen
 */
_g void posbodylenSSa(addr pos, size_t array, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "type error");
	Check(array != GetLenArraySS(pos), "array length error");
	*body = PtrBodySSa(pos, array);
	*len = GetLenBodySS(pos);
}
_g void posbodylenABa(addr pos, size_t array, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "type error");
	Check(array != GetLenArrayAB(pos), "array length error");
	*body = PtrBodyABa(pos, array);
	*len = GetLenBodyAB(pos);
}
_g void posbodylenB2(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY2, "size error");
	*body = PtrBodyB2(pos);
	*len = GetLenBodyB2(pos);
}
_g void posbodylenSS(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	*body = PtrBodySS(pos);
	*len = GetLenBodySS(pos);
}
_g void posbodylenB4(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY4, "size error");
	*body = PtrBodyB4(pos);
	*len = GetLenBodyB4(pos);
}
_g void posbodylenAB(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	*body = PtrBodyAB(pos);
	*len = GetLenBodyAB(pos);
}
#ifdef LISP_ARCH_64BIT
_g void posbodylenB8(addr pos, addr *body, size_t *len)
{
	Check(GetStatusSize(pos) != LISPSIZE_BODY8, "size error");
	*body = PtrBodyB8(pos);
	*len = GetLenBodyB8(pos);
}
#endif
_g void posbodylen(addr pos, addr *body, size_t *len)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_BODY2:
			*body = PtrBodyB2(pos);
			*len = GetLenBodyB2(pos);
			break;

		case LISPSIZE_SMALLSIZE:
			*body = PtrBodySS(pos);
			*len = GetLenBodySS(pos);
			break;

		case LISPSIZE_BODY4:
			*body = PtrBodyB4(pos);
			*len = GetLenBodyB4(pos);
			break;

		case LISPSIZE_ARRAYBODY:
			*body = PtrBodyAB(pos);
			*len = GetLenBodyAB(pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_BODY8:
			*body = PtrBodyB8(pos);
			*len = (size_t)GetLenBodyB8(pos);
			break;
#endif

		default:
			Abort2("size error: %d", (int)GetStatusSize(pos));
			break;
	}
}


/*
 *  getarray
 */
_g void getarrayA2(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	*value = PtrArrayA2(pos)[index];
}
_g void getarraySS(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	*value = PtrArraySS(pos)[index];
}
_g void getarrayA4(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	*value = PtrArrayA4(pos)[index];
}
_g void getarrayAB(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	*value = PtrArrayAB(pos)[index];
}
#ifdef LISP_ARCH_64BIT
_g void getarrayA8(addr pos, size_t index, addr *value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	*value = PtrArrayA8(pos)[index];
}
#endif
_g void getarray(addr pos, size_t index, addr *value)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			getarrayA2(pos, index, value);
			break;

		case LISPSIZE_SMALLSIZE:
			getarraySS(pos, index, value);
			break;

		case LISPSIZE_ARRAY4:
			getarrayA4(pos, index, value);
			break;

		case LISPSIZE_ARRAYBODY:
			getarrayAB(pos, index, value);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			getarrayA8(pos, index, value);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

_g addr refarrayA2(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	return PtrArrayA2(pos)[index];
}
_g addr refarraySS(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	return PtrArraySS(pos)[index];
}
_g addr refarrayA4(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	return PtrArrayA4(pos)[index];
}
_g addr refarrayAB(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	return PtrArrayAB(pos)[index];
}
#ifdef LISP_ARCH_64BIT
_g addr refarrayA8(addr pos, size_t index)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	return PtrArrayA8(pos)[index];
}
#endif
_g addr refarray(addr pos, size_t index)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return PtrArrayA2(pos)[index];

		case LISPSIZE_SMALLSIZE:
			return refarraySS(pos, index);

		case LISPSIZE_ARRAY4:
			return refarrayA4(pos, index);

		case LISPSIZE_ARRAYBODY:
			return refarrayAB(pos, index);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return refarrayA8(pos, index);
#endif

		default:
			Abort("type error");
			break;
	}

	return NULL;
}


/*
 *  setarray
 */
_g int checkdynamic(addr pos, addr value)
{
	int check;

	check = (value != Unbound) &&
		(value != NULL) &&
		(! GetStatusDynamic(pos)) &&
		GetStatusDynamic(value);
	if (check) {
		/* for breakpoint */
		Debug("checkdynamic error.");
		return 1;
	}
	return 0;
}

_g void setarray_chain(addr *ptr, addr value)
{
	byte *p;

	/* decrement */
	if (*ptr != Unbound) {
		p = (byte *)PtrChain(*ptr);
		Check(*p == 0, "Chain decrement error");
		if (*p != 0xFF)
			(*p)--;
	}

	/* increment */
	if (value != Unbound) {
		p = (byte *)PtrChain(value);
		if (*p != 0xFF)
			(*p)++;
	}

	/* setq */
	*ptr = value;
}

#if 0
#define SetArray_chain(array, pos, index, value) \
	setarray_chain(array(pos) + index, value)
#else
#define SetArray_chain(array, pos, index, value) \
	(array(pos)[index] = value)
#endif

_g void setarrayA2(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA2, pos, index, value);
}
_g void setarraySS(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArraySS(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArraySS, pos, index, value);
}
_g void setarrayA4(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA4, pos, index, value);
}
_g void setarrayAB(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayAB, pos, index, value);
}
#ifdef LISP_ARCH_64BIT
_g void setarrayA8(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	CheckDynamic(pos, value);
	SetArray_chain(PtrArrayA8, pos, index, value);
}
#endif
_g void setarray(addr pos, size_t index, addr value)
{
	Check(GetStatusReadOnly(pos), "readonly error");
	Check(lenarrayr(pos) <= index, "length error");
	CheckDynamic(pos, value);

	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			SetArray_chain(PtrArrayA2, pos, index, value);
			break;

		case LISPSIZE_SMALLSIZE:
			SetArray_chain(PtrArraySS, pos, index, value);
			break;

		case LISPSIZE_ARRAY4:
			SetArray_chain(PtrArrayA4, pos, index, value);
			break;

		case LISPSIZE_ARRAYBODY:
			SetArray_chain(PtrArrayAB, pos, index, value);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			SetArray_chain(PtrArrayA8, pos, index, value);
			break;
#endif

		default:
			Abort("type error");
			break;
	}
}

_g void setarrayA2_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2, "size error");
	Check(GetLenArrayA2(pos) <= index, "length error");
	SetArray_chain(PtrArrayA2, pos, index, value);
}
_g void setarraySS_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	Check(GetLenArraySS(pos) <= index, "length error");
	SetArray_chain(PtrArraySS, pos, index, value);
}
_g void setarrayA4_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4, "size error");
	Check(GetLenArrayA4(pos) <= index, "length error");
	SetArray_chain(PtrArrayA4, pos, index, value);
}
_g void setarrayAB_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	Check(GetLenArrayAB(pos) <= index, "length error");
	SetArray_chain(PtrArrayAB, pos, index, value);
}
#ifdef LISP_ARCH_64BIT
_g void setarrayA8_force(addr pos, size_t index, addr value)
{
	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	Check(GetLenArrayA8(pos) <= index, "length error");
	SetArray_chain(PtrArrayA8, pos, index, value);
}
#endif


/*
 *  object
 */
_g void nilarray2(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2 &&
			GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	array = PtrArrayA2(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
_g void nilarray4(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4 &&
			GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	array = PtrArrayA4(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
#ifdef LISP_ARCH_64BIT
_g void nilarray8(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	array = PtrArrayA8(pos);
	for (i = 0; i < size; i++)
		array[i] = Nil;
}
#endif

_g void unboundarray2(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY2 &&
			GetStatusSize(pos) != LISPSIZE_SMALLSIZE, "size error");
	array = PtrArrayA2(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
_g void unboundarray4(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY4 &&
			GetStatusSize(pos) != LISPSIZE_ARRAYBODY, "size error");
	array = PtrArrayA4(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
#ifdef LISP_ARCH_64BIT
_g void unboundarray8(addr pos, size_t size)
{
	size_t i;
	addr *array;

	Check(GetStatusSize(pos) != LISPSIZE_ARRAY8, "size error");
	array = PtrArrayA8(pos);
	for (i = 0; i < size; i++)
		array[i] = Unbound;
}
#endif

_g size_t size_split(size_t size)
{
	AlignSize8Front(size, &size);
	return size;
}

