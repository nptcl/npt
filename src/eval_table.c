#include "copy.h"
#include "equal.h"
#include "eval.h"
#include "eval_table.h"
#include "heap.h"
#include "local.h"
#include "object.h"
#include "parse.h"

enum TABLEVALUE_INDEX {
	TABLEVALUE_INDEX_NAME,
	TABLEVALUE_INDEX_TYPE,
	TABLEVALUE_INDEX_SIZE
};
enum TABLEFUNCTION_INDEX {
	TABLEFUNCTION_INDEX_NAME,
	TABLEFUNCTION_INDEX_TYPE,
	TABLEFUNCTION_INDEX_SIZE
};
enum TABLECALL_INDEX {
	TABLECALL_INDEX_VALUE,
	TABLECALL_INDEX_TYPE,
	TABLECALL_INDEX_SIZE
};
enum TABLETAGBODY_INDEX {
	TABLETAGBODY_INDEX_TAG,
	TABLETAGBODY_INDEX_SIZE
};
enum TABLEBLOCK_INDEX {
	TABLEBLOCK_INDEX_NAME,
	TABLEBLOCK_INDEX_SIZE
};

struct tablevalue {
	unsigned specialp : 1;
	unsigned dynamic : 1;
	unsigned reference : 1;
	unsigned check : 1;
	enum IgnoreType ignore : 4; /* signed */
};
struct tablefunction {
	unsigned globalp : 1;
	unsigned dynamic : 1;
	unsigned reference : 1;
	unsigned check : 1;
	enum IgnoreType ignore : 4; /* signed */
	enum InlineType Inline : 4; /* signed */
};
struct tablecall {
	unsigned check : 1;
};
struct tabletagbody {
	unsigned reference : 1;
};

#define PtrBody_value(x)		PtrEvalBody((x), TABLEVALUE_INDEX_SIZE)
#define PtrBody_function(x)		PtrEvalBody((x), TABLEFUNCTION_INDEX_SIZE)
#define PtrBody_call(x)			PtrEvalBody((x), TABLECALL_INDEX_SIZE)
#define PtrBody_tagbody(x)		PtrEvalBody((x), TABLETAGBODY_INDEX_SIZE)

#define StructTableValue(p)		((struct tablevalue *)(PtrBody_value(p)))
#define StructTableFunction(p)	((struct tablefunction *)(PtrBody_function(p)))
#define StructTableCall(p)		((struct tablecall *)PtrBody_call(p))
#define StructTableTagBody(p)	((struct tabletagbody *)PtrBody_tagbody(p))

#define CheckTableValue(pos)	Check(! eval_tablevalue_p(pos), "type error")
#define CheckTableFunction(pos)	Check(! eval_tablefunction_p(pos), "type error")
#define CheckTableCall(pos)		Check(! eval_tablecall_p(pos), "type error")
#define CheckTableTagBody(pos)	Check(! eval_tabletagbody_p(pos), "type error")
#define CheckTableBlock(pos)	Check(! eval_tableblock_p(pos), "type error")


/*
 *  tablevalue
 */
static void alloc_tablevalue(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEVALUE,
			TABLEVALUE_INDEX_SIZE,
			sizeoft(struct tablevalue));
}

_g void make_tablevalue(LocalRoot local, addr symbol, addr *ret)
{
	addr pos;
	struct tablevalue *ptr;

	alloc_tablevalue(local, &pos);
	ptr = StructTableValue(pos);
	memset(ptr, 0, sizeoft(struct tablevalue));
	SetEval(pos, TABLEVALUE_INDEX_NAME, symbol);
	*ret = pos;
}

_g void copy_tablevalue(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableValue(pos);
	alloc_tablevalue(local, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_value(one), PtrBody_value(pos), size);

	for (i = 0; i < TABLEVALUE_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(local, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

_g void copylocal_tablevalue(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableValue(pos);
	alloc_tablevalue(local, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_value(one), PtrBody_value(pos), size);

	for (i = 0; i < TABLEVALUE_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copylocal_object(local, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

_g void getname_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_NAME, ret);
}
_g void setname_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_NAME, value);
}

_g void gettype_tablevalue(addr pos, addr *ret)
{
	CheckTableValue(pos);
	GetEval(pos, TABLEVALUE_INDEX_TYPE, ret);
}
_g void settype_tablevalue(addr pos, addr value)
{
	CheckTableValue(pos);
	SetEval(pos, TABLEVALUE_INDEX_TYPE, value);
}

_g int getspecialp_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->specialp;
}
_g void setspecialp_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->specialp = (value != 0);
}

_g int getdynamic_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->dynamic;
}
_g void setdynamic_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->dynamic = (value != 0);
}

_g enum IgnoreType getignore_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->ignore;
}
_g void setignore_tablevalue(addr pos, enum IgnoreType value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->ignore = value;
}

_g int getreference_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->reference;
}
_g void setreference_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->reference = (value != 0);
}

_g int getcheck_tablevalue(addr pos)
{
	CheckTableValue(pos);
	return StructTableValue(pos)->check;
}
_g void setcheck_tablevalue(addr pos, int value)
{
	CheckTableValue(pos);
	StructTableValue(pos)->check = (value != 0);
}


/*
 *  tablefunction
 */
static void alloc_tablefunction(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLEFUNCTION,
			TABLEFUNCTION_INDEX_SIZE,
			sizeoft(struct tablefunction));
}

_g void make_tablefunction(LocalRoot local, addr call, addr *ret)
{
	addr pos;
	struct tablefunction *ptr;

	alloc_tablefunction(local, &pos);
	ptr = StructTableFunction(pos);
	memset(ptr, 0, sizeoft(struct tablefunction));
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, call);
	*ret = pos;
}

_g void copy_tablefunction(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableFunction(pos);
	alloc_tablefunction(local, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_function(one), PtrBody_function(pos), size);

	for (i = 0; i < TABLEFUNCTION_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(local, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

_g void copylocal_tablefunction(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableFunction(pos);
	alloc_tablefunction(local, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_function(one), PtrBody_function(pos), size);

	for (i = 0; i < TABLEFUNCTION_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copylocal_object(local, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

_g void getname_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	GetEval(pos, TABLEFUNCTION_INDEX_NAME, ret);
}
_g void setname_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	SetEval(pos, TABLEFUNCTION_INDEX_NAME, value);
}

_g void gettype_tablefunction(addr pos, addr *ret)
{
	CheckTableFunction(pos);
	GetEval(pos, TABLEFUNCTION_INDEX_TYPE, ret);
}
_g void settype_tablefunction(addr pos, addr value)
{
	CheckTableFunction(pos);
	SetEval(pos, TABLEFUNCTION_INDEX_TYPE, value);
}

_g int getglobalp_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->globalp;
}
_g void setglobalp_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->globalp = (value != 0);
}

_g int getdynamic_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->dynamic;
}
_g void setdynamic_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->dynamic = (value != 0);
}

_g int getreference_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->reference;
}
_g void setreference_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->reference = (value != 0);
}

_g int getcheck_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->check;
}
_g void setcheck_tablefunction(addr pos, int value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->check = (value != 0);
}

_g enum IgnoreType getignore_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->ignore;
}
_g void setignore_tablefunction(addr pos, enum IgnoreType value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->ignore = value;
}

_g enum InlineType getinline_tablefunction(addr pos)
{
	CheckTableFunction(pos);
	return StructTableFunction(pos)->Inline;
}
_g void setinline_tablefunction(addr pos, enum InlineType value)
{
	CheckTableFunction(pos);
	StructTableFunction(pos)->Inline = value;
}


/*
 *  tablecall
 */
_g void alloc_tablecall(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLECALL,
			TABLECALL_INDEX_SIZE,
			sizeoft(struct tablecall));
}

_g void make_tablecall(LocalRoot local, addr *ret)
{
	addr pos;
	struct tablecall *ptr;

	alloc_tablecall(local, &pos);
	ptr = StructTableCall(pos);
	memset(ptr, 0, sizeoft(struct tablecall));
	*ret = pos;
}

_g int getcheck_tablecall(addr pos)
{
	CheckTableCall(pos);
	return StructTableCall(pos)->check;
}
_g void setcheck_tablecall(addr pos, int value)
{
	CheckTableCall(pos);
	StructTableCall(pos)->check = (value != 0);
}

_g void getvalue_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_VALUE, ret);
}
_g void setvalue_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_VALUE, value);
}

_g void gettype_tablecall(addr pos, addr *ret)
{
	CheckTableCall(pos);
	GetEval(pos, TABLECALL_INDEX_TYPE, ret);
}
_g void settype_tablecall(addr pos, addr value)
{
	CheckTableCall(pos);
	SetEval(pos, TABLECALL_INDEX_TYPE, value);
}


/*
 *  tabletagbody
 */
_g void alloc_tabletagbody(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_TABLETAGBODY,
			TABLETAGBODY_INDEX_SIZE,
			sizeoft(struct tabletagbody));
}

_g void make_tabletagbody(LocalRoot local, addr *ret, addr tag)
{
	addr pos;
	struct tabletagbody *ptr;

	Check(! tagbody_tag_p(tag), "tag error");
	alloc_tabletagbody(local, &pos);
	ptr = StructTableTagBody(pos);
	memset(ptr, 0, sizeoft(struct tabletagbody));
	SetEval(pos, TABLETAGBODY_INDEX_TAG, tag);
	*ret = pos;
}

_g void copy_tabletagbody(LocalRoot local, addr *ret, addr pos)
{
	addr one, value;
	size_t i, size;

	CheckTableTagBody(pos);
	alloc_tabletagbody(local, &one);

	LenBodyEval(one, &size);
	memcpy(PtrBody_tagbody(one), PtrBody_tagbody(pos), size);

	for (i = 0; i < TABLETAGBODY_INDEX_SIZE; i++) {
		GetEval(pos, i, &value);
		copyhard_object(local, &value, value);
		SetEval(one, i, value);
	}
	*ret = one;
}

_g void gettag_tabletagbody(addr pos, addr *ret)
{
	CheckTableTagBody(pos);
	GetEval(pos, TABLETAGBODY_INDEX_TAG, ret);
}
_g void settag_tabletagbody(addr pos, addr value)
{
	CheckTableTagBody(pos);
	Check(! tagbody_tag_p(value), "tag error");
	SetEval(pos, TABLETAGBODY_INDEX_TAG, value);
}

_g int getreference_tabletagbody(addr pos)
{
	CheckTableTagBody(pos);
	return StructTableTagBody(pos)->reference;
}
_g void setreference_tabletagbody(addr pos, int value)
{
	CheckTableTagBody(pos);
	StructTableTagBody(pos)->reference = (value != 0);
}

_g int equal_tabletagbody(addr left, addr right)
{
	CheckTableTagBody(left);
	CheckTableTagBody(right);
	GetEval(left, TABLETAGBODY_INDEX_TAG, &left);
	GetEval(right, TABLETAGBODY_INDEX_TAG, &right);
	return equal_function(left, right);
}

