#ifndef __EVAL_TABLE_HEADER__
#define __EVAL_TABLE_HEADER__

#include "eval.h"
#include "execute.h"
#include "local.h"
#include "typedef.h"

#define make_tablevalue _n(make_tablevalue)
#define copy_tablevalue _n(copy_tablevalue)
#define getname_tablevalue _n(getname_tablevalue)
#define setname_tablevalue _n(setname_tablevalue)
#define gettype_tablevalue _n(gettype_tablevalue)
#define settype_tablevalue _n(settype_tablevalue)
#define getspecialp_tablevalue _n(getspecialp_tablevalue)
#define setspecialp_tablevalue _n(setspecialp_tablevalue)
#define getdynamic_tablevalue _n(getdynamic_tablevalue)
#define setdynamic_tablevalue _n(setdynamic_tablevalue)
#define getignore_tablevalue _n(getignore_tablevalu)
#define setignore_tablevalue _n(setignore_tablevalue)
#define getreference_tablevalue _n(getreference_tablevalue)
#define setreference_tablevalue _n(setreference_tablevalue)
#define getcheck_tablevalue _n(getcheck_tablevalue)
#define setcheck_tablevalue _n(setcheck_tablevalue)
#define getlexical_tablevalue _n(getlexical_tablevalue)
#define setlexical_tablevalue _n(setlexical_tablevalue)
#define getlet_tablevalue _n(getlet_tablevalue)
#define setlet_tablevalue _n(setlet_tablevalue)
#define getclosurep_tablevalue _n(getclosurep_tablevalue)
#define setclosurep_tablevalue _n(setclosurep_tablevalue)
#define getbasep_tablevalue _n(getbasep_tablevalue)
#define setbasep_tablevalue _n(setbasep_tablevalue)
#define getglobalp_tablevalue _n(getglobalp_tablevalue)
#define setglobalp_tablevalue _n(setglobalp_tablevalue)
#define getclosure_tablevalue _n(getclosure_tablevalue)
#define setclosure_tablevalue _n(setclosure_tablevalue)
#define getvalue_tablevalue _n(getvalue_tablevalue)
#define setvalue_tablevalue _n(setvalue_tablevalue)
#define make_tablefunction _n(make_tablefunction)
#define copy_tablefunction _n(copy_tablefunction)
#define getname_tablefunction _n(getname_tablefunction)
#define setname_tablefunction _n(setname_tablefunction)
#define gettype_tablefunction _n(gettype_tablefunction)
#define settype_tablefunction _n(settype_tablefunction)
#define getglobalp_tablefunction _n(getglobalp_tablefunction)
#define setglobalp_tablefunction _n(setglobalp_tablefunction)
#define getdynamic_tablefunction _n(getdynamic_tablefunction)
#define setdynamic_tablefunction _n(setdynamic_tablefunction)
#define getreference_tablefunction _n(getreference_tablefunction)
#define setreference_tablefunction _n(setreference_tablefunction)
#define getcheck_tablefunction _n(getcheck_tablefunction)
#define setcheck_tablefunction _n(setcheck_tablefunction)
#define getignore_tablefunction _n(getignore_tablefunction)
#define setignore_tablefunction _n(setignore_tablefunction)
#define getinline_tablefunction _n(getinline_tablefunction)
#define setinline_tablefunction _n(setinline_tablefunction)
#define getlexical_tablefunction _n(getlexical_tablefunction)
#define setlexical_tablefunction _n(setlexical_tablefunction)
#define getclosurep_tablefunction _n(getclosurep_tablefunction)
#define setclosurep_tablefunction _n(setclosurep_tablefunction)
#define getclosure_tablefunction _n(getclosure_tablefunction)
#define setclosure_tablefunction _n(setclosure_tablefunction)
#define getvalue_tablefunction _n(getvalue_tablefunction)
#define setvalue_tablefunction _n(setvalue_tablefunction)
#define make_tabletagbody _n(make_tabletagbody)
#define copy_tabletagbody _n(copy_tabletagbody)
#define getname_tabletagbody _n(getname_tabletagbody)
#define setname_tabletagbody _n(setname_tabletagbody)
#define getreference_tabletagbody _n(getreference_tabletagbody)
#define setreference_tabletagbody _n(setreference_tabletagbody)
#define equal_tabletagbody _n(equal_tabletagbody)
#define getlexical_tabletagbody _n(getlexical_tabletagbody)
#define setlexical_tabletagbody _n(setlexical_tabletagbody)
#define getclosurep_tabletagbody _n(getclosurep_tabletagbody)
#define setclosurep_tabletagbody _n(setclosurep_tabletagbody)
#define getclosure_tabletagbody _n(getclosure_tabletagbody)
#define setclosure_tabletagbody _n(setclosure_tabletagbody)
#define getjump_tabletagbody _n(getjump_tabletagbody)
#define setjump_tabletagbody _n(setjump_tabletagbody)
#define make_tableblock _n(make_tableblock)
#define copy_tableblock _n(copy_tableblock)
#define getname_tableblock _n(getname_tableblock)
#define setname_tableblock _n(setname_tableblock)
#define getreference_tableblock _n(getreference_tableblock)
#define setreference_tableblock _n(setreference_tableblock)
#define equal_tableblock _n(equal_tableblock)
#define getlexical_tableblock _n(getlexical_tableblock)
#define setlexical_tableblock _n(setlexical_tableblock)
#define getclosurep_tableblock _n(getclosurep_tableblock)
#define setclosurep_tableblock _n(setclosurep_tableblock)
#define getclosure_tableblock _n(getclosure_tableblock)
#define setclosure_tableblock _n(setclosure_tableblock)
#define evaltable_value_heap _n(evaltable_value_heap)
#define evaltable_function_heap _n(evaltable_function_heap)
#define evaltable_tagbody_heap _n(evaltable_tagbody_heap)
#define evaltable_block_heap _n(evaltable_block_heap)
#define gettype_evaltable _n(gettype_evaltable)
#define get_evaltable _n(get_evaltable)
#define getvalue_evaltable _n(getvalue_evaltable)
#define getfunction_evaltable _n(getfunction_evaltable)
#define gettagbody_evaltable _n(gettagbody_evaltable)
#define getblock_evaltable _n(getblock_evaltable)
#define getclosurep_evaltable _n(getclosurep_evaltable)
#define make_tablecall _n(make_tablecall)
#define getcheck_tablecall _n(getcheck_tablecall)
#define setcheck_tablecall _n(setcheck_tablecall)
#define getvalue_tablecall _n(getvalue_tablecall)
#define setvalue_tablecall _n(setvalue_tablecall)
#define gettype_tablecall _n(gettype_tablecall)
#define settype_tablecall _n(settype_tablecall)

enum EvalTable {
	EvalTable_Value,
	EvalTable_Function,
	EvalTable_TagBody,
	EvalTable_Block,
	EvalTable_Self
};

enum IgnoreType {
	IgnoreType_None = 0,
	IgnoreType_Ignore,
	IgnoreType_Ignorable
};

enum InlineType {
	InlineType_None = 0,
	InlineType_Inline,
	InlineType_NotInline
};

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
enum TABLETAGBODY_INDEX {
	TABLETAGBODY_INDEX_NAME,
	TABLETAGBODY_INDEX_SIZE
};
enum TABLEBLOCK_INDEX {
	TABLEBLOCK_INDEX_NAME,
	TABLEBLOCK_INDEX_SIZE
};
enum TABLECALL_INDEX {
	TABLECALL_INDEX_VALUE,
	TABLECALL_INDEX_TYPE,
	TABLECALL_INDEX_SIZE
};

struct tablevalue {
	unsigned specialp : 1;
	unsigned dynamic : 1;
	unsigned reference : 1;
	unsigned check : 1;
	unsigned closurep : 1;
	unsigned basep : 1;
	unsigned globalp : 1;
	enum IgnoreType ignore : 4; /* signed */
	size_t lexical, closure, let;
};
struct tablefunction {
	unsigned globalp : 1;
	unsigned dynamic : 1;
	unsigned reference : 1;
	unsigned check : 1;
	unsigned closurep : 1;
	enum IgnoreType ignore : 4; /* signed */
	enum InlineType Inline : 4; /* signed */
	size_t lexical, closure;
};
struct tabletagbody {
	unsigned reference : 1;
	unsigned closurep : 1;
	size_t lexical, closure, jump;
};
struct tableblock {
	unsigned reference : 1;
	unsigned closurep : 1;
	size_t lexical, closure;
};
struct eval_table {
	enum EvalTable type;
};
struct tablecall {
	unsigned check : 1;
};

#define PtrBody_value(x)		PtrEvalBody((x), TABLEVALUE_INDEX_SIZE)
#define PtrBody_function(x)		PtrEvalBody((x), TABLEFUNCTION_INDEX_SIZE)
#define PtrBody_tagbody(x)		PtrEvalBody((x), TABLETAGBODY_INDEX_SIZE)
#define PtrBody_block(x)		PtrEvalBody((x), TABLEBLOCK_INDEX_SIZE)
#define PtrBody_table(x)		PtrEvalBody((x), 1)
#define PtrBody_call(x)			PtrEvalBody((x), TABLECALL_INDEX_SIZE)

#define StructTableValue(p)		((struct tablevalue *)(PtrBody_value(p)))
#define StructTableFunction(p)	((struct tablefunction *)(PtrBody_function(p)))
#define StructTableTagBody(p)	((struct tabletagbody *)PtrBody_tagbody(p))
#define StructTableBlock(p)		((struct tableblock *)PtrBody_block(p))
#define StructEvalTable(p)		((struct eval_table *)PtrBody_table(p))
#define StructTableCall(p)		((struct tablecall *)PtrBody_call(p))

#define CheckTableValue(pos)	Check(! eval_tablevalue_p(pos), "type error")
#define CheckTableFunction(pos)	Check(! eval_tablefunction_p(pos), "type error")
#define CheckTableTagBody(pos)	Check(! eval_tabletagbody_p(pos), "type error")
#define CheckTableBlock(pos)	Check(! eval_tableblock_p(pos), "type error")
#define CheckTableTable(pos)	Check(! eval_table_p(pos), "type error")
#define CheckTableCall(pos)		Check(! eval_tablecall_p(pos), "type error")



/*
 *  tablevalue
 */
_g void make_tablevalue(addr *ret, addr symbol);
_g void copy_tablevalue(addr *ret, addr arg);

_g void getname_tablevalue(addr pos, addr *ret);
_g void setname_tablevalue(addr pos, addr value);
_g void gettype_tablevalue(addr pos, addr *ret);
_g void settype_tablevalue(addr pos, addr value);
_g int getspecialp_tablevalue(addr pos);
_g void setspecialp_tablevalue(addr pos, int value);
_g int getdynamic_tablevalue(addr pos);
_g void setdynamic_tablevalue(addr pos, int value);
_g enum IgnoreType getignore_tablevalue(addr pos);
_g void setignore_tablevalue(addr pos, enum IgnoreType value);
_g int getreference_tablevalue(addr pos);
_g void setreference_tablevalue(addr pos, int value);
_g int getcheck_tablevalue(addr pos);
_g void setcheck_tablevalue(addr pos, int value);
_g size_t getlexical_tablevalue(addr pos);
_g void setlexical_tablevalue(addr pos, size_t value);
_g size_t getlet_tablevalue(addr pos);
_g void setlet_tablevalue(addr pos, size_t value);
_g int getclosurep_tablevalue(addr pos);
_g void setclosurep_tablevalue(addr pos, int value);
_g int getbasep_tablevalue(addr pos);
_g void setbasep_tablevalue(addr pos, int value);
_g int getglobalp_tablevalue(addr pos);
_g void setglobalp_tablevalue(addr pos, int value);
_g size_t getclosure_tablevalue(addr pos);
_g void setclosure_tablevalue(addr pos, size_t value);
_g void getvalue_tablevalue(Execute ptr, addr pos, addr *ret);
_g void setvalue_tablevalue(Execute ptr, addr pos, addr value);


/*
 *  tablefunction
 */
_g void make_tablefunction(addr *ret, addr call);
_g void copy_tablefunction(addr *ret, addr arg);

_g void getname_tablefunction(addr pos, addr *ret);
_g void setname_tablefunction(addr pos, addr value);
_g void gettype_tablefunction(addr pos, addr *ret);
_g void settype_tablefunction(addr pos, addr value);
_g int getglobalp_tablefunction(addr pos);
_g void setglobalp_tablefunction(addr pos, int value);
_g int getdynamic_tablefunction(addr pos);
_g void setdynamic_tablefunction(addr pos, int value);
_g int getreference_tablefunction(addr pos);
_g void setreference_tablefunction(addr pos, int value);
_g int getcheck_tablefunction(addr pos);
_g void setcheck_tablefunction(addr pos, int value);
_g enum IgnoreType getignore_tablefunction(addr pos);
_g void setignore_tablefunction(addr pos, enum IgnoreType value);
_g enum InlineType getinline_tablefunction(addr pos);
_g void setinline_tablefunction(addr pos, enum InlineType value);
_g size_t getlexical_tablefunction(addr pos);
_g void setlexical_tablefunction(addr pos, size_t value);
_g int getclosurep_tablefunction(addr pos);
_g void setclosurep_tablefunction(addr pos, int value);
_g size_t getclosure_tablefunction(addr pos);
_g void setclosure_tablefunction(addr pos, size_t value);
_g void getvalue_tablefunction(Execute ptr, addr pos, addr *ret);
_g void setvalue_tablefunction(Execute ptr, addr pos, addr value);


/*
 *  tabletagbody
 */
_g void make_tabletagbody(addr *ret, addr tag);
_g void copy_tabletagbody(addr *ret, addr arg);

_g void getname_tabletagbody(addr pos, addr *ret);
_g void setname_tabletagbody(addr pos, addr value);
_g int getreference_tabletagbody(addr pos);
_g void setreference_tabletagbody(addr pos, int value);
_g int equal_tabletagbody(addr left, addr right);
_g size_t getlexical_tabletagbody(addr pos);
_g void setlexical_tabletagbody(addr pos, size_t value);
_g int getclosurep_tabletagbody(addr pos);
_g void setclosurep_tabletagbody(addr pos, int value);
_g size_t getclosure_tabletagbody(addr pos);
_g void setclosure_tabletagbody(addr pos, size_t value);
_g size_t getjump_tabletagbody(addr pos);
_g void setjump_tabletagbody(addr pos, size_t value);


/*
 *  tableblock
 */
_g void make_tableblock(addr *ret, addr tag);
_g void copy_tableblock(addr *ret, addr arg);

_g void getname_tableblock(addr pos, addr *ret);
_g void setname_tableblock(addr pos, addr value);
_g int getreference_tableblock(addr pos);
_g void setreference_tableblock(addr pos, int value);
_g int equal_tableblock(addr left, addr right);
_g size_t getlexical_tableblock(addr pos);
_g void setlexical_tableblock(addr pos, size_t value);
_g int getclosurep_tableblock(addr pos);
_g void setclosurep_tableblock(addr pos, int value);
_g size_t getclosure_tableblock(addr pos);
_g void setclosure_tableblock(addr pos, size_t value);


/*
 *  evaltable
 */
_g void evaltable_value_heap(addr *ret, addr pos);
_g void evaltable_function_heap(addr *ret, addr pos);
_g void evaltable_tagbody_heap(addr *ret, addr pos);
_g void evaltable_block_heap(addr *ret, addr pos);
_g enum EvalTable gettype_evaltable(addr pos);
_g void get_evaltable(addr pos, addr *ret);
_g int getvalue_evaltable(addr list, addr pos, addr *ret);
_g int getfunction_evaltable(addr list, addr pos, addr *ret);
_g int gettagbody_evaltable(addr list, addr pos, addr *ret);
_g int getblock_evaltable(addr list, addr pos, addr *ret);
_g int getclosurep_evaltable(addr table);


/*
 *  tablecall
 */
_g void make_tablecall(addr *ret);
_g int getcheck_tablecall(addr pos);
_g void setcheck_tablecall(addr pos, int value);
_g void getvalue_tablecall(addr pos, addr *ret);
_g void setvalue_tablecall(addr pos, addr value);
_g void gettype_tablecall(addr pos, addr *ret);
_g void settype_tablecall(addr pos, addr value);

#endif

