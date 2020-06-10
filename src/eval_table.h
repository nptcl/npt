#ifndef __EVAL_TABLE_HEADER__
#define __EVAL_TABLE_HEADER__

#include "eval.h"
#include "execute.h"
#include "local.h"
#include "typedef.h"

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
_g void getvalue_tabletagbody(Execute ptr, addr pos, addr *ret);
_g void setvalue_tabletagbody(Execute ptr, addr pos, addr value);


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
_g void getvalue_tableblock(Execute ptr, addr pos, addr *ret);
_g void setvalue_tableblock(Execute ptr, addr pos, addr value);


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

