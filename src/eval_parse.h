#ifndef __HEADER_EVAL_PARSE__
#define __HEADER_EVAL_PARSE__

#include "eval.h"
#include "execute.h"
#include "local.h"

enum EVAL_PARSE {
	EVAL_PARSE_EMPTY,
	/* constant */
	EVAL_PARSE_NIL,
	EVAL_PARSE_T,
	EVAL_PARSE_INTEGER,
	EVAL_PARSE_RATIONAL,
	EVAL_PARSE_COMPLEX,
	EVAL_PARSE_CHARACTER,
	EVAL_PARSE_ARRAY,
	EVAL_PARSE_VECTOR,
	EVAL_PARSE_BITVECTOR,
	EVAL_PARSE_STRING,
	EVAL_PARSE_SYMBOL,
	EVAL_PARSE_FLOAT,
	EVAL_PARSE_DECLAIM,
	EVAL_PARSE_PATHNAME,
	/* cons */
	EVAL_PARSE_PROGN,
	EVAL_PARSE_LET,
	EVAL_PARSE_LETA,
	EVAL_PARSE_SETQ,
	EVAL_PARSE_DEFUN,
	EVAL_PARSE_DEFMACRO,
	EVAL_PARSE_DESTRUCTURING_BIND,
	EVAL_PARSE_DEFINE_SYMBOL_MACRO,
	EVAL_PARSE_SYMBOL_MACROLET,
	EVAL_PARSE_MACRO_LAMBDA,
	EVAL_PARSE_QUOTE,
	EVAL_PARSE_FUNCTION,
	EVAL_PARSE_LAMBDA,
	EVAL_PARSE_IF,
	EVAL_PARSE_UNWIND_PROTECT,
	EVAL_PARSE_TAGBODY,
	EVAL_PARSE_TAG,
	EVAL_PARSE_GO,
	EVAL_PARSE_BLOCK,
	EVAL_PARSE_RETURN_FROM,
	EVAL_PARSE_CATCH,
	EVAL_PARSE_THROW,
	EVAL_PARSE_FLET,
	EVAL_PARSE_LABELS,
	EVAL_PARSE_THE,
	EVAL_PARSE_EVAL_WHEN,
	EVAL_PARSE_VALUES,
	EVAL_PARSE_LOCALLY,
	EVAL_PARSE_CALL,
	EVAL_PARSE_MULTIPLE_VALUE_BIND,
	EVAL_PARSE_MULTIPLE_VALUE_CALL,
	EVAL_PARSE_MULTIPLE_VALUE_PROG1,
	EVAL_PARSE_NTH_VALUE,
	EVAL_PARSE_PROGV,
	/* size */
	EVAL_PARSE_SIZE
};

struct eval_parse {
	enum EVAL_PARSE type;
};

#define StructEvalParse_Low(x)		((struct eval_parse *)PtrEvalBodyAny(x))
#define RefEvalParse_Low(x,i)		RefEval((x),(i))
#define GetEvalParse_Low(x,i,v)		GetEval((x),(i),(v))
#define SetEvalParse_Low(x,i,v)		SetEval((x),(i),(v))
#define RefEvalParseType_Low(x)		(StructEvalParse_Low(x)->type)
#define GetEvalParseType_Low(x,v)	(*(v) = RefEvalParseType_Low(x))
#define SetEvalParseType_Low(x,v)	(RefEvalParseType_Low(x) = (v))

#ifdef LISP_DEBUG
#define StructEvalParse(x)			structevalparse(x)
#define RefEvalParse(x,i)			refevalparse(x,i)
#define GetEvalParse(x,i,v)			getevalparse(x,i,v)
#define SetEvalParse(x,i,v)			setevalparse(x,i,v)
#define RefEvalParseType(x)			refevalparsetype(x)
#define GetEvalParseType(x,v)		getevalparsetype(x,v)
#define SetEvalParseType(x,v)		setevalparsetype(x,v)
#else
#define StructEvalParse(x)			StructEvalParse_Low(x)
#define RefEvalParse(x,i)			RefEvalParse_Low(x,i)
#define GetEvalParse(x,i,v)			GetEvalParse_Low(x,i,v)
#define SetEvalParse(x,i,v)			SetEvalParse_Low(x,i,v)
#define RefEvalParseType(x)			RefEvalParseType_Low(x)
#define GetEvalParseType(x,v)		GetEvalParseType_Low(x,v)
#define SetEvalParseType(x,v)		SetEvalParseType_Low(x,v)
#endif

void eval_parse_alloc(LocalRoot local, addr *ret, enum EVAL_PARSE type, byte array);
void eval_parse_local(LocalRoot local, addr *ret, enum EVAL_PARSE type, byte array);
void eval_parse_heap(addr *ret, enum EVAL_PARSE type, byte array);

void eval_single_parse_alloc(LocalRoot local,
		addr *ret, enum EVAL_PARSE type, addr value);
void eval_single_parse_local(LocalRoot local,
		addr *ret, enum EVAL_PARSE type, addr value);
void eval_single_parse_heap(addr *ret, enum EVAL_PARSE type, addr value);

struct eval_parse *structevalparse(addr pos);
addr refevalparse(addr pos, size_t index);
void getevalparse(addr pos, size_t index, addr *ret);
void setevalparse(addr pos, size_t index, addr value);
enum EVAL_PARSE refevalparsetype(addr pos);
void getevalparsetype(addr pos, enum EVAL_PARSE *ret);
void setevalparsetype(addr pos, enum EVAL_PARSE value);
void check_variable(addr symbol);
void check_function_variable(addr symbol);
int tagbody_tag_p(addr pos);
int eval_parse(addr *ret, addr pos);

void copy_eval_parse_alloc(LocalRoot local, addr *ret, addr eval);
void copy_eval_parse_local(LocalRoot local, addr *ret, addr eval);
void copy_eval_parse_heap(addr *ret, addr eval);

int parse_macro_lambda_list(addr *ret, addr args);
int findmacro_environment(addr symbol, addr env, addr *ret);
int macroexpand1(addr *ret, addr form, addr env, int *result);
int macroexpand(addr *ret, addr form, addr env, int *result);

#endif

