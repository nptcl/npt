#ifndef __PARSE_OBJECT_HEADER__
#define __PARSE_OBJECT_HEADER__

#include "eval_object.h"
#include "local.h"
#include "parse_typedef.h"
#include "typedef.h"

#define structevalparse _n(structevalparse)
#define refevalparse _n(refevalparse)
#define getevalparse _n(getevalparse)
#define setevalparse _n(setevalparse)
#define refevalparsetype _n(refevalparsetype)
#define getevalparsetype _n(getevalparsetype)
#define setevalparsetype _n(setevalparsetype)
#define eval_parse_alloc _n(eval_parse_alloc)
#define eval_parse_local _n(eval_parse_local)
#define eval_parse_heap _n(eval_parse_heap)
#define eval_single_parse_alloc _n(eval_single_parse_alloc)
#define eval_single_parse_local _n(eval_single_parse_local)
#define eval_single_parse_heap _n(eval_single_parse_heap)
#define eval_parse2_alloc _n(eval_parse2_alloc)
#define eval_parse2_local _n(eval_parse2_local)
#define eval_parse2_heap _n(eval_parse2_heap)

struct parse_struct {
	EvalParse type;
};

#define StructEvalParse_Low(x)		((struct parse_struct *)PtrEvalBodyAny(x))
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

struct parse_struct *structevalparse(addr pos);
addr refevalparse(addr pos, size_t index);
void getevalparse(addr pos, size_t index, addr *ret);
void setevalparse(addr pos, size_t index, addr value);
EvalParse refevalparsetype(addr pos);
void getevalparsetype(addr pos, EvalParse *ret);
void setevalparsetype(addr pos, EvalParse value);

void eval_parse_alloc(LocalRoot local, addr *ret, EvalParse type, byte array);
void eval_parse_local(LocalRoot local, addr *ret, EvalParse type, byte array);
void eval_parse_heap(addr *ret, EvalParse type, byte array);

void eval_single_parse_alloc(LocalRoot local, addr *ret, EvalParse type, addr value);
void eval_single_parse_local(LocalRoot local, addr *ret, EvalParse type, addr value);
void eval_single_parse_heap(addr *ret, EvalParse type, addr value);
void eval_parse2_alloc(LocalRoot local, addr *ret, EvalParse type, addr x, addr y);
void eval_parse2_local(LocalRoot local, addr *ret, EvalParse type, addr x, addr y);
void eval_parse2_heap(addr *ret, EvalParse type, addr x, addr y);

#endif

