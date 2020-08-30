#include "eval_object.h"
#include "heap.h"
#include "local.h"
#include "parse_object.h"

/*
 *  access
 */
_g struct parse_struct *structevalparse(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return StructEvalParse_Low(pos);
}
_g addr refevalparse(addr pos, size_t index)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParse_Low(pos, index);
}
_g void getevalparse(addr pos, size_t index, addr *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParse_Low(pos, index, ret);
}
_g void setevalparse(addr pos, size_t index, addr value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParse_Low(pos, index, value);
}
_g EvalParse refevalparsetype(addr pos)
{
	Check(! eval_parse_p(pos), "type error");
	return RefEvalParseType_Low(pos);
}
_g void getevalparsetype(addr pos, EvalParse *ret)
{
	Check(! eval_parse_p(pos), "type error");
	GetEvalParseType_Low(pos, ret);
}
_g void setevalparsetype(addr pos, EvalParse value)
{
	Check(! eval_parse_p(pos), "type error");
	SetEvalParseType_Low(pos, value);
}


/*
 *  memory
 */
_g void eval_parse_alloc(LocalRoot local, addr *ret, EvalParse type, byte array)
{
	addr pos;

	Check(0xFF < sizeof(struct parse_struct), "struct size error");
	eval_alloc(local, &pos, EVAL_TYPE_PARSE, array, sizeoft(struct parse_struct));
	SetEvalParseType(pos, type);

	*ret = pos;
}
_g void eval_parse_local(LocalRoot local, addr *ret, EvalParse type, byte array)
{
	Check(local == NULL, "local error");
	eval_parse_alloc(local, ret, type, array);
}
_g void eval_parse_heap(addr *ret, EvalParse type, byte array)
{
	eval_parse_alloc(NULL, ret, type, array);
}

_g void eval_single_parse_alloc(LocalRoot local, addr *ret, EvalParse type, addr value)
{
	eval_parse_alloc(local, ret, type, 1);
	SetEvalParse(*ret, 0, value);
}
_g void eval_single_parse_local(LocalRoot local, addr *ret, EvalParse type, addr value)
{
	Check(local == NULL, "local error");
	eval_single_parse_alloc(local, ret, type, value);
}
_g void eval_single_parse_heap(addr *ret, EvalParse type, addr value)
{
	eval_single_parse_alloc(NULL, ret, type, value);
}

