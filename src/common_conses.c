/*
 *  ANSI COMMON LISP: 14. Conses
 */
#include "common_header.h"
#include "cons.h"
#include "equal.h"
#include "format.h"
#include "integer.h"
#include "sequence.h"
#include "setf.h"
#include "type_parse.h"

/* (defun cons (object1 object2) ...) -> cons */
static void function_cons(Execute ptr, addr var1, addr var2)
{
	cons_heap(&var1, var1, var2);
	setresult_control(ptr, var1);
}

static void type_cons_common(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Asterisk);
	typeargs_var2(&arg, arg, arg);
	GetTypeValues(&values, Cons);
	type_compiled_heap(arg, values, ret);
}

static void defun_cons(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_cons);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cons_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun consp (object) ...) -> boolean */
static void function_consp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_CONS);
}

static void defun_consp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_consp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atom (object) ...) -> boolean */
static void function_atom(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) != LISPTYPE_CONS);
}

static void defun_atom(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATOM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_atom);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplaca (cons object) ...) -> cons */
static void function_rplaca(Execute ptr, addr cons, addr object)
{
	SetCar(cons, object);
	setresult_control(ptr, cons);
}

static void defun_rplaca(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACA, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_rplaca);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rplaca);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplacd (cons object) ...) -> cons */
static void function_rplacd(Execute ptr, addr cons, addr object)
{
	SetCdr(cons, object);
	setresult_control(ptr, cons);
}

static void defun_rplacd(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_rplacd);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rplaca);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun car (list) ...) -> object
 * (defun cddddr (list-cdddr) ...) -> object
 */
static void type_cxr(addr *ret, enum TypeTable cxr)
{
	addr arg, values;

	gettypetable(cxr, &arg);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_cxr(constindex index,
		void (*call)(Execute, addr),
		enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, call);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cxr(&type, cxr);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_car(Execute ptr, addr list)
{
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cdr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_caar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cdar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_caaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cdaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_caaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cadaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cadadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_caddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cadddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
}

static void function_cdaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cddaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cddadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cdddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

static void function_cddddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
}

#define DefunCxr(x,y,z) defun_cxr(CONSTANT_COMMON_##x, function_##y, TypeTable_##z)
static void defun_car(void)
{
	DefunCxr(CAR, car, Cxr);
	DefunCxr(CDR, cdr, Cxr);
	DefunCxr(CAAR, caar, Cxar);
	DefunCxr(CADR, cadr, Cxdr);
	DefunCxr(CDAR, cdar, Cxar);
	DefunCxr(CDDR, cddr, Cxdr);
	DefunCxr(CAAAR, caaar, Cxaar);
	DefunCxr(CAADR, caadr, Cxadr);
	DefunCxr(CADAR, cadar, Cxdar);
	DefunCxr(CADDR, caddr, Cxddr);
	DefunCxr(CDAAR, cdaar, Cxaar);
	DefunCxr(CDADR, cdadr, Cxadr);
	DefunCxr(CDDAR, cddar, Cxdar);
	DefunCxr(CDDDR, cdddr, Cxddr);
	DefunCxr(CAAAAR, caaaar, Cxaaar);
	DefunCxr(CAAADR, caaadr, Cxaadr);
	DefunCxr(CAADAR, caadar, Cxadar);
	DefunCxr(CAADDR, caaddr, Cxaddr);
	DefunCxr(CADAAR, cadaar, Cxdaar);
	DefunCxr(CADADR, cadadr, Cxdadr);
	DefunCxr(CADDAR, caddar, Cxddar);
	DefunCxr(CADDDR, cadddr, Cxdddr);
	DefunCxr(CDAAAR, cdaaar, Cxaaar);
	DefunCxr(CDAADR, cdaadr, Cxaadr);
	DefunCxr(CDADAR, cdadar, Cxadar);
	DefunCxr(CDADDR, cdaddr, Cxaddr);
	DefunCxr(CDDAAR, cddaar, Cxdaar);
	DefunCxr(CDDADR, cddadr, Cxdadr);
	DefunCxr(CDDDAR, cdddar, Cxddar);
	DefunCxr(CDDDDR, cddddr, Cxdddr);
}


/* (defun (setf car) (object list) ...) -> object
 * (defun (setf cddddr) (object list-cdddr) ...) -> object
 */
static void type_setf_cxr(addr *ret, enum TypeTable cxr)
{
	addr arg, values, type;

	gettypetable(cxr, &arg);
	GetTypeTable(&type, T);
	typeargs_var2(&arg, type, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_cxr(constindex index,
		void (*call)(Execute, addr, addr),
		enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, call);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_cxr(&type, cxr);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static void function_setf_car(Execute ptr, addr value, addr cons)
{
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdr(Execute ptr, addr value, addr cons)
{
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cadaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cadadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_caddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cadddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cddaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cddadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cdddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

static void function_setf_cddddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
}

#define DefunSetfCxr(x,y,z) { \
	defun_setf_cxr(CONSTANT_COMMON_##x, function_setf_##y, TypeTable_##z); \
}
static void defun_setf_car(void)
{
	DefunSetfCxr(CAR, car, Cons);
	DefunSetfCxr(CDR, cdr, Cons);
	DefunSetfCxr(CAAR, caar, SetfCxar);
	DefunSetfCxr(CADR, cadr, SetfCxdr);
	DefunSetfCxr(CDAR, cdar, SetfCxar);
	DefunSetfCxr(CDDR, cddr, SetfCxdr);
	DefunSetfCxr(CAAAR, caaar, SetfCxaar);
	DefunSetfCxr(CAADR, caadr, SetfCxadr);
	DefunSetfCxr(CADAR, cadar, SetfCxdar);
	DefunSetfCxr(CADDR, caddr, SetfCxddr);
	DefunSetfCxr(CDAAR, cdaar, SetfCxaar);
	DefunSetfCxr(CDADR, cdadr, SetfCxadr);
	DefunSetfCxr(CDDAR, cddar, SetfCxdar);
	DefunSetfCxr(CDDDR, cdddr, SetfCxddr);
	DefunSetfCxr(CAAAAR, caaaar, SetfCxaaar);
	DefunSetfCxr(CAAADR, caaadr, SetfCxaadr);
	DefunSetfCxr(CAADAR, caadar, SetfCxadar);
	DefunSetfCxr(CAADDR, caaddr, SetfCxaddr);
	DefunSetfCxr(CADAAR, cadaar, SetfCxdaar);
	DefunSetfCxr(CADADR, cadadr, SetfCxdadr);
	DefunSetfCxr(CADDAR, caddar, SetfCxddar);
	DefunSetfCxr(CADDDR, cadddr, SetfCxdddr);
	DefunSetfCxr(CDAAAR, cdaaar, SetfCxaaar);
	DefunSetfCxr(CDAADR, cdaadr, SetfCxaadr);
	DefunSetfCxr(CDADAR, cdadar, SetfCxadar);
	DefunSetfCxr(CDADDR, cdaddr, SetfCxaddr);
	DefunSetfCxr(CDDAAR, cddaar, SetfCxdaar);
	DefunSetfCxr(CDDADR, cddadr, SetfCxdadr);
	DefunSetfCxr(CDDDAR, cdddar, SetfCxddar);
	DefunSetfCxr(CDDDDR, cddddr, SetfCxdddr);
}


/* (defun first (list) ...) -> object
 * (defun tenth (list-tenth) ...) -> object
 */
static void function_fifth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCar(list, &list); /*5*/
	setresult_control(ptr, list);
}

static void function_sixth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCar(list, &list); /*6*/
	setresult_control(ptr, list);
}

static void function_seventh(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCar(list, &list); /*7*/
	setresult_control(ptr, list);
}

static void function_eighth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCar(list, &list); /*8*/
	setresult_control(ptr, list);
}

static void function_ninth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCar(list, &list); /*9*/
	setresult_control(ptr, list);
}

static void function_tenth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCdr(list, &list); /*9*/
	GetCar(list, &list); /*10*/
	setresult_control(ptr, list);
}

static void defun_first(void)
{
	DefunCxr(REST, cdr, Cxr);
	DefunCxr(FIRST, car, Cxr);
	DefunCxr(SECOND, cadr, Cxdr);
	DefunCxr(THIRD, caddr, Cxddr);
	DefunCxr(FOURTH, cadddr, Cxdddr);
	DefunCxr(FIFTH, fifth, Fifth);
	DefunCxr(SIXTH, sixth, Sixth);
	DefunCxr(SEVENTH, seventh, Seventh);
	DefunCxr(EIGHTH, eighth, Eighth);
	DefunCxr(NINTH, ninth, Ninth);
	DefunCxr(TENTH, tenth, Tenth);
}


/* (defun (setf car) (object list) ...) -> object
 * (defun (setf cddddr) (object list-cdddr) ...) -> object
 */
static void function_setf_fifth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void function_setf_sixth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void function_setf_seventh(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void function_setf_eighth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void function_setf_ninth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void function_setf_tenth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCdr(list, &list); /*7*/
	GetCdr(list, &list); /*8*/
	GetCdr(list, &list); /*9*/
	SetCar(list, value);
	setresult_control(ptr, value);
}

static void defun_setf_first(void)
{
	DefunSetfCxr(REST, cdr, Cxr);
	DefunSetfCxr(FIRST, car, Cxr);
	DefunSetfCxr(SECOND, cadr, Cxdr);
	DefunSetfCxr(THIRD, caddr, Cxddr);
	DefunSetfCxr(FOURTH, cadddr, Cxdddr);
	DefunSetfCxr(FIFTH, fifth, Fifth);
	DefunSetfCxr(SIXTH, sixth, Sixth);
	DefunSetfCxr(SEVENTH, seventh, Seventh);
	DefunSetfCxr(EIGHTH, eighth, Eighth);
	DefunSetfCxr(NINTH, ninth, Ninth);
	DefunSetfCxr(TENTH, tenth, Tenth);
}


/* (defun copy-list (list) ...) -> list */
static void function_copy_list(Execute ptr, addr list)
{
	copy_list_heap_safe(&list, list);
	setresult_control(ptr, list);
}

static void defun_copy_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_copy_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-tree (list) ...) -> list */
static void function_copy_tree(Execute ptr, addr list)
{
	copy_tree_heap(&list, list);
	setresult_control(ptr, list);
}

static void defun_copy_tree(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_TREE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_copy_tree);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sublis (alist tree &key key test test-not) ...) -> tree
 *   alist     list
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
struct sublis_struct {
	Execute ptr;
	addr alist, key, test1, test2;
	int test;
};

static int sublis_default(addr alist, addr left, addr *ret)
{
	addr right, value;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (eql_function(left, right)) {
			*ret = value;
			return 1;
		}
	}
	*ret = left;

	return 0;
}

static int sublis_test(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (callclang_funcall(ptr, &check, test, left, right, NULL))
			return 1;
		if (check != Nil) {
			*result = 1;
			*ret = value;
			return 0;
		}
	}
	*result = 0;
	*ret = left;
	return 0;
}

static int sublis_test_not(Execute ptr,
		addr alist, addr test, addr left, int *result, addr *ret)
{
	addr right, value, check;

	while (alist != Nil) {
		getcons(alist, &right, &alist);
		getcons(right, &right, &value);
		if (callclang_funcall(ptr, &check, test, left, right, NULL))
			return 1;
		if (check == Nil) {
			*result = 1;
			*ret = value;
			return 0;
		}
	}
	*result = 0;
	*ret = left;
	return 0;
}

static int sublis_replace(struct sublis_struct *str, addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = sublis_default(str->alist, tree, ret);
			return 0;

		case 1: /* :test */
			return sublis_test(str->ptr,
					str->alist, str->test1, tree, result, ret);

		case 2: /* :test-not */
			return sublis_test_not(str->ptr,
					str->alist, str->test2, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int sublis_recursive(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (sublis_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (sublis_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (sublis_recursive(str, car, &car))
				return 1;
		}
		if (sublis_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (sublis_recursive(str, cdr, &cdr))
				return 1;
		}
		cons_heap(ret, car, cdr);
	}

	return 0;
}

static int sublis_argument(Execute ptr,
		struct sublis_struct *str, addr alist, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist(rest, key, &key)) key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	/* recursive call */
	str->ptr = ptr;
	str->alist = alist;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static void function_sublis(Execute ptr, addr alist, addr tree, addr rest)
{
	struct sublis_struct str;

	if (sublis_argument(ptr, &str, alist, rest))
		fmte("SUBLIS don't accept both :test and :test-not parameter.", NULL);
	if (sublis_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_sublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBLIS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_sublis);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sublis);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsublis (alist tree &key key test test-not) ...) -> tree
 *   alist     list
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int nsublis_recursive(struct sublis_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (sublis_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (sublis_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (nsublis_recursive(str, car, &car))
				return 1;
		}
		if (sublis_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (nsublis_recursive(str, cdr, &cdr))
				return 1;
		}
		SetCons(tree, car, cdr);
		*ret = tree;
	}

	return 0;
}

static void function_nsublis(Execute ptr, addr alist, addr tree, addr rest)
{
	struct sublis_struct str;

	if (sublis_argument(ptr, &str, alist, rest))
		fmte("NSUBLIS don't accept both :test and :test-not parameter.", NULL);
	if (nsublis_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_nsublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBLIS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_nsublis);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sublis);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst (new old tree &key key test test-not) ...) -> tree
 *   new       object
 *   old       object
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
struct subst_struct {
	Execute ptr;
	addr make, old, key, test1, test2;
	int test;
};

static int subst_argument(Execute ptr,
		struct subst_struct *str, addr one, addr old, addr rest)
{
	addr key, test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		key = test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_KEY, &key);
		if (getplist(rest, key, &key)) key = Nil;
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->make = one;
	str->old = old;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int subst_default(struct subst_struct *str, addr tree, addr *ret)
{
	if (eql_function(str->old, tree)) {
		*ret = str->make;
		return 1;
	}
	else {
		*ret = tree;
		return 0;
	}
}

static int subst_test(struct subst_struct *str, addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test1, str->old, tree, NULL))
		return 1;
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int subst_test_not(struct subst_struct *str, addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test2, str->old, tree, NULL))
		return 1;
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int subst_replace(struct subst_struct *str, addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 0: /* nil */
			*result = subst_default(str, tree, ret);
			return 0;

		case 1: /* :test */
			return subst_test(str, tree, result, ret);

		case 2: /* :test-not */
			return subst_test_not(str, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int subst_recursive(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (subst_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (subst_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (subst_recursive(str, car, &car))
				return 1;
		}
		if (subst_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (subst_recursive(str, cdr, &cdr))
				return 1;
		}
		cons_heap(ret, car, cdr);
	}

	return 0;
}

static void function_subst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	struct subst_struct str;

	if (subst_argument(ptr, &str, one, old, key))
		fmte("SUBST don't accept both :test and :test-not parameter.", NULL);
	if (subst_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_subst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_subst);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Subst);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubst (new old tree &key key test test-not) ...) -> tree
 *   new       object
 *   old       object
 *   tree      list
 *   key       (or (function (t &rest t) *) symbol) ;; or null
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
static int nsubst_recursive(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (subst_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (subst_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (nsubst_recursive(str, car, &car))
				return 1;
		}
		if (subst_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (nsubst_recursive(str, cdr, &cdr))
				return 1;
		}
		SetCons(tree, car, cdr);
		*ret = tree;
	}

	return 0;
}

static void function_nsubst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	struct subst_struct str;

	if (subst_argument(ptr, &str, one, old, key))
		fmte("NSUBST don't accept both :test and :test-not parameter.", NULL);
	if (nsubst_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_nsubst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_nsubst);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Subst);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int subst_if_argument(Execute ptr,
		struct subst_struct *str, addr one, addr test1, addr test2, addr rest)
{
	addr key;

	GetConst(KEYWORD_KEY, &key);
	if (getplist(rest, key, &key)) key = Nil;

	clearpoint(str);
	str->ptr = ptr;
	str->make = one;
	str->key = key;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int subst_if_call(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test1, tree, NULL))
		return 1;
	if (check != Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int subst_if_not_call(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	addr check;

	if (callclang_funcall(str->ptr, &check, str->test2, tree, NULL))
		return 1;
	if (check == Nil) {
		*result = 1;
		*ret = str->make;
	}
	else {
		*result = 0;
		*ret = tree;
	}

	return 0;
}

static int subst_if_replace(struct subst_struct *str,
		addr tree, int *result, addr *ret)
{
	/* key */
	if (str->key != Nil) {
		if (callclang_funcall(str->ptr, &tree, str->key, tree, NULL))
			return 1;
	}
	/* test */
	switch (str->test) {
		case 1: /* :test */
			return subst_if_call(str, tree, result, ret);

		case 2: /* :test-not */
			return subst_if_not_call(str, tree, result, ret);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int subst_if_recursive(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (subst_if_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (subst_if_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (subst_if_recursive(str, car, &car))
				return 1;
		}
		if (subst_if_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (subst_if_recursive(str, cdr, &cdr))
				return 1;
		}
		cons_heap(ret, car, cdr);
	}

	return 0;
}

static void function_subst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	struct subst_struct str;

	subst_if_argument(ptr, &str, one, predicate, Nil, key);
	if (subst_if_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_subst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_subst_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nsubst-if (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static int nsubst_if_recursive(struct subst_struct *str, addr tree, addr *ret)
{
	int check;
	addr car, cdr;

	if (! consp(tree)) {
		if (subst_if_replace(str, tree, &check, ret))
			return 1;
	}
	else {
		GetCons(tree, &car, &cdr);
		if (subst_if_replace(str, car, &check, &car))
			return 1;
		if (! check) {
			if (nsubst_if_recursive(str, car, &car))
				return 1;
		}
		if (subst_if_replace(str, cdr, &check, &cdr))
			return 1;
		if (! check) {
			if (nsubst_if_recursive(str, cdr, &cdr))
				return 1;
		}
		SetCons(tree, car, cdr);
		*ret = tree;
	}

	return 0;
}

static void function_nsubst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	struct subst_struct str;

	subst_if_argument(ptr, &str, one, predicate, Nil, key);
	if (nsubst_if_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_nsubst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_nsubst_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if-not (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static void function_subst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	struct subst_struct str;

	subst_if_argument(ptr, &str, one, Nil, predicate, key);
	if (subst_if_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_subst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_subst_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subst-if-not (new predicate tree &key key) ...) -> tree
 *   new        object
 *   predicate  (or (function (t &rest t) *) symbol)
 *   tree       list
 *   key        (or (function (t &rest t) *) symbol) ;; or null
 */
static void function_nsubst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	struct subst_struct str;

	subst_if_argument(ptr, &str, one, Nil, predicate, key);
	if (nsubst_if_recursive(&str, tree, &tree)) return;
	setresult_control(ptr, tree);
}

static void defun_nsubst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, function_nsubst_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, SubstIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tree-equal (tree1 tree2 &key test test-not) ...) -> boolean
 *   tree1     t
 *   tree2     t
 *   test      (or (function (t t &rest t) *) symbol)
 *   test-not  (or (function (t t &rest t) *) symbol)
 */
struct tree_equal_struct {
	Execute ptr;
	addr test1, test2;
	int test;
};

static int tree_equal_argument(Execute ptr,
		struct tree_equal_struct *str, addr rest)
{
	addr test1, test2;

	clearpoint(str);
	if (rest == Nil) {
		test1 = test2 = Nil;
	}
	else {
		GetConst(KEYWORD_TEST, &test1);
		if (getplist(rest, test1, &test1)) test1 = Nil;
		GetConst(KEYWORD_TEST_NOT, &test2);
		if (getplist(rest, test2, &test2)) test2 = Nil;
		if (test1 != Nil && test2 != Nil)
			return 1;
	}

	str->ptr = ptr;
	str->test1 = test1;
	str->test2 = test2;
	if (test1 == Nil && test2 == Nil)
		str->test = 0;
	else if (test1 != Nil)
		str->test = 1;
	else
		str->test = 2;

	return 0;
}

static int tree_equal_test(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	if (callclang_funcall(ptr, &test, test, left, right, NULL)) return 1;
	*result = (test != Nil);
	return 0;
}

static int tree_equal_test_not(Execute ptr,
		int *result, addr test, addr left, addr right)
{
	if (callclang_funcall(ptr, &test, test, left, right, NULL)) return 1;
	*result = (test == Nil);
	return 0;
}

static int tree_equal_replace(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	switch (str->test) {
		case 0: /* nil */
			*result = eql(tree1, tree2);
			return 0;

		case 1: /* :test */
			return tree_equal_test(str->ptr, result, str->test1, tree1, tree2);

		case 2: /* :test-not */
			return tree_equal_test_not(str->ptr, result, str->test2, tree1, tree2);

		default:
			fmte("Invalid test mode.", NULL);
			return 1;
	}
}

static int tree_equal_recursive(struct tree_equal_struct *str,
		int *result, addr tree1, addr tree2)
{
	int check;
	addr car1, cdr1, car2, cdr2;

	if (atom(tree1) || atom(tree2))
		return tree_equal_replace(str, result, tree1, tree2);
	GetCons(tree1, &car1, &cdr1);
	GetCons(tree2, &car2, &cdr2);

	if (tree_equal_recursive(str, &check, car1, car2))
		return 1;
	if (! check) {
		*result = 0;
		return 0;
	}
	return tree_equal_recursive(str, result, cdr1, cdr2);
}

static void function_tree_equal(Execute ptr, addr tree1, addr tree2, addr key)
{
	int result;
	struct tree_equal_struct str;

	if (tree_equal_argument(ptr, &str, key))
		fmte("TREE-EQUAL don't accept both :test and :test-not parameter.", NULL);
	if (tree_equal_recursive(&str, &result, tree1, tree2)) return;
	setbool_control(ptr, result);
}

static void type_tree_equal(addr *ret)
{
	addr arg, values, key, test, test_not, call;

	GetConst(KEYWORD_TEST, &test);
	GetConst(KEYWORD_TEST_NOT, &test_not);
	GetTypeTable(&call, FunctionDesigner);
	cons_heap(&test, test, call);
	cons_heap(&test_not, test_not, call);
	list_heap(&key, test, test_not, NULL);
	GetTypeTable(&arg, T);
	typeargs_var2key(&arg, arg, arg, key);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_tree_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TREE_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_tree_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_tree_equal(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list (&rest objests) ...) -> list */
static void function_list(Execute ptr, addr rest)
{
	setresult_control(ptr, rest);
}

static void type_list_common(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_rest(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_rest(pos, function_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list* (object &rest objects) ...) -> object */
static void function_lista(Execute ptr, addr var, addr rest)
{
	lista_heap_safe(&var, var, rest);
	setresult_control(ptr, var);
}

static void type_lista_common(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	typeargs_var1rest(&arg, arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_lista(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTA, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_lista);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_lista_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-length (list) ...) -> (or index null) */
static void function_list_length(Execute ptr, addr list)
{
	size_t size;

	if (list_length_safe(list, &size)) {
		setresult_control(ptr, Nil);
	}
	else {
		make_index_integer_alloc(NULL, &list, size);
		setresult_control(ptr, list);
	}
}

static void type_list_length(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	typeargs_var1rest(&arg, arg, arg);
	GetTypeTable(&values, IndexNull);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_list_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST_LENGTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_list_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun listp (object) ...) -> boolean */
static void function_listp(Execute ptr, addr var)
{
	setbool_control(ptr, IsList(var));
}

static void defun_listp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_listp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-list (size &key initial-element) ...) -> list
 *   size             index
 *   initial-element  t  ;; default nil
 */
static void function_make_list(Execute ptr, addr var, addr rest)
{
	addr element, list;
	size_t size;

	/* argument */
	if (getindex_integer(var, &size))
		fmte("Too large index value ~S.", var, NULL);
	if (getplist_constant(rest, CONSTANT_KEYWORD_INITIAL_ELEMENT, &element))
		element = Nil;
	/* make-list */
	for (list = Nil; size--; )
		cons_heap(&list, element, list);
	/* result */
	setresult_control(ptr, list);
}

static void type_make_list(addr *ret)
{
	addr arg, values, key, symbol, type;

	GetTypeTable(&arg, Index);
	GetConst(KEYWORD_INITIAL_ELEMENT, &symbol);
	GetTypeTable(&type, T);
	cons_heap(&key, symbol, type);
	conscar_heap(&key, key);
	typeargs_var1key(&arg, arg, key);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_make_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro push (item place) ...) -> value
 *   item   t
 *   place  setf-place
 *   value  t
 */
static void expansion_push_single(Execute ptr,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (cons value r)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, cons, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (cons value r)) */
	getcar(g, &g);
	list_heap(&cons, cons, item, r, NULL);
	list_heap(&x, g, cons, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(&leta, leta, args, declare, w, g, NULL);
	setresult_control(ptr, leta);
}

static void expansion_push_multiple(Execute ptr,
		addr item, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (cons v g1))
	 *   (setq g2 (cons v g2))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, cons, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CONS, &cons);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	make_gensym(ptr, &v);
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (cons v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&y, cons, v, x, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(&pos, pos);
	setresult_control(ptr, pos);
}

static void expansion_push(Execute ptr, addr item, addr place, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	if (singlep(g))
		expansion_push_single(ptr, item, a, b, g, w, r);
	else
		expansion_push_multiple(ptr, item, a, b, g, w, r);
}

static void function_push(Execute ptr, addr form, addr env)
{
	addr args, item, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &item, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;
	expansion_push(ptr, item, place, env);
	return;

error:
	fmte("PUSH argument ~S must be a (push item place) form.", form, NULL);
}

static void defmacro_push(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSH, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_push);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro pop (place) ...) -> t */
static void expansion_pop_single(Execute ptr,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (c r)
	 *        (g (cdr c)))
	 *   (declare (ignorable a1 a2))
	 *   w
	 *   (car c))
	 */
	addr list1, list2, leta, car, cdr, declare, ignorable, args, x, y, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (c r) */
	make_gensym(ptr, &c);
	list_heap(&x, c, r, NULL);
	cons_heap(&args, x, args);
	/* (g (cdr c)) */
	getcar(g, &g);
	list_heap(&x, cdr, c, NULL);
	list_heap(&x, g, x, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(&x, car, c, NULL);
	list_heap(&x, leta, args, declare, w, x, NULL);
	setresult_control(ptr, x);
}

static void expansion_pop_multiple(Execute ptr,
		addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2 ...))
	 *   (multiple-value-bind (r1 r2 ...) r
	 *     (setq g1 (cdr r1))
	 *     (setq g2 (cdr r2))
	 *     ...)
	 *   w
	 *   (values (car r1) (car r2) ...))
	 */
	addr leta, declare, ignorable, mvbind, setq, car, cdr, values;
	addr list1, list2, args, x, y, pos, c;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		getcons(list1, &x, &list1);
		getcons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-bind (r1 r2 ...) r
	 *   (setq g1 (cdr r1)) ...)  */
	c = Nil;
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		make_gensym(ptr, &y);
		cons_heap(&c, y, c);
		list_heap(&y, cdr, y, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&args, x, args);
	}
	nreverse_list_unsafe(&c, c);
	nreverse_list_unsafe(&args, args);
	conscar_heap(&args, mvbind);
	cons_heap(&args, c, args);
	cons_heap(&args, r, args);
	cons_heap(&pos, args, pos);
	/* w */
	cons_heap(&pos, w, pos);
	/* (values (car r1) (car r2) ...) */
	args = Nil;
	for (list1 = c; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		list_heap(&x, car, x, NULL);
		cons_heap(&args, x, args);
	}
	nreverse_list_unsafe(&args, args);
	cons_heap(&values, values, args);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(&pos, pos);
	setresult_control(ptr, pos);
}

static void expansion_pop(Execute ptr, addr place, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	if (singlep(g))
		expansion_pop_single(ptr, a, b, g, w, r);
	else
		expansion_pop_multiple(ptr, a, b, g, w, r);
}

static void function_pop(Execute ptr, addr form, addr env)
{
	addr args, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (args != Nil) goto error;
	expansion_pop(ptr, place, env);
	return;

error:
	fmte("POP argument ~S must be a (pop place) form.", form, NULL);
}

static void defmacro_pop(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_POP, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_pop);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun nth (index list) ...) -> object
 *   index  (integer 0 *)  ;; Don't use index (SizeMax)
 *   list   list
 */
static void function_nth(Execute ptr, addr index, addr list)
{
	size_t size;

	if (getindex_integer(index, &size))
		getnth_large(list, index, &list);
	else
		getnth(list, size, &list);
	setresult_control(ptr, list);
}

static void defun_nth(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_nth);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nth);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf nth) (value index list) ...) -> value
 *   index  index
 *   list   list
 */
static void function_setf_nth(Execute ptr, addr value, addr index, addr list)
{
	size_t size;

	if (getindex_integer(index, &size))
		fmte("Too large index value ~S.", index, NULL);
	setnth(list, size, value);
	setresult_control(ptr, value);
}

static void type_setf_nth(addr *ret)
{
	addr arg, values, type, value;

	GetTypeTable(&value, T);
	GetTypeTable(&arg, Index);
	GetTypeTable(&type, List);
	typeargs_var3(&arg, value, arg, type);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_setf_nth(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, function_setf_nth);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_nth(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun nthcdr (index list) ...) -> object */
static void function_nthcdr(Execute ptr, addr index, addr list)
{
	size_t size;

	if (getindex_integer(index, &size))
		getnthcdr_large(list, index, &list);
	else
		getnthcdr(list, size, &list);
	setresult_control(ptr, list);
}

static void defun_nthcdr(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTHCDR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_nthcdr);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nth);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member (item list &key test test-not) ...) -> tail
 *   item      t
 *   list      list
 *   key       (or function-designer null))
 *   test      function-designer
 *   test-not  function-designer
 *   tail      list
 */
static int function_member_call(Execute ptr, int *result,
		addr item, addr key, addr test, addr check, int notret)
{
	if (key != Nil) {
		if (callclang_funcall(ptr, &check, key, check, NULL))
			return 1;
	}
	if (callclang_funcall(ptr, &check, test, item, check, NULL))
		return 1;
	*result = (notret? (check == Nil): (check != Nil));
	return 0;
}

static void function_member_test(Execute ptr,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr value, next;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_member_call(ptr, &check, item, key, call, value, notret))
			return;
		if (check) {
			setresult_control(ptr, list);
			return;
		}
		list = next;
	}
	setresult_control(ptr, Nil);
}

static void function_member(Execute ptr, addr item, addr list, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("MEMBER don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_member_test(ptr, item, list, key, testnot, 1);
	else if (check1)
		function_member_test(ptr, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_member_test(ptr, item, list, key, test, 0);
	}
}

static void defun_member(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_member);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member-if (call list &key key) ...) -> tail
 *   call   function-designer
 *   list   list
 *   key    (or function-designer null)
 *   tail   list
 */
static int function_member_if_call(Execute ptr, int *result,
		addr key, addr call, addr check)
{
	if (key != Nil) {
		if (callclang_funcall(ptr, &check, key, check, NULL))
			return 1;
	}
	if (callclang_funcall(ptr, &check, call, check, NULL))
		return 1;
	*result = (check != Nil);
	return 0;
}

static void function_member_if(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, next;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (check) {
			setresult_control(ptr, list);
			return;
		}
		list = next;
	}
	setresult_control(ptr, Nil);
}

static void defun_member_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_member_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun member-if-not (call list &key key) ...) -> tail
 *   call   function-designer
 *   list   list
 *   key    (or function-designer null)
 *   tail   list
 */
static void function_member_if_not(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, next;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &value, &next);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (! check) {
			setresult_control(ptr, list);
			return;
		}
		list = next;
	}
	setresult_control(ptr, Nil);
}

static void defun_member_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_member_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapc (call list &rest list) ...) -> list */
static void function_mapc(Execute ptr, addr call, addr rest)
{
	addr result, pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return;

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
	}

finish:
	rollback_local(local, stack);
	setresult_control(ptr, result);
}

static void defun_mapc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_mapc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcar (call list &rest list) ...) -> list */
static void function_mapcar(Execute ptr, addr call, addr rest)
{
	addr result, pos, car, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return;
	cons_heap(&result, pos, result);

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
		cons_heap(&result, pos, result);
	}

finish:
	rollback_local(local, stack);
	nreverse_list_unsafe(&result, result);
	setresult_control(ptr, result);
}

static void defun_mapcar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_mapcar);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcan (call list &rest list) ...) -> list */
static void setlastcdr(addr list, addr cdr)
{
	addr check;

	for (;;) {
		getcdr(list, &check);
		if (! consp(check)) {
			SetCdr(list, cdr);
			return;
		}
		list = check;
	}
}

static void function_mapcan(Execute ptr, addr call, addr rest)
{
	addr result, pos, car, cdr, args, next, temp1, temp2, head;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcons(pos, &car, &cdr);
		cons_local(local, &args, car, args);
		cons_local(local, &next, cdr, next);
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &head, call, args))
		return;
	result = head;

	/* second */
	for (;;) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			if (cdr == Nil) goto finish;
			getcons(cdr, &car, &cdr);
			SetCar(temp1, car);
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
		/* nconc */
		if (pos != Nil) {
			setlastcdr(head, pos);
			head = pos;
		}
	}

finish:
	rollback_local(local, stack);
	setresult_control(ptr, result);
}

static void defun_mapcan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_mapcan);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapl (call list &rest list) ...) -> list */
static void function_mapl(Execute ptr, addr call, addr rest)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	GetCar(rest, &result);
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return;

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
	}

finish:
	rollback_local(local, stack);
	setresult_control(ptr, result);
}

static void defun_mapl(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_mapl);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun maplist (call list &rest list) ...) -> list */
static void function_maplist(Execute ptr, addr call, addr rest)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &pos, call, args))
		return;
	cons_heap(&result, pos, result);

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
		cons_heap(&result, pos, result);
	}

finish:
	rollback_local(local, stack);
	nreverse_list_unsafe(&result, result);
	setresult_control(ptr, result);
}

static void defun_maplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPLIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_maplist);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcon (call list &rest list) ...) -> list */
static void function_mapcon(Execute ptr, addr call, addr rest)
{
	int loop;
	addr result, pos, cdr, args, next, temp1, temp2, head;
	LocalRoot local;
	LocalStack stack;

	result = Nil;
	local = ptr->local;
	push_local(local, &stack);

	/* first */
	if (rest == Nil) goto finish;
	args = next = Nil;
	loop = 1;
	while (rest != Nil) {
		getcons(rest, &pos, &rest);
		if (pos == Nil) goto finish;
		getcdr(pos, &cdr);
		cons_local(local, &args, pos, args);
		cons_local(local, &next, cdr, next);
		if (cdr == Nil) loop = 0;
	}
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&rest, next);
	if (callclang_apply(ptr, &head, call, args))
		return;
	result = head;

	/* second */
	while (loop) {
		temp1 = args;
		temp2 = rest;
		while (temp1 != Nil) {
			GetCar(temp2, &cdr);
			SetCar(temp1, cdr);
			GetCdr(cdr, &cdr);
			if (cdr == Nil) loop = 0;
			SetCar(temp2, cdr);
			GetCdr(temp1, &temp1);
			GetCdr(temp2, &temp2);
		}
		if (callclang_apply(ptr, &pos, call, args))
			return;
		/* nconc */
		if (pos != Nil) {
			setlastcdr(head, pos);
			head = pos;
		}
	}

finish:
	rollback_local(local, stack);
	setresult_control(ptr, result);
}

static void defun_mapcon(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCON, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_mapcon);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun endp (list) ...) -> boolean */
static void function_endp(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
}

static void type_endp(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_endp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENDP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_endp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_endp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun null (object) ...) -> boolean */
static void function_null(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
}

static void defun_null(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NULL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_null);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nconc (&rest object) ...) -> result
 *   object  t  ;; (list list ... . t)
 *   result  t  ;; (or list t)
 */
static void function_nconc(Execute ptr, addr list)
{
	nconc_safe(list, &list);
	setresult_control(ptr, list);
}

static void defun_nconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NCONC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_nconc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun append (&rest object) ...) -> result
 *   object  t  ;; (list list ... . t)
 *   result  t  ;; (or list t)
 */
static void function_append(Execute ptr, addr list)
{
	append_safe(list, &list);
	setresult_control(ptr, list);
}

static void defun_append(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPEND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_append);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun revappend (list tail) ...) -> object
 *   list    list
 *   tail    t
 *   object  t
 */
static void function_revappend(Execute ptr, addr list, addr tail)
{
	revappend_safe(&list, list, tail);
	setresult_control(ptr, list);
}

static void defun_revappend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REVAPPEND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_revappend);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nreconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nreconc (list tail) ...) -> object
 *   list    list
 *   tail    t
 *   object  t
 */
static void function_nreconc(Execute ptr, addr list, addr tail)
{
	nreconc_safe(&list, list, tail);
	setresult_control(ptr, list);
}

static void defun_nreconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NRECONC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_nreconc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nreconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun butlast (list &optional index) ...) -> list
 *    index  (integer 0 *)
 */
static void function_butlast(Execute ptr, addr list, addr index)
{
	size_t size;

	if (index == Unbound) {
		butlast_safe(&list, list, 1);
	}
	else {
		if (getindex_integer(index, &size))
			butlast_large(&list, list, index);
		else
			butlast_safe(&list, list, size);
	}
	setresult_control(ptr, list);
}

static void defun_butlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BUTLAST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_butlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nbutlast (list &optional index) ...) -> list
 *    index  (integer 0 *)
 */
static void function_nbutlast(Execute ptr, addr list, addr index)
{
	size_t size;

	if (index == Unbound) {
		nbutlast_safe(&list, list, 1);
	}
	else {
		if (getindex_integer(index, &size))
			nbutlast_large(&list, list, index);
		else
			nbutlast_safe(&list, list, size);
	}
	setresult_control(ptr, list);
}

static void defun_nbutlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NBUTLAST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_nbutlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun last (list &optional index) ...) -> object
 *   index   (integer 0 *)
 *   object  t
 */
static void function_last(Execute ptr, addr list, addr index)
{
	size_t size;

	if (index == Unbound) {
		last_safe(&list, list, 1);
	}
	else {
		if (getindex_integer(index, &size))
			last_large(&list, list, index);
		else
			last_safe(&list, list, size);
	}
	setresult_control(ptr, list);
}

static void type_last(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, List);
	GetTypeTable(&type, Intplus);
	typeargs_var1opt1(&arg, arg, type);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_last(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LAST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_last);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_last(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ldiff (list object) ...) -> list */
static void function_ldiff(Execute ptr, addr list, addr object)
{
	addr root, pos;

	root = Nil;
	for (;;) {
		if (list == object) {
			list = Nil;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			break;
		}
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe_dotted(&root, root, list);
	setresult_control(ptr, root);
}

static void type_ldiff(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, List);
	GetTypeTable(&type, T);
	typeargs_var2(&arg, arg, type);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_ldiff(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LDIFF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_ldiff);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ldiff(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tailp (object list) ...) -> boolean */
static void function_tailp(Execute ptr, addr object, addr list)
{
	int check;

	for (;;) {
		if (list == object) {
			check = 1;
			break;
		}
		if (GetType(list) != LISPTYPE_CONS) {
			check = 0;
			break;
		}
		GetCdr(list, &list);
	}

	setbool_control(ptr, check);
}

static void type_tailp(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, T);
	GetTypeTable(&type, List);
	typeargs_var2(&arg, arg, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_tailp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TAILP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_tailp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_tailp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acons (key datum alist) ...) -> list
 *   key    t
 *   datum  t
 *   alist  list
 */
static void function_acons(Execute ptr, addr key, addr datum, addr list)
{
	cons_heap(&key, key, datum);
	cons_heap(&list, key, list);
	setresult_control(ptr, list);
}

static void type_acons(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, T);
	GetTypeTable(&type, List);
	typeargs_var3(&arg, arg, arg, type);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_acons(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACONS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, function_acons);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_acons(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc (item list &key key test test-not) ...) -> entry
 *   item   t
 *   &key   [key, test, test-not type]
 *   entry  list
 */
static void function_assoc_test(Execute ptr,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_member_call(ptr, &check, item, key, call, value, notret))
			return;
		if (check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void function_assoc(Execute ptr, addr item, addr list, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("ASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_assoc_test(ptr, item, list, key, testnot, 1);
	else if (check1)
		function_assoc_test(ptr, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_assoc_test(ptr, item, list, key, test, 0);
	}
}

static void defun_assoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_assoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if (call list &key key) ...) -> list */
static void function_assoc_if(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_assoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_assoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if (call list &key key) ...) -> list */
static void function_assoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcar(cons, &value);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (! check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_assoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_assoc_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-alist (list) ...) -> list */
static void function_copy_alist(Execute ptr, addr list)
{
	addr root, cons, car, cdr;

	for (root = Nil; list != Nil; ) {
		getcons(list, &cons, &list);
		getcons(cons, &car, &cdr);
		cons_heap(&cons, car, cdr);
		cons_heap(&root, cons, root);
	}
	nreverse_list_unsafe(&root, root);
	setresult_control(ptr, root);
}

static void defun_copy_alist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_ALIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_copy_alist);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pairlis (keys data &optional list) ...) -> list
 *   keys    list
 *   data    list
 */
static void function_pairlis(Execute ptr, addr keys, addr data, addr list)
{
	int check1, check2;
	addr car, cdr;

	if (list == Unbound) list = Nil;
	for (;;) {
		check1 = (keys == Nil);
		check2 = (data == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			fmte("The length of keys isn't equal to the data.", NULL);
		getcons(keys, &car, &keys);
		getcons(data, &cdr, &data);
		cons_heap(&cdr, car, cdr);
		cons_heap(&list, cdr, list);
	}
	setresult_control(ptr, list);
}

static void type_pairlis(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	typeargs_var2opt1(&arg, arg, arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_pairlis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PAIRLIS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, function_pairlis);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_pairlis(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc (item list &key key test test-not) ...) -> entry
 *   item   t
 *   &key   [key, test, test-not type]
 *   entry  list
 */
static void function_rassoc_test(Execute ptr,
		addr item, addr list, addr key, addr call, int notret)
{
	int check;
	addr cons, value;

	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_member_call(ptr, &check, item, key, call, value, notret))
			return;
		if (check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void function_rassoc(Execute ptr, addr item, addr list, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("RASSOC don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_rassoc_test(ptr, item, list, key, testnot, 1);
	else if (check1)
		function_rassoc_test(ptr, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_rassoc_test(ptr, item, list, key, test, 0);
	}
}

static void defun_rassoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_rassoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static void function_rassoc_if(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_rassoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_rassoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static void function_rassoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	int check;
	addr key, value, cons;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("The list ~S don't accept dotted list.", list, NULL);
		GetCons(list, &cons, &list);
		getcdr(cons, &value);
		if (function_member_if_call(ptr, &check, key, call, value))
			return;
		if (! check) {
			setresult_control(ptr, cons);
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_rassoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF_NOT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_rassoc_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-properties (plist indicator-list) ...) -> indicator, value, tail
 *   plist           list
 *   indicator-list  list
 *   indicator       t
 *   value           t
 *   tail            list
 */
static void function_get_properties(Execute ptr, addr plist, addr indicator)
{
	addr key, value, next, list, check;

	while (plist != Nil) {
		getcons(plist, &key, &next);
		getcons(next, &value, &next);
		for (list = indicator; list != Nil; ) {
			getcons(list, &check, &list);
			if (check == key)
				goto find;
		}
		plist = next;
	}
	setvalues_control(ptr, Nil, Nil, Nil, NULL);
	return;
find:
	setvalues_control(ptr, key, value, plist, NULL);
}

static void type_get_properties(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, List);
	typeargs_var2(&arg, arg, arg);
	GetTypeTable(&type, T);
	GetTypeTable(&values, List);
	typevalues_values3(&values, type, type, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_get_properties(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_PROPERTIES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_get_properties);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_properties(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun getf (list indicator &optional default) ...) -> value
 *   indicator  t
 *   default    t   ;; default nil
 *   value      t
 */
static void function_getf(Execute ptr, addr list, addr key, addr value)
{
	if (value == Unbound) value = Nil;
	if (getplist_safe(list, key, &key))
		key = value;
	setresult_control(ptr, key);
}

static void type_getf(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, List);
	typeargs_var2opt1(&arg, values, arg, arg);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_getf(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GETF, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, function_getf);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_getf(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (define-setf-expander getf (place indicator &optional default) ...)
 *   place      t
 *   default    t   ;; default nil
 *   value      t
 */
static void define_setf_expander_getf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_GETF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, setf_getf);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro remf (place indicator) ...) -> boolean */
static void expansion_remf(Execute ptr, addr place, addr indicator, addr env)
{
	/* (let* ((a1 b1)
	 *        (a2 b2))
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-bind (g c) (remlist indicator r)
	 *     w c))
	 */
	addr list1, list2, args, x, y, c;
	addr leta, remplist, declare, ignorable, mvbind;
	addr a, b, g, w, r;

	/* get-setf-expansion */
	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	/* macro */
	GetConst(COMMON_LETA, &leta);
	GetConst(SYSTEM_REMPLIST, &remplist);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* (multiple-value-bind (g c) (remplist indicator r) w c) */
	getcar(g, &g);
	make_gensym(ptr, &c);
	list_heap(&g, g, c, NULL);
	list_heap(&remplist, remplist, indicator, r, NULL);
	list_heap(&mvbind, mvbind, g, remplist, w, c, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(&leta, leta, args, declare, mvbind, NULL);
	setresult_control(ptr, leta);
}

static void function_remf(Execute ptr, addr form, addr env)
{
	addr args, place, indicator;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (! consp(args)) goto error;
	GetCons(args, &indicator, &args);
	if (args != Nil) goto error;
	expansion_remf(ptr, place, indicator, env);
	return;

error:
	fmte("REMF argument ~S must be a (place indicator) form.", form, NULL);
}

static void defmacro_remf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_REMF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_remf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun intersection (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int intersection_test(Execute ptr, int *result,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr right;

	if (key != Nil) {
		if (callclang_funcall(ptr, &left, key, left, NULL))
			return 1;
	}
	while (list != Nil) {
		getcons(list, &right, &list);
		if (function_member_call(ptr, &check, left, key, test, right, notret))
			return 1;
		if (check) {
			*result = 1;
			return 0;
		}
	}
	*result = 0;
	return 0;
}

static void function_intersection_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;

	for (list = Nil; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return;
		if (check)
			cons_heap(&list, left, list);
	}
	setresult_control(ptr, list);
}

static void function_intersection(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("INTERSECTION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_intersection_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_intersection_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_intersection_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_intersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERSECTION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_intersection);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nintersection (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static void function_nintersection_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left, next1, next2;

	/* first */
	list = list1;
	for (;;) {
		getcons(list1, &left, &next1);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return;
		if (check)
			break;
		list = list1 = next1;
		if (list1 == Nil)
			goto finish;
	}

	/* tail */
	while (list1 != Nil) {
		getcons(list1, &left, &next1);
		while (next1 != Nil) {
			getcons(next1, &left, &next2);
			if (intersection_test(ptr, &check, left, list2, key, test, notret))
				return;
			if (! check)
				break;
			next1 = next2;
		}
		if (next1 == Nil)
			goto finish;
		setcdr(list1, next2);
		list1 = next2;
	}
finish:
	setresult_control(ptr, list);
}

static void function_nintersection(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NINTERSECTION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_nintersection_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_nintersection_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_nintersection_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_nintersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NINTERSECTION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_nintersection);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjoin (item list &key key test test-not) ...) -> list
 *   item  t
 */
static void function_adjoin_test(Execute ptr,
		addr left, addr list, addr key, addr test, int notret)
{
	int check;
	addr find, right;

	for (find = list; find != Nil; ) {
		getcons(find, &right, &find);
		if (function_member_call(ptr, &check, left, key, test, right, notret))
			return;
		if (check) {
			setresult_control(ptr, list);
			return;
		}
	}
	cons_heap(&list, left, list);
	setresult_control(ptr, list);
}

static void function_adjoin(Execute ptr, addr item, addr list, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("ADJOIN don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_adjoin_test(ptr, item, list, key, testnot, 1);
	else if (check1)
		function_adjoin_test(ptr, item, list, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_adjoin_test(ptr, item, list, key, test, 0);
	}
}

static void type_adjoin(addr *ret)
{
	addr arg, values, key;

	GetTypeTable(&arg, T);
	GetTypeTable(&values, List);
	GetTypeTable(&key, KeyTestList);
	typeargs_var2key(&arg, arg, values, key);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_adjoin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ADJOIN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_adjoin);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_adjoin(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro pushnew (item place &rest args) ...) -> value
 *   item   t
 *   place  setf-place
 *   value  t
 */
static void expansion_pushnew_single(Execute ptr,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        (g (adjoin value r . rest)))
	 *   (declare (ignorable a1 a2))
	 *   w g)
	 */
	addr list1, list2, leta, adjoin, declare, ignorable, args, x, y;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	/* (an bn) */
	list1 = a;
	list2 = b;
	args = Nil;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g (adjoin value r . rest)) */
	getcar(g, &g);
	lista_heap(&adjoin, adjoin, item, r, rest, NULL);
	list_heap(&x, g, adjoin, NULL);
	cons_heap(&args, x, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	nreverse_list_unsafe(&args, args);
	list_heap(&leta, leta, args, declare, w, g, NULL);
	setresult_control(ptr, leta);
}

static void expansion_pushnew_multiple(Execute ptr,
		addr item, addr rest, addr a, addr b, addr g, addr w, addr r)
{
	/* (let* ((v value)
	 *        (a1 b1)
	 *        (a2 b2)
	 *        g1 g2 ...)
	 *   (declare (ignorable a1 a2))
	 *   (multiple-value-setq (g1 g2 ...) r)
	 *   (setq g1 (adjoin v g1 . rest))
	 *   (setq g2 (adjoin v g2 . rest))
	 *   ....
	 *   w
	 *   (values g1 g2 ...))
	 */
	addr leta, adjoin, declare, ignorable, mvsetq, setq, values;
	addr list1, list2, args, v, x, y, pos;

	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_ADJOIN, &adjoin);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MULTIPLE_VALUE_SETQ, &mvsetq);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_VALUES, &values);
	/* (v value) */
	make_gensym(ptr, &v);
	list_heap(&args, v, item, NULL);
	conscar_heap(&args, args);
	/* (an bn) */
	list1 = a;
	list2 = b;
	while (list1 != Nil) {
		GetCons(list1, &x, &list1);
		GetCons(list2, &y, &list2);
		list_heap(&x, x, y, NULL);
		cons_heap(&args, x, args);
	}
	/* (g1 g2 ...) */
	nreconc_unsafe(&args, args, g);
	conscar_heap(&pos, args);
	/* (declare (ignorable a1 a2)) */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, a);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	cons_heap(&pos, declare, pos);
	/* (multiple-value-setq (g1 g2 ...) r */
	list_heap(&mvsetq, mvsetq, g, r, NULL);
	/* (setq g1 (adjoin v g1)) */
	for (list1 = g; list1 != Nil; ) {
		GetCons(list1, &x, &list1);
		lista_heap(&y, adjoin, v, x, rest, NULL);
		list_heap(&x, setq, x, y, NULL);
		cons_heap(&pos, x, pos);
	}
	/* w */
	cons_heap(&pos, w, pos);
	/* (values g1 g2 ...) */
	cons_heap(&values, values, g);
	cons_heap(&pos, values, pos);
	/* let* */
	nreverse_list_unsafe(&pos, pos);
	setresult_control(ptr, pos);
}

static void expansion_pushnew(Execute ptr, addr item, addr place, addr rest, addr env)
{
	addr a, b, g, w, r;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	if (singlep(g))
		expansion_pushnew_single(ptr, item, rest, a, b, g, w, r);
	else
		expansion_pushnew_multiple(ptr, item, rest, a, b, g, w, r);
}

static void function_pushnew(Execute ptr, addr form, addr env)
{
	addr args, item, place;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &item, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	expansion_pushnew(ptr, item, place, args, env);
	return;

error:
	fmte("PUSH argument ~S must be a (item place &rest args) form.", form, NULL);
}

static void defmacro_pushnew(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSHNEW, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_pushnew);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun set-difference (list1 list2) &key key test test-not) ...) -> list */
static void function_set_difference_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;

	for (list = Nil; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return;
		if (! check)
			cons_heap(&list, left, list);
	}
	setresult_control(ptr, list);
}

static void function_set_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("SET-DIFFERENCE don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_set_difference_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_set_difference_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_set_difference_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_set_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_DIFFERENCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_set_difference);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nset-difference (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int nset_difference_test(Execute ptr, int *result, addr *ret,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left, next1, next2;

	/* first */
	list = list1;
	for (;;) {
		getcons(list1, &left, &next1);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return 1;
		if (! check)
			break;
		list = list1 = next1;
		if (list1 == Nil)
			goto finish;
	}

	/* tail */
	while (list1 != Nil) {
		getcons(list1, &left, &next1);
		while (next1 != Nil) {
			getcons(next1, &left, &next2);
			if (intersection_test(ptr, &check, left, list2, key, test, notret))
				return 1;
			if (check)
				break;
			next1 = next2;
		}
		if (next1 == Nil)
			goto finish;
		setcdr(list1, next2);
		list1 = next2;
	}
finish:
	*ret = list;
	return 0;
}

static void function_nset_difference_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	if (nset_difference_test(ptr, &check, &list1, list1, list2, key, test, notret))
		return;
	setresult_control(ptr, list1);
}

static void function_nset_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NSET-DIFFERENCE don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_nset_difference_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_nset_difference_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_nset_difference_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_nset_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_DIFFERENCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_nset_difference);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-exclusive-or (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static void function_set_exclusive_or_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr result, list, left;

	result = Nil;
	/* left -> right */
	for (list = list1; list != Nil; ) {
		getcons(list, &left, &list);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return;
		if (! check)
			cons_heap(&result, left, result);
	}

	/* right -> left */
	for (list = list2; list != Nil; ) {
		getcons(list, &left, &list);
		if (intersection_test(ptr, &check, left, list1, key, test, notret))
			return;
		if (! check)
			cons_heap(&result, left, result);
	}

	/* result */
	setresult_control(ptr, result);
}

static void function_set_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		fmte("SET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		function_set_exclusive_or_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_set_exclusive_or_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_set_exclusive_or_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_set_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_EXCLUSIVE_OR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_set_exclusive_or);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nset-exclusive-or (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static void function_nset_exclusive_or_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr result, list, left;

	/* right -> left */
	result = Nil;
	for (list = list2; list != Nil; ) {
		getcons(list, &left, &list);
		if (intersection_test(ptr, &check, left, list1, key, test, notret))
			return;
		if (! check)
			cons_heap(&result, left, result);
	}

	/* left -> right */
	if (nset_difference_test(ptr, &check, &list1, list1, list2, key, test, notret))
		return;
	nconc2_safe(result, list1, &result);

	/* result */
	setresult_control(ptr, result);
}

static void function_nset_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2) {
		fmte("NSET-EXCLUSIVE-OR "
				"don't accept both :test and :test-not parameter.", NULL);
	}
	else if (check2)
		function_nset_exclusive_or_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_nset_exclusive_or_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_nset_exclusive_or_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_nset_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_EXCLUSIVE_OR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_nset_exclusive_or);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subsetp (list1 list2 &key test test-not) ...) -> boolean */
static void function_subsetp_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left, result;

	for (result = T; list1 != Nil; ) {
		getcons(list1, &left, &list1);
		if (intersection_test(ptr, &check, left, list2, key, test, notret))
			return;
		if (! check) {
			result = Nil;
			break;
		}
	}
	setresult_control(ptr, result);
}

static void function_subsetp(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("SUBSETP don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_subsetp_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_subsetp_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_subsetp_test(ptr, list1, list2, key, test, 0);
	}
}

static void type_subsetp(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, List);
	GetTypeTable(&values, KeyTestList);
	typeargs_var2key(&arg, arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_subsetp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBSETP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_subsetp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_subsetp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun union (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static void function_union_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr list, left;

	/* left */
	list = Nil;
	while (list1 != Nil) {
		getcons(list1, &left, &list1);
		if (intersection_test(ptr, &check, left, list, key, test, notret))
			return;
		if (! check)
			cons_heap(&list, left, list);
	}

	/* right */
	while (list2 != Nil) {
		getcons(list2, &left, &list2);
		if (intersection_test(ptr, &check, left, list, key, test, notret))
			return;
		if (! check)
			cons_heap(&list, left, list);
	}

	/* result */
	setresult_control(ptr, list);
}

static void function_union(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("UNION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_union_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_union_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_union_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_union(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_union);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nunion (list1 list2 &key test test-not) ...) -> result
 *   list1   list
 *   list2   list
 *   result  list
 */
static int nunion_single(Execute ptr, addr *ret,
		addr list1, addr key, addr test, int notret)
{
	int check;
	addr list2, list3, left, right;

	/* nil */
	if (list1 == Nil) {
		*ret = Nil;
		return 0;
	}

	/* single */
	getcons(list1, &left, &list2);
	*ret = list1;
	if (list2 == Nil) {
		return 0;
	}
	if (key != Nil) {
		if (callclang_funcall(ptr, &left, key, left, NULL))
			return 1;
	}

	/* list */
	while (list2 != Nil) {
		getcons(list2, &right, &list3);
		if (function_member_call(ptr, &check, left, key, test, right, notret))
			return 1;
		if (check) {
			SetCdr(list1, list3);
			list2 = list3;
		}
		else {
			left = right;
			if (key != Nil) {
				if (callclang_funcall(ptr, &left, key, left, NULL))
					return 1;
			}
			list1 = list2;
			list2 = list3;
		}
	}

	return 0;
}

static void function_nunion_test(Execute ptr,
		addr list1, addr list2, addr key, addr test, int notret)
{
	int check;
	addr left;

	/* left */
	if (nunion_single(ptr, &list1, list1, key, test, notret))
		return;

	/* right */
	while (list2 != Nil) {
		getcons(list2, &left, &list2);
		if (intersection_test(ptr, &check, left, list1, key, test, notret))
			return;
		if (! check)
			cons_heap(&list1, left, list1);
	}

	/* result */
	setresult_control(ptr, list1);
}

static void function_nunion(Execute ptr, addr list1, addr list2, addr rest)
{
	int check1, check2;
	addr key, test, testnot;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (getkeyargs(rest, KEYWORD_TEST, &test)) test = Unbound;
	if (getkeyargs(rest, KEYWORD_TEST_NOT, &testnot)) testnot = Unbound;
	check1 = (test != Unbound);
	check2 = (testnot != Unbound);
	if (check1 && check2)
		fmte("NUNION don't accept both :test and :test-not parameter.", NULL);
	else if (check2)
		function_nunion_test(ptr, list1, list2, key, testnot, 1);
	else if (check1)
		function_nunion_test(ptr, list1, list2, key, test, 0);
	else {
		GetConst(COMMON_EQL, &test);
		function_nunion_test(ptr, list1, list2, key, test, 0);
	}
}

static void defun_nunion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUNION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, function_nunion);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
void intern_common_conses(void)
{
	defun_cons();
	defun_consp();
	defun_atom();
	defun_rplaca();
	defun_rplacd();
	defun_car();
	defun_setf_car();
	defun_first();
	defun_setf_first();
	defun_copy_list();
	defun_copy_tree();
	defun_sublis();
	defun_nsublis();
	defun_subst();
	defun_nsubst();
	defun_subst_if();
	defun_nsubst_if();
	defun_subst_if_not();
	defun_nsubst_if_not();
	defun_tree_equal();
	defun_list();
	defun_lista();
	defun_list_length();
	defun_listp();
	defun_make_list();
	defmacro_push();
	defmacro_pop();
	defun_nth();
	defun_setf_nth();
	defun_endp();
	defun_null();
	defun_nconc();
	defun_append();
	defun_revappend();
	defun_nreconc();
	defun_butlast();
	defun_nbutlast();
	defun_last();
	defun_ldiff();
	defun_tailp();
	defun_nthcdr();
	defun_member();
	defun_member_if();
	defun_member_if_not();
	defun_mapc();
	defun_mapcar();
	defun_mapcan();
	defun_mapl();
	defun_maplist();
	defun_mapcon();
	defun_acons();
	defun_assoc();
	defun_assoc_if();
	defun_assoc_if_not();
	defun_copy_alist();
	defun_pairlis();
	defun_rassoc();
	defun_rassoc_if();
	defun_rassoc_if_not();
	defun_get_properties();
	defun_getf();
	define_setf_expander_getf();
	defmacro_remf();
	defun_intersection();
	defun_nintersection();
	defun_adjoin();
	defmacro_pushnew();
	defun_set_difference();
	defun_nset_difference();
	defun_set_exclusive_or();
	defun_nset_exclusive_or();
	defun_subsetp();
	defun_union();
	defun_nunion();
}

