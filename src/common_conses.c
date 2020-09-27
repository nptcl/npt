/*
 *  ANSI COMMON LISP: 14. Conses
 */
#include "call_conses.h"
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "setf.h"

/* (defun cons (object1 object2) ...) -> cons */
static int function_cons(Execute ptr, addr var1, addr var2)
{
	cons_heap(&var1, var1, var2);
	setresult_control(ptr, var1);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_cons);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cons_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun consp (object) ...) -> boolean */
static int function_consp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_CONS);
	return 0;
}

static void defun_consp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_consp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atom (object) ...) -> boolean */
static int function_atom(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) != LISPTYPE_CONS);
	return 0;
}

static void defun_atom(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATOM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_atom);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplaca (cons object) ...) -> cons */
static int function_rplaca(Execute ptr, addr cons, addr object)
{
	SetCar(cons, object);
	setresult_control(ptr, cons);
	return 0;
}

static void defun_rplaca(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACA, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rplaca);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Rplaca);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rplacd (cons object) ...) -> cons */
static int function_rplacd(Execute ptr, addr cons, addr object)
{
	SetCdr(cons, object);
	setresult_control(ptr, cons);
	return 0;
}

static void defun_rplacd(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RPLACD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_rplacd);
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

static void defun_cxr(constindex index, pointer p, enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cxr(&type, cxr);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int function_car(Execute ptr, addr list)
{
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_caddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cadddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdadar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdaddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddaar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddadr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cdddar(Execute ptr, addr list)
{
	GetCar(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

static int function_cddddr(Execute ptr, addr list)
{
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	GetCdr(list, &list);
	setresult_control(ptr, list);
	return 0;
}

#define DefunCxr(x,y,z) defun_cxr(CONSTANT_COMMON_##x, p_defun_##y, TypeTable_##z)
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

static void defun_setf_cxr(constindex index, pointer p, enum TypeTable cxr)
{
	addr symbol, pos, type;

	/* function */
	GetConstant(index, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_cxr(&type, cxr);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int function_setf_car(Execute ptr, addr value, addr cons)
{
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdr(Execute ptr, addr value, addr cons)
{
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_caddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cadddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCar(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdadar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdaddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddaar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddadr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cdddar(Execute ptr, addr value, addr cons)
{
	GetCar(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_cddddr(Execute ptr, addr value, addr cons)
{
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	GetCdr(cons, &cons);
	SetCdr(cons, value);
	setresult_control(ptr, value);
	return 0;
}

#define DefunSetfCxr(x,y,z) { \
	defun_setf_cxr(CONSTANT_COMMON_##x, p_defun_setf_##y, TypeTable_##z); \
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
static int function_fifth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCar(list, &list); /*5*/
	setresult_control(ptr, list);
	return 0;
}

static int function_sixth(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCar(list, &list); /*6*/
	setresult_control(ptr, list);
	return 0;
}

static int function_seventh(Execute ptr, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	GetCar(list, &list); /*7*/
	setresult_control(ptr, list);
	return 0;
}

static int function_eighth(Execute ptr, addr list)
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
	return 0;
}

static int function_ninth(Execute ptr, addr list)
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
	return 0;
}

static int function_tenth(Execute ptr, addr list)
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
	return 0;
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
static int function_setf_fifth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_sixth(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_seventh(Execute ptr, addr value, addr list)
{
	GetCdr(list, &list); /*1*/
	GetCdr(list, &list); /*2*/
	GetCdr(list, &list); /*3*/
	GetCdr(list, &list); /*4*/
	GetCdr(list, &list); /*5*/
	GetCdr(list, &list); /*6*/
	SetCar(list, value);
	setresult_control(ptr, value);
	return 0;
}

static int function_setf_eighth(Execute ptr, addr value, addr list)
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
	return 0;
}

static int function_setf_ninth(Execute ptr, addr value, addr list)
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
	return 0;
}

static int function_setf_tenth(Execute ptr, addr value, addr list)
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
	return 0;
}

static void defun_setf_first(void)
{
	DefunSetfCxr(REST, cdr, Cons);
	DefunSetfCxr(FIRST, car, Cons);
	DefunSetfCxr(SECOND, cadr, SetfCxdr);
	DefunSetfCxr(THIRD, caddr, SetfCxddr);
	DefunSetfCxr(FOURTH, cadddr, SetfCxdddr);
	DefunSetfCxr(FIFTH, fifth, SetfFifth);
	DefunSetfCxr(SIXTH, sixth, SetfSixth);
	DefunSetfCxr(SEVENTH, seventh, SetfSeventh);
	DefunSetfCxr(EIGHTH, eighth, SetfEighth);
	DefunSetfCxr(NINTH, ninth, SetfNinth);
	DefunSetfCxr(TENTH, tenth, SetfTenth);
}


/* (defun copy-list (list) ...) -> list */
static int function_copy_list(Execute ptr, addr list)
{
	copy_list_heap_safe(&list, list);
	setresult_control(ptr, list);
	return 0;
}

static void defun_copy_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, List_List);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-tree (t) ...) -> t */
static int function_copy_tree(Execute ptr, addr list)
{
	copy_tree_heap(&list, list);
	setresult_control(ptr, list);
	return 0;
}

static void type_copy_tree(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_tree(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_TREE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_tree);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_tree(&type);
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
static int function_sublis(Execute ptr, addr alist, addr tree, addr rest)
{
	Return(sublis_common(ptr, alist, tree, rest, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_sublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBLIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_sublis);
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
static int function_nsublis(Execute ptr, addr alist, addr tree, addr rest)
{
	Return(nsublis_common(ptr, alist, tree, rest, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsublis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBLIS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nsublis);
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
static int function_subst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	Return(subst_common(ptr, one, old, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst);
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
static int function_nsubst(Execute ptr, addr one, addr old, addr tree, addr key)
{
	Return(nsubst_common(ptr, one, old, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst);
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
static int function_subst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(subst_if_common(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst_if);
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
static int function_nsubst_if(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(nsubst_if_common(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst_if);
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
static int function_subst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(subst_if_not_common(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_subst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SUBST_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_subst_if_not);
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
static int function_nsubst_if_not(Execute ptr,
		addr one, addr predicate, addr tree, addr key)
{
	Return(nsubst_if_not_common(ptr, one, predicate, tree, key, &tree));
	setresult_control(ptr, tree);
	return 0;
}

static void defun_nsubst_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSUBST_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_nsubst_if_not);
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
static int function_tree_equal(Execute ptr, addr tree1, addr tree2, addr key)
{
	int result;

	Return(tree_equal_common(ptr, tree1, tree2, key, &result));
	setbool_control(ptr, result);

	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_tree_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_tree_equal(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list (&rest objests) ...) -> list */
static int function_list(Execute ptr, addr rest)
{
	setresult_control(ptr, rest);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list* (object &rest objects) ...) -> object */
static int function_lista(Execute ptr, addr var, addr rest)
{
	Return(lista_safe_heap_(&var, var, rest));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_lista);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_lista_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-length (list) ...) -> (or index null) */
static int function_list_length(Execute ptr, addr list)
{
	Return(list_length_common(list, &list));
	setresult_control(ptr, list);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_list_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun listp (object) ...) -> boolean */
static int function_listp(Execute ptr, addr var)
{
	setbool_control(ptr, IsList(var));
	return 0;
}

static void defun_listp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_listp);
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
static int function_make_list(Execute ptr, addr var, addr rest)
{
	Return(make_list_common(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_list);
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
static int function_push(Execute ptr, addr form, addr env)
{
	Return(push_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_push(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSH, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_push);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro pop (place) ...) -> t */
static int function_pop(Execute ptr, addr form, addr env)
{
	Return(pop_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_pop(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_POP, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pop);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun nth (index list) ...) -> object
 *   index  (integer 0 *)  ;; Don't use index (SizeMax)
 *   list   list
 */
static int function_nth(Execute ptr, addr index, addr list)
{
	Return(nth_common(index, list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nth(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nth);
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
static int function_setf_nth(Execute ptr, addr value, addr index, addr list)
{
	Return(setf_nth_common(value, index, list));
	setresult_control(ptr, value);
	return 0;
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
	compiled_setf_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_setf_nth);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_nth(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun nthcdr (index list) ...) -> object */
static int function_nthcdr(Execute ptr, addr index, addr list)
{
	Return(nthcdr_common(index, list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nthcdr(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NTHCDR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nthcdr);
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
static int function_member(Execute ptr, addr item, addr list, addr rest)
{
	Return(member_common(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_member(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member);
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
static int function_member_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(member_if_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_member_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member_if);
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
static int function_member_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(member_if_not_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_member_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MEMBER_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_member_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapc (call list &rest list) ...) -> list */
static int function_mapc(Execute ptr, addr call, addr rest)
{
	Return(mapc_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcar (call list &rest list) ...) -> list */
static int function_mapcar(Execute ptr, addr call, addr rest)
{
	Return(mapcar_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcar(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcar);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcan (call list &rest list) ...) -> list */
static int function_mapcan(Execute ptr, addr call, addr rest)
{
	Return(mapcan_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCAN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcan);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapl (call list &rest list) ...) -> list */
static int function_mapl(Execute ptr, addr call, addr rest)
{
	Return(mapl_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapl(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapl);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun maplist (call list &rest list) ...) -> list */
static int function_maplist(Execute ptr, addr call, addr rest)
{
	Return(maplist_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_maplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_maplist);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mapcon (call list &rest list) ...) -> list */
static int function_mapcon(Execute ptr, addr call, addr rest)
{
	Return(mapcon_common(ptr, call, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_mapcon(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAPCON, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_mapcon);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Mapc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun endp (list) ...) -> boolean */
static int function_endp(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_endp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_endp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun null (object) ...) -> boolean */
static int function_null(Execute ptr, addr list)
{
	setbool_control(ptr, list == Nil);
	return 0;
}

static void defun_null(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NULL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_null);
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
static int function_nconc(Execute ptr, addr list)
{
	Return(nconc_common(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NCONC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_nconc);
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
static int function_append(Execute ptr, addr list)
{
	Return(append_common(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_append(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APPEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_append);
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
static int function_revappend(Execute ptr, addr list, addr tail)
{
	Return(revappend_common(list, tail, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_revappend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REVAPPEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_revappend);
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
static int function_nreconc(Execute ptr, addr list, addr tail)
{
	Return(nreconc_common(list, tail, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nreconc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NRECONC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_nreconc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Nreconc);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun butlast (list &optional intplus) ...) -> list
 *    index  (integer 0 *)
 */
static int function_butlast(Execute ptr, addr list, addr index)
{
	Return(butlast_common(list, index, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_butlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BUTLAST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_butlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun nbutlast (list &optional intplus) ...) -> list
 *    index  (integer 0 *)
 */
static int function_nbutlast(Execute ptr, addr list, addr index)
{
	Return(nbutlast_common(list, index, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_nbutlast(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NBUTLAST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_nbutlast);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ButLast);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun last (list &optional intplus) ...) -> object
 *   index   (integer 0 *)
 *   object  t
 */
static int function_last(Execute ptr, addr list, addr index)
{
	Return(last_common(list, index, &list));
	setresult_control(ptr, list);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_last);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_last(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ldiff (list object) ...) -> list */
static int function_ldiff(Execute ptr, addr list, addr object)
{
	ldiff_common(list, object, &list);
	setresult_control(ptr, list);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_ldiff);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ldiff(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tailp (object list) ...) -> boolean */
static int function_tailp(Execute ptr, addr object, addr list)
{
	int check;

	tailp_common(object, list, &check);
	setbool_control(ptr, check);

	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_tailp);
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
static int function_acons(Execute ptr, addr key, addr datum, addr list)
{
	cons_heap(&key, key, datum);
	cons_heap(&list, key, list);
	setresult_control(ptr, list);

	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_acons);
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
static int function_assoc(Execute ptr, addr item, addr list, addr rest)
{
	Return(assoc_common(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_assoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if (call list &key key) ...) -> list */
static int function_assoc_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(assoc_if_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_assoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun assoc-if-not (call list &key key) ...) -> list */
static int function_assoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(assoc_if_not_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_assoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASSOC_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_assoc_if_not);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-alist (list) ...) -> list */
static int function_copy_alist(Execute ptr, addr list)
{
	Return(copy_alist_common(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_copy_alist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_ALIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_copy_alist);
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
static int function_pairlis(Execute ptr, addr keys, addr data, addr list)
{
	Return(pairlis_common(keys, data, list, &list));
	setresult_control(ptr, list);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_pairlis);
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
static int function_rassoc(Execute ptr, addr item, addr list, addr rest)
{
	Return(rassoc_common(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
}

static void defun_rassoc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Member);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static int function_rassoc_if(Execute ptr, addr call, addr list, addr rest)
{
	Return(rassoc_if_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_rassoc_if(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc_if);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MemberIf);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rassoc-if (call list &key key) ...) -> list */
static int function_rassoc_if_not(Execute ptr, addr call, addr list, addr rest)
{
	Return(rassoc_if_not_common(ptr, call, list, rest, &list));
	setresult_control(ptr, list);
	return 0;
}

static void defun_rassoc_if_not(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RASSOC_IF_NOT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_rassoc_if_not);
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
static int function_get_properties(Execute ptr, addr plist, addr indicator)
{
	addr key, value, list;

	Return(get_properties_common(plist, indicator, &key, &value, &list));
	setvalues_control(ptr, key, value, list, NULL);

	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_get_properties);
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
static int function_getf(Execute ptr, addr list, addr key, addr value)
{
	Return(getf_common(list, key, value, &value));
	setresult_control(ptr, value);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_getf);
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
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_setf_getf);
	SetSetfMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro remf (place indicator) ...) -> boolean */
static int function_remf(Execute ptr, addr form, addr env)
{
	Return(remf_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_remf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_REMF, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_remf);
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
static int function_intersection(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(intersection_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_intersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERSECTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_intersection);
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
static int function_nintersection(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nintersection_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nintersection(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NINTERSECTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nintersection);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun adjoin (item list &key key test test-not) ...) -> list
 *   item  t
 */
static int function_adjoin(Execute ptr, addr item, addr list, addr rest)
{
	Return(adjoin_common(ptr, item, list, rest, &item));
	setresult_control(ptr, item);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_adjoin);
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
static int function_pushnew(Execute ptr, addr form, addr env)
{
	Return(pushnew_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_pushnew(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_PUSHNEW, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_pushnew);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun set-difference (list1 list2) &key key test test-not) ...) -> list */
static int function_set_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(set_difference_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_set_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_DIFFERENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_set_difference);
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
static int function_nset_difference(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nset_difference_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nset_difference(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_DIFFERENCE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nset_difference);
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
static int function_set_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(set_exclusive_or_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_set_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET_EXCLUSIVE_OR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_set_exclusive_or);
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
static int function_nset_exclusive_or(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nset_exclusive_or_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nset_exclusive_or(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NSET_EXCLUSIVE_OR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nset_exclusive_or);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subsetp (list1 list2 &key test test-not) ...) -> boolean */
static int function_subsetp(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(subsetp_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_subsetp);
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
static int function_union(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(union_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_union(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_union);
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
static int function_nunion(Execute ptr, addr list1, addr list2, addr rest)
{
	Return(nunion_common(ptr, list1, list2, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_nunion(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUNION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_nunion);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intersection);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_conses(void)
{
	SetPointerCall(defun, var1, car);
	SetPointerCall(defun, var1, cdr);
	SetPointerCall(defun, var1, caar);
	SetPointerCall(defun, var1, cadr);
	SetPointerCall(defun, var1, cdar);
	SetPointerCall(defun, var1, cddr);
	SetPointerCall(defun, var1, caaar);
	SetPointerCall(defun, var1, caadr);
	SetPointerCall(defun, var1, cadar);
	SetPointerCall(defun, var1, caddr);
	SetPointerCall(defun, var1, cdaar);
	SetPointerCall(defun, var1, cdadr);
	SetPointerCall(defun, var1, cddar);
	SetPointerCall(defun, var1, cdddr);
	SetPointerCall(defun, var1, caaaar);
	SetPointerCall(defun, var1, caaadr);
	SetPointerCall(defun, var1, caadar);
	SetPointerCall(defun, var1, caaddr);
	SetPointerCall(defun, var1, cadaar);
	SetPointerCall(defun, var1, cadadr);
	SetPointerCall(defun, var1, caddar);
	SetPointerCall(defun, var1, cadddr);
	SetPointerCall(defun, var1, cdaaar);
	SetPointerCall(defun, var1, cdaadr);
	SetPointerCall(defun, var1, cdadar);
	SetPointerCall(defun, var1, cdaddr);
	SetPointerCall(defun, var1, cddaar);
	SetPointerCall(defun, var1, cddadr);
	SetPointerCall(defun, var1, cdddar);
	SetPointerCall(defun, var1, cddddr);
	SetPointerCall(defun, var1, fifth);
	SetPointerCall(defun, var1, sixth);
	SetPointerCall(defun, var1, seventh);
	SetPointerCall(defun, var1, eighth);
	SetPointerCall(defun, var1, ninth);
	SetPointerCall(defun, var1, tenth);

	SetPointerCall(defun, var2, setf_car);
	SetPointerCall(defun, var2, setf_cdr);
	SetPointerCall(defun, var2, setf_caar);
	SetPointerCall(defun, var2, setf_cadr);
	SetPointerCall(defun, var2, setf_cdar);
	SetPointerCall(defun, var2, setf_cddr);
	SetPointerCall(defun, var2, setf_caaar);
	SetPointerCall(defun, var2, setf_caadr);
	SetPointerCall(defun, var2, setf_cadar);
	SetPointerCall(defun, var2, setf_caddr);
	SetPointerCall(defun, var2, setf_cdaar);
	SetPointerCall(defun, var2, setf_cdadr);
	SetPointerCall(defun, var2, setf_cddar);
	SetPointerCall(defun, var2, setf_cdddr);
	SetPointerCall(defun, var2, setf_caaaar);
	SetPointerCall(defun, var2, setf_caaadr);
	SetPointerCall(defun, var2, setf_caadar);
	SetPointerCall(defun, var2, setf_caaddr);
	SetPointerCall(defun, var2, setf_cadaar);
	SetPointerCall(defun, var2, setf_cadadr);
	SetPointerCall(defun, var2, setf_caddar);
	SetPointerCall(defun, var2, setf_cadddr);
	SetPointerCall(defun, var2, setf_cdaaar);
	SetPointerCall(defun, var2, setf_cdaadr);
	SetPointerCall(defun, var2, setf_cdadar);
	SetPointerCall(defun, var2, setf_cdaddr);
	SetPointerCall(defun, var2, setf_cddaar);
	SetPointerCall(defun, var2, setf_cddadr);
	SetPointerCall(defun, var2, setf_cdddar);
	SetPointerCall(defun, var2, setf_cddddr);
	SetPointerCall(defun, var2, setf_fifth);
	SetPointerCall(defun, var2, setf_sixth);
	SetPointerCall(defun, var2, setf_seventh);
	SetPointerCall(defun, var2, setf_eighth);
	SetPointerCall(defun, var2, setf_ninth);
	SetPointerCall(defun, var2, setf_tenth);

	SetPointerCall(defun, var2, cons);
	SetPointerCall(defun, var1, consp);
	SetPointerCall(defun, var1, atom);
	SetPointerCall(defun, var2, rplaca);
	SetPointerCall(defun, var2, rplacd);
	SetPointerCall(defun, var1, copy_list);
	SetPointerCall(defun, var1, copy_tree);
	SetPointerCall(defun, var2dynamic, sublis);
	SetPointerCall(defun, var2dynamic, nsublis);
	SetPointerCall(defun, var3dynamic, subst);
	SetPointerCall(defun, var3dynamic, nsubst);
	SetPointerCall(defun, var3dynamic, subst_if);
	SetPointerCall(defun, var3dynamic, nsubst_if);
	SetPointerCall(defun, var3dynamic, subst_if_not);
	SetPointerCall(defun, var3dynamic, nsubst_if_not);
	SetPointerCall(defun, var2dynamic, tree_equal);
	SetPointerCall(defun, rest, list);
	SetPointerCall(defun, var1dynamic, lista);
	SetPointerCall(defun, var1, list_length);
	SetPointerCall(defun, var1, listp);
	SetPointerCall(defun, var1dynamic, make_list);
	SetPointerCall(defmacro, macro, push);
	SetPointerCall(defmacro, macro, pop);
	SetPointerCall(defun, var2, nth);
	SetPointerCall(defun, var3, setf_nth);
	SetPointerCall(defun, var2, nthcdr);
	SetPointerCall(defun, var2dynamic, member);
	SetPointerCall(defun, var2dynamic, member_if);
	SetPointerCall(defun, var2dynamic, member_if_not);
	SetPointerCall(defun, var1dynamic, mapc);
	SetPointerCall(defun, var1dynamic, mapcar);
	SetPointerCall(defun, var1dynamic, mapcan);
	SetPointerCall(defun, var1dynamic, mapl);
	SetPointerCall(defun, var1dynamic, maplist);
	SetPointerCall(defun, var1dynamic, mapcon);
	SetPointerCall(defun, var1, endp);
	SetPointerCall(defun, var1, null);
	SetPointerCall(defun, dynamic, nconc);
	SetPointerCall(defun, dynamic, append);
	SetPointerCall(defun, var2, revappend);
	SetPointerCall(defun, var2, nreconc);
	SetPointerCall(defun, var1opt1, butlast);
	SetPointerCall(defun, var1opt1, nbutlast);
	SetPointerCall(defun, var1opt1, last);
	SetPointerCall(defun, var2, ldiff);
	SetPointerCall(defun, var2, tailp);
	SetPointerCall(defun, var3, acons);
	SetPointerCall(defun, var2dynamic, assoc);
	SetPointerCall(defun, var2dynamic, assoc_if);
	SetPointerCall(defun, var2dynamic, assoc_if_not);
	SetPointerCall(defun, var1, copy_alist);
	SetPointerCall(defun, var2opt1, pairlis);
	SetPointerCall(defun, var2dynamic, rassoc);
	SetPointerCall(defun, var2dynamic, rassoc_if);
	SetPointerCall(defun, var2dynamic, rassoc_if_not);
	SetPointerCall(defun, var2, get_properties);
	SetPointerCall(defun, var2opt1, getf);
	SetPointerCall(defmacro, macro, setf_getf);
	SetPointerCall(defmacro, macro, remf);
	SetPointerCall(defun, var2dynamic, intersection);
	SetPointerCall(defun, var2dynamic, nintersection);
	SetPointerCall(defun, var2dynamic, adjoin);
	SetPointerCall(defmacro, macro, pushnew);
	SetPointerCall(defun, var2dynamic, set_difference);
	SetPointerCall(defun, var2dynamic, nset_difference);
	SetPointerCall(defun, var2dynamic, set_exclusive_or);
	SetPointerCall(defun, var2dynamic, nset_exclusive_or);
	SetPointerCall(defun, var2dynamic, subsetp);
	SetPointerCall(defun, var2dynamic, union);
	SetPointerCall(defun, var2dynamic, nunion);
}

_g void build_common_conses(void)
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

