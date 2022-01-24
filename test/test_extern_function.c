#include "extern_function.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "control_execute.h"
#include "control_operator.h"
#include "degrade.h"
#include "declare.h"
#include "extern_control.h"
#include "extern_execute.h"
#include "extern_sequence.h"
#include "extern_object.h"
#include "extern_type.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  function
 */
static int test_lisp_get_function(void)
{
	enum PACKAGE_TYPE type;
	Execute ptr;
	addr control, x, pos;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	x = Lisp_hold();

	/* normal function */
	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_function(&pos, pos);
	test(pos == NULL, "lisp_get_function.1");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_function(&pos, pos);
	test(functionp(pos), "lisp_get_function.2");

	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp_get_function(x, pos);
	test(lisp_null_p(x), "lisp_get_function.3");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_function(x, pos);
	test(lisp_function_p(x), "lisp_get_function.4");

	/* escape function */
	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_function(&pos, pos);
	test(functionp(pos), "lisp_get_function.5");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_function(x, pos);
	test(lisp_function_p(x), "lisp_get_function.6");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_function8(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_function8_(&pos, "CDR");
	test(lisp_function_p(pos), "ilsp_get_function8.1");

	lisp_get_function8_(x, "CDR");
	test(lisp_function_p(x), "ilsp_get_function8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_function16(void)
{
	addr control, x, pos;
	const byte16 str[] = { 'C','O','N','S',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_function16_(&pos, (const void *)str);
	test(lisp_function_p(pos), "ilsp_get_function16.1");

	lisp_get_function16_(x, (const void *)str);
	test(lisp_function_p(x), "ilsp_get_function16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_function32(void)
{
	addr control, x, pos;
	const unicode str[] = { 'C','O','N','S',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_function32_(&pos, (const void *)str);
	test(lisp_function_p(pos), "ilsp_get_function32.1");

	lisp_get_function32_(x, (const void *)str);
	test(lisp_function_p(x), "ilsp_get_function32.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_setf(void)
{
	enum PACKAGE_TYPE type;
	Execute ptr;
	addr control, x, pos;

	ptr = Execute_Thread;
	lisp_push_control(&control);
	x = Lisp_hold();

	/* normal function */
	internchar_default_(ptr, "HELLO", &pos, &type);
	setsetf_symbol(pos, Unbound);
	lisp0_get_setf(&pos, pos);
	test(pos == NULL, "lisp_get_setf.1");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_setf(&pos, pos);
	test(functionp(pos), "lisp_get_setf.2");

	internchar_default_(ptr, "HELLO", &pos, &type);
	setspecial_local(ptr, pos, Unbound);
	lisp_get_setf(x, pos);
	test(lisp_null_p(x), "lisp_get_setf.3");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_setf(x, pos);
	test(lisp_function_p(x), "lisp_get_setf.4");

	/* escape function */
	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp0_get_setf_(&pos, pos);
	test(functionp(pos), "lisp_get_setf.5");

	lisp0_intern8_(&pos, NULL, "CAR");
	setspecial_local(ptr, pos, Unbound);
	lisp_get_setf_(x, pos);
	test(lisp_function_p(x), "lisp_get_setf.6");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_setf8(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_setf8_(&pos, "CDR");
	test(lisp_function_p(pos), "ilsp_get_setf8.1");

	lisp_get_setf8_(x, "CDR");
	test(lisp_function_p(x), "ilsp_get_setf8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_setf16(void)
{
	addr control, x, pos;
	const byte16 str[] = { 'C','D','R',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_setf16_(&pos, (const void *)str);
	test(lisp_function_p(pos), "ilsp_get_setf16.1");

	lisp_get_setf16_(x, (const void *)str);
	test(lisp_function_p(x), "ilsp_get_setf16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_get_setf32(void)
{
	addr control, x, pos;
	const unicode str[] = { 'C','D','R',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_get_setf32_(&pos, (const void *)str);
	test(lisp_function_p(pos), "ilsp_get_setf32.1");

	lisp_get_setf32_(x, (const void *)str);
	test(lisp_function_p(x), "ilsp_get_setf32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  compiled
 */
static int test_lisp_compiled_dynamic_(addr list)
{
	Execute ptr;
	addr call;

	ptr = Execute_Thread;
	GetConst(COMMON_PLUS, &call);
	GetFunctionSymbol(call, &call);
	Return(apply1_control_(ptr, &call, call, list));
	setresult_control(ptr, call);

	return 0;
}

static int test_lisp_compiled_function(void)
{
	enum PACKAGE_TYPE type;
	addr control, pos, x, y, z;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_compiled_function_(&pos, 0, NULL);
	test(functionp(pos), "lisp_compiled_function.1");
	lisp_funcall_(x, pos, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_function.2");
	test(RefFixnum(y) == 30, "lisp_compiled_function.3");

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_function_(x, 0, z);
	lisp_hold_value(x, &y);
	GetNameFunction(y, &y);
	test(symbolp_callname(y), "lisp_compiled_function.4");
	GetCallName(y, &y);
	test(y == z, "lisp_compiled_function.5");

	lisp_intern8_(x, NULL, "HELLO");
	lisp_compiled_function_(x, 0, x);
	lisp_hold_value(x, &y);
	GetNameFunction(y, &y);
	test(symbolp_callname(y), "lisp_compiled_function.5");
	GetCallName(y, &y);
	test(y == z, "lisp_compiled_function.6");

	test(lisp_function_p(x), "lisp_compiled_function.7");
	lisp_funcall_(x, x, fixnumh(30), fixnumh(40), fixnumh(50), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_function.8");
	test(RefFixnum(y) == 120, "lisp_compiled_function.9");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_function8(void)
{
	enum PACKAGE_TYPE type;
	addr control, pos, x, y, z;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_compiled_function8_(&pos, 0, "HELLO");
	test(functionp(pos), "lisp_compiled_function8.1");
	lisp_funcall_(x, pos, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_function8.2");
	test(RefFixnum(y) == 30, "lisp_compiled_function8.3");

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_function8_(x, 0, "HELLO");
	lisp_hold_value(x, &y);
	GetNameFunction(y, &y);
	test(symbolp_callname(y), "lisp_compiled_function8.4");
	GetCallName(y, &y);
	test(y == z, "lisp_compiled_function8.5");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_function16(void)
{
	enum PACKAGE_TYPE type;
	addr control, pos, x, y, z;
	Execute ptr;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_compiled_function16_(&pos, 0, str);
	test(functionp(pos), "lisp_compiled_function16.1");
	lisp_funcall_(x, pos, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_function16.2");
	test(RefFixnum(y) == 30, "lisp_compiled_function16.3");

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_function16_(x, 0, str);
	lisp_hold_value(x, &y);
	GetNameFunction(y, &y);
	test(symbolp_callname(y), "lisp_compiled_function16.4");
	GetCallName(y, &y);
	test(y == z, "lisp_compiled_function16.5");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_function32(void)
{
	enum PACKAGE_TYPE type;
	addr control, pos, x, y, z;
	Execute ptr;
	const unicode str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_compiled_function32_(&pos, 0, str);
	test(functionp(pos), "lisp_compiled_function32.1");
	lisp_funcall_(x, pos, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_function32.2");
	test(RefFixnum(y) == 30, "lisp_compiled_function32.3");

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_function32_(x, 0, str);
	lisp_hold_value(x, &y);
	GetNameFunction(y, &y);
	test(symbolp_callname(y), "lisp_compiled_function32.4");
	GetCallName(y, &y);
	test(y == z, "lisp_compiled_function32.5");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun(void)
{
	enum PACKAGE_TYPE type;
	addr control, x, y, z;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_defun_(0, z);
	lisp_funcall8_(x, "HELLO", fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun.2");

	lisp_intern8_(x, NULL, "HELLO");
	lisp_compiled_defun_(0, x);
	lisp_funcall8_(x, "HELLO", fixnumh(20), fixnumh(30), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun.3");
	test(RefFixnum(y) == 50, "lisp_compiled_defun.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun8(void)
{
	addr control, x, y;

	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_compiled_defun8_(0, "HELLO");
	lisp_funcall8_(x, "HELLO", fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun8.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun16(void)
{
	addr control, x, y;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_compiled_defun16_(0, str);
	lisp_funcall8_(x, "HELLO", fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun16.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun32(void)
{
	addr control, x, y;
	const unicode str[] = { 'H','E','L','L','O',0 };

	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_compiled_defun32_(0, str);
	lisp_funcall8_(x, "HELLO", fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun32.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun32.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun_setf(void)
{
	enum PACKAGE_TYPE type;
	addr control, x, y, z;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_defun_setf_(0, z);
	getsetf_symbol(z, &y);
	lisp_funcall_(x, y, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun_setf.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun_setf.2");

	lisp_intern8_(x, NULL, "HELLO");
	lisp_compiled_defun_setf_(0, x);
	getsetf_symbol(z, &y);
	lisp_funcall_(x, y, fixnumh(20), fixnumh(30), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun_setf.3");
	test(RefFixnum(y) == 50, "lisp_compiled_defun_setf.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun_setf8(void)
{
	enum PACKAGE_TYPE type;
	addr control, x, y, z;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_defun_setf8_(0, "HELLO");
	getsetf_symbol(z, &y);
	lisp_funcall_(x, y, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun_setf8.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun_setf8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun_setf16(void)
{
	enum PACKAGE_TYPE type;
	addr control, x, y, z;
	Execute ptr;
	const byte16 str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_defun_setf16_(0, str);
	getsetf_symbol(z, &y);
	lisp_funcall_(x, y, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun_setf16.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun_setf16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_defun_setf32(void)
{
	enum PACKAGE_TYPE type;
	addr control, x, y, z;
	Execute ptr;
	const unicode str[] = { 'H','E','L','L','O',0 };

	ptr = Execute_Thread;
	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);

	lisp_push_control(&control);
	x = Lisp_hold();

	internchar_default_(ptr, "HELLO", &z, &type);
	lisp_compiled_defun_setf32_(0, str);
	getsetf_symbol(z, &y);
	lisp_funcall_(x, y, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &y);
	test(fixnump(y), "lisp_compiled_defun_setf32.1");
	test(RefFixnum(y) == 30, "lisp_compiled_defun_setf32.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_dynamic(void)
{
	addr control, x;

	lisp_compiled_dynamic(0, test_lisp_compiled_dynamic_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_funcall_(x, x, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &x);
	test(fixnump(x), "lisp_compiled_dynamic.1");
	test(RefFixnum(x) == 30, "lisp_compiled_dynamic.2");
	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_rest(void)
{
	addr control, x;

	lisp_compiled_rest(0, test_lisp_compiled_dynamic_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_funcall_(x, x, fixnumh(10), fixnumh(20), NULL);
	lisp_hold_value(x, &x);
	test(fixnump(x), "lisp_compiled_rest.1");
	test(RefFixnum(x) == 30, "lisp_compiled_rest.2");
	lisp_pop_control_(control);

	RETURN;
}

static int lisp_compiled_empty_value = 0;
static int test_lisp_compiled_empty_(void)
{
	lisp_compiled_empty_value = 1;
	return 0;
}
static int test_lisp_compiled_empty(void)
{
	addr control, x;

	lisp_compiled_empty(0, test_lisp_compiled_empty_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_empty_value = 0;
	lisp_funcall_(x, x, NULL);
	test(lisp_compiled_empty_value, "lisp_compiled_empty.1");
	lisp_pop_control_(control);

	RETURN;
}

static fixnum lisp_compiled_var_value = 0;
static int test_lisp_compiled_var1_(addr x)
{
	lisp_compiled_var_value = RefFixnum(x);
	return 0;
}
static int test_lisp_compiled_var1(void)
{
	addr control, x;

	lisp_compiled_var1(0, test_lisp_compiled_var1_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_var_value = 0;
	lisp_funcall_(x, x, fixnumh(10), NULL);
	test(lisp_compiled_var_value == 10, "lisp_compiled_var1.1");
	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_var2_(addr x, addr y)
{
	lisp_compiled_var_value = RefFixnum(x) + RefFixnum(y);
	return 0;
}
static int test_lisp_compiled_var2(void)
{
	addr control, x;

	lisp_compiled_var2(0, test_lisp_compiled_var2_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_var_value = 0;
	lisp_funcall_(x, x, fixnumh(10), fixnumh(20), NULL);
	test(lisp_compiled_var_value == 30, "lisp_compiled_var2.1");
	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_var3_(addr x, addr y, addr z)
{
	lisp_compiled_var_value = (RefFixnum(x) + RefFixnum(y)) * RefFixnum(z);
	return 0;
}
static int test_lisp_compiled_var3(void)
{
	addr control, x;

	lisp_compiled_var3(0, test_lisp_compiled_var3_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_var_value = 0;
	lisp_funcall_(x, x, fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	test(lisp_compiled_var_value == 900, "lisp_compiled_var3.1");
	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_compiled_value_(void)
{
	addr x;

	lisp_compiled_getvalue(&x);
	if (x == Nil)
		lisp_compiled_var_value = 999;
	else
		lisp_compiled_var_value = RefFixnum(x);

	return 0;
}
static int test_lisp_compiled_value(void)
{
	addr control, x;

	lisp_compiled_empty(0, test_lisp_compiled_value_);
	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_setvalue(x, fixnumh(10));
	lisp_compiled_var_value = 0;
	lisp_funcall_(x, x, NULL);
	test(lisp_compiled_var_value == 10, "lisp_compiled_value.1");

	lisp_compiled_function_(x, 0, NULL);
	lisp_compiled_setvalue(x, NULL);
	lisp_compiled_var_value = 0;
	lisp_funcall_(x, x, NULL);
	test(lisp_compiled_var_value == 999, "lisp_compiled_value.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_function(void)
{
	/* function */
	TestBreak(test_lisp_get_function);
	TestBreak(test_lisp_get_function8);
	TestBreak(test_lisp_get_function16);
	TestBreak(test_lisp_get_function32);
	TestBreak(test_lisp_get_setf);
	TestBreak(test_lisp_get_setf8);
	TestBreak(test_lisp_get_setf16);
	TestBreak(test_lisp_get_setf32);
	/* compiled */
	TestBreak(test_lisp_compiled_function);
	TestBreak(test_lisp_compiled_function8);
	TestBreak(test_lisp_compiled_function16);
	TestBreak(test_lisp_compiled_function32);
	TestBreak(test_lisp_compiled_defun);
	TestBreak(test_lisp_compiled_defun8);
	TestBreak(test_lisp_compiled_defun16);
	TestBreak(test_lisp_compiled_defun32);
	TestBreak(test_lisp_compiled_defun_setf);
	TestBreak(test_lisp_compiled_defun_setf8);
	TestBreak(test_lisp_compiled_defun_setf16);
	TestBreak(test_lisp_compiled_defun_setf32);
	TestBreak(test_lisp_compiled_dynamic);
	TestBreak(test_lisp_compiled_rest);
	TestBreak(test_lisp_compiled_empty);
	TestBreak(test_lisp_compiled_var1);
	TestBreak(test_lisp_compiled_var2);
	TestBreak(test_lisp_compiled_var3);
	TestBreak(test_lisp_compiled_value);

	return 0;
}

static void testinit_extern_function(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_common();
	build_reader();
	build_pathname();
	build_declare();
	build_code();
}

int test_extern_function(void)
{
	DegradeTitle;
	return DegradeCode(extern_function);
}

