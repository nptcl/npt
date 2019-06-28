/*
 *  ANSI COMMON LISP: 25. Environment
 */
#include "common_header.h"
#include "environment.h"
#include "type_parse.h"

/* (defun lisp-implementation-type () ...) -> (or string null)  */
static void function_lisp_implementation_type(Execute ptr)
{
	addr pos;
	implementation_type_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_lisp_implementation_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISP_IMPLEMENTATION_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_lisp_implementation_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lisp-implementation-version () ...) -> (or string null)  */
static void function_lisp_implementation_version(Execute ptr)
{
	addr pos;
	implementation_version_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_lisp_implementation_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISP_IMPLEMENTATION_VERSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_lisp_implementation_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun short-site-name () ...) -> (or string null) */
static void function_short_site_name(Execute ptr)
{
	addr pos;
	short_site_name_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_short_site_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHORT_SITE_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_short_site_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun long-site-name () ...) -> (or string null) */
static void function_long_site_name(Execute ptr)
{
	addr pos;
	long_site_name_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_long_site_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LONG_SITE_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_long_site_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-instance() ...) -> (or string null)  */
static void function_machine_instance(Execute ptr)
{
	addr pos;
	machine_instance_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_machine_instance(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_INSTANCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_instance);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-type() ...) -> (or string null)  */
static void function_machine_type(Execute ptr)
{
	addr pos;
	machine_type_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_machine_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun machine-version() ...) -> (or string null)  */
static void function_machine_version(Execute ptr)
{
	addr pos;
	machine_version_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_machine_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MACHINE_VERSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_machine_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun software-type() ...) -> (or string null)  */
static void function_software_type(Execute ptr)
{
	addr pos;
	software_type_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_software_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOFTWARE_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_software_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun software-version() ...) -> (or string null)  */
static void function_software_version(Execute ptr)
{
	addr pos;
	software_version_common(&pos);
	setresult_control(ptr, pos);
}

static void defun_software_version(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SOFTWARE_VERSION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_software_version);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EnvInfo);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun user-homedir-pathname (&optinal host) ...) -> result
 *   host    (or string list (eql :unspecific)
 *   result  (or pathname null)
 */
static void function_user_homedir_pathname(Execute ptr, addr host)
{
	/* (declare (ignore host)) */
	user_homedir_pathname_common(ptr, &host);
	setresult_control(ptr, host);
}

static void type_user_homedir_pathname(addr *ret)
{
	addr arg, values, type1, type2, type3;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, List);
	GetConst(KEYWORD_UNSPECIFIC, &type3);
	type_eql_heap(type3, &type3);
	type3or_heap(type1, type2, type3, &arg);
	typeargs_opt1(&arg, arg);
	GetTypeValues(&values, PathnameNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_user_homedir_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USER_HOMEDIR_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_user_homedir_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_user_homedir_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_environment(void)
{
	SetPointerCall(defun, empty, lisp_implementation_type);
	SetPointerCall(defun, empty, lisp_implementation_version);
	SetPointerCall(defun, empty, short_site_name);
	SetPointerCall(defun, empty, long_site_name);
	SetPointerCall(defun, empty, machine_instance);
	SetPointerCall(defun, empty, machine_type);
	SetPointerCall(defun, empty, machine_version);
	SetPointerCall(defun, empty, software_type);
	SetPointerCall(defun, empty, software_version);
	SetPointerCall(defun, opt1, user_homedir_pathname);
}

void build_common_environment(void)
{
	defun_lisp_implementation_type();
	defun_lisp_implementation_version();
	defun_short_site_name();
	defun_long_site_name();
	defun_machine_instance();
	defun_machine_type();
	defun_machine_version();
	defun_software_type();
	defun_software_version();
	defun_user_homedir_pathname();
}

