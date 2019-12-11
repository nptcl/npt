/*
 *  ANSI COMMON LISP: 25. Environment
 */
#include "common_header.h"
#include "environment.h"
#include "stream.h"
#include "type_parse.h"

/* (defun decode-universal-time (time &optional zone) ...)
 *     -> second, minute, hour, date, month, year, day, daylight-p, zone
 *   time        integer
 *   zone        (or null (rational -24 24))
 *   second      (integer 0 59)  ;; ignoring leap seconds
 *   minute      (integer 0 59)
 *   hour        (integer 0 23)
 *   date        (integer 1 31)
 *   month       (integer 1 12)
 *   year        integer
 *   day         (integer 0 6)  ;; 0 means Monday.
 *   daylight-p  boolean
 */
static void function_decode_universal_time(Execute ptr, addr pos, addr zone)
{
	struct universal_time_struct u;

	if (zone == Unbound) zone = Nil;
	decode_universal_time(ptr->local, &u, pos, zone);
	setvalues_control(ptr,
			u.second, u.minute, u.hour,
			u.date, u.month, u.year,
			u.day, u.daylight_p, u.zone, NULL);
}

static void type_decode_universal_time(addr *ret)
{
	addr integer, sec, hour, date, mon, day, daylight, zone;
	addr args, values;

	/* zone */
	fixnum_heap(&args, -24);
	fixnum_heap(&values, 24);
	type4_heap(LISPDECL_RATIONAL, Nil, args, Nil, values, &zone);
	GetTypeTable(&args, Null);
	type2or_heap(args, zone, &zone);
	/* others */
	GetTypeTable(&integer, Integer);
	type4integer_heap(Nil, 0, Nil, 59, &sec);
	type4integer_heap(Nil, 0, Nil, 23, &hour);
	type4integer_heap(Nil, 1, Nil, 31, &date);
	type4integer_heap(Nil, 1, Nil, 12, &mon);
	type4integer_heap(Nil, 0, Nil, 6, &day);
	GetTypeTable(&daylight, Boolean);
	/* type */
	typeargs_var1opt1(&args, integer, zone);
	typevalues_values_va(&values,
			sec, sec, hour, date, mon, integer, day, daylight, zone, NULL);
	type_compiled_heap(args, values, ret);
}

static void defun_decode_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DECODE_UNIVERSAL_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_decode_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_decode_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun disassemble (extended-function-designer) ...) -> nil */
static void function_disassemble(Execute ptr, addr var)
{
	addr stream, check;

	standard_output_stream(ptr, &stream);
	if (symbolp(var)) {
		getfunction_local(ptr, var, &check);
		if (check == Unbound) {
			getmacro_symbol(var, &check);
			if (check == Unbound)
				fmte("Invalid argument ~S.", var);
		}
		var = check;
	}
	Return0(disassemble_common(ptr, stream, var));
	setresult_control(ptr, Nil);
}

static void type_disassemble(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_disassemble(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DISASSEMBLE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_disassemble);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_disassemble(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


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
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, String);
	GetTypeTable(&type2, List);
	GetConst(KEYWORD_UNSPECIFIC, &type3);
	type_eql_heap(type3, &type3);
	type3or_heap(type1, type2, type3, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, PathnameNull);
	type_compiled_heap(args, values, ret);
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
_g void init_common_environment(void)
{
	SetPointerCall(defun, var1opt1, decode_universal_time);
	SetPointerCall(defun, var1, disassemble);
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

_g void build_common_environment(void)
{
	defun_decode_universal_time();
	/*defun_encode_universal_time();*/
	/*get_universal_time*/
	/*get_decoded_time*/
	/*sleep*/
	/*apropos*/
	/*apropos_list*/
	/*describe*/
	/*describe_object*/
	/*trace*/
	/*untrace*/
	/*step*/
	/*time*/
	/*internal_time_units_per_second*/
	/*get_internal_real_time*/
	/*get_internal_run_time*/
	defun_disassemble();
	/*documentation*/
	/*(setf documentation)*/
	/*room*/
	/*ed*/
	/*inspect*/
	/*dribble*/
	/*minus1*/
	/*plus1*/
	/*plus2*/
	/*plus3*/
	/*asterisk1*/
	/*asterisk2*/
	/*asterisk3*/
	/*slash1*/
	/*slash2*/
	/*slash3*/
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

