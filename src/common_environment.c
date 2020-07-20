/*
 *  ANSI COMMON LISP: 25. Environment
 */
#include "call_environment.h"
#include "common_header.h"
#include "cons.h"
#include "env_code.h"
#include "env_describe.h"
#include "env_time.h"
#include "env_version.h"
#include "step.h"
#include "stream.h"
#include "type_parse.h"

/* (defun decode-universal-time (time &optional zone) ...)
 *     -> second, minute, hour, day, month, year, day, daylight-p, zone
 *   time        integer
 *   zone        (or null (rational -24 24))
 *   second      (integer 0 59)  ;; ignoring leap seconds
 *   minute      (integer 0 59)
 *   hour        (integer 0 23)
 *   day         (integer 1 31)
 *   month       (integer 1 12)
 *   year        integer
 *   week        (integer 0 6)  ;; 0 means Monday.
 *   daylight-p  boolean
 */
static int function_decode_universal_time(Execute ptr, addr pos, addr zone)
{
	struct universal_time_struct u;

	if (zone == Unbound)
		zone = Nil;
	decode_universal_time_common(ptr->local, &u, pos, zone);
	setvalues_control(ptr,
			u.second, u.minute, u.hour,
			u.date, u.month, u.year,
			u.week, u.daylight_p, u.zone, NULL);

	return 0;
}

static void type_decode_universal_time(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Intplus);
	GetTypeTable(&values, TimeZone);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, DecodeUniversalTime);
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


/* (defun encode-universal-time
 *     (second minute hour day month year &optional zone) ...)
 *     -> universal-time
 *   second          (integer 0 59)
 *   minute          (integer 0 59)
 *   hour            (integer 0 23)
 *   day             (integer 1 31)
 *   month           (integer 1 12)
 *   year            (integer 0 *)
 *   zone            (rational -24 24)
 *   universal-time  (integer 0 *)
 */
static int function_encode_universal_time(Execute ptr, addr rest)
{
	addr s, mi, h, d, m, y, z;

	if (! consp_getcons(rest, &s, &rest))
		goto error;
	if (! consp_getcons(rest, &mi, &rest))
		goto error;
	if (! consp_getcons(rest, &h, &rest))
		goto error;
	if (! consp_getcons(rest, &d, &rest))
		goto error;
	if (! consp_getcons(rest, &m, &rest))
		goto error;
	if (! consp_getcons(rest, &y, &rest))
		goto error;
	if (! consp_getcons(rest, &z, &rest))
		z = Unbound;
	if (consp(rest))
		goto error;
	encode_universal_time_common(ptr->local, &s, s, mi, h, d, m, y, z);
	setresult_control(ptr, s);
	return 0;

error:
	return fmte_("Invalid argument ENCODE-UNIVERSAL-TIME.", NULL);
}

static void type_encode_universal_time(addr *ret)
{
	addr args, values, plus, sec, hour, day, month, zone;

	GetTypeTable(&sec, TimeSecond);
	GetTypeTable(&hour, TimeHour);
	GetTypeTable(&day, TimeDay);
	GetTypeTable(&month, TimeMonth);
	GetTypeTable(&plus, Intplus);
	GetTypeTable(&zone, TimeZone);
	list_heap(&args, sec, sec, hour, day, month, plus, NULL);
	list_heap(&values, zone, NULL);
	typeargs_full(&args, args, values, Nil, Nil);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_encode_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ENCODE_UNIVERSAL_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_encode_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_encode_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-universal-time () ...) -> universal-time */
static int function_get_universal_time(Execute ptr)
{
	addr pos;

	get_universal_time_common(ptr->local, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static void type_get_universal_time(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_get_universal_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_UNIVERSAL_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_universal_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_universal_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-decoded-time () ...)
 *   -> second, minute, hour, date, month, year, week, daylight-p, zone
 * (get-decoded-time) == (decode-universal-time (get-universal-time))
 */
static int function_get_decoded_time(Execute ptr)
{
	struct universal_time_struct u;

	get_decoded_time_common(ptr->local, &u);
	setvalues_control(ptr,
			u.second, u.minute, u.hour,
			u.date, u.month, u.year,
			u.week, u.daylight_p, u.zone, NULL);

	return 0;
}

static void type_get_decoded_time(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, DecodeUniversalTime);
	type_compiled_heap(args, values, ret);
}

static void defun_get_decoded_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_DECODED_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_decoded_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_decoded_time(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sleep (seconds) ...) -> null
 *   seconds  (real 0 *)
 */
static int function_sleep(Execute ptr, addr var)
{
	Return(sleep_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_sleep(addr *ret)
{
	addr args, values;

	type2realf_ab_heap(Nil, 0, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_sleep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLEEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_sleep);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_sleep(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun apropos (string-designer) ...) -> (values) */
static int function_apropos(Execute ptr, addr var, addr opt)
{
	Return(apropos_common(ptr, var, (opt == Unbound)? Nil: opt));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_apropos(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, PackageDesignerNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Empty);
	type_compiled_heap(args, values, ret);
}

static void defun_apropos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APROPOS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_apropos);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apropos(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun apropos-list (string-designer) ...) -> (values) */
static int function_apropos_list(Execute ptr, addr var, addr opt)
{
	Return(apropos_list_common(ptr, var, (opt == Unbound)? Nil: opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_apropos_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, PackageDesignerNull);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_apropos_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_APROPOS_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_apropos_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_apropos_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun describe (object &optional stream) ...) -> (values) */
static int function_describe(Execute ptr, addr var, addr opt)
{
	Return(describe_common(ptr, var, opt));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_describe(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Empty);
	type_compiled_heap(args, values, ret);
}

static void defun_describe(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DESCRIBE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_describe);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_describe(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro trace (&rest args) ...) -> list */
static int function_trace(Execute ptr, addr form, addr env)
{
	trace_common(form, env, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defun_trace(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TRACE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_trace);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro untrace (&rest args) ...) -> list */
static int function_untrace(Execute ptr, addr form, addr env)
{
	untrace_common(form, env, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defun_untrace(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_UNTRACE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_untrace);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro step (form) ...) -> result */
static int function_step(Execute ptr, addr form, addr env)
{
	Return(step_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_step(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_STEP, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_step);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro time (form) ...) -> result) */
static int function_time(Execute ptr, addr form, addr env)
{
	Return(time_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_time(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_TIME, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_time);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defconstant internal-time-units-per-second [imlementation-dependency]) */
static void defconstant_internal_time_units_per_second(void)
{
	addr symbol, value;
	fixnum units;

	GetConst(COMMON_INTERNAL_TIME_UNITS_PER_SECOND, &symbol);
	get_internal_time_units_per_second(&units);
	fixnum_heap(&value, units);
	SetValueSymbol(symbol, value);
}


/* (defun get-internal-real-time () ...) -> (integer 0 *) */
static int function_get_internal_real_time(Execute ptr)
{
	addr pos;

	get_internal_real_time_common(ptr->local, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static void defun_get_internal_real_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_INTERNAL_REAL_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_internal_real_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, GetInternalRealTime);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-internal-run-time () ...) -> (integer 0 *) */
static int function_get_internal_run_time(Execute ptr)
{
	addr pos;

	get_internal_run_time_common(&pos);
	setresult_control(ptr, pos);

	return 0;
}

static void defun_get_internal_run_time(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_INTERNAL_RUN_TIME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_get_internal_run_time);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, GetInternalRealTime);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun disassemble (extended-function-designer) ...) -> nil */
static int function_disassemble(Execute ptr, addr var)
{
	Return(disassemble_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
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


/* (defun room (&optional x) ...) -> null
 *   x  (member t nil :default)
 */
static int function_room(Execute ptr, addr var)
{
	Return(room_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_room(addr *ret)
{
	addr args, values;

	GetConst(KEYWORD_DEFAULT, &args);
	type_member_heap(&args, Nil, T, args, NULL);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_room(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROOM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_room);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_room(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ed (&optional x) ...) -> null */
static int function_ed(Execute ptr, addr var)
{
	Return(ed_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_ed(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, Null);
	GetTypeTable(&type2, PathnameDesigner);
	GetTypeTable(&type3, FunctionName);
	type3or_heap(type1, type2, type3, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_ed(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ED, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_ed);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ed(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun inspect (object) ...) -> null */
static int function_inspect(Execute ptr, addr var)
{
	Return(inspect_common(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_inspect(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_inspect(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INSPECT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_inspect);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_inspect(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dribble (&optional pathname) ...) -> null */
static int function_dribble(Execute ptr, addr var)
{
	Return(dribble_common(ptr, (var == Unbound)? Nil: var));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_dribble(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_dribble(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DRIBBLE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_dribble);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_dribble(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lisp-implementation-type () ...) -> (or string null)  */
static int function_lisp_implementation_type(Execute ptr)
{
	addr pos;

	implementation_type_common(&pos);
	setresult_control(ptr, pos);

	return 0;
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
static int function_lisp_implementation_version(Execute ptr)
{
	addr pos;

	implementation_version_common(&pos);
	setresult_control(ptr, pos);

	return 0;
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
static int function_short_site_name(Execute ptr)
{
	addr pos;

	Return(short_site_name_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_long_site_name(Execute ptr)
{
	addr pos;

	Return(long_site_name_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_machine_instance(Execute ptr)
{
	addr pos;

	Return(machine_instance_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_machine_type(Execute ptr)
{
	addr pos;

	Return(machine_type_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_machine_version(Execute ptr)
{
	addr pos;

	Return(machine_version_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_software_type(Execute ptr)
{
	addr pos;

	Return(software_type_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_software_version(Execute ptr)
{
	addr pos;

	Return(software_version_common(&pos));
	setresult_control(ptr, pos);

	return 0;
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
static int function_user_homedir_pathname(Execute ptr, addr host)
{
	/* (declare (ignore host)) */
	Return(user_homedir_pathname_common(ptr, &host));
	setresult_control(ptr, host);
	return 0;
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
	SetPointerCall(defun, dynamic, encode_universal_time);
	SetPointerCall(defun, empty, get_universal_time);
	SetPointerCall(defun, empty, get_decoded_time);
	SetPointerCall(defun, var1, sleep);
	SetPointerCall(defun, var1opt1, apropos);
	SetPointerCall(defun, var1opt1, apropos_list);
	SetPointerCall(defun, var1opt1, describe);
	SetPointerCall(defmacro, macro, trace);
	SetPointerCall(defmacro, macro, untrace);
	SetPointerCall(defmacro, macro, step);
	SetPointerCall(defmacro, macro, time);
	SetPointerCall(defun, empty, get_internal_real_time);
	SetPointerCall(defun, empty, get_internal_run_time);
	SetPointerCall(defun, var1, disassemble);
	SetPointerCall(defun, opt1, room);
	SetPointerCall(defun, opt1, ed);
	SetPointerCall(defun, var1, inspect);
	SetPointerCall(defun, opt1, dribble);
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
	defun_encode_universal_time();
	defun_get_universal_time();
	defun_get_decoded_time();
	defun_sleep();
	defun_apropos();
	defun_apropos_list();
	defun_describe();
	defun_trace();
	defun_untrace();
	defmacro_step();
	defmacro_time();
	defconstant_internal_time_units_per_second();
	defun_get_internal_real_time();
	defun_get_internal_run_time();
	defun_disassemble();
	defun_room();
	defun_ed();
	defun_inspect();
	defun_dribble();
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

