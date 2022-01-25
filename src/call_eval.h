#ifndef __CALL_EVAL_HEADER__
#define __CALL_EVAL_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define lambda_common _n(lambda_common)
#define compile_common_ _n(compile_common_)
#define eval_common_ _n(eval_common_)
#define compiler_macro_function_common_ _n(compiler_macro_function_common_)
#define setf_compiler_macro_function_common_ _n(setf_compiler_macro_function_common_)
#define define_compiler_macro_common_ _n(define_compiler_macro_common_)
#define set_define_compiler_macro_ _n(set_define_compiler_macro_)
#define defmacro_common_ _n(defmacro_common_)
#define macro_function_common_ _n(macro_function_common_)
#define macroexpand_common_ _n(macroexpand_common_)
#define macroexpand_1_common_ _n(macroexpand_1_common_)
#define define_symbol_macro_common_ _n(define_symbol_macro_common_)
#define declaim_common_ _n(declaim_common_)
#define constantp_common_ _n(constantp_common_)

void lambda_common(addr form, addr *ret);
int compile_common_(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3);
int eval_common_(Execute ptr, addr var);
int compiler_macro_function_common_(addr var, addr env, addr *ret);
int setf_compiler_macro_function_common_(addr value, addr var, addr env);
int define_compiler_macro_common_(Execute ptr, addr form, addr env, addr *ret);
int set_define_compiler_macro_(addr callname, addr value);
int defmacro_common_(Execute ptr, addr right, addr env, addr *ret);
int macro_function_common_(addr symbol, addr env, addr *ret);
int macroexpand_common_(Execute ptr, addr form, addr env, addr *ret, addr *sec);
int macroexpand_1_common_(Execute ptr, addr form, addr env, addr *ret, addr *sec);
int define_symbol_macro_common_(addr form, addr env, addr *ret);
int declaim_common_(Execute ptr, addr form, addr env, addr *ret);
int constantp_common_(Execute ptr, addr var, addr opt, addr *ret);

#endif

