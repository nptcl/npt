#ifndef __CALL_EVAL_HEADER__
#define __CALL_EVAL_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define lambda_common _n(lambda_common)
#define eval_common _n(eval_common)
#define compiler_macro_function_common _n(compiler_macro_function_common)
#define setf_compiler_macro_function_common _n(setf_compiler_macro_function_common)
#define define_compiler_macro_common _n(define_compiler_macro_common)
#define set_define_compiler_macro _n(set_define_compiler_macro)
#define compile_common _n(compile_common)
#define defmacro_common _n(defmacro_common)
#define macro_function_common_ _n(macro_function_common_)
#define macroexpand_common _n(macroexpand_common)
#define macroexpand_1_common _n(macroexpand_1_common)
#define define_symbol_macro_common _n(define_symbol_macro_common)
#define declaim_common _n(declaim_common)
#define constantp_common _n(constantp_common)

_g void lambda_common(addr form, addr *ret);
_g int eval_common(Execute ptr, addr var);
_g int compiler_macro_function_common(addr var, addr env, addr *ret);
_g int setf_compiler_macro_function_common(addr value, addr var, addr env);
_g int define_compiler_macro_common(Execute ptr, addr form, addr env, addr *ret);
_g int set_define_compiler_macro(addr callname, addr value);
_g int compile_common(Execute ptr, addr var, addr opt,
		addr *ret1, addr *ret2, addr *ret3);
_g int defmacro_common(Execute ptr, addr right, addr env, addr *ret);
_g int macro_function_common_(addr symbol, addr env, addr *ret);
_g int macroexpand_common(Execute ptr, addr form, addr env, addr *ret, addr *sec);
_g int macroexpand_1_common(Execute ptr, addr form, addr env, addr *ret, addr *sec);
_g int define_symbol_macro_common(addr form, addr env, addr *ret);
_g int declaim_common(Execute ptr, addr form, addr env, addr *ret);
_g int constantp_common(Execute ptr, addr var, addr opt, addr *ret);

#endif

