#ifndef __CALL_DATA_HEADER__
#define __CALL_DATA_HEADER__

#include "execute.h"
#include "typedef.h"

#define apply_common_ _n(apply_common_)
#define funcall_common_ _n(funcall_common_)
#define defun_common _n(defun_common)
#define fdefinition_common _n(fdefinition_common)
#define setf_fdefinition_common _n(setf_fdefinition_common)
#define fboundp_common_ _n(fboundp_common_)
#define fmakunbound_common _n(fmakunbound_common)
#define function_lambda_expression_common _n(function_lambda_expression_common)
#define lambda_list_keywords_common _n(lambda_list_keywords_common)
#define defconstant_common _n(defconstant_common)
#define defparameter_common _n(defparameter_common)
#define defvar_common _n(defvar_common)
#define destructuring_bind_common _n(destructuring_bind_common)
#define psetq_common _n(psetq_common)
#define psetf_common _n(psetf_common)
#define return_common _n(return_common)
#define complement_common _n(complement_common)
#define constantly_common _n(constantly_common)
#define every_common _n(every_common)
#define notevery_common _n(notevery_common)
#define some_common _n(some_common)
#define notany_common _n(notany_common)
#define and_common _n(and_common)
#define cond_common _n(cond_common)
#define or_common _n(or_common)
#define when_common _n(when_common)
#define unless_common _n(unless_common)
#define case_common _n(case_common)
#define ecase_common _n(ecase_common)
#define ccase_common _n(ccase_common)
#define typecase_common _n(typecase_common)
#define etypecase_common _n(etypecase_common)
#define ctypecase_common _n(ctypecase_common)
#define multiple_value_bind_common _n(multiple_value_bind_common)
#define multiple_value_list_common _n(multiple_value_list_common)
#define multiple_value_setq_common _n(multiple_value_setq_common)
#define nth_value_common _n(nth_value_common)
#define prog_common _n(prog_common)
#define proga_common _n(proga_common)
#define prog1_common _n(prog1_common)
#define prog2_common _n(prog2_common)
#define define_modify_macro_common _n(define_modify_macro_common)
#define defsetf_common _n(defsetf_common)
#define define_setf_expander_common _n(define_setf_expander_common)
#define setf_common _n(setf_common)
#define shiftf_common _n(shiftf_common)
#define rotatef_common _n(rotatef_common)

int apply_common_(Execute ptr, addr call, addr arg, addr args);
int funcall_common_(Execute ptr, addr call, addr args);
int defun_common(Execute ptr, addr right, addr env, addr *ret);
int fdefinition_common(Execute ptr, addr name, addr *ret);
int setf_fdefinition_common(addr value, addr name);
int fboundp_common_(addr name, int *ret);
int fmakunbound_common(addr name);
void function_lambda_expression_common(addr var, addr *ret1, addr *ret2, addr *ret3);
void lambda_list_keywords_common(addr *ret);
int defconstant_common(addr form, addr env, addr *ret);
int defparameter_common(addr form, addr env, addr *ret);
int defvar_common(addr form, addr env, addr *ret);
int destructuring_bind_common(Execute ptr, addr form, addr env, addr *ret);
int psetq_common(Execute ptr, addr form, addr env, addr *ret);
int psetf_common(Execute ptr, addr form, addr env, addr *ret);
int return_common(addr form, addr env, addr *ret);
void complement_common(addr var, addr *ret);
void constantly_common(addr var, addr *ret);
int every_common(Execute ptr, addr call, addr rest, addr *ret);
int notevery_common(Execute ptr, addr call, addr rest, addr *ret);
int some_common(Execute ptr, addr call, addr rest, addr *ret);
int notany_common(Execute ptr, addr call, addr rest, addr *ret);
int and_common(addr form, addr env, addr *ret);
int cond_common(addr form, addr env, addr *ret);
int or_common(Execute ptr, addr form, addr env, addr *ret);
int when_common(addr form, addr env, addr *ret);
int unless_common(addr form, addr env, addr *ret);
int case_common(Execute ptr, addr form, addr env, addr *ret);
int ecase_common(Execute ptr, addr form, addr env, addr *ret);
int ccase_common(Execute ptr, addr form, addr env, addr *ret);
int typecase_common(Execute ptr, addr form, addr env, addr *ret);
int etypecase_common(Execute ptr, addr form, addr env, addr *ret);
int ctypecase_common(Execute ptr, addr form, addr env, addr *ret);
int multiple_value_bind_common(Execute ptr, addr form, addr env, addr *ret);
int multiple_value_list_common(addr form, addr env, addr *ret);
int multiple_value_setq_common(addr form, addr env, addr *ret);
int nth_value_common(addr form, addr env, addr *ret);
int prog_common(addr form, addr env, addr *ret);
int proga_common(addr form, addr env, addr *ret);
int prog1_common(Execute ptr, addr form, addr env, addr *ret);
int prog2_common(addr form, addr env, addr *ret);
int define_modify_macro_common(LocalRoot local, addr form, addr env, addr *ret);
int defsetf_common(Execute ptr, addr form, addr env, addr *ret);
int define_setf_expander_common(addr form, addr env, addr *ret);
int setf_common(Execute ptr, addr form, addr env, addr *ret);
int shiftf_common(Execute ptr, addr form, addr env, addr *ret);
int rotatef_common(Execute ptr, addr form, addr env, addr *ret);

#endif

