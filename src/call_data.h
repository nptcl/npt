#ifndef __CALL_DATA_HEADER__
#define __CALL_DATA_HEADER__

#include "execute.h"
#include "typedef.h"

#define apply_common_ _n(apply_common_)
#define defun_common_ _n(defun_common_)
#define fdefinition_common_ _n(fdefinition_common_)
#define setf_fdefinition_common_ _n(setf_fdefinition_common_)
#define fboundp_common_ _n(fboundp_common_)
#define fmakunbound_common_ _n(fmakunbound_common_)
#define funcall_common_ _n(funcall_common_)
#define function_lambda_expression_common _n(function_lambda_expression_common)
#define lambda_list_keywords_common _n(lambda_list_keywords_common)
#define defconstant_common_ _n(defconstant_common_)
#define defparameter_common_ _n(defparameter_common_)
#define defvar_common_ _n(defvar_common_)
#define destructuring_bind_common_ _n(destructuring_bind_common_)
#define psetq_common_ _n(psetq_common_)
#define psetf_common_ _n(psetf_common_)
#define return_common_ _n(return_common_)
#define complement_common _n(complement_common)
#define constantly_common _n(constantly_common)
#define every_common_ _n(every_common_)
#define some_common_ _n(some_common_)
#define notevery_common_ _n(notevery_common_)
#define notany_common_ _n(notany_common_)
#define and_common_ _n(and_common_)
#define cond_common_ _n(cond_common_)
#define or_common_ _n(or_common_)
#define when_common_ _n(when_common_)
#define unless_common_ _n(unless_common_)
#define case_common_ _n(case_common_)
#define ecase_common_ _n(ecase_common_)
#define ccase_common_ _n(ccase_common_)
#define typecase_common_ _n(typecase_common_)
#define etypecase_common_ _n(etypecase_common_)
#define ctypecase_common_ _n(ctypecase_common_)
#define multiple_value_bind_common_ _n(multiple_value_bind_common_)
#define multiple_value_list_common_ _n(multiple_value_list_common_)
#define multiple_value_setq_common_ _n(multiple_value_setq_common_)
#define nth_value_common_ _n(nth_value_common_)
#define prog_common_ _n(prog_common_)
#define proga_common_ _n(proga_common_)
#define prog1_common_ _n(prog1_common_)
#define prog2_common_ _n(prog2_common_)
#define define_modify_macro_common_ _n(define_modify_macro_common_)
#define defsetf_common_ _n(defsetf_common_)
#define define_setf_expander_common_ _n(define_setf_expander_common_)
#define setf_common_ _n(setf_common_)
#define shiftf_common_ _n(shiftf_common_)
#define rotatef_common_ _n(rotatef_common_)

int apply_common_(Execute ptr, addr call, addr arg, addr args);
int defun_common_(Execute ptr, addr right, addr env, addr *ret);
int fdefinition_common_(Execute ptr, addr name, addr *ret);
int setf_fdefinition_common_(addr value, addr name);
int fboundp_common_(addr name, int *ret);
int fmakunbound_common_(addr name);
int funcall_common_(Execute ptr, addr call, addr args);
void function_lambda_expression_common(addr var, addr *ret1, addr *ret2, addr *ret3);
void lambda_list_keywords_common(addr *ret);
int defconstant_common_(addr form, addr env, addr *ret);
int defparameter_common_(addr form, addr env, addr *ret);
int defvar_common_(addr form, addr env, addr *ret);
int destructuring_bind_common_(Execute ptr, addr form, addr env, addr *ret);
int psetq_common_(Execute ptr, addr form, addr env, addr *ret);
int psetf_common_(Execute ptr, addr form, addr env, addr *ret);
int return_common_(addr form, addr env, addr *ret);
void complement_common(addr var, addr *ret);
void constantly_common(addr var, addr *ret);
int every_common_(Execute ptr, addr call, addr rest, addr *ret);
int some_common_(Execute ptr, addr call, addr rest, addr *ret);
int notevery_common_(Execute ptr, addr call, addr rest, addr *ret);
int notany_common_(Execute ptr, addr call, addr rest, addr *ret);
int and_common_(addr form, addr env, addr *ret);
int cond_common_(addr form, addr env, addr *ret);
int or_common_(Execute ptr, addr form, addr env, addr *ret);
int when_common_(addr form, addr env, addr *ret);
int unless_common_(addr form, addr env, addr *ret);
int case_common_(Execute ptr, addr form, addr env, addr *ret);
int ecase_common_(Execute ptr, addr form, addr env, addr *ret);
int ccase_common_(Execute ptr, addr form, addr env, addr *ret);
int typecase_common_(Execute ptr, addr form, addr env, addr *ret);
int etypecase_common_(Execute ptr, addr form, addr env, addr *ret);
int ctypecase_common_(Execute ptr, addr form, addr env, addr *ret);
int multiple_value_bind_common_(Execute ptr, addr form, addr env, addr *ret);
int multiple_value_list_common_(addr form, addr env, addr *ret);
int multiple_value_setq_common_(addr form, addr env, addr *ret);
int nth_value_common_(addr form, addr env, addr *ret);
int prog_common_(addr form, addr env, addr *ret);
int proga_common_(addr form, addr env, addr *ret);
int prog1_common_(Execute ptr, addr form, addr env, addr *ret);
int prog2_common_(addr form, addr env, addr *ret);
int define_modify_macro_common_(LocalRoot local, addr form, addr env, addr *ret);
int defsetf_common_(Execute ptr, addr form, addr env, addr *ret);
int define_setf_expander_common_(addr form, addr env, addr *ret);
int setf_common_(Execute ptr, addr form, addr env, addr *ret);
int shiftf_common_(Execute ptr, addr form, addr env, addr *ret);
int rotatef_common_(Execute ptr, addr form, addr env, addr *ret);

#endif

