#include "condition.h"
#include "local.h"
#include "make.h"
#include "make_call.h"
#include "make_function.h"
#include "make_queue.h"
#include "make_typedef.h"
#include "make_value.h"
#include "parse_object.h"
#include "scope_object.h"
#include "typedef.h"

/*
 *  code
 */
static code_make_calltype CodeMakeTable[EVAL_PARSE_SIZE];

void set_code_make_struct(struct code_make_struct *str, Execute ptr, addr code)
{
	str->escape = 0;
	str->ptr = ptr;
	str->local = ptr->local;
	str->code = code;
}

int code_make_execute_(CodeMake ptr, addr scope)
{
	EvalParse type;
	code_make_calltype call_;

	Check(! eval_scope_p(scope), "type error");
	GetEvalScopeType(scope, &type);
	call_ = CodeMakeTable[type];
	if (call_ == NULL)
		return fmte_("Invalid scope type.", NULL);

	return (*call_)(ptr, scope);
}

int code_make_(Execute ptr, addr *ret, addr scope)
{
	addr code;
	LocalRoot local;
	LocalStack stack;
	struct code_make_struct str;

	local = ptr->local;
	push_local(local, &stack);
	code_queue_local(local, &code);
	set_code_make_struct(&str, ptr, code);
	Return(code_make_execute_set_(&str, scope));
	code_queue_pop(&str, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  table
 */
void init_make(void)
{
	CodeMakeTable[EVAL_PARSE_NIL] = code_make_nil_;
	CodeMakeTable[EVAL_PARSE_T] = code_make_t_;
	CodeMakeTable[EVAL_PARSE_CLOS] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_INTEGER] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_RATIONAL] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_COMPLEX] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_CHARACTER] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_ARRAY] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_VECTOR] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_BITVECTOR] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_STRING] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_SYMBOL] = code_make_symbol_;
	CodeMakeTable[EVAL_PARSE_FLOAT] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_DECLAIM] = code_make_declaim_;
	CodeMakeTable[EVAL_PARSE_PACKAGE] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_RANDOM_STATE] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_PATHNAME] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_ENVIRONMENT] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_LEXICAL] = code_make_lexical_;
	CodeMakeTable[EVAL_PARSE_PROGN] = code_make_progn_;
	CodeMakeTable[EVAL_PARSE_LET] = code_make_let_;
	CodeMakeTable[EVAL_PARSE_LETA] = code_make_leta_;
	CodeMakeTable[EVAL_PARSE_SETQ] = code_make_setq_;
	CodeMakeTable[EVAL_PARSE_DEFUN] = code_make_defun_;
	CodeMakeTable[EVAL_PARSE_DEFMACRO] = code_make_defmacro_;
	CodeMakeTable[EVAL_PARSE_MACRO_LAMBDA] = code_make_macro_lambda_;
	CodeMakeTable[EVAL_PARSE_DEFTYPE] = code_make_deftype_;
	CodeMakeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = code_make_define_compiler_macro_;
	CodeMakeTable[EVAL_PARSE_DESTRUCTURING_BIND] = code_make_destructuring_bind_;
	CodeMakeTable[EVAL_PARSE_QUOTE] = code_make_value_;
	CodeMakeTable[EVAL_PARSE_FUNCTION] = code_make_function_;
	CodeMakeTable[EVAL_PARSE_LAMBDA] = code_make_lambda_;
	CodeMakeTable[EVAL_PARSE_IF] = code_make_if_;
	CodeMakeTable[EVAL_PARSE_UNWIND_PROTECT] = code_make_unwind_protect_;
	CodeMakeTable[EVAL_PARSE_TAGBODY] = code_make_tagbody_;
	CodeMakeTable[EVAL_PARSE_GO] = code_make_go_;
	CodeMakeTable[EVAL_PARSE_BLOCK] = code_make_block_;
	CodeMakeTable[EVAL_PARSE_RETURN_FROM] = code_make_return_from_;
	CodeMakeTable[EVAL_PARSE_CATCH] = code_make_catch_;
	CodeMakeTable[EVAL_PARSE_THROW] = code_make_throw_;
	CodeMakeTable[EVAL_PARSE_FLET] = code_make_flet_;
	CodeMakeTable[EVAL_PARSE_LABELS] = code_make_labels_;
	CodeMakeTable[EVAL_PARSE_THE] = code_make_the_;
	CodeMakeTable[EVAL_PARSE_EVAL_WHEN] = code_make_eval_when_;
	CodeMakeTable[EVAL_PARSE_VALUES] = code_make_values_;
	CodeMakeTable[EVAL_PARSE_LOCALLY] = code_make_locally_;
	CodeMakeTable[EVAL_PARSE_CALL] = code_make_call_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = code_make_multiple_value_bind_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = code_make_multiple_value_call_;
	CodeMakeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = code_make_multiple_value_prog1_;
	CodeMakeTable[EVAL_PARSE_NTH_VALUE] = code_make_nth_value_;
	CodeMakeTable[EVAL_PARSE_PROGV] = code_make_progv_;
	CodeMakeTable[EVAL_PARSE_LOAD_TIME_VALUE] = code_make_load_time_value_;
	CodeMakeTable[EVAL_PARSE_STEP] = code_make_step_;
}

