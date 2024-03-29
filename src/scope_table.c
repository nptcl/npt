#include "compile_file.h"
#include "eval_object.h"
#include "eval_stack.h"
#include "load_instance.h"
#include "load_time_value.h"
#include "parse.h"
#include "parse_object.h"
#include "scope_call.h"
#include "scope_check.h"
#include "scope_defun.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_let.h"
#include "scope_object.h"
#include "scope_table.h"
#include "step.h"
#include "type_value.h"
#include "type_table.h"

/* nil */
static int scope_nil_(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_nil(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_NIL, eval, Nil);
}


/* t */
static int scope_t_(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_t(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_T, eval, T);
}


/* clos */
static int scope_clos_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_clos_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CLOS, type, eval);
}


/* integer */
static int scope_integer_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_integer(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_INTEGER, type, eval);
}


/* rational */
static int scope_rational_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_rational(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RATIONAL, type, eval);
}


/* complex */
static int scope_complex_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_complex_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_COMPLEX, type, eval);
}


/* character */
static int scope_character_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_character(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CHARACTER, type, eval);
}


/* array */
static int scope_array_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_array_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ARRAY, type, eval);
}


/* vector */
static int scope_vector_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_vector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_VECTOR, type, eval);
}


/* bitvector */
static int scope_bitvector_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_bitvector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_BITVECTOR, type, eval);
}


/* string */
static int scope_string_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_STRING, type, eval);
}


/* symbol */
static int scope_symbol_(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	return scope_symbol_call_(ptr, ret, eval);
}


/* float */
static int scope_float_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_float(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_FLOAT, type, eval);
}


/* declaim */
static int scope_declaim(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(apply_declaim_stack_(ptr, eval));
	type_value_nil(&type);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_DECLAIM, type, eval);
}


/* package */
static int scope_package_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_package(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PACKAGE, type, eval);
}


/* random-state */
static int scope_random_state_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_random_state(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RANDOM_STATE, type, eval);
}


/* pathname */
static int scope_pathname_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_pathname(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PATHNAME, type, eval);
}


/* environment */
static int scope_environment_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_environment(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ENVIRONMENT, type, eval);
}


/* paper */
static int scope_paper_(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_paper(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PAPER, type, eval);
}


/* progn */
int scope_progn_(Execute ptr, addr *ret, addr eval)
{
	addr form, list, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);

	Return(scope_allcons(ptr, &list, &type, list));
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_PROGN, type, form));
	SetEvalScopeIndex(eval, 0, list);
	return Result(ret, eval);
}


/* let */
static int scope_let_(Execute ptr, addr *ret, addr eval)
{
	addr form;
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.cons);

	Return(scope_let_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LET, str.the, form));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* let* */
static int scope_leta_(Execute ptr, addr *ret, addr eval)
{
	addr form;
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.cons);

	Return(scope_leta_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LETA, str.the, form));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* setq */
static int scope_setq_(Execute ptr, addr *ret, addr eval)
{
	addr form, list, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &list);

	Return(scope_setq_call(ptr, list, &list, &type));
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_SETQ, type, form));
	SetEvalScopeIndex(eval, 0, list);
	return Result(ret, eval);
}


/* function */
static int scope_function_(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_function_call_(ptr, ret, eval);
}


/* lambda */
static int scope_lambda_(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_lambda_call_(ptr, ret, eval);
}


/* defun */
static int scope_defun_(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFUN, 1);
	GetEvalParse(eval, 0, &str.defun);
	GetEvalParse(eval, 1, &str.call);
	GetEvalParse(eval, 2, &str.args);
	GetEvalParse(eval, 3, &str.decl);
	GetEvalParse(eval, 4, &str.doc);
	GetEvalParse(eval, 5, &str.cons);
	return scope_defun_call_(ptr, &str, ret);
}


/* macro-lambda */
static int scope_macro_lambda(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	GetEvalParse(eval, 4, &str.call);
	return scope_macro_lambda_call_(ptr, &str, ret);
}


/* defmacro */
static int scope_defmacro(Execute ptr, addr *ret, addr eval)
{
	addr symbol, lambda, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &lambda);

	Return(scope_eval(ptr, &lambda, lambda));
	GetTypeTable(&type, Symbol);
	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_DEFMACRO, type, Nil));
	SetEvalScopeIndex(eval, 0, symbol);
	SetEvalScopeIndex(eval, 1, lambda);
	return Result(ret, eval);
}


/* deftype */
static int scope_deftype(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFTYPE, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	return scope_deftype_call_(ptr, &str, ret);
}


/* define-compiler-macro */
static int scope_define_compiler_macro(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFINE_COMPILER_MACRO, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	return scope_define_compiler_macro_call_(ptr, &str, ret);
}


/* destructuring-bind */
static int scope_destructuring_bind_(Execute ptr, addr *ret, addr eval)
{
	addr form, expr, args;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &args);

	hold = LocalHold_array(ptr, 1);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_set(hold, 0, expr);
	Return(scope_bind_call_(ptr, &eval, form, expr, args));
	localhold_end(hold);

	return Result(ret, eval);
}


/* quote */
static int scope_quote_(Execute ptr, addr *ret, addr eval)
{
	addr form, pos, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &pos);

	Return(type_value_(&type, pos));
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_QUOTE, type, form));
	SetEvalScopeIndex(eval, 0, pos);
	return Result(ret, eval);
}


/* flet */
static int scope_flet_(Execute ptr, addr *ret, addr eval)
{
	addr form;
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.cons);

	Return(scope_flet_call_(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_FLET, str.the, form));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* labels */
static int scope_labels_(Execute ptr, addr *ret, addr eval)
{
	addr form;
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.cons);

	Return(scope_labels_call_(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_LABELS, str.the, form));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* call */
static int scope_call_(Execute ptr, addr *ret, addr eval)
{
	addr form, first, args;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &first);
	GetEvalParse(eval, 2, &args);
	return scope_call_call_(ptr, form, first, args, ret);
}


/* values */
static int scope_values_(Execute ptr, addr *ret, addr eval)
{
	addr form, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &cons);

	Return(scope_values_call_(ptr, cons, &cons, &type));
	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_VALUES, type, form));
	SetEvalScopeIndex(eval, 0, cons);
	return Result(ret, eval);
}


/* the */
static int scope_the_(Execute ptr, addr *ret, addr eval)
{
	addr form, type, expr;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &type);
	GetEvalParse(eval, 2, &expr);
	return scope_the_call_(ptr, form, type, expr, ret);
}


/* locally */
static int scope_locally_(Execute ptr, addr *ret, addr eval)
{
	addr form, decl, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);
	return scope_locally_call_(ptr, form, decl, cons, ret);
}


/* if */
static int scope_if_(Execute ptr, addr *ret, addr eval)
{
	addr form, expr, then, last, type1, type2, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &then);
	GetEvalParse(eval, 3, &last);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_eval(hold, ptr, &then, then));
	Return(localhold_scope_eval(hold, ptr, &last, last));
	localhold_end(hold);

	GetEvalScopeThe(then, &type1);
	GetEvalScopeThe(last, &type2);
	type2or_heap(type1, type2, &type);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_IF, type, form));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, then);
	SetEvalScopeIndex(eval, 2, last);
	return Result(ret, eval);
}


/* unwind-protect */
static int scope_unwind_protect_(Execute ptr, addr *ret, addr eval)
{
	addr form, protect, cleanup, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &protect);
	GetEvalParse(eval, 2, &cleanup);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &protect, protect));
	Return(localhold_scope_allcons(hold, ptr, &cleanup, NULL, cleanup));
	localhold_end(hold);
	GetEvalScopeThe(protect, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_UNWIND_PROTECT, type, form));
	SetEvalScopeIndex(eval, 0, protect);
	SetEvalScopeIndex(eval, 1, cleanup);
	return Result(ret, eval);
}


/* tagbody */
static int scope_tagbody_(Execute ptr, addr *ret, addr eval)
{
	addr form, tag, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &body);

	Return(scope_tagbody_call(ptr, tag, body, &tag, &body));
	GetTypeTable(&type, Null);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_TAGBODY, type, form));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, body);
	return Result(ret, eval);
}


/* go */
static int scope_go(Execute ptr, addr *ret, addr eval)
{
	addr tag, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	Return(scope_go_call_(ptr, &tag, tag));
	GetTypeTable(&type, Nil);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_GO, type, tag);
}


/* block */
static int scope_block_(Execute ptr, addr *ret, addr eval)
{
	addr form, name, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &cons);

	Return(scope_block_call(ptr, name, cons, &name, &cons, &type));
	/* type -> (or block return-from1 return-from2 ...) */
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_BLOCK, type, form));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* return-from */
static int scope_return_from_(Execute ptr, addr *ret, addr eval)
{
	addr form, name, expr, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &name);
	GetEvalParse(eval, 2, &expr);

	Return(scope_return_from_call(ptr, name, expr, &name, &expr));
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_RETURN_FROM, type, form));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, expr);
	return Result(ret, eval);
}


/* catch */
static int scope_catch_(Execute ptr, addr *ret, addr eval)
{
	addr form, tag, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_allcons(hold, ptr, &cons, &type, cons));
	localhold_end(hold);
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CATCH, type, form));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* throw */
static int scope_throw_(Execute ptr, addr *ret, addr eval)
{
	addr form, tag, expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &tag);
	GetEvalParse(eval, 2, &expr);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_THROW, type, form));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, expr);
	return Result(ret, eval);
}


/* eval-when */
int scope_eval_when(Execute ptr, addr *ret, addr eval)
{
	addr form, progn, type, compile, load, exec, toplevel, mode;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &progn);
	GetEvalParse(eval, 2, &compile);
	GetEvalParse(eval, 3, &load);
	GetEvalParse(eval, 4, &exec);
	GetEvalParse(eval, 5, &toplevel);
	GetEvalParse(eval, 6, &mode);

	Return(scope_allcons(ptr, &progn, &type, progn));

	Return(eval_scope_size_(ptr, &eval, 6, EVAL_PARSE_EVAL_WHEN, type, form));
	SetEvalScopeIndex(eval, 0, progn);
	SetEvalScopeIndex(eval, 1, compile);
	SetEvalScopeIndex(eval, 2, load);
	SetEvalScopeIndex(eval, 3, exec);
	SetEvalScopeIndex(eval, 4, toplevel);
	SetEvalScopeIndex(eval, 5, mode);
	return Result(ret, eval);
}

/* multiple-value-bind */
static int scope_multiple_value_bind_(Execute ptr, addr *ret, addr eval)
{
	addr form;
	struct mvbind_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_mvbind(&str);
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.expr);
	GetEvalParse(eval, 3, &str.decl);
	GetEvalParse(eval, 4, &str.doc);
	GetEvalParse(eval, 5, &str.cons);

	Return(scope_multiple_value_bind_call_(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 7,
				EVAL_PARSE_MULTIPLE_VALUE_BIND, str.the, form));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.expr);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.doc);
	SetEvalScopeIndex(eval, 4, str.cons);
	SetEvalScopeIndex(eval, 5, str.free);
	SetEvalScopeIndex(eval, 6, str.allocate);
	return Result(ret, eval);
}


/* multiple-value-call */
static int scope_multiple_value_call_(Execute ptr, addr *ret, addr eval)
{
	addr form, expr, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &cons);
	return scope_multiple_value_call_call_(ptr, form, expr, cons, ret);
}


/* multiple-value-prog1 */
static int scope_multiple_value_prog1_(Execute ptr, addr *ret, addr eval)
{
	addr form, expr, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2,
				EVAL_PARSE_MULTIPLE_VALUE_PROG1, type, form));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* nth-value */
static int scope_nth_value_(Execute ptr, addr *ret, addr eval)
{
	addr form, nth, expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &nth);
	GetEvalParse(eval, 2, &expr);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &nth, nth));
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_NTH_VALUE, type, form));
	SetEvalScopeIndex(eval, 0, nth);
	SetEvalScopeIndex(eval, 1, expr);
	return Result(ret, eval);
}


/* progv */
static int scope_progv_(Execute ptr, addr *ret, addr eval)
{
	addr form, symbols, values, body, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &form);
	GetEvalParse(eval, 1, &symbols);
	GetEvalParse(eval, 2, &values);
	GetEvalParse(eval, 3, &body);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &symbols, symbols));
	Return(localhold_scope_eval(hold, ptr, &values, values));
	Return(localhold_scope_allcons(hold, ptr, &body, &type, body));
	localhold_end(hold);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_PROGV, type, form));
	SetEvalScopeIndex(eval, 0, symbols);
	SetEvalScopeIndex(eval, 1, values);
	SetEvalScopeIndex(eval, 2, body);
	return Result(ret, eval);
}


/*
 *  initialize
 */
void init_scope_function(void)
{
	EvalScopeTable[EVAL_PARSE_NIL] = scope_nil_;
	EvalScopeTable[EVAL_PARSE_T] = scope_t_;
	EvalScopeTable[EVAL_PARSE_CLOS] = scope_clos_;
	EvalScopeTable[EVAL_PARSE_INTEGER] = scope_integer_;
	EvalScopeTable[EVAL_PARSE_RATIONAL] = scope_rational_;
	EvalScopeTable[EVAL_PARSE_COMPLEX] = scope_complex_;
	EvalScopeTable[EVAL_PARSE_CHARACTER] = scope_character_;
	EvalScopeTable[EVAL_PARSE_ARRAY] = scope_array_;
	EvalScopeTable[EVAL_PARSE_VECTOR] = scope_vector_;
	EvalScopeTable[EVAL_PARSE_BITVECTOR] = scope_bitvector_;
	EvalScopeTable[EVAL_PARSE_STRING] = scope_string_;
	EvalScopeTable[EVAL_PARSE_SYMBOL] = scope_symbol_;
	EvalScopeTable[EVAL_PARSE_FLOAT] = scope_float_;
	EvalScopeTable[EVAL_PARSE_DECLAIM] = scope_declaim;
	EvalScopeTable[EVAL_PARSE_PACKAGE] = scope_package_;
	EvalScopeTable[EVAL_PARSE_RANDOM_STATE] = scope_random_state_;
	EvalScopeTable[EVAL_PARSE_PATHNAME] = scope_pathname_;
	EvalScopeTable[EVAL_PARSE_ENVIRONMENT] = scope_environment_;
	EvalScopeTable[EVAL_PARSE_PAPER] = scope_paper_;
	EvalScopeTable[EVAL_PARSE_PROGN] = scope_progn_;
	EvalScopeTable[EVAL_PARSE_LET] = scope_let_;
	EvalScopeTable[EVAL_PARSE_LETA] = scope_leta_;
	EvalScopeTable[EVAL_PARSE_SETQ] = scope_setq_;
	EvalScopeTable[EVAL_PARSE_DEFUN] = scope_defun_;
	EvalScopeTable[EVAL_PARSE_DEFMACRO] = scope_defmacro;
	EvalScopeTable[EVAL_PARSE_MACRO_LAMBDA] = scope_macro_lambda;
	EvalScopeTable[EVAL_PARSE_DEFTYPE] = scope_deftype;
	EvalScopeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = scope_define_compiler_macro;
	EvalScopeTable[EVAL_PARSE_DESTRUCTURING_BIND] = scope_destructuring_bind_;
	EvalScopeTable[EVAL_PARSE_QUOTE] = scope_quote_;
	EvalScopeTable[EVAL_PARSE_FUNCTION] = scope_function_;
	EvalScopeTable[EVAL_PARSE_LAMBDA] = scope_lambda_;
	EvalScopeTable[EVAL_PARSE_IF] = scope_if_;
	EvalScopeTable[EVAL_PARSE_UNWIND_PROTECT] = scope_unwind_protect_;
	EvalScopeTable[EVAL_PARSE_TAGBODY] = scope_tagbody_;
	EvalScopeTable[EVAL_PARSE_GO] = scope_go;
	EvalScopeTable[EVAL_PARSE_BLOCK] = scope_block_;
	EvalScopeTable[EVAL_PARSE_RETURN_FROM] = scope_return_from_;
	EvalScopeTable[EVAL_PARSE_CATCH] = scope_catch_;
	EvalScopeTable[EVAL_PARSE_THROW] = scope_throw_;
	EvalScopeTable[EVAL_PARSE_FLET] = scope_flet_;
	EvalScopeTable[EVAL_PARSE_LABELS] = scope_labels_;
	EvalScopeTable[EVAL_PARSE_THE] = scope_the_;
	EvalScopeTable[EVAL_PARSE_EVAL_WHEN] = scope_eval_when;
	EvalScopeTable[EVAL_PARSE_VALUES] = scope_values_;
	EvalScopeTable[EVAL_PARSE_LOCALLY] = scope_locally_;
	EvalScopeTable[EVAL_PARSE_CALL] = scope_call_;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = scope_multiple_value_bind_;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = scope_multiple_value_call_;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = scope_multiple_value_prog1_;
	EvalScopeTable[EVAL_PARSE_NTH_VALUE] = scope_nth_value_;
	EvalScopeTable[EVAL_PARSE_PROGV] = scope_progv_;
	EvalScopeTable[EVAL_PARSE_LOAD_TIME_VALUE] = scope_load_time_value_;
	EvalScopeTable[EVAL_PARSE_STEP] = scope_step_;
}

