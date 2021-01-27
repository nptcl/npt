#include "eval_object.h"
#include "eval_stack.h"
#include "load_time_value.h"
#include "parse.h"
#include "parse_object.h"
#include "scope_call.h"
#include "scope_check.h"
#include "scope_function.h"
#include "scope_lambda.h"
#include "scope_object.h"
#include "step.h"
#include "type_value.h"
#include "type_table.h"

/* nil */
static int scope_nil(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_nil(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_NIL, eval, Nil);
}


/* t */
static int scope_t(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_t(&eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_T, eval, T);
}


/* type */
static int scope_type(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_type(&type);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_TYPE, type, eval);
}


/* clos */
static int scope_clos(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_clos_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CLOS, type, eval);
}


/* integer */
static int scope_integer(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_integer(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_INTEGER, type, eval);
}


/* rational */
static int scope_rational(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_rational(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RATIONAL, type, eval);
}


/* complex */
static int scope_complex(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_complex_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_COMPLEX, type, eval);
}


/* character */
static int scope_character(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_character(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_CHARACTER, type, eval);
}


/* array */
static int scope_array(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_array_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ARRAY, type, eval);
}


/* vector */
static int scope_vector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_vector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_VECTOR, type, eval);
}


/* bitvector */
static int scope_bitvector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_bitvector(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_BITVECTOR, type, eval);
}


/* string */
static int scope_string(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(type_value_(&type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_STRING, type, eval);
}


/* symbol */
static int scope_symbol(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	return scope_symbol_call(ptr, ret, eval);
}


/* float */
static int scope_float(Execute ptr, addr *ret, addr eval)
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


/* random-state */
static int scope_random_state(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_random_state(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_RANDOM_STATE, type, eval);
}


/* pathname */
static int scope_pathname(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_pathname(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PATHNAME, type, eval);
}


/* environment */
static int scope_environment(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_environment(&type, eval);
	return make_eval_scope_(ptr, ret, EVAL_PARSE_ENVIRONMENT, type, eval);
}


/* progn */
int scope_progn(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_allcons(ptr, &eval, &type, eval));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_PROGN, type, eval);
}


/* let */
static int scope_let(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_let_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LET, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* let* */
static int scope_leta(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_leta_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LETA, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	SetEvalScopeIndex(eval, 4, str.allocate);

	return Result(ret, eval);
}


/* setq */
static int scope_setq(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &cons);
	Return(scope_setq_call(ptr, cons, &cons, &type));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_SETQ, type, cons);
}


/* function */
static int scope_function(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_function_call(ptr, ret, eval);
}


/* lambda */
static int scope_lambda(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	return scope_lambda_call(ptr, ret, eval);
}


/* defun */
static int scope_defun(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_DEFUN, 1);
	GetEvalParse(eval, 0, &str.call);
	GetEvalParse(eval, 1, &str.args);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);
	GetEvalParse(eval, 5, &str.defun);
	return scope_defun_call(ptr, &str, ret);
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
	return scope_macro_lambda_call(ptr, &str, ret);
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
	return scope_deftype_call(ptr, &str, ret);
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
	return scope_define_compiler_macro_call(ptr, &str, ret);
}


/* destructuring-bind */
static int scope_destructuring_bind(Execute ptr, addr *ret, addr eval)
{
	addr args, expr;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &args);

	hold = LocalHold_array(ptr, 1);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_set(hold, 0, expr);
	Return(scope_bind_call(ptr, &eval, expr, args));
	localhold_end(hold);

	return Result(ret, eval);
}


/* define-symbol-macro */
static int scope_define_symbol_macro(Execute ptr, addr *ret, addr eval)
{
	addr symbol, form, body;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbol);
	GetEvalParse(eval, 1, &form);
	GetEvalParse(eval, 2, &body);
	return scope_define_symbol_macro_call_(ptr, symbol, form, body, ret);
}


/* symbol-macrolet */
static int scope_symbol_macrolet(Execute ptr, addr *ret, addr eval)
{
	addr args, decl, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &args);
	GetEvalParse(eval, 1, &decl);
	GetEvalParse(eval, 2, &cons);
	return scope_symbol_macrolet_call(ptr, args, decl, cons, ret);
}


/* quote */
static int scope_quote(Execute ptr, addr *ret, addr eval)
{
	addr value, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &value);
	Return(type_value_(&type, value));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_QUOTE, type, value);
}


/* flet */
static int scope_flet(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_flet_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_FLET, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* labels */
static int scope_labels(Execute ptr, addr *ret, addr eval)
{
	struct let_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_let(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.cons);

	Return(scope_labels_call(ptr, &str));
	Return(eval_scope_size_(ptr, &eval, 4, EVAL_PARSE_LABELS, str.the, eval));
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.decl);
	SetEvalScopeIndex(eval, 2, str.cons);
	SetEvalScopeIndex(eval, 3, str.free);
	return Result(ret, eval);
}


/* call */
static int scope_call(Execute ptr, addr *ret, addr eval)
{
	addr first, args;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &first);
	GetEvalParse(eval, 1, &args);
	return scope_call_call_(ptr, first, args, ret);
}


/* values */
static int scope_values(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_values_call(ptr, eval, &cons, &type));
	return make_eval_scope_(ptr, ret, EVAL_PARSE_VALUES, type, cons);
}


/* the */
static int scope_the(Execute ptr, addr *ret, addr eval)
{
	addr type, form;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &type);
	GetEvalParse(eval, 1, &form);
	return scope_the_call(ptr, type, form, ret);
}


/* locally */
int scope_locally(Execute ptr, addr *ret, addr eval)
{
	addr decl, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &decl);
	GetEvalParse(eval, 1, &cons);
	return scope_locally_call(ptr, decl, cons, ret);
}


/* if */
static int scope_if(Execute ptr, addr *ret, addr eval)
{
	addr expr, then, last, type1, type2, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &then);
	GetEvalParse(eval, 2, &last);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_eval(hold, ptr, &then, then));
	Return(localhold_scope_eval(hold, ptr, &last, last));
	localhold_end(hold);

	GetEvalScopeThe(then, &type1);
	GetEvalScopeThe(last, &type2);
	type2or_heap(type1, type2, &type);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_IF, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, then);
	SetEvalScopeIndex(eval, 2, last);
	return Result(ret, eval);
}


/* unwind-protect */
static int scope_unwind_protect(Execute ptr, addr *ret, addr eval)
{
	addr protect, cleanup, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &protect);
	GetEvalParse(eval, 1, &cleanup);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &protect, protect));
	Return(localhold_scope_allcons(hold, ptr, &cleanup, NULL, cleanup));
	localhold_end(hold);
	GetEvalScopeThe(protect, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_UNWIND_PROTECT, type, eval));
	SetEvalScopeIndex(eval, 0, protect);
	SetEvalScopeIndex(eval, 1, cleanup);
	return Result(ret, eval);
}


/* tagbody */
static int scope_tagbody(Execute ptr, addr *ret, addr eval)
{
	addr tag, body, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &body);

	Return(scope_tagbody_call(ptr, tag, body, &tag, &body));
	GetTypeTable(&type, Null);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_TAGBODY, type, Nil));
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
static int scope_block(Execute ptr, addr *ret, addr eval)
{
	addr name, cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &cons);

	Return(scope_block_call(ptr, name, cons, &name, &cons, &type));
	/* type -> (or block return-from1 return-from2 ...) */
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_BLOCK, type, Nil));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* return-from */
static int scope_return_from(Execute ptr, addr *ret, addr eval)
{
	addr name, form, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &name);
	GetEvalParse(eval, 1, &form);

	Return(scope_return_from_call(ptr, name, form, &name, &form));
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_RETURN_FROM, type, Nil));
	SetEvalScopeIndex(eval, 0, name);
	SetEvalScopeIndex(eval, 1, form);
	return Result(ret, eval);
}


/* catch */
static int scope_catch(Execute ptr, addr *ret, addr eval)
{
	addr tag, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_allcons(hold, ptr, &cons, &type, cons));
	localhold_end(hold);
	GetTypeTable(&type, Asterisk);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_CATCH, type, Nil));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* throw */
static int scope_throw(Execute ptr, addr *ret, addr eval)
{
	addr tag, form, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &tag);
	GetEvalParse(eval, 1, &form);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &tag, tag));
	Return(localhold_scope_eval(hold, ptr, &form, form));
	localhold_end(hold);
	GetTypeTable(&type, Nil);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_THROW, type, eval));
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, form);
	return Result(ret, eval);
}


/* eval-when */
int scope_eval_when(Execute ptr, addr *ret, addr eval)
{
	addr progn, type, compile, load, exec, toplevel, mode;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &progn);
	GetEvalParse(eval, 1, &compile);
	GetEvalParse(eval, 2, &load);
	GetEvalParse(eval, 3, &exec);
	GetEvalParse(eval, 4, &toplevel);
	GetEvalParse(eval, 5, &mode);

	Return(scope_allcons(ptr, &progn, &type, progn));

	Return(eval_scope_size_(ptr, &eval, 6, EVAL_PARSE_EVAL_WHEN, type, Nil));
	SetEvalScopeIndex(eval, 0, progn);
	SetEvalScopeIndex(eval, 1, compile);
	SetEvalScopeIndex(eval, 2, load);
	SetEvalScopeIndex(eval, 3, exec);
	SetEvalScopeIndex(eval, 4, toplevel);
	SetEvalScopeIndex(eval, 5, mode);
	return Result(ret, eval);
}


/* multiple-value-bind */
static int scope_multiple_value_bind(Execute ptr, addr *ret, addr eval)
{
	struct mvbind_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_mvbind(&str);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.expr);
	GetEvalParse(eval, 2, &str.decl);
	GetEvalParse(eval, 3, &str.doc);
	GetEvalParse(eval, 4, &str.cons);

	Return(scope_multiple_value_bind_call(ptr, &str));

	Return(eval_scope_size_(ptr, &eval, 7, EVAL_PARSE_MULTIPLE_VALUE_BIND, str.the, Nil));
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
static int scope_multiple_value_call(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);
	return scope_multiple_value_call_call(ptr, expr, cons, ret);
}


/* multiple-value-prog1 */
static int scope_multiple_value_prog1(Execute ptr, addr *ret, addr eval)
{
	addr expr, cons, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &cons);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(localhold_scope_allcons(hold, ptr, &cons, NULL, cons));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_PROG1, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, cons);
	return Result(ret, eval);
}


/* nth-value */
static int scope_nth_value(Execute ptr, addr *ret, addr eval)
{
	addr nth, expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &nth);
	GetEvalParse(eval, 1, &expr);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &nth, nth));
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 2, EVAL_PARSE_NTH_VALUE, type, eval));
	SetEvalScopeIndex(eval, 0, nth);
	SetEvalScopeIndex(eval, 1, expr);
	return Result(ret, eval);
}


/* progv */
static int scope_progv(Execute ptr, addr *ret, addr eval)
{
	addr symbols, values, body, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &symbols);
	GetEvalParse(eval, 1, &values);
	GetEvalParse(eval, 2, &body);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &symbols, symbols));
	Return(localhold_scope_eval(hold, ptr, &values, values));
	Return(localhold_scope_allcons(hold, ptr, &body, &type, body));
	localhold_end(hold);

	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_PROGV, type, eval));
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
	EvalScopeTable[EVAL_PARSE_NIL] = scope_nil;
	EvalScopeTable[EVAL_PARSE_T] = scope_t;
	EvalScopeTable[EVAL_PARSE_TYPE] = scope_type;
	EvalScopeTable[EVAL_PARSE_CLOS] = scope_clos;
	EvalScopeTable[EVAL_PARSE_INTEGER] = scope_integer;
	EvalScopeTable[EVAL_PARSE_RATIONAL] = scope_rational;
	EvalScopeTable[EVAL_PARSE_COMPLEX] = scope_complex;
	EvalScopeTable[EVAL_PARSE_CHARACTER] = scope_character;
	EvalScopeTable[EVAL_PARSE_ARRAY] = scope_array;
	EvalScopeTable[EVAL_PARSE_VECTOR] = scope_vector;
	EvalScopeTable[EVAL_PARSE_BITVECTOR] = scope_bitvector;
	EvalScopeTable[EVAL_PARSE_STRING] = scope_string;
	EvalScopeTable[EVAL_PARSE_SYMBOL] = scope_symbol;
	EvalScopeTable[EVAL_PARSE_FLOAT] = scope_float;
	EvalScopeTable[EVAL_PARSE_DECLAIM] = scope_declaim;
	EvalScopeTable[EVAL_PARSE_RANDOM_STATE] = scope_random_state;
	EvalScopeTable[EVAL_PARSE_PATHNAME] = scope_pathname;
	EvalScopeTable[EVAL_PARSE_ENVIRONMENT] = scope_environment;
	EvalScopeTable[EVAL_PARSE_PROGN] = scope_progn;
	EvalScopeTable[EVAL_PARSE_LET] = scope_let;
	EvalScopeTable[EVAL_PARSE_LETA] = scope_leta;
	EvalScopeTable[EVAL_PARSE_SETQ] = scope_setq;
	EvalScopeTable[EVAL_PARSE_DEFUN] = scope_defun;
	EvalScopeTable[EVAL_PARSE_DEFMACRO] = scope_defmacro;
	EvalScopeTable[EVAL_PARSE_MACRO_LAMBDA] = scope_macro_lambda;
	EvalScopeTable[EVAL_PARSE_DEFTYPE] = scope_deftype;
	EvalScopeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = scope_define_compiler_macro;
	EvalScopeTable[EVAL_PARSE_DESTRUCTURING_BIND] = scope_destructuring_bind;
	EvalScopeTable[EVAL_PARSE_DEFINE_SYMBOL_MACRO] = scope_define_symbol_macro;
	EvalScopeTable[EVAL_PARSE_SYMBOL_MACROLET] = scope_symbol_macrolet;
	EvalScopeTable[EVAL_PARSE_QUOTE] = scope_quote;
	EvalScopeTable[EVAL_PARSE_FUNCTION] = scope_function;
	EvalScopeTable[EVAL_PARSE_LAMBDA] = scope_lambda;
	EvalScopeTable[EVAL_PARSE_IF] = scope_if;
	EvalScopeTable[EVAL_PARSE_UNWIND_PROTECT] = scope_unwind_protect;
	EvalScopeTable[EVAL_PARSE_TAGBODY] = scope_tagbody;
	EvalScopeTable[EVAL_PARSE_GO] = scope_go;
	EvalScopeTable[EVAL_PARSE_BLOCK] = scope_block;
	EvalScopeTable[EVAL_PARSE_RETURN_FROM] = scope_return_from;
	EvalScopeTable[EVAL_PARSE_CATCH] = scope_catch;
	EvalScopeTable[EVAL_PARSE_THROW] = scope_throw;
	EvalScopeTable[EVAL_PARSE_FLET] = scope_flet;
	EvalScopeTable[EVAL_PARSE_LABELS] = scope_labels;
	EvalScopeTable[EVAL_PARSE_THE] = scope_the;
	EvalScopeTable[EVAL_PARSE_EVAL_WHEN] = scope_eval_when;
	EvalScopeTable[EVAL_PARSE_VALUES] = scope_values;
	EvalScopeTable[EVAL_PARSE_LOCALLY] = scope_locally;
	EvalScopeTable[EVAL_PARSE_CALL] = scope_call;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = scope_multiple_value_bind;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = scope_multiple_value_call;
	EvalScopeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = scope_multiple_value_prog1;
	EvalScopeTable[EVAL_PARSE_NTH_VALUE] = scope_nth_value;
	EvalScopeTable[EVAL_PARSE_PROGV] = scope_progv;
	EvalScopeTable[EVAL_PARSE_LOAD_TIME_VALUE] = scope_load_time_value;
	EvalScopeTable[EVAL_PARSE_STEP] = scope_step;
}

