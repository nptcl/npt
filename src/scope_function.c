#include "eval_stack.h"
#include "parse.h"
#include "parse_object.h"
#include "scope_call.h"
#include "scope_lambda.h"
#include "scope_object.h"
#include "type_value.h"
#include "type_table.h"

/* nil */
static int scope_nil(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_nil(&eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_NIL, eval, Nil);

	return 0;
}


/* t */
static int scope_t(Execute ptr, addr *ret, addr eval)
{
	Check(! eval_parse_p(eval), "type error");
	type_value_t(&eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_T, eval, T);

	return 0;
}


/* clos */
static int scope_clos(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_clos(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_CLOS, type, eval);

	return 0;
}


/* integer */
static int scope_integer(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_integer(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_INTEGER, type, eval);

	return 0;
}


/* rational */
static int scope_rational(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_rational(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_RATIONAL, type, eval);

	return 0;
}


/* complex */
static int scope_complex(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_complex(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_COMPLEX, type, eval);

	return 0;
}


/* character */
static int scope_character(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_character(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_CHARACTER, type, eval);

	return 0;
}


/* array */
static int scope_array(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_array(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_ARRAY, type, eval);

	return 0;
}


/* vector */
static int scope_vector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_vector(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_VECTOR, type, eval);

	return 0;
}


/* bitvector */
static int scope_bitvector(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_bitvector(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_BITVECTOR, type, eval);

	return 0;
}


/* string */
static int scope_string(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_string(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_STRING, type, eval);

	return 0;
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
	make_eval_scope(ptr, ret, EVAL_PARSE_FLOAT, type, eval);

	return 0;
}


/* declaim */
static int scope_declaim(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	apply_declaim_stack(ptr, eval);
	type_value_nil(&type);
	make_eval_scope(ptr, ret, EVAL_PARSE_DECLAIM, type, eval);

	return 0;
}


/* pathname */
static int scope_pathname(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_pathname(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_PATHNAME, type, eval);

	return 0;
}


/* environment */
static int scope_environment(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	type_value_environment(&type, eval);
	make_eval_scope(ptr, ret, EVAL_PARSE_ENVIRONMENT, type, eval);

	return 0;
}


/* progn */
_g int scope_progn(Execute ptr, addr *ret, addr eval)
{
	addr type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_allcons(ptr, &eval, &type, eval));
	make_eval_scope(ptr, ret, EVAL_PARSE_PROGN, type, eval);

	return 0;
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
	eval_scope_size(ptr, &eval, 5, EVAL_PARSE_LET, str.the, eval);
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
	eval_scope_size(ptr, &eval, 5, EVAL_PARSE_LETA, str.the, eval);
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
	make_eval_scope(ptr, ret, EVAL_PARSE_SETQ, type, cons);

	return 0;
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

	GetTypeTable(&type, Symbol);
	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_DEFMACRO, type, Nil);
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
static int scope_destructuring_bind_lambda(Execute ptr, addr *ret, addr eval)
{
	struct lambda_struct str;

	Check(! eval_parse_p(eval), "type error");
	scope_init_lambda(&str, EVAL_PARSE_MACRO_LAMBDA, 1);
	GetEvalParse(eval, 0, &str.args);
	GetEvalParse(eval, 1, &str.decl);
	GetEvalParse(eval, 2, &str.doc);
	GetEvalParse(eval, 3, &str.cons);
	return scope_destructuring_bind_call(ptr, &str, ret);
}

static int scope_destructuring_bind(Execute ptr, addr *ret, addr eval)
{
	addr args, expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &args);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	Return(scope_destructuring_bind_lambda(ptr, &args, args));
	localhold_end(hold);

	GetEvalScopeThe(args, &type);
	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_DESTRUCTURING_BIND, type, Nil);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, args);
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
	scope_define_symbol_macro_call(ptr, symbol, form, body, ret);
	return 0;
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
	type_value(&type, value);
	make_eval_scope(ptr, ret, EVAL_PARSE_QUOTE, type, value);

	return 0;
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
	eval_scope_size(ptr, &eval, 4, EVAL_PARSE_FLET, str.the, eval);
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
	eval_scope_size(ptr, &eval, 4, EVAL_PARSE_LABELS, str.the, eval);
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
	return scope_call_call(ptr, first, args, ret);
}


/* values */
static int scope_values(Execute ptr, addr *ret, addr eval)
{
	addr cons, type;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &eval);
	Return(scope_values_call(ptr, eval, &cons, &type));
	make_eval_scope(ptr, ret, EVAL_PARSE_VALUES, type, cons);

	return 0;
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
_g int scope_locally(Execute ptr, addr *ret, addr eval)
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

	eval_scope_size(ptr, &eval, 3, EVAL_PARSE_IF, type, eval);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_UNWIND_PROTECT, type, eval);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_TAGBODY, type, Nil);
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
	scope_go_call(ptr, &tag, tag);
	GetTypeTable(&type, Nil);
	make_eval_scope(ptr, ret, EVAL_PARSE_GO, type, tag);

	return 0;
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_BLOCK, type, Nil);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_RETURN_FROM, type, Nil);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_CATCH, type, Nil);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_THROW, type, eval);
	SetEvalScopeIndex(eval, 0, tag);
	SetEvalScopeIndex(eval, 1, form);
	return Result(ret, eval);
}


/* eval-when */
_g int scope_eval_when(Execute ptr, addr *ret, addr eval)
{
	addr cons, compilep, loadp, evalp;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &cons);
	GetEvalParse(eval, 1, &compilep);
	GetEvalParse(eval, 2, &loadp);
	GetEvalParse(eval, 3, &evalp);
	return scope_eval_when_call(ptr, cons, compilep, loadp, evalp, ret);
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

	eval_scope_size(ptr, &eval, 6, EVAL_PARSE_MULTIPLE_VALUE_BIND, str.the, Nil);
	SetEvalScopeIndex(eval, 0, str.args);
	SetEvalScopeIndex(eval, 1, str.expr);
	SetEvalScopeIndex(eval, 2, str.decl);
	SetEvalScopeIndex(eval, 3, str.doc);
	SetEvalScopeIndex(eval, 4, str.cons);
	SetEvalScopeIndex(eval, 5, str.free);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_MULTIPLE_VALUE_PROG1, type, eval);
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

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_NTH_VALUE, type, eval);
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

	eval_scope_size(ptr, &eval, 3, EVAL_PARSE_PROGV, type, eval);
	SetEvalScopeIndex(eval, 0, symbols);
	SetEvalScopeIndex(eval, 1, values);
	SetEvalScopeIndex(eval, 2, body);
	return Result(ret, eval);
}


/*
 *  initialize
 */
_g void init_scope_function(void)
{
	EvalScopeTable[EVAL_PARSE_NIL] = scope_nil;
	EvalScopeTable[EVAL_PARSE_T] = scope_t;
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
}
