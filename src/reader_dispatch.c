#include "control_operator.h"
#include "function.h"
#include "reader_function.h"
#include "symbol.h"
#include "type_table.h"

/*****************************************************************************
 *  macro character
 *****************************************************************************/
/* (defun double-quote-reader (stream character) ...) -> * */
static int function_reader_double_quote(Execute ptr, addr pos, addr code)
{
	Return(double_quote_reader(ptr->local, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_double_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_QUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_double_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-reader (stream character) ...) -> * */
static int function_reader_single_quote(Execute ptr, addr pos, addr code)
{
	Return(single_quote_reader(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_single_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-reader (stream character) ...) -> * */
static int function_reader_parensis_open(Execute ptr, addr stream, addr code)
{
	return parensis_open_reader(ptr, stream);
}

static void defun_parensis_open_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-reader (stream character) ...) -> * */
static int function_reader_parensis_close(Execute ptr, addr stream, addr code)
{
	return parensis_close_reader();
}

static void defun_parensis_close_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun semicolon-reader (stream character) ...) -> * */
static int function_reader_semicolon(Execute ptr, addr stream, addr code)
{
	Return(semicolon_reader_(stream));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_semicolon_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SEMICOLON_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_semicolon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backquote-reader (stream character) ...) -> * */
static int function_reader_backquote(Execute ptr, addr stream, addr code)
{
	Return(backquote_reader(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_backquote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKQUOTE_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_backquote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* (defun comma-reader (stream character) ...) -> * */
static int function_reader_comma(Execute ptr, addr stream, addr code)
{
	Return(comma_reader(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_comma_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMMA_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_comma);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-reader (stream character) ...) -> * */
static int function_reader_sharp(Execute ptr, addr stream, addr code)
{
	return sharp_reader(ptr, stream, code);
}

static void defun_sharp_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_READER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* (setq dispatch-function #'sharp-reader) */
	GetConst(SYSTEM_DISPATCH_FUNCTION, &symbol);
	SetFunctionSymbol(symbol, pos);
}


/* build */
static void reader_dispatch_function(void)
{
	defun_double_quote_reader(); /* " */
	defun_single_quote_reader(); /* ' */
	defun_parensis_open_reader(); /* ( */
	defun_parensis_close_reader();  /* ) */
	defun_semicolon_reader(); /* ; */
	defun_backquote_reader(); /* ` */
	defun_comma_reader(); /* , */
	defun_sharp_reader(); /* # */
}


/*****************************************************************************
 *  dispatch character
 *****************************************************************************/
/* (defun error-dispatch (stream code arg) ...) -> * */
static int function_dispatch_error(Execute ptr, addr stream, addr code, addr arg)
{
	return error_dispatch(code);
}

static void defun_error_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ERROR_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_equal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(equal_dispatch(ptr, stream, code, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_equal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EQUAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_equal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-dispatch (stream code arg) ...) -> * */
static int function_dispatch_sharp(Execute ptr, addr stream, addr code, addr arg)
{
	Return(sharp_dispatch(ptr, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_sharp_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-dispatch (stream code arg) ...) -> * */
static int function_dispatch_single_quote(Execute ptr,
		addr stream, addr code, addr arg)
{
	Return(single_quote_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_single_quote_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-dispatch (stream code arg) ...) -> * */
static int function_dispatch_parensis_open(Execute ptr,
		addr stream, addr code, addr arg)
{
	Return(parensis_open_dispatch(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_parensis_open_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-dispatch (stream code arg) ...) -> * */
static int function_dispatch_parensis_close(Execute ptr,
		addr stream, addr code, addr arg)
{
	return parensis_close_dispatch();
}

static void defun_parensis_close_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asterisk-dispatch (stream code arg) ...) -> * */
static int function_dispatch_asterisk(Execute ptr, addr stream, addr code, addr arg)
{
	Return(asterisk_dispatch_(ptr, stream, code, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_asterisk_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ASTERISK_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_asterisk);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun colon-dispatch (stream code arg) ...) -> * */
static int function_dispatch_colon(Execute ptr, addr stream, addr code, addr arg)
{
	Return(colon_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_colon_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COLON_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_colon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun less-dispatch (stream code arg) ...) -> * */
static int function_dispatch_less(Execute ptr, addr stream, addr code, addr arg)
{
	return less_dispatch();
}

static void defun_less_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LESS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_less);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backslash-dispatch (stream code arg) ...) -> * */
static int function_dispatch_backslash(Execute ptr, addr stream, addr code, addr arg)
{
	Return(backslash_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_backslash_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKSLASH_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_backslash);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun or-dispatch (stream code arg) ...) -> (values) */
static int function_dispatch_or(Execute ptr, addr stream, addr code, addr arg)
{
	Return(or_dispatch_(stream));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_or_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OR_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_or);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun plus-dispatch (stream code arg) ...) -> * */
static int function_dispatch_plus(Execute ptr, addr stream, addr code, addr arg)
{
	return plus_dispatch(ptr, stream);
}

static void defun_plus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PLUS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_plus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun minus-dispatch (stream code arg) ...) -> * */
static int function_dispatch_minus(Execute ptr, addr stream, addr code, addr arg)
{
	return minus_dispatch(ptr, stream);
}

static void defun_minus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MINUS_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_minus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dot-dispatch (stream code arg) ...) -> * */
static int function_dispatch_dot(Execute ptr, addr stream, addr code, addr arg)
{
	Return(dot_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_dot_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOT_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_dot);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun radix-dispatch (stream code arg) ...) -> * */
static int function_dispatch_radix(Execute ptr, addr stream, addr code, addr arg)
{
	Return(radix_dispatch(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_radix_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RADIX_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_radix);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun binary-dispatch (stream code arg) ...) -> * */
static int function_dispatch_binary(Execute ptr, addr stream, addr code, addr arg)
{
	Return(binary_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_binary_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BINARY_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_binary);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun octal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_octal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(octal_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_octal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OCTAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_octal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hexadecimal-dispatch (stream code arg) ...) -> * */
static int function_dispatch_hexadecimal(Execute ptr, addr stream, addr code, addr arg)
{
	Return(hexadecimal_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_hexadecimal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HEXADECIMAL_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_hexadecimal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complex-dispatch (stream code arg) ...) -> * */
static int function_dispatch_complex(Execute ptr, addr stream, addr code, addr arg)
{
	Return(complex_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_complex_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMPLEX_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-dispatch (stream code arg) ...) -> * */
static int function_dispatch_array(Execute ptr, addr stream, addr code, addr arg)
{
	Return(array_dispatch(ptr, stream, arg, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_array_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_array);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-dispatch (stream code arg) ...) -> * */
static int function_dispatch_pathname(Execute ptr, addr stream, addr code, addr arg)
{
	Return(pathname_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_pathname_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PATHNAME_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_pathname);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun structure-dispatch (stream code arg) ...) -> structure-object */
static int function_dispatch_structure(Execute ptr, addr stream, addr code, addr arg)
{
	Return(structure_dispatch(ptr, stream, &code));
	setresult_control(ptr, code);
	return 0;
}

static void defun_structure_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_STRUCTURE_DISPATCH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_structure);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void reader_dispatch_sharp(void)
{
	defun_error_dispatch();           /* whitespace */
	defun_equal_dispatch();           /* #= */
	defun_sharp_dispatch();           /* ## */
	defun_single_quote_dispatch();    /* #' */
	defun_parensis_open_dispatch();   /* #( */
	defun_parensis_close_dispatch();  /* #) */
	defun_asterisk_dispatch();        /* #* */
	defun_colon_dispatch();           /* #: */
	defun_less_dispatch();            /* #< */
	defun_backslash_dispatch();       /* #\ */
	defun_or_dispatch();              /* #| */
	defun_plus_dispatch();            /* #+ */
	defun_minus_dispatch();           /* #- */
	defun_dot_dispatch();             /* #. */
	defun_radix_dispatch();           /* #R */
	defun_binary_dispatch();          /* #B */
	defun_octal_dispatch();           /* #O */
	defun_hexadecimal_dispatch();     /* #X */
	defun_complex_dispatch();         /* #C */
	defun_array_dispatch();           /* #A */
	defun_pathname_dispatch();        /* #P */
	defun_structure_dispatch();       /* #S */
}


/*****************************************************************************
 *  build-readtable
 *****************************************************************************/
static void reader_dispatch_constant(void)
{
	addr symbol, gensym, name;

	/* (setq readtable-dot (make-symbol "READTABLE-DOT")) */
	GetConst(SYSTEM_READTABLE_DOT, &symbol);
	symbol_heap(&gensym);
	GetNameSymbol(symbol, &name);
	SetNameSymbol(gensym, name);
	SetValueSymbol(symbol, gensym);
	SetStatusReadOnly(symbol);
}

_g void build_reader_dispatch(void)
{
	reader_dispatch_function();
	reader_dispatch_sharp();
	reader_dispatch_constant();
}

_g void init_reader_dispatch(void)
{
	SetPointerCall(defun, var2, reader_double_quote);
	SetPointerCall(defun, var2, reader_single_quote);
	SetPointerCall(defun, var2, reader_parensis_open);
	SetPointerCall(defun, var2, reader_parensis_close);
	SetPointerCall(defun, var2, reader_semicolon);
	SetPointerCall(defun, var2, reader_backquote);
	SetPointerCall(defun, var2, reader_comma);
	SetPointerCall(defun, var2, reader_sharp);
	SetPointerCall(defun, var3, dispatch_error);
	SetPointerCall(defun, var3, dispatch_equal);
	SetPointerCall(defun, var3, dispatch_sharp);
	SetPointerCall(defun, var3, dispatch_single_quote);
	SetPointerCall(defun, var3, dispatch_parensis_open);
	SetPointerCall(defun, var3, dispatch_parensis_close);
	SetPointerCall(defun, var3, dispatch_asterisk);
	SetPointerCall(defun, var3, dispatch_colon);
	SetPointerCall(defun, var3, dispatch_less);
	SetPointerCall(defun, var3, dispatch_backslash);
	SetPointerCall(defun, var3, dispatch_or);
	SetPointerCall(defun, var3, dispatch_plus);
	SetPointerCall(defun, var3, dispatch_minus);
	SetPointerCall(defun, var3, dispatch_dot);
	SetPointerCall(defun, var3, dispatch_radix);
	SetPointerCall(defun, var3, dispatch_binary);
	SetPointerCall(defun, var3, dispatch_octal);
	SetPointerCall(defun, var3, dispatch_hexadecimal);
	SetPointerCall(defun, var3, dispatch_complex);
	SetPointerCall(defun, var3, dispatch_array);
	SetPointerCall(defun, var3, dispatch_pathname);
	SetPointerCall(defun, var3, dispatch_structure);
}

