#ifndef __COMPILE_TYPEDEF__
#define __COMPILE_TYPEDEF__

#include "compile_faslcode.h"
#include "constant.h"
#include "define.h"
#include "pointer.h"

#define CompileWrite _n(CompileWrite)
#define CompileRead _n(CompileRead)
#define get_compile_write _n(get_compile_write)
#define get_compile_read _n(get_compile_read)
#define init_compile_typedef _n(init_compile_typedef)

extern enum FaslCode CompileWrite[p_size_code];
extern constindex CompileRead[FaslCode_size];

#ifdef LISP_DEBUG
#define GetCompileWrite(x)	get_compile_write(x)
#define GetCompileRead(x)	get_compile_read(x)
#else
#define GetCompileWrite(x)	CompileWrite[x]
#define GetCompileRead(x)	CompileRead[x]
#endif

enum FaslCode get_compile_write(pointer id);
constindex get_compile_read(enum FaslCode id);
void init_compile_typedef(void);

#endif

