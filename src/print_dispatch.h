#ifndef __PRINT_DISPATCH_HEADER__
#define __PRINT_DISPATCH_HEADER__

#include "execute.h"
#include "typedef.h"

enum PrintTable_Index {
	PrintTable_type,
	PrintTable_specifier,
	PrintTable_function,
	PrintTable_priority,
	PrintTable_size
};

enum PrintDispatch_Index {
	PrintDispatch_list,
	PrintDispatch_size
};

#define GetListPrintDispatch_Low(x,y)   GetArrayA2((x), PrintDispatch_list, (y))
#define SetListPrintDispatch_Low(x,y)   SetArrayA2((x), PrintDispatch_list, (y))
#define GetTypePrintTable_Low(x,y)  GetArrayA2((x), PrintTable_type, (y))
#define SetTypePrintTable_Low(x,y)  SetArrayA2((x), PrintTable_type, (y))
#define GetSpecifierPrintTable_Low(x,y)  GetArrayA2((x), PrintTable_specifier, (y))
#define SetSpecifierPrintTable_Low(x,y)  SetArrayA2((x), PrintTable_specifier, (y))
#define GetFunctionPrintTable_Low(x,y)  GetArrayA2((x), PrintTable_function, (y))
#define SetFunctionPrintTable_Low(x,y)  SetArrayA2((x), PrintTable_function, (y))
#define GetPriorityPrintTable_Low(x,y)  GetArrayA2((x), PrintTable_priority, (y))
#define SetPriorityPrintTable_Low(x,y)  SetArrayA2((x), PrintTable_priority, (y))

#ifdef LISP_DEBUG
#define GetListPrintDispatch(x,y)    getlistprintdispatch((x),(y))
#define SetListPrintDispatch(x,y)    setlistprintdispatch((x),(y))
#define GetTypePrintTable(x,y)       gettypeprinttable((x),(y))
#define SetTypePrintTable(x,y)       settypeprinttable((x),(y))
#define GetSpecifierPrintTable(x,y)  getspecifierprinttable((x),(y))
#define SetSpecifierPrintTable(x,y)  setspecifierprinttable((x),(y))
#define GetFunctionPrintTable(x,y)   getfunctionprinttable((x),(y))
#define SetFunctionPrintTable(x,y)   setfunctionprinttable((x),(y))
#define GetPriorityPrintTable(x,y)   getpriorityprinttable((x),(y))
#define SetPriorityPrintTable(x,y)   setpriorityprinttable((x),(y))
#else
#define GetListPrintDispatch(x,y)    GetListPrintDispatch_Low((x),(y))
#define SetListPrintDispatch(x,y)    SetListPrintDispatch_Low((x),(y))
#define GetTypePrintTable(x,y)       GetTypePrintTable_Low((x),(y))
#define SetTypePrintTable(x,y)       SetTypePrintTable_Low((x),(y))
#define GetSpecifierPrintTable(x,y)  GetSpecifierPrintTable_Low((x),(y))
#define SetSpecifierPrintTable(x,y)  SetSpecifierPrintTable_Low((x),(y))
#define GetFunctionPrintTable(x,y)   GetFunctionPrintTable_Low((x),(y))
#define SetFunctionPrintTable(x,y)   SetFunctionPrintTable_Low((x),(y))
#define GetPriorityPrintTable(x,y)   GetPriorityPrintTable_Low((x),(y))
#define SetPriorityPrintTable(x,y)   SetPriorityPrintTable_Low((x),(y))
#endif

/* access */
_g void getlistprintdispatch(addr pos, addr *ret);
_g void setlistprintdispatch(addr pos, addr value);
_g void gettypeprinttable(addr pos, addr *ret);
_g void settypeprinttable(addr pos, addr value);
_g void getspecifierprinttable(addr pos, addr *ret);
_g void setspecifierprinttable(addr pos, addr value);
_g void getfunctionprinttable(addr pos, addr *ret);
_g void setfunctionprinttable(addr pos, addr value);
_g void getpriorityprinttable(addr pos, addr *ret);
_g void setpriorityprinttable(addr pos, addr value);

/* function */
_g int find_function_print_dispatch(Execute ptr, addr var, addr table, addr *ret);
_g int print_dispatch_p(addr pos);
_g void pprint_dispatch_heap(addr *ret);
_g int copy_pprint_dispatch_common_(Execute ptr, addr var, addr *ret);
_g int pprint_dispatch_common_(Execute ptr, addr var, addr table, addr *x, addr *y);
_g int set_pprint_dispatch_print_(LocalRoot local,
		addr spec, addr type, addr call, addr priority, addr table);
_g int build_print_dispatch_(void);

#endif

