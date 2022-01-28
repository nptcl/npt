#ifndef __PRINT_DISPATCH_HEADER__
#define __PRINT_DISPATCH_HEADER__

#include "execute.h"
#include "typedef.h"

#define getlistprintdispatch _n(getlistprintdispatch)
#define setlistprintdispatch _n(setlistprintdispatch)
#define gettypeprinttable _n(gettypeprinttable)
#define settypeprinttable _n(settypeprinttable)
#define getspecifierprinttable _n(getspecifierprinttable)
#define setspecifierprinttable _n(setspecifierprinttable)
#define getfunctionprinttable _n(getfunctionprinttable)
#define setfunctionprinttable _n(setfunctionprinttable)
#define getpriorityprinttable _n(getpriorityprinttable)
#define setpriorityprinttable _n(setpriorityprinttable)
#define find_function_print_dispatch_ _n(find_function_print_dispatch_)
#define print_dispatch_p _n(print_dispatch_p)
#define pprint_dispatch_heap _n(pprint_dispatch_heap)
#define copy_pprint_dispatch_common_ _n(copy_pprint_dispatch_common_)
#define pprint_dispatch_common_ _n(pprint_dispatch_common_)
#define set_pprint_dispatch_print_ _n(set_pprint_dispatch_print_)
#define build_print_dispatch_ _n(build_print_dispatch_)

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
void getlistprintdispatch(addr pos, addr *ret);
void setlistprintdispatch(addr pos, addr value);
void gettypeprinttable(addr pos, addr *ret);
void settypeprinttable(addr pos, addr value);
void getspecifierprinttable(addr pos, addr *ret);
void setspecifierprinttable(addr pos, addr value);
void getfunctionprinttable(addr pos, addr *ret);
void setfunctionprinttable(addr pos, addr value);
void getpriorityprinttable(addr pos, addr *ret);
void setpriorityprinttable(addr pos, addr value);

/* function */
int find_function_print_dispatch_(Execute ptr, addr var, addr table, addr *ret);
int print_dispatch_p(addr pos);
void pprint_dispatch_heap(addr *ret);
int copy_pprint_dispatch_common_(Execute ptr, addr var, addr *ret);
int pprint_dispatch_common_(Execute ptr, addr var, addr table, addr *x, addr *y);
int set_pprint_dispatch_print_(LocalRoot local,
		addr spec, addr type, addr call, addr priority, addr table);
int build_print_dispatch_(void);

#endif

