#ifndef __EXECUTE_VALUES_HEADER__
#define __EXECUTE_VALUES_HEADER__

#include "execute_typedef.h"
#include "typedef.h"

#define clear_values_execute _n(clear_values_execute)
#define setresult_control _n(setresult_control)
#define setbool_control _n(setbool_control)
#define setvalues_control _n(setvalues_control)
#define setvalues_nil_control _n(setvalues_nil_control)
#define setvalues_list_control _n(setvalues_list_control)
#define getresult_control _n(getresult_control)
#define getvalues_control _n(getvalues_control)
#define getvalues_list_control_local _n(getvalues_list_control_local)
#define getvalues_list_control_heap _n(getvalues_list_control_heap)
#define lengthvalues_control _n(lengthvalues_control)
#define getvalues_root_control _n(getvalues_root_control)
#define getvalues_pop_control _n(getvalues_pop_control)

_g void clear_values_execute(Execute ptr);
_g void setresult_control(Execute ptr, addr value);
_g void setbool_control(Execute ptr, int value);
_g void setvalues_control(Execute ptr, ...);
_g void setvalues_nil_control(Execute ptr);
_g void setvalues_list_control(Execute ptr, addr list);
_g void getresult_control(Execute ptr, addr *ret);
_g void getvalues_control(Execute ptr, size_t index, addr *ret);
_g void getvalues_list_control_local(Execute ptr, addr *ret);
_g void getvalues_list_control_heap(Execute ptr, addr *ret);
_g size_t lengthvalues_control(Execute ptr);

_g void getvalues_root_control(Execute ptr, addr *ret);
_g void getvalues_pop_control(Execute ptr, addr *ret);

#endif

