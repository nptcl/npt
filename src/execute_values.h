#ifndef __EXECUTE_VALUES_HEADER__
#define __EXECUTE_VALUES_HEADER__

#include "execute_typedef.h"
#include "typedef.h"

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

#endif

