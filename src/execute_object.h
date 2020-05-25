#ifndef __EXECUTE_OBJECT_HEADER__
#define __EXECUTE_OBJECT_HEADER__

#include "execute.h"
#include "typedef.h"

/* values */
#define SetExecuteValues			SetArrayA4
#define GetExecuteValues			GetArrayA4
#define SetExecuteValuesList(x,y)	SetExecuteValues((x),EXECUTE_VALUES,(y))
#define GetExecuteValuesList(x,y)	GetExecuteValues((x),EXECUTE_VALUES,(y))
_g void init_execute_values(struct execute *bit);
_g void save_values_control(struct execute *ptr, addr *ret, size_t *rsize);
_g void restore_values_control(struct execute *ptr, addr pos, size_t size);


/* lexical */
#define SetExecuteLexical		SetArrayA4
#ifdef LISP_DEBUG
#define getlow_lexical_control(p,i,r) getlow_lexical_debug((p),(i),(r))
#define setlow_lexical_control(p,i,v) setlow_lexical_debug((p),(i),(v))
#else
#define getlow_lexical_control(p,i,r) (*(r) = (p)->lexical_reader[i])
#define setlow_lexical_control(p,i,v) SetExecuteLexical((p)->lexical_vector,(i),(v))
#endif

_g void lexical_control(struct execute *ptr, size_t size);
_g void getlow_lexical_debug(struct execute *ptr, size_t index, addr *ret);
_g void setlow_lexical_debug(struct execute *ptr, size_t index, addr value);
_g void get_lexical_control(struct execute *ptr, size_t index, addr *ret);
_g void set_lexical_control(struct execute *ptr, size_t index, addr value);
_g void reference_lexical_control(struct execute *ptr, size_t index);


/* closure */
_g void closure_heap(addr *ret, addr value, size_t lexical);
_g void get_closure(addr pos, addr *ret);
_g size_t lexical_closure(addr pos);


/* reference */
_g void reference_heap(addr *ret, addr value);
_g void get_reference(addr pos, addr *ret);
_g void set_reference(addr pos, addr value);
_g void getvalue_reference(addr pos, addr *ret);

#endif
