#ifndef __EXECUTE_OBJECT_HEADER__
#define __EXECUTE_OBJECT_HEADER__

#include "execute.h"
#include "typedef.h"

#define init_execute_values _n(init_execute_values)
#define save_values_control _n(save_values_control)
#define restore_values_control _n(restore_values_control)
#define normal_throw_control _n(normal_throw_control)
#define save_throw_control _n(save_throw_control)
#define restore_throw_control _n(restore_throw_control)
#define save_execute_control _n(save_execute_control)
#define restore_execute_control _n(restore_execute_control)
#define lexical_control _n(lexical_control)
#define getlow_lexical_debug _n(getlow_lexical_debug)
#define setlow_lexical_debug _n(setlow_lexical_debug)
#define get_lexical_control _n(get_lexical_control)
#define set_lexical_control _n(set_lexical_control)
#define reference_lexical_control _n(reference_lexical_control)
#define closure_heap _n(closure_heap)
#define get_closure _n(get_closure)
#define lexical_closure _n(lexical_closure)
#define reference_heap _n(reference_heap)
#define get_reference _n(get_reference)
#define set_reference _n(set_reference)
#define getvalue_reference _n(getvalue_reference)

/* values */
#define SetExecuteValues			SetArrayA4
#define GetExecuteValues			GetArrayA4
#define SetExecuteValuesList(x,y)	SetExecuteValues((x),EXECUTE_VALUES,(y))
#define GetExecuteValuesList(x,y)	GetExecuteValues((x),EXECUTE_VALUES,(y))
_g void init_execute_values(struct execute *bit);
_g void save_values_control(struct execute *ptr, addr *ret, size_t *rsize);
_g void restore_values_control(struct execute *ptr, addr pos, size_t size);


/* throw */
_g void normal_throw_control(struct execute *ptr);
_g void save_throw_control(struct execute *ptr, struct execute_throw *save);
_g void restore_throw_control(struct execute *ptr, const struct execute_throw *save);


/* save */
_g void save_execute_control(struct execute *ptr, addr *ret);
_g void restore_execute_control(struct execute *ptr, addr pos);


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

