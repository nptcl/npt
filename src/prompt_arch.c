#include "prompt_arch.h"

#ifdef LISP_PROMPT_XTERM
#include "prompt_xterm.h"
#endif

#ifdef LISP_PROMPT_DISABLE
#include "prompt_disable.h"
#endif

#ifdef LISP_PROMPT_READLINE
#include "prompt_module.h"
#endif

#ifdef LISP_PROMPT_EDITLINE
#include "prompt_module.h"
#endif

