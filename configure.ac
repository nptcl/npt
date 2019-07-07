AC_PREREQ([2.69])
AC_INIT([npt], m4_include([config.version]), [nptcl <nptlisp@gmail.com>],
  [npt], [https://github.com/nptcl/npt])
AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile])
AM_INIT_AUTOMAKE([foreign subdir-objects])

# Checks for programs.
AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_CC_C99
AC_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Checks for libraries.
AC_CHECK_LIB([m], [csinl], [lisp_csinl=yes], [lisp_csinl=no])
AC_CHECK_LIB([pthread], [pthread_create], [lisp_thread=yes], [lisp_thread=no])
AC_CHECK_LIB([edit], [readline], [lisp_editline=yes], [lisp_editline=no])
AC_CHECK_LIB([readline], [readline], [lisp_readline=yes], [lisp_readline=no])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h float.h inttypes.h limits.h locale.h stddef.h] dnl
[stdint.h stdlib.h string.h wchar.h])
AC_CHECK_HEADERS([sys/stat.h sys/types.h sys/time.h unistd.h])
AC_CHECK_HEADERS([readline/readline.h])
AC_CHECK_HEADERS([edit/readline.h edit/readline/readline.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_CHECK_SIZEOF(void *)

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_MEMCMP
AC_FUNC_STRTOD
AC_FUNC_STRFTIME
AC_FUNC_STRTOD
AC_FUNC_STRTOLD
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor memset mkdir modf pow setlocale sqrt strchr strtol strtoul ])
AC_FUNC_STAT
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_CHECK_FUNCS([gethostname gettimeofday uname select])

# Arguments
AC_ARG_WITH([readline],
  AC_HELP_STRING([--with-readline], [use readline]))
AC_ARG_WITH([editline],
  AC_HELP_STRING([--with-editline], [use editline]))
AC_ARG_WITH([regression],
  AC_HELP_STRING([--with-regression], [use regression code]))
AC_ARG_ENABLE([standalone],
  AC_HELP_STRING([--enable-standalone], [[mode] use default standalone-mode]))
AC_ARG_ENABLE([core],
  AC_HELP_STRING([--enable-core], [[mode] use default core-mode]))
AC_ARG_ENABLE([degrade],
  AC_HELP_STRING([--enable-degrade], [[mode] use default degrade-mode]))
AC_ARG_ENABLE([debug],
  AC_HELP_STRING([--enable-debug], [develop mode (very slow)]))
AC_ARG_ENABLE([fixnum-size],
  AC_HELP_STRING([--enable-fixnum], [use fixnum size either 32 or 64]))

AC_CANONICAL_HOST
AS_CASE([$host_os],
  [freebsd*], [lisp_freebsd=yes]
              [lisp_posix=yes],
  [linux*],   [lisp_linux=yes]
              [lisp_posix=yes],
  [*cygwin*], [lisp_ansi=yes]
              [lisp_ansi_windows=yes]
              [lisp_posix=yes],
  [win*],     [lisp_windows=yes]
              [lisp_posix=no],
  [*],        [lisp_ansi=yes]
              [lisp_posix=no])

AS_IF([test x$lisp_ansi = xyes],
        AC_DEFINE([LISP_ANSI],         [1], [ANSI-C]),
      [test x$lisp_freebsd = xyes],
        AC_DEFINE([LISP_FREEBSD],      [1], [FreeBSD]),
      [test x$lisp_linux = xyes],
        AC_DEFINE([LISP_LINUX],        [1], [Linux]),
      [test x$lisp_windows = xyes],
        AC_DEFINE([LISP_WINDOWS],      [1], [Windows]),
      [test x$lisp_ansi_windows = xyes],
        AC_DEFINE([LISP_ANSI_WINDOWS], [1], [ANSI-C Windows]))

AS_IF([test x$with_readline = xyes -a x$with_editline = xyes],
        AC_MSG_ERROR([The argument must be either] dnl
        [--with-readline or --with-editline]),
      [test x$with_readline = xyes],
        [lisp_readline=yes]
        [lisp_editline=no],
      [test x$with_editline = xyes],
        [lisp_readline=no]
        [lisp_editline=yes],
      [test x$lisp_readline = xyes],
        [lisp_readline=yes]
        [lisp_editline=no],
      [test x$lisp_editline = xyes],
        [lisp_readline=no]
        [lisp_editline=yes],
      [test x$lisp_linux = xyes],
        [lisp_readline=yes]
        [lisp_editline=no],
      [test x$lisp_freebsd = xyes],
        [lisp_readline=no]
        [lisp_editline=yes])

## readline
AS_IF([test x$lisp_readline = xyes],
  AC_DEFINE([LISP_PROMPT_READLINE], [1], [use readline]))

## editline
AS_IF([test x$lisp_editline = xyes],
  AC_DEFINE([LISP_PROMPT_EDITLINE], [1], [use editline]))

## debug
AS_IF([test x$enable_debug = xyes],
  AC_DEFINE([LISP_DEBUG], [1], [debug mode])
  AC_SUBST([lisp_optimize], []),
  AC_SUBST([lisp_optimize], [-O3]))

## thread-mode
AS_IF([test x$lisp_ansi != xyes],
  AC_DEFINE([LISP_THREAD], [1], [thread mode])
  [lisp_thread=yes])

## library
library="-lm"
AS_IF([test x$lisp_thread = xyes -a x$lisp_windows != xyes],
  [library="$library -lpthread"])
AS_IF([test x$lisp_readline = xyes],
  [library="$library -lreadline"])
AS_IF([test x$lisp_editline = xyes],
  [library="$library -ledit"])
AC_SUBST([LIBS], [$library])

## regression test
AS_IF([test x$with_regression = xyes],
  AC_DEFINE([LISP_DEGRADE], [1], [regression test]))
AM_CONDITIONAL([LISP_DEGRADE], [test x$with_regression = xyes])

## mode
AS_IF([test x$enable_standalone = xyes],
  AC_DEFINE([LISP_MODE_STANDALONE], [1], [standalone-mode]))
AS_IF([test x$enable_core = xyes],
  AC_DEFINE([LISP_MODE_CORE], [1], [core-mode]))
AS_IF([test x$enable_degrade = xyes],
  AC_DEFINE([LISP_MODE_DEGRADE], [1], [degrade-mode]))

## output
AC_OUTPUT

