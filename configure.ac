#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.59])
AC_INIT([npt],[0.0.0],nptcl)

AM_CONFIG_HEADER(config.h)
AC_CONFIG_FILES([Makefile])
AC_CONFIG_SRCDIR([Makefile.am])
AM_INIT_AUTOMAKE([foreign subdir-objects])
AM_MAINTAINER_MODE([enable])
# Checks for programs.
AC_PROG_CC
AC_PROG_CC_STDC
AM_PROG_CC_C_O
AC_CANONICAL_HOST
AC_CHECK_LIB(pthread, pthread_create)
AC_CHECK_LIB(m, sinl)
AC_CHECK_LIB(edit, main)
AC_CHECK_LIB(readline, main)
AC_CHECK_HEADERS(edit/readline/readline.h)
AC_CHECK_HEADERS(readline/readline.h)

AC_CHECK_SIZEOF(void*)
case $host_os in
  freebsd*) AC_DEFINE([LISP_FREEBSD], [], [Freebsd]);;
   *) ;;
esac

AC_ARG_ENABLE([lisp_degrade],
  [AC_HELP_STRING([--enable-lisp-degrade],[degrade])],
  [case "${enableval}" in
    yes) AC_DEFINE(LISP_DEGRADE,[],[gen]);;
    no)  ;;
    *);;
   esac],[])

AC_OUTPUT
