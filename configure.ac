AC_PREREQ([2.63])
AC_INIT([sipxopenacd], [4.6.0])

AC_ARG_ENABLE(dep-check, 
  AS_HELP_STRING([--disable-dep-check], 
  [Do not check for dependencies other then those required for dist target]))

AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_ERLANG_PATH_ERL

EZ_NEED_PROG([REBAR], [rebar], [Rebar Erlang build tool [autodetected]])dnl

# EZ_ERLANG_NEED_LIB()

AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
