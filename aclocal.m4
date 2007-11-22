dnl check whether integer types $1 and $2 are the same

AC_DEFUN([__CIL_CHECK_INTEGER_TYPE_TYPE], [
  if test -z "$real_type"; then
    AC_COMPILE_IFELSE(AC_LANG_SOURCE([
#include <stddef.h>
#include <wchar.h>
/* We define a prototype with one type and the function with
   another type.  This will result in compilation error
   unless the types are really identical. */
$2 foo($2 x);
$1 foo($1 x) { return x; }]),
    real_type='$2')
  fi
])


dnl check whether integer type $1 is the
dnl the signed or unsigned variant of $2

AC_DEFUN([__CIL_CHECK_INTEGER_TYPE_SIGNS], [
  __CIL_CHECK_INTEGER_TYPE_TYPE([$1], [$2])
  __CIL_CHECK_INTEGER_TYPE_TYPE([$1], [unsigned $2])
])


dnl set configuration macro $2 to a string representing
dnl the real integer type corresponding to typedef $1

AC_DEFUN([CIL_CHECK_INTEGER_TYPE], [
  AC_MSG_CHECKING([for real definition of $1])
  real_type=''
  __CIL_CHECK_INTEGER_TYPE_SIGNS([$1], int)
  __CIL_CHECK_INTEGER_TYPE_SIGNS([$1], long)
  __CIL_CHECK_INTEGER_TYPE_SIGNS([$1], short)
  __CIL_CHECK_INTEGER_TYPE_SIGNS([$1], char)
  if test -z "$real_type"; then
    AC_MSG_ERROR([cannot find definition of $1])
  fi
  AC_DEFINE_UNQUOTED([$2], "[$real_type]")
  AC_MSG_RESULT([$real_type])
])
