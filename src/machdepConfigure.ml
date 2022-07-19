module C = Configurator.V1

let has_header_code f =
  Format.sprintf {|
#include <%s>
int main() { return 0; } // Just so that dune-configurator linking works
  |} f

let has_header c f =
  C.c_test c (has_header_code f)

let builtin_va_list_code = {|
int
main (void)
{
if (sizeof (__builtin_va_list))
	 return 0;
  ;
  return 0;
}
|}

let thread_is_keyword_code = {|
int main(int __thread) { return 0; }
|}

let underscore_name_code = {|
int main() { __asm__("jmp _main"); }
|}

let cil_check_integer_type_type_code t1 t2 =
  Format.sprintf {|
#include <stddef.h>
#include <wchar.h>
#include <stdint.h>
#if __APPLE__
  // C11 7.28 defines these to be the same as uint_least16_t and uint_least32_t.
  // The standard mandates a uchar.h file to contain these typedefs, but Mac does
  // not have that header file
  typedef uint_least16_t char16_t;
  typedef uint_least32_t char32_t;
#else
  #include <uchar.h>
#endif
/* We define a prototype with one type and the function with
   another type.  This will result in compilation error
   unless the types are really identical. */
%s foo(%s x);
%s foo(%s x) { return x; }

int main() { return 0; } // Just so that dune-configurator linking works
  |} t2 t2 t1 t1

exception FoundType of string

let cil_check_integer_type_type c t1 t2 =
  if C.c_test c (cil_check_integer_type_type_code t1 t2) then
    raise (FoundType t2)

let cil_check_integer_type_signs c t1 t2 =
  cil_check_integer_type_type c t1 t2;
  cil_check_integer_type_type c t1 ("unsigned " ^ t2)

let cil_check_integer_type c t1 =
  try
    cil_check_integer_type_signs c t1 "int";
    cil_check_integer_type_signs c t1 "long";
    cil_check_integer_type_signs c t1 "long long";
    cil_check_integer_type_signs c t1 "short";
    cil_check_integer_type_signs c t1 "char";
    failwith ("cannot find definition of " ^ t1)
  with FoundType t2 ->
    t2

let () =
  C.main ~name:"machdep" (fun c ->
      let have_builtin_va_list = C.c_test c builtin_va_list_code in
      let thread_is_keyword = not @@ C.c_test c thread_is_keyword_code in
      let underscore_name = C.c_test c underscore_name_code in

      C.C_define.gen_header_file c ~fname:"machdep-config.h" [
        ("HAVE_STDLIB_H", Switch (has_header c "stdlib.h"));
        ("HAVE_WCHAR_H", Switch (has_header c "wchar.h"));
        ("HAVE_STDBOOL_H", Switch (has_header c "stdbool.h"));
        ("HAVE_INTTYPES_H", Switch (has_header c "inttypes.h"));
        ("HAVE_STDINT_H", Switch (has_header c "stdint.h"));

        ("HAVE_BUILTIN_VA_LIST_DEF", Switch have_builtin_va_list);
        ("THREAD_IS_KEYWORD_DEF", Switch thread_is_keyword);
        ("UNDERSCORE_NAME_DEF", Switch underscore_name);

        ("TYPE_SIZE_T", String (cil_check_integer_type c "size_t"));
        ("TYPE_WCHAR_T", String (cil_check_integer_type c "wchar_t"));
        ("TYPE_CHAR16_T", String (cil_check_integer_type c "char16_t"));
        ("TYPE_CHAR32_T", String (cil_check_integer_type c "char32_t"));
      ]
    )
