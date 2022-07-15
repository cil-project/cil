module C = Configurator.V1

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

let () =
  C.main ~name:"machdep" (fun c ->
      let have_builtin_va_list = C.c_test c builtin_va_list_code in
      let thread_is_keyword = not @@ C.c_test c thread_is_keyword_code in
      let underscore_name = C.c_test c underscore_name_code in

      C.C_define.gen_header_file c ~fname:"machdep-config.h" [
        ("HAVE_BUILTIN_VA_LIST_DEF", Switch have_builtin_va_list);
        ("THREAD_IS_KEYWORD_DEF", Switch thread_is_keyword);
        ("UNDERSCORE_NAME_DEF", Switch underscore_name);
      ]
    )
