typedef void (*__sighandler_t)  (int)  ;

extern __sighandler_t  
  signal(int __sig, __sighandler_t __handler)      
    __asm__ (""     "__sysv_signal"    ) ;

int main()
{
  return 0;
}
