extern void exit(int);
extern int strcmp(const char*, const char*);
extern int printf(const char*, ...);


int main(void) {

  printf("__FUNCTION__ = %s\n", __FUNCTION__);
  printf("__PRETTY_FUNCTION__ = %s\n", __PRETTY_FUNCTION__);
  
  if(strcmp("This is " __FUNCTION__, "This is main") ||
     strcmp("This is " __PRETTY_FUNCTION__, "This is main")) {
    exit(1);
  }
  exit(0);
}
