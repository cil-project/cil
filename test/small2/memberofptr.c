struct posix_header {
  char name[100] ;
  char typeflag ;
  char prefix[155] ;
} ; /*onlytypedef*/
union block {
  struct posix_header header ;
} ; /*onlytypedef*/


enum read_header {
  HEADER_FAILURE
} ; /*onlytypedef*/


enum read_header read_header(void  )
{
  union block *  header ; /*decdef*/
  static char *  next; /*decdef*/
  if((header->header).typeflag == 'L' )
    {
      struct posix_header *  h; /*decdef*/
      char namebuf[sizeof(h->prefix) + 1] ; /*decdef*/
    }
}

int main () {
  return 0;
}
