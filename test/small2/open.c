// testing problem with args to open...

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main()
{
  int fd = open("/dev/zero", O_RDONLY);
  char buf;

  if (read(fd, &buf, 1) != 1) {
    perror("read");
    return 2;
  }
  else {
    close(fd);
    return buf;   // should be 0
  }
}
