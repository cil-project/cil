#include <sys/select.h>

typedef struct isc_mem {
  unsigned int magic;
} isc_mem_t;

typedef struct isc_socketmgr {
  isc_mem_t	       *mctx;
  fd_set			read_fds;
  fd_set			write_fds;
  int			fdstate[FD_SETSIZE];
} isc_socketmgr_t;

typedef struct isc_socket {
  isc_socketmgr_t	       *manager;
  int			fd;
} isc_socket_t;

typedef struct isc_socketevent {
	unsigned int		minimum;	
} isc_socketevent_t;

static void
wakeup_socket(isc_socketmgr_t *manager, int fd, int msg) {
	isc_socket_t *sock;

	if (manager->fdstate[fd] == 0) {
	  FD_CLR(fd, &manager->read_fds);
	  FD_CLR(fd, &manager->write_fds);
	  return;
	}
	FD_SET(sock->fd, &manager->read_fds);
	FD_SET(sock->fd, &manager->write_fds);
}

static void
allocate_socketevent(isc_socket_t *sock, unsigned int eventtype,
		     int action, const void *arg)
{
  isc_socketevent_t *ev;
  
  ev = (isc_socketevent_t *)isc_event_allocate(sock->manager->mctx,
					       sock, eventtype,
					       action, arg,
					       sizeof (*ev));
  
}










