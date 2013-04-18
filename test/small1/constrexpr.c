# 1 "hw/acpi/piix4.c"
# 1 "/home/gabriel/repos/qemu//"
# 1 "<command-line>"
# 1 "hw/acpi/piix4.c"
# 21 "hw/acpi/piix4.c"
# 1 "/home/gabriel/repos/qemu/include/hw/hw.h" 1




# 1 "/home/gabriel/repos/qemu/include/qemu-common.h" 1
# 15 "/home/gabriel/repos/qemu/include/qemu-common.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/compiler.h" 1





# 1 "./config-host.h" 1
# 7 "/home/gabriel/repos/qemu/include/qemu/compiler.h" 2
# 16 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "./config-host.h" 1
# 17 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/typedefs.h" 1





typedef struct QEMUTimer QEMUTimer;
typedef struct QEMUFile QEMUFile;
typedef struct QEMUBH QEMUBH;

struct Monitor;
typedef struct Monitor Monitor;
typedef struct MigrationParams MigrationParams;

typedef struct Property Property;
typedef struct PropertyInfo PropertyInfo;
typedef struct CompatProperty CompatProperty;
typedef struct DeviceState DeviceState;
typedef struct BusState BusState;
typedef struct BusClass BusClass;

typedef struct AddressSpace AddressSpace;
typedef struct MemoryRegion MemoryRegion;
typedef struct MemoryRegionSection MemoryRegionSection;

typedef struct NICInfo NICInfo;
typedef struct HCIInfo HCIInfo;
typedef struct AudioState AudioState;
typedef struct BlockDriverState BlockDriverState;
typedef struct DriveInfo DriveInfo;
typedef struct DisplayState DisplayState;
typedef struct DisplayChangeListener DisplayChangeListener;
typedef struct DisplaySurface DisplaySurface;
typedef struct PixelFormat PixelFormat;
typedef struct QemuConsole QemuConsole;
typedef struct CharDriverState CharDriverState;
typedef struct MACAddr MACAddr;
typedef struct NetClientState NetClientState;
typedef struct i2c_bus i2c_bus;
typedef struct ISABus ISABus;
typedef struct ISADevice ISADevice;
typedef struct SMBusDevice SMBusDevice;
typedef struct PCIHostState PCIHostState;
typedef struct PCIExpressHost PCIExpressHost;
typedef struct PCIBus PCIBus;
typedef struct PCIDevice PCIDevice;
typedef struct PCIExpressDevice PCIExpressDevice;
typedef struct PCIBridge PCIBridge;
typedef struct PCIEAERMsg PCIEAERMsg;
typedef struct PCIEAERLog PCIEAERLog;
typedef struct PCIEAERErr PCIEAERErr;
typedef struct PCIEPort PCIEPort;
typedef struct PCIESlot PCIESlot;
typedef struct MSIMessage MSIMessage;
typedef struct SerialState SerialState;
typedef struct PCMCIACardState PCMCIACardState;
typedef struct MouseTransformInfo MouseTransformInfo;
typedef struct uWireSlave uWireSlave;
typedef struct I2SCodec I2SCodec;
typedef struct SSIBus SSIBus;
typedef struct EventNotifier EventNotifier;
typedef struct VirtIODevice VirtIODevice;
typedef struct QEMUSGList QEMUSGList;
typedef struct SHPCDevice SHPCDevice;
# 18 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 26 "/home/gabriel/repos/qemu/include/qemu-common.h"
# 1 "/usr/include/stdlib.h" 1 3 4
# 25 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 323 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4
# 324 "/usr/include/features.h" 2 3 4
# 356 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 1 3 4
# 359 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 360 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 2 3 4
# 357 "/usr/include/features.h" 2 3 4
# 388 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 1 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 5 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4




# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs-64.h" 1 3 4
# 10 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4
# 389 "/usr/include/features.h" 2 3 4
# 26 "/usr/include/stdlib.h" 2 3 4







# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 213 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 325 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef int wchar_t;
# 34 "/usr/include/stdlib.h" 2 3 4








# 1 "/usr/include/x86_64-linux-gnu/bits/waitflags.h" 1 3 4
# 43 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 1 3 4
# 65 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/endian.h" 1 3 4
# 38 "/usr/include/endian.h" 2 3 4
# 61 "/usr/include/endian.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 2 3 4
# 62 "/usr/include/endian.h" 2 3 4
# 66 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 2 3 4

union wait
  {
    int w_status;
    struct
      {

 unsigned int __w_termsig:7;
 unsigned int __w_coredump:1;
 unsigned int __w_retcode:8;
 unsigned int:16;







      } __wait_terminated;
    struct
      {

 unsigned int __w_stopval:8;
 unsigned int __w_stopsig:8;
 unsigned int:16;






      } __wait_stopped;
  };
# 44 "/usr/include/stdlib.h" 2 3 4
# 68 "/usr/include/stdlib.h" 3 4
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));
# 96 "/usr/include/stdlib.h" 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;







__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;


# 140 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern double atof (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern int atoi (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern long int atol (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





__extension__ extern long long int atoll (__const char *__nptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern double strtod (__const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern float strtof (__const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern long double strtold (__const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern long int strtol (__const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern unsigned long int strtoul (__const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));




__extension__
extern long long int strtoq (__const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

__extension__
extern unsigned long long int strtouq (__const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





__extension__
extern long long int strtoll (__const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

__extension__
extern unsigned long long int strtoull (__const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

# 236 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/xlocale.h" 1 3 4
# 28 "/usr/include/xlocale.h" 3 4
typedef struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
} *__locale_t;


typedef __locale_t locale_t;
# 237 "/usr/include/stdlib.h" 2 3 4



extern long int strtol_l (__const char *__restrict __nptr,
     char **__restrict __endptr, int __base,
     __locale_t __loc) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 4))) __attribute__ ((__warn_unused_result__));

extern unsigned long int strtoul_l (__const char *__restrict __nptr,
        char **__restrict __endptr,
        int __base, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 4))) __attribute__ ((__warn_unused_result__));

__extension__
extern long long int strtoll_l (__const char *__restrict __nptr,
    char **__restrict __endptr, int __base,
    __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 4))) __attribute__ ((__warn_unused_result__));

__extension__
extern unsigned long long int strtoull_l (__const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 4))) __attribute__ ((__warn_unused_result__));

extern double strtod_l (__const char *__restrict __nptr,
   char **__restrict __endptr, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3))) __attribute__ ((__warn_unused_result__));

extern float strtof_l (__const char *__restrict __nptr,
         char **__restrict __endptr, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3))) __attribute__ ((__warn_unused_result__));

extern long double strtold_l (__const char *__restrict __nptr,
         char **__restrict __endptr,
         __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3))) __attribute__ ((__warn_unused_result__));





extern __inline double
__attribute__ ((__nothrow__)) atof (__const char *__nptr)
{
  return strtod (__nptr, (char **) ((void *)0));
}
extern __inline int
__attribute__ ((__nothrow__)) atoi (__const char *__nptr)
{
  return (int) strtol (__nptr, (char **) ((void *)0), 10);
}
extern __inline long int
__attribute__ ((__nothrow__)) atol (__const char *__nptr)
{
  return strtol (__nptr, (char **) ((void *)0), 10);
}




__extension__ extern __inline long long int
__attribute__ ((__nothrow__)) atoll (__const char *__nptr)
{
  return strtoll (__nptr, (char **) ((void *)0), 10);
}

# 311 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));


extern long int a64l (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));




# 1 "/usr/include/x86_64-linux-gnu/sys/types.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/types.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;







typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
# 131 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/typesizes.h" 1 3 4
# 132 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef long int __swblk_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;

typedef long int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;
# 31 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;





typedef __ino64_t ino_t;




typedef __ino64_t ino64_t;




typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;







typedef __off64_t off_t;




typedef __off64_t off64_t;




typedef __pid_t pid_t;





typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 133 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 58 "/usr/include/time.h" 3 4


typedef __clock_t clock_t;



# 74 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 92 "/usr/include/time.h" 3 4
typedef __clockid_t clockid_t;
# 104 "/usr/include/time.h" 3 4
typedef __timer_t timer_t;
# 134 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;





# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 148 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
# 195 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));
# 220 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/select.h" 1 3 4
# 31 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/select.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/select.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/select.h" 2 3 4
# 32 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 35 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 120 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };
# 45 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/time.h" 1 3 4
# 75 "/usr/include/x86_64-linux-gnu/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 47 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4
# 55 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
typedef long int __fd_mask;
# 67 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;
# 99 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4

# 109 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 121 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);



# 221 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/sys/sysmacros.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/sys/sysmacros.h" 3 4
__extension__
extern unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__));
__extension__
extern unsigned long long int gnu_dev_makedev (unsigned int __major,
            unsigned int __minor)
     __attribute__ ((__nothrow__));


__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_major (unsigned long long int __dev)
{
  return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
}

__extension__ extern __inline unsigned int
__attribute__ ((__nothrow__)) gnu_dev_minor (unsigned long long int __dev)
{
  return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
}

__extension__ extern __inline unsigned long long int
__attribute__ ((__nothrow__)) gnu_dev_makedev (unsigned int __major, unsigned int __minor)
{
  return ((__minor & 0xff) | ((__major & 0xfff) << 8)
   | (((unsigned long long int) (__minor & ~0xff)) << 12)
   | (((unsigned long long int) (__major & ~0xfff)) << 32));
}
# 224 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4





typedef __blksize_t blksize_t;
# 249 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef __blkcnt64_t blkcnt_t;



typedef __fsblkcnt64_t fsblkcnt_t;



typedef __fsfilcnt64_t fsfilcnt_t;





typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





# 1 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 2 3 4
# 50 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
typedef unsigned long int pthread_t;


typedef union
{
  char __size[56];
  long int __align;
} pthread_attr_t;



typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 76 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;

    unsigned int __nusers;



    int __kind;

    int __spins;
    __pthread_list_t __list;
# 101 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
  } __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    __extension__ unsigned long long int __total_seq;
    __extension__ unsigned long long int __wakeup_seq;
    __extension__ unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;





typedef union
{

  struct
  {
    int __lock;
    unsigned int __nr_readers;
    unsigned int __readers_wakeup;
    unsigned int __writer_wakeup;
    unsigned int __nr_readers_queued;
    unsigned int __nr_writers_queued;
    int __writer;
    int __shared;
    unsigned long int __pad1;
    unsigned long int __pad2;


    unsigned int __flags;
  } __data;
# 187 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 272 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



# 321 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) __attribute__ ((__nothrow__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));






extern int rand (void) __attribute__ ((__nothrow__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__));




extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__));







extern double drand48 (void) __attribute__ ((__nothrow__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) __attribute__ ((__nothrow__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) __attribute__ ((__nothrow__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));









extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__warn_unused_result__));

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__warn_unused_result__));










extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));

extern void free (void *__ptr) __attribute__ ((__nothrow__));




extern void cfree (void *__ptr) __attribute__ ((__nothrow__));



# 1 "/usr/include/alloca.h" 1 3 4
# 25 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 26 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) __attribute__ ((__nothrow__));






# 498 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__warn_unused_result__));




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));




extern void abort (void) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 528 "/usr/include/stdlib.h" 3 4
extern int at_quick_exit (void (*__func) (void)) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern void exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));







extern void quick_exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));







extern void _Exit (int __status) __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));






extern char *getenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));




extern char *__secure_getenv (__const char *__name)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int putenv (char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int setenv (__const char *__name, __const char *__value, int __replace)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));


extern int unsetenv (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) __attribute__ ((__nothrow__));
# 606 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 623 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __asm__ ("" "mkstemp64")
     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int mkstemp64 (char *__template) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 645 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __asm__ ("" "mkstemps64")
                     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int mkstemps64 (char *__template, int __suffixlen)
     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 663 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 677 "/usr/include/stdlib.h" 3 4
extern int mkostemp (char *__template, int __flags) __asm__ ("" "mkostemp64")
     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int mkostemp64 (char *__template, int __flags) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 698 "/usr/include/stdlib.h" 3 4
extern int mkostemps (char *__template, int __suffixlen, int __flags) __asm__ ("" "mkostemps64")

     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int mkostemps64 (char *__template, int __suffixlen, int __flags)
     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));









extern int system (__const char *__command) __attribute__ ((__warn_unused_result__));






extern char *canonicalize_file_name (__const char *__name)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 734 "/usr/include/stdlib.h" 3 4
extern char *realpath (__const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));






typedef int (*__compar_fn_t) (__const void *, __const void *);


typedef __compar_fn_t comparison_fn_t;



typedef int (*__compar_d_fn_t) (__const void *, __const void *, void *);





extern void *bsearch (__const void *__key, __const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) __attribute__ ((__warn_unused_result__));



extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));

extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
       __compar_d_fn_t __compar, void *__arg)
  __attribute__ ((__nonnull__ (1, 4)));




extern int abs (int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));
extern long int labs (long int __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));



__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));







extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));




__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__)) __attribute__ ((__warn_unused_result__));

# 808 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) __attribute__ ((__warn_unused_result__));




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) __attribute__ ((__warn_unused_result__));




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3))) __attribute__ ((__warn_unused_result__));




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) __attribute__ ((__warn_unused_result__));
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4))) __attribute__ ((__warn_unused_result__));
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3))) __attribute__ ((__warn_unused_result__));




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4, 5)));







extern int mblen (__const char *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));


extern int mbtowc (wchar_t *__restrict __pwc,
     __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   __const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__));

extern size_t wcstombs (char *__restrict __s,
   __const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__));








extern int rpmatch (__const char *__response) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 896 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *__const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2, 3))) __attribute__ ((__warn_unused_result__));





extern void setkey (__const char *__key) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







extern int posix_openpt (int __oflag) __attribute__ ((__warn_unused_result__));







extern int grantpt (int __fd) __attribute__ ((__nothrow__));



extern int unlockpt (int __fd) __attribute__ ((__nothrow__));




extern char *ptsname (int __fd) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));






extern int ptsname_r (int __fd, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));


extern int getpt (void);






extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





# 1 "/usr/include/x86_64-linux-gnu/bits/stdlib.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/stdlib.h" 3 4
extern char *__realpath_chk (__const char *__restrict __name,
        char *__restrict __resolved,
        size_t __resolvedlen) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern char *__realpath_alias (__const char *__restrict __name, char *__restrict __resolved) __asm__ ("" "realpath") __attribute__ ((__nothrow__))

                                                 __attribute__ ((__warn_unused_result__));
extern char *__realpath_chk_warn (__const char *__restrict __name, char *__restrict __resolved, size_t __resolvedlen) __asm__ ("" "__realpath_chk") __attribute__ ((__nothrow__))


                                                __attribute__ ((__warn_unused_result__))
     __attribute__((__warning__ ("second argument of realpath must be either NULL or at " "least PATH_MAX bytes long buffer")))
                                      ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
__attribute__ ((__nothrow__)) realpath (__const char *__restrict __name, char *__restrict __resolved)
{
  if (__builtin_object_size (__resolved, 2 > 1) != (size_t) -1)
    {




      return __realpath_chk (__name, __resolved, __builtin_object_size (__resolved, 2 > 1));
    }

  return __realpath_alias (__name, __resolved);
}


extern int __ptsname_r_chk (int __fd, char *__buf, size_t __buflen,
       size_t __nreal) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int __ptsname_r_alias (int __fd, char *__buf, size_t __buflen) __asm__ ("" "ptsname_r") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (2)));
extern int __ptsname_r_chk_warn (int __fd, char *__buf, size_t __buflen, size_t __nreal) __asm__ ("" "__ptsname_r_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (2))) __attribute__((__warning__ ("ptsname_r called with buflen bigger than " "size of buf")))
                   ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) ptsname_r (int __fd, char *__buf, size_t __buflen)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__buflen))
 return __ptsname_r_chk (__fd, __buf, __buflen, __builtin_object_size (__buf, 2 > 1));
      if (__buflen > __builtin_object_size (__buf, 2 > 1))
 return __ptsname_r_chk_warn (__fd, __buf, __buflen, __builtin_object_size (__buf, 2 > 1));
    }
  return __ptsname_r_alias (__fd, __buf, __buflen);
}


extern int __wctomb_chk (char *__s, wchar_t __wchar, size_t __buflen)
  __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern int __wctomb_alias (char *__s, wchar_t __wchar) __asm__ ("" "wctomb") __attribute__ ((__nothrow__))
              __attribute__ ((__warn_unused_result__));

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) int
__attribute__ ((__nothrow__)) wctomb (char *__s, wchar_t __wchar)
{







  if (__builtin_object_size (__s, 2 > 1) != (size_t) -1 && 16 > __builtin_object_size (__s, 2 > 1))
    return __wctomb_chk (__s, __wchar, __builtin_object_size (__s, 2 > 1));
  return __wctomb_alias (__s, __wchar);
}


extern size_t __mbstowcs_chk (wchar_t *__restrict __dst,
         __const char *__restrict __src,
         size_t __len, size_t __dstlen) __attribute__ ((__nothrow__));
extern size_t __mbstowcs_alias (wchar_t *__restrict __dst, __const char *__restrict __src, size_t __len) __asm__ ("" "mbstowcs") __attribute__ ((__nothrow__))


                                  ;
extern size_t __mbstowcs_chk_warn (wchar_t *__restrict __dst, __const char *__restrict __src, size_t __len, size_t __dstlen) __asm__ ("" "__mbstowcs_chk") __attribute__ ((__nothrow__))



     __attribute__((__warning__ ("mbstowcs called with dst buffer smaller than len " "* sizeof (wchar_t)")))
                        ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) size_t
__attribute__ ((__nothrow__)) mbstowcs (wchar_t *__restrict __dst, __const char *__restrict __src, size_t __len)

{
  if (__builtin_object_size (__dst, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__len))
 return __mbstowcs_chk (__dst, __src, __len,
          __builtin_object_size (__dst, 2 > 1) / sizeof (wchar_t));

      if (__len > __builtin_object_size (__dst, 2 > 1) / sizeof (wchar_t))
 return __mbstowcs_chk_warn (__dst, __src, __len,
         __builtin_object_size (__dst, 2 > 1) / sizeof (wchar_t));
    }
  return __mbstowcs_alias (__dst, __src, __len);
}


extern size_t __wcstombs_chk (char *__restrict __dst,
         __const wchar_t *__restrict __src,
         size_t __len, size_t __dstlen) __attribute__ ((__nothrow__));
extern size_t __wcstombs_alias (char *__restrict __dst, __const wchar_t *__restrict __src, size_t __len) __asm__ ("" "wcstombs") __attribute__ ((__nothrow__))


                                  ;
extern size_t __wcstombs_chk_warn (char *__restrict __dst, __const wchar_t *__restrict __src, size_t __len, size_t __dstlen) __asm__ ("" "__wcstombs_chk") __attribute__ ((__nothrow__))



     __attribute__((__warning__ ("wcstombs called with dst buffer smaller than len")));

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) size_t
__attribute__ ((__nothrow__)) wcstombs (char *__restrict __dst, __const wchar_t *__restrict __src, size_t __len)

{
  if (__builtin_object_size (__dst, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__len))
 return __wcstombs_chk (__dst, __src, __len, __builtin_object_size (__dst, 2 > 1));
      if (__len > __builtin_object_size (__dst, 2 > 1))
 return __wcstombs_chk_warn (__dst, __src, __len, __builtin_object_size (__dst, 2 > 1));
    }
  return __wcstombs_alias (__dst, __src, __len);
}
# 956 "/usr/include/stdlib.h" 2 3 4
# 964 "/usr/include/stdlib.h" 3 4

# 27 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/stdio.h" 1 3 4
# 30 "/usr/include/stdio.h" 3 4




# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 35 "/usr/include/stdio.h" 2 3 4
# 45 "/usr/include/stdio.h" 3 4
struct _IO_FILE;



typedef struct _IO_FILE FILE;





# 65 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 75 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 15 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 16 "/usr/include/_G_config.h" 2 3 4




# 1 "/usr/include/wchar.h" 1 3 4
# 83 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {

    unsigned int __wch;



    char __wchb[4];
  } __value;
} __mbstate_t;
# 21 "/usr/include/_G_config.h" 2 3 4

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 53 "/usr/include/_G_config.h" 3 4
typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3 4
# 53 "/usr/include/libio.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3 4
# 170 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 180 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 203 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 271 "/usr/include/libio.h" 3 4
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 319 "/usr/include/libio.h" 3 4
  __off64_t _offset;
# 328 "/usr/include/libio.h" 3 4
  void *__pad1;
  void *__pad2;
  void *__pad3;
  void *__pad4;
  size_t __pad5;

  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 364 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);




typedef __io_read_fn cookie_read_function_t;
typedef __io_write_fn cookie_write_function_t;
typedef __io_seek_fn cookie_seek_function_t;
typedef __io_close_fn cookie_close_function_t;


typedef struct
{
  __io_read_fn *read;
  __io_write_fn *write;
  __io_seek_fn *seek;
  __io_close_fn *close;
} _IO_cookie_io_functions_t;
typedef _IO_cookie_io_functions_t cookie_io_functions_t;

struct _IO_cookie_file;


extern void _IO_cookie_init (struct _IO_cookie_file *__cfile, int __read_write,
        void *__cookie, _IO_cookie_io_functions_t __fns);







extern int __underflow (_IO_FILE *);
extern int __uflow (_IO_FILE *);
extern int __overflow (_IO_FILE *, int);
# 460 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp);
extern int _IO_putc (int __c, _IO_FILE *__fp);
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));

extern int _IO_peekc_locked (_IO_FILE *__fp);





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));
# 490 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
   __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
    __gnuc_va_list);
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t);

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int);

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));
# 76 "/usr/include/stdio.h" 2 3 4




typedef __gnuc_va_list va_list;
# 109 "/usr/include/stdio.h" 3 4




typedef _G_fpos64_t fpos_t;



typedef _G_fpos64_t fpos64_t;
# 161 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio_lim.h" 1 3 4
# 162 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;







extern int remove (__const char *__filename) __attribute__ ((__nothrow__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));




extern int renameat (int __oldfd, __const char *__old, int __newfd,
       __const char *__new) __attribute__ ((__nothrow__));



# 195 "/usr/include/stdio.h" 3 4
extern FILE *tmpfile (void) __asm__ ("" "tmpfile64") __attribute__ ((__warn_unused_result__));






extern FILE *tmpfile64 (void) __attribute__ ((__warn_unused_result__));



extern char *tmpnam (char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
# 224 "/usr/include/stdio.h" 3 4
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__warn_unused_result__));








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 249 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 259 "/usr/include/stdio.h" 3 4
extern int fcloseall (void);




# 280 "/usr/include/stdio.h" 3 4
extern FILE *fopen (__const char *__restrict __filename, __const char *__restrict __modes) __asm__ ("" "fopen64")

  __attribute__ ((__warn_unused_result__));
extern FILE *freopen (__const char *__restrict __filename, __const char *__restrict __modes, FILE *__restrict __stream) __asm__ ("" "freopen64")


  __attribute__ ((__warn_unused_result__));







extern FILE *fopen64 (__const char *__restrict __filename,
        __const char *__restrict __modes) __attribute__ ((__warn_unused_result__));
extern FILE *freopen64 (__const char *__restrict __filename,
   __const char *__restrict __modes,
   FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));




extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));





extern FILE *fopencookie (void *__restrict __magic_cookie,
     __const char *__restrict __modes,
     _IO_cookie_io_functions_t __io_funcs) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern FILE *fmemopen (void *__s, size_t __len, __const char *__modes)
  __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));






extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__));








extern int fprintf (FILE *__restrict __stream,
      __const char *__restrict __format, ...);




extern int printf (__const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      __const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));





extern int snprintf (char *__restrict __s, size_t __maxlen,
       __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));






extern int vasprintf (char **__restrict __ptr, __const char *__restrict __f,
        __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0))) __attribute__ ((__warn_unused_result__));
extern int __asprintf (char **__restrict __ptr,
         __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) __attribute__ ((__warn_unused_result__));
extern int asprintf (char **__restrict __ptr,
       __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3))) __attribute__ ((__warn_unused_result__));
# 414 "/usr/include/stdio.h" 3 4
extern int vdprintf (int __fd, __const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, __const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));








extern int fscanf (FILE *__restrict __stream,
     __const char *__restrict __format, ...) __attribute__ ((__warn_unused_result__));




extern int scanf (__const char *__restrict __format, ...) __attribute__ ((__warn_unused_result__));

extern int sscanf (__const char *__restrict __s,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
# 465 "/usr/include/stdio.h" 3 4








extern int vfscanf (FILE *__restrict __s, __const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) __attribute__ ((__warn_unused_result__));





extern int vscanf (__const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) __attribute__ ((__warn_unused_result__));


extern int vsscanf (__const char *__restrict __s,
      __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
# 524 "/usr/include/stdio.h" 3 4









extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 552 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 563 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 596 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     __attribute__ ((__warn_unused_result__));






extern char *gets (char *__s) __attribute__ ((__warn_unused_result__));

# 642 "/usr/include/stdio.h" 3 4
extern char *fgets_unlocked (char *__restrict __s, int __n,
        FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
# 658 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
          size_t *__restrict __n, int __delimiter,
          FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern __ssize_t getdelim (char **__restrict __lineptr,
        size_t *__restrict __n, int __delimiter,
        FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));







extern __ssize_t getline (char **__restrict __lineptr,
       size_t *__restrict __n,
       FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));








extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);





extern int puts (__const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));




extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);

# 719 "/usr/include/stdio.h" 3 4
extern int fputs_unlocked (__const char *__restrict __s,
      FILE *__restrict __stream);
# 730 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) __attribute__ ((__warn_unused_result__));




extern void rewind (FILE *__stream);

# 774 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off64_t __off, int __whence) __asm__ ("" "fseeko64")

                  ;
extern __off64_t ftello (FILE *__stream) __asm__ ("" "ftello64");








# 799 "/usr/include/stdio.h" 3 4
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos) __asm__ ("" "fgetpos64")
                                          ;
extern int fsetpos (FILE *__stream, __const fpos_t *__pos) __asm__ ("" "fsetpos64")
                                                            ;








extern int fseeko64 (FILE *__stream, __off64_t __off, int __whence);
extern __off64_t ftello64 (FILE *__stream) __attribute__ ((__warn_unused_result__));
extern int fgetpos64 (FILE *__restrict __stream, fpos64_t *__restrict __pos);
extern int fsetpos64 (FILE *__stream, __const fpos64_t *__pos);




extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));








extern void perror (__const char *__s);






# 1 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern __const char *__const sys_errlist[];


extern int _sys_nerr;
extern __const char *__const _sys_errlist[];
# 847 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
# 866 "/usr/include/stdio.h" 3 4
extern FILE *popen (__const char *__command, __const char *__modes) __attribute__ ((__warn_unused_result__));





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__));





extern char *cuserid (char *__s);




struct obstack;


extern int obstack_printf (struct obstack *__restrict __obstack,
      __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 3)));
extern int obstack_vprintf (struct obstack *__restrict __obstack,
       __const char *__restrict __format,
       __gnuc_va_list __args)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 2, 0)));







extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__));
# 927 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio.h" 1 3 4
# 44 "/usr/include/x86_64-linux-gnu/bits/stdio.h" 3 4
extern __inline int
getchar (void)
{
  return _IO_getc (stdin);
}




extern __inline int
fgetc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}





extern __inline int
getc_unlocked (FILE *__fp)
{
  return (__builtin_expect (((__fp)->_IO_read_ptr >= (__fp)->_IO_read_end), 0) ? __uflow (__fp) : *(unsigned char *) (__fp)->_IO_read_ptr++);
}


extern __inline int
getchar_unlocked (void)
{
  return (__builtin_expect (((stdin)->_IO_read_ptr >= (stdin)->_IO_read_end), 0) ? __uflow (stdin) : *(unsigned char *) (stdin)->_IO_read_ptr++);
}




extern __inline int
putchar (int __c)
{
  return _IO_putc (__c, stdout);
}




extern __inline int
fputc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}





extern __inline int
putc_unlocked (int __c, FILE *__stream)
{
  return (__builtin_expect (((__stream)->_IO_write_ptr >= (__stream)->_IO_write_end), 0) ? __overflow (__stream, (unsigned char) (__c)) : (unsigned char) (*(__stream)->_IO_write_ptr++ = (__c)));
}


extern __inline int
putchar_unlocked (int __c)
{
  return (__builtin_expect (((stdout)->_IO_write_ptr >= (stdout)->_IO_write_end), 0) ? __overflow (stdout, (unsigned char) (__c)) : (unsigned char) (*(stdout)->_IO_write_ptr++ = (__c)));
}





extern __inline __ssize_t
getline (char **__lineptr, size_t *__n, FILE *__stream)
{
  return __getdelim (__lineptr, __n, '\n', __stream);
}





extern __inline int
__attribute__ ((__nothrow__)) feof_unlocked (FILE *__stream)
{
  return (((__stream)->_flags & 0x10) != 0);
}


extern __inline int
__attribute__ ((__nothrow__)) ferror_unlocked (FILE *__stream)
{
  return (((__stream)->_flags & 0x20) != 0);
}
# 928 "/usr/include/stdio.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/stdio2.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/stdio2.h" 3 4
extern int __sprintf_chk (char *__restrict __s, int __flag, size_t __slen,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__));
extern int __vsprintf_chk (char *__restrict __s, int __flag, size_t __slen,
      __const char *__restrict __format,
      __gnuc_va_list __ap) __attribute__ ((__nothrow__));


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) sprintf (char *__restrict __s, __const char *__restrict __fmt, ...)
{
  return __builtin___sprintf_chk (__s, 2 - 1,
      __builtin_object_size (__s, 2 > 1), __fmt, __builtin_va_arg_pack ());
}






extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) vsprintf (char *__restrict __s, __const char *__restrict __fmt, __gnuc_va_list __ap)

{
  return __builtin___vsprintf_chk (__s, 2 - 1,
       __builtin_object_size (__s, 2 > 1), __fmt, __ap);
}



extern int __snprintf_chk (char *__restrict __s, size_t __n, int __flag,
      size_t __slen, __const char *__restrict __format,
      ...) __attribute__ ((__nothrow__));
extern int __vsnprintf_chk (char *__restrict __s, size_t __n, int __flag,
       size_t __slen, __const char *__restrict __format,
       __gnuc_va_list __ap) __attribute__ ((__nothrow__));


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) snprintf (char *__restrict __s, size_t __n, __const char *__restrict __fmt, ...)

{
  return __builtin___snprintf_chk (__s, __n, 2 - 1,
       __builtin_object_size (__s, 2 > 1), __fmt, __builtin_va_arg_pack ());
}






extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) vsnprintf (char *__restrict __s, size_t __n, __const char *__restrict __fmt, __gnuc_va_list __ap)

{
  return __builtin___vsnprintf_chk (__s, __n, 2 - 1,
        __builtin_object_size (__s, 2 > 1), __fmt, __ap);
}





extern int __fprintf_chk (FILE *__restrict __stream, int __flag,
     __const char *__restrict __format, ...);
extern int __printf_chk (int __flag, __const char *__restrict __format, ...);
extern int __vfprintf_chk (FILE *__restrict __stream, int __flag,
      __const char *__restrict __format, __gnuc_va_list __ap);
extern int __vprintf_chk (int __flag, __const char *__restrict __format,
     __gnuc_va_list __ap);


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
fprintf (FILE *__restrict __stream, __const char *__restrict __fmt, ...)
{
  return __fprintf_chk (__stream, 2 - 1, __fmt,
   __builtin_va_arg_pack ());
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
printf (__const char *__restrict __fmt, ...)
{
  return __printf_chk (2 - 1, __fmt, __builtin_va_arg_pack ());
}







extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
vprintf (__const char *__restrict __fmt, __gnuc_va_list __ap)
{

  return __vfprintf_chk (stdout, 2 - 1, __fmt, __ap);



}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
vfprintf (FILE *__restrict __stream,
   __const char *__restrict __fmt, __gnuc_va_list __ap)
{
  return __vfprintf_chk (__stream, 2 - 1, __fmt, __ap);
}



extern int __asprintf_chk (char **__restrict __ptr, int __flag,
      __const char *__restrict __fmt, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4))) __attribute__ ((__warn_unused_result__));
extern int __vasprintf_chk (char **__restrict __ptr, int __flag,
       __const char *__restrict __fmt, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0))) __attribute__ ((__warn_unused_result__));
extern int __dprintf_chk (int __fd, int __flag, __const char *__restrict __fmt,
     ...) __attribute__ ((__format__ (__printf__, 3, 4)));
extern int __vdprintf_chk (int __fd, int __flag,
      __const char *__restrict __fmt, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 3, 0)));
extern int __obstack_printf_chk (struct obstack *__restrict __obstack,
     int __flag, __const char *__restrict __format,
     ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));
extern int __obstack_vprintf_chk (struct obstack *__restrict __obstack,
      int __flag,
      __const char *__restrict __format,
      __gnuc_va_list __args)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) asprintf (char **__restrict __ptr, __const char *__restrict __fmt, ...)
{
  return __asprintf_chk (__ptr, 2 - 1, __fmt,
    __builtin_va_arg_pack ());
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) __asprintf (char **__restrict __ptr, __const char *__restrict __fmt, ...)

{
  return __asprintf_chk (__ptr, 2 - 1, __fmt,
    __builtin_va_arg_pack ());
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
dprintf (int __fd, __const char *__restrict __fmt, ...)
{
  return __dprintf_chk (__fd, 2 - 1, __fmt,
   __builtin_va_arg_pack ());
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) obstack_printf (struct obstack *__restrict __obstack, __const char *__restrict __fmt, ...)

{
  return __obstack_printf_chk (__obstack, 2 - 1, __fmt,
          __builtin_va_arg_pack ());
}
# 195 "/usr/include/x86_64-linux-gnu/bits/stdio2.h" 3 4
extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) vasprintf (char **__restrict __ptr, __const char *__restrict __fmt, __gnuc_va_list __ap)

{
  return __vasprintf_chk (__ptr, 2 - 1, __fmt, __ap);
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
vdprintf (int __fd, __const char *__restrict __fmt, __gnuc_va_list __ap)
{
  return __vdprintf_chk (__fd, 2 - 1, __fmt, __ap);
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) obstack_vprintf (struct obstack *__restrict __obstack, __const char *__restrict __fmt, __gnuc_va_list __ap)

{
  return __obstack_vprintf_chk (__obstack, 2 - 1, __fmt,
    __ap);
}





extern char *__gets_chk (char *__str, size_t) __attribute__ ((__warn_unused_result__));
extern char *__gets_warn (char *__str) __asm__ ("" "gets")
     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("please use fgets or getline instead, gets can't " "specify buffer size")))
                               ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
gets (char *__str)
{
  if (__builtin_object_size (__str, 2 > 1) != (size_t) -1)
    return __gets_chk (__str, __builtin_object_size (__str, 2 > 1));
  return __gets_warn (__str);
}

extern char *__fgets_chk (char *__restrict __s, size_t __size, int __n,
     FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern char *__fgets_alias (char *__restrict __s, int __n, FILE *__restrict __stream) __asm__ ("" "fgets")

                                        __attribute__ ((__warn_unused_result__));
extern char *__fgets_chk_warn (char *__restrict __s, size_t __size, int __n, FILE *__restrict __stream) __asm__ ("" "__fgets_chk")


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("fgets called with bigger size than length " "of destination buffer")))
                                 ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
{
  if (__builtin_object_size (__s, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__n) || __n <= 0)
 return __fgets_chk (__s, __builtin_object_size (__s, 2 > 1), __n, __stream);

      if ((size_t) __n > __builtin_object_size (__s, 2 > 1))
 return __fgets_chk_warn (__s, __builtin_object_size (__s, 2 > 1), __n, __stream);
    }
  return __fgets_alias (__s, __n, __stream);
}

extern size_t __fread_chk (void *__restrict __ptr, size_t __ptrlen,
      size_t __size, size_t __n,
      FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern size_t __fread_alias (void *__restrict __ptr, size_t __size, size_t __n, FILE *__restrict __stream) __asm__ ("" "fread")


            __attribute__ ((__warn_unused_result__));
extern size_t __fread_chk_warn (void *__restrict __ptr, size_t __ptrlen, size_t __size, size_t __n, FILE *__restrict __stream) __asm__ ("" "__fread_chk")




     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("fread called with bigger size * nmemb than length " "of destination buffer")))
                                 ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) size_t
fread (void *__restrict __ptr, size_t __size, size_t __n,
       FILE *__restrict __stream)
{
  if (__builtin_object_size (__ptr, 0) != (size_t) -1)
    {
      if (!__builtin_constant_p (__size)
   || !__builtin_constant_p (__n)
   || (__size | __n) >= (((size_t) 1) << (8 * sizeof (size_t) / 2)))
 return __fread_chk (__ptr, __builtin_object_size (__ptr, 0), __size, __n, __stream);

      if (__size * __n > __builtin_object_size (__ptr, 0))
 return __fread_chk_warn (__ptr, __builtin_object_size (__ptr, 0), __size, __n, __stream);
    }
  return __fread_alias (__ptr, __size, __n, __stream);
}


extern char *__fgets_unlocked_chk (char *__restrict __s, size_t __size,
       int __n, FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern char *__fgets_unlocked_alias (char *__restrict __s, int __n, FILE *__restrict __stream) __asm__ ("" "fgets_unlocked")

                                                 __attribute__ ((__warn_unused_result__));
extern char *__fgets_unlocked_chk_warn (char *__restrict __s, size_t __size, int __n, FILE *__restrict __stream) __asm__ ("" "__fgets_unlocked_chk")


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("fgets_unlocked called with bigger size than length " "of destination buffer")))
                                 ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
fgets_unlocked (char *__restrict __s, int __n, FILE *__restrict __stream)
{
  if (__builtin_object_size (__s, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__n) || __n <= 0)
 return __fgets_unlocked_chk (__s, __builtin_object_size (__s, 2 > 1), __n, __stream);

      if ((size_t) __n > __builtin_object_size (__s, 2 > 1))
 return __fgets_unlocked_chk_warn (__s, __builtin_object_size (__s, 2 > 1), __n, __stream);
    }
  return __fgets_unlocked_alias (__s, __n, __stream);
}




extern size_t __fread_unlocked_chk (void *__restrict __ptr, size_t __ptrlen,
        size_t __size, size_t __n,
        FILE *__restrict __stream) __attribute__ ((__warn_unused_result__));
extern size_t __fread_unlocked_alias (void *__restrict __ptr, size_t __size, size_t __n, FILE *__restrict __stream) __asm__ ("" "fread_unlocked")


                     __attribute__ ((__warn_unused_result__));
extern size_t __fread_unlocked_chk_warn (void *__restrict __ptr, size_t __ptrlen, size_t __size, size_t __n, FILE *__restrict __stream) __asm__ ("" "__fread_unlocked_chk")




     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("fread_unlocked called with bigger size * nmemb than " "length of destination buffer")))
                                        ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) size_t
fread_unlocked (void *__restrict __ptr, size_t __size, size_t __n,
  FILE *__restrict __stream)
{
  if (__builtin_object_size (__ptr, 0) != (size_t) -1)
    {
      if (!__builtin_constant_p (__size)
   || !__builtin_constant_p (__n)
   || (__size | __n) >= (((size_t) 1) << (8 * sizeof (size_t) / 2)))
 return __fread_unlocked_chk (__ptr, __builtin_object_size (__ptr, 0), __size, __n,
         __stream);

      if (__size * __n > __builtin_object_size (__ptr, 0))
 return __fread_unlocked_chk_warn (__ptr, __builtin_object_size (__ptr, 0), __size, __n,
       __stream);
    }


  if (__builtin_constant_p (__size)
      && __builtin_constant_p (__n)
      && (__size | __n) < (((size_t) 1) << (8 * sizeof (size_t) / 2))
      && __size * __n <= 8)
    {
      size_t __cnt = __size * __n;
      char *__cptr = (char *) __ptr;
      if (__cnt == 0)
 return 0;

      for (; __cnt > 0; --__cnt)
 {
   int __c = (__builtin_expect (((__stream)->_IO_read_ptr >= (__stream)->_IO_read_end), 0) ? __uflow (__stream) : *(unsigned char *) (__stream)->_IO_read_ptr++);
   if (__c == (-1))
     break;
   *__cptr++ = __c;
 }
      return (__cptr - (char *) __ptr) / __size;
    }

  return __fread_unlocked_alias (__ptr, __size, __n, __stream);
}
# 931 "/usr/include/stdio.h" 2 3 4






# 28 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdarg.h" 1 3 4
# 29 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdbool.h" 1 3 4
# 30 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/string.h" 1 3 4
# 29 "/usr/include/string.h" 3 4





# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 35 "/usr/include/string.h" 2 3 4









extern void *memcpy (void *__restrict __dest,
       __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern void *memmove (void *__dest, __const void *__src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));






extern void *memccpy (void *__restrict __dest, __const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));





extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int memcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 95 "/usr/include/string.h" 3 4
extern void *memchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 109 "/usr/include/string.h" 3 4
extern void *rawmemchr (__const void *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 120 "/usr/include/string.h" 3 4
extern void *memrchr (__const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));






extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncpy (char *__restrict __dest,
        __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strcat (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncat (char *__restrict __dest, __const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int strncmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcoll (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strxfrm (char *__restrict __dest,
         __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));

# 165 "/usr/include/string.h" 3 4
extern int strcoll_l (__const char *__s1, __const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));

extern size_t strxfrm_l (char *__dest, __const char *__src, size_t __n,
    __locale_t __l) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));





extern char *strdup (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));






extern char *strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
# 210 "/usr/include/string.h" 3 4

# 235 "/usr/include/string.h" 3 4
extern char *strchr (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 262 "/usr/include/string.h" 3 4
extern char *strrchr (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 276 "/usr/include/string.h" 3 4
extern char *strchrnul (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));






extern size_t strcspn (__const char *__s, __const char *__reject)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern size_t strspn (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 314 "/usr/include/string.h" 3 4
extern char *strpbrk (__const char *__s, __const char *__accept)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 342 "/usr/include/string.h" 3 4
extern char *strstr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strtok (char *__restrict __s, __const char *__restrict __delim)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));




extern char *__strtok_r (char *__restrict __s,
    __const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));

extern char *strtok_r (char *__restrict __s, __const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
# 373 "/usr/include/string.h" 3 4
extern char *strcasestr (__const char *__haystack, __const char *__needle)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));







extern void *memmem (__const void *__haystack, size_t __haystacklen,
       __const void *__needle, size_t __needlelen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 3)));



extern void *__mempcpy (void *__restrict __dest,
   __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern void *mempcpy (void *__restrict __dest,
        __const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));





extern size_t strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern size_t strnlen (__const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern char *strerror (int __errnum) __attribute__ ((__nothrow__));

# 438 "/usr/include/string.h" 3 4
extern char *strerror_r (int __errnum, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));





extern char *strerror_l (int __errnum, __locale_t __l) __attribute__ ((__nothrow__));





extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern void bcopy (__const void *__src, void *__dest, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int bcmp (__const void *__s1, __const void *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 489 "/usr/include/string.h" 3 4
extern char *index (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 517 "/usr/include/string.h" 3 4
extern char *rindex (__const char *__s, int __c)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern int ffs (int __i) __attribute__ ((__nothrow__)) __attribute__ ((__const__));




extern int ffsl (long int __l) __attribute__ ((__nothrow__)) __attribute__ ((__const__));

__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));




extern int strcasecmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strncasecmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));





extern int strcasecmp_l (__const char *__s1, __const char *__s2,
    __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));

extern int strncasecmp_l (__const char *__s1, __const char *__s2,
     size_t __n, __locale_t __loc)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 4)));





extern char *strsep (char **__restrict __stringp,
       __const char *__restrict __delim)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strsignal (int __sig) __attribute__ ((__nothrow__));


extern char *__stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, __const char *__restrict __src)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern char *__stpncpy (char *__restrict __dest,
   __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        __const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int strverscmp (__const char *__s1, __const char *__s2)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strfry (char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern void *memfrob (void *__s, size_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 606 "/usr/include/string.h" 3 4
extern char *basename (__const char *__filename) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 634 "/usr/include/string.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/string.h" 1 3 4
# 635 "/usr/include/string.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/string2.h" 1 3 4
# 394 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern void *__rawmemchr (const void *__s, int __c);
# 969 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern __inline size_t __strcspn_c1 (__const char *__s, int __reject);
extern __inline size_t
__strcspn_c1 (__const char *__s, int __reject)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject)
    ++__result;
  return __result;
}

extern __inline size_t __strcspn_c2 (__const char *__s, int __reject1,
         int __reject2);
extern __inline size_t
__strcspn_c2 (__const char *__s, int __reject1, int __reject2)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject1
  && __s[__result] != __reject2)
    ++__result;
  return __result;
}

extern __inline size_t __strcspn_c3 (__const char *__s, int __reject1,
         int __reject2, int __reject3);
extern __inline size_t
__strcspn_c3 (__const char *__s, int __reject1, int __reject2,
       int __reject3)
{
  register size_t __result = 0;
  while (__s[__result] != '\0' && __s[__result] != __reject1
  && __s[__result] != __reject2 && __s[__result] != __reject3)
    ++__result;
  return __result;
}
# 1045 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern __inline size_t __strspn_c1 (__const char *__s, int __accept);
extern __inline size_t
__strspn_c1 (__const char *__s, int __accept)
{
  register size_t __result = 0;

  while (__s[__result] == __accept)
    ++__result;
  return __result;
}

extern __inline size_t __strspn_c2 (__const char *__s, int __accept1,
        int __accept2);
extern __inline size_t
__strspn_c2 (__const char *__s, int __accept1, int __accept2)
{
  register size_t __result = 0;

  while (__s[__result] == __accept1 || __s[__result] == __accept2)
    ++__result;
  return __result;
}

extern __inline size_t __strspn_c3 (__const char *__s, int __accept1,
        int __accept2, int __accept3);
extern __inline size_t
__strspn_c3 (__const char *__s, int __accept1, int __accept2, int __accept3)
{
  register size_t __result = 0;

  while (__s[__result] == __accept1 || __s[__result] == __accept2
  || __s[__result] == __accept3)
    ++__result;
  return __result;
}
# 1121 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern __inline char *__strpbrk_c2 (__const char *__s, int __accept1,
         int __accept2);
extern __inline char *
__strpbrk_c2 (__const char *__s, int __accept1, int __accept2)
{

  while (*__s != '\0' && *__s != __accept1 && *__s != __accept2)
    ++__s;
  return *__s == '\0' ? ((void *)0) : (char *) (size_t) __s;
}

extern __inline char *__strpbrk_c3 (__const char *__s, int __accept1,
         int __accept2, int __accept3);
extern __inline char *
__strpbrk_c3 (__const char *__s, int __accept1, int __accept2,
       int __accept3)
{

  while (*__s != '\0' && *__s != __accept1 && *__s != __accept2
  && *__s != __accept3)
    ++__s;
  return *__s == '\0' ? ((void *)0) : (char *) (size_t) __s;
}
# 1172 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern __inline char *__strtok_r_1c (char *__s, char __sep, char **__nextp);
extern __inline char *
__strtok_r_1c (char *__s, char __sep, char **__nextp)
{
  char *__result;
  if (__s == ((void *)0))
    __s = *__nextp;
  while (*__s == __sep)
    ++__s;
  __result = ((void *)0);
  if (*__s != '\0')
    {
      __result = __s++;
      while (*__s != '\0')
 if (*__s++ == __sep)
   {
     __s[-1] = '\0';
     break;
   }
    }
  *__nextp = __s;
  return __result;
}
# 1204 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern char *__strsep_g (char **__stringp, __const char *__delim);
# 1222 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern __inline char *__strsep_1c (char **__s, char __reject);
extern __inline char *
__strsep_1c (char **__s, char __reject)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0) && (*__s = (__extension__ (__builtin_constant_p (__reject) && !__builtin_constant_p (__retval) && (__reject) == '\0' ? (char *) __rawmemchr (__retval, __reject) : __builtin_strchr (__retval, __reject)))) != ((void *)0))
    *(*__s)++ = '\0';
  return __retval;
}

extern __inline char *__strsep_2c (char **__s, char __reject1, char __reject2);
extern __inline char *
__strsep_2c (char **__s, char __reject1, char __reject2)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0))
    {
      register char *__cp = __retval;
      while (1)
 {
   if (*__cp == '\0')
     {
       __cp = ((void *)0);
   break;
     }
   if (*__cp == __reject1 || *__cp == __reject2)
     {
       *__cp++ = '\0';
       break;
     }
   ++__cp;
 }
      *__s = __cp;
    }
  return __retval;
}

extern __inline char *__strsep_3c (char **__s, char __reject1, char __reject2,
       char __reject3);
extern __inline char *
__strsep_3c (char **__s, char __reject1, char __reject2, char __reject3)
{
  register char *__retval = *__s;
  if (__retval != ((void *)0))
    {
      register char *__cp = __retval;
      while (1)
 {
   if (*__cp == '\0')
     {
       __cp = ((void *)0);
   break;
     }
   if (*__cp == __reject1 || *__cp == __reject2 || *__cp == __reject3)
     {
       *__cp++ = '\0';
       break;
     }
   ++__cp;
 }
      *__s = __cp;
    }
  return __retval;
}
# 1303 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern char *__strdup (__const char *__string) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
# 1322 "/usr/include/x86_64-linux-gnu/bits/string2.h" 3 4
extern char *__strndup (__const char *__string, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));
# 638 "/usr/include/string.h" 2 3 4




# 1 "/usr/include/x86_64-linux-gnu/bits/string3.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/string3.h" 3 4
extern void __warn_memset_zero_len (void) __attribute__((__warning__ ("memset used with constant zero length parameter; this could be due to transposed parameters")))
                                                                                                   ;
# 48 "/usr/include/x86_64-linux-gnu/bits/string3.h" 3 4
extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void *
__attribute__ ((__nothrow__)) memcpy (void *__restrict __dest, __const void *__restrict __src, size_t __len)

{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void *
__attribute__ ((__nothrow__)) memmove (void *__dest, __const void *__src, size_t __len)
{
  return __builtin___memmove_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void *
__attribute__ ((__nothrow__)) mempcpy (void *__restrict __dest, __const void *__restrict __src, size_t __len)

{
  return __builtin___mempcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}
# 76 "/usr/include/x86_64-linux-gnu/bits/string3.h" 3 4
extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void *
__attribute__ ((__nothrow__)) memset (void *__dest, int __ch, size_t __len)
{
  if (__builtin_constant_p (__len) && __len == 0
      && (!__builtin_constant_p (__ch) || __ch != 0))
    {
      __warn_memset_zero_len ();
      return __dest;
    }
  return __builtin___memset_chk (__dest, __ch, __len, __builtin_object_size (__dest, 0));
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void
__attribute__ ((__nothrow__)) bcopy (__const void *__src, void *__dest, size_t __len)
{
  (void) __builtin___memmove_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) void
__attribute__ ((__nothrow__)) bzero (void *__dest, size_t __len)
{
  (void) __builtin___memset_chk (__dest, '\0', __len, __builtin_object_size (__dest, 0));
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) strcpy (char *__restrict __dest, __const char *__restrict __src)
{
  return __builtin___strcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) stpcpy (char *__restrict __dest, __const char *__restrict __src)
{
  return __builtin___stpcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}



extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) strncpy (char *__restrict __dest, __const char *__restrict __src, size_t __len)

{
  return __builtin___strncpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}


extern char *__stpncpy_chk (char *__dest, __const char *__src, size_t __n,
       size_t __destlen) __attribute__ ((__nothrow__));
extern char *__stpncpy_alias (char *__dest, __const char *__src, size_t __n) __asm__ ("" "stpncpy") __attribute__ ((__nothrow__))

                                 ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) stpncpy (char *__dest, __const char *__src, size_t __n)
{
  if (__builtin_object_size (__dest, 2 > 1) != (size_t) -1
      && (!__builtin_constant_p (__n) || __n <= __builtin_object_size (__dest, 2 > 1)))
    return __stpncpy_chk (__dest, __src, __n, __builtin_object_size (__dest, 2 > 1));
  return __stpncpy_alias (__dest, __src, __n);
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) strcat (char *__restrict __dest, __const char *__restrict __src)
{
  return __builtin___strcat_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}


extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) char *
__attribute__ ((__nothrow__)) strncat (char *__restrict __dest, __const char *__restrict __src, size_t __len)

{
  return __builtin___strncat_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}
# 643 "/usr/include/string.h" 2 3 4




# 31 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/strings.h" 1 3 4
# 32 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/inttypes.h" 1 3 4
# 28 "/usr/include/inttypes.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h" 1 3 4


# 1 "/usr/include/stdint.h" 1 3 4
# 27 "/usr/include/stdint.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wchar.h" 1 3 4
# 28 "/usr/include/stdint.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/stdint.h" 2 3 4
# 49 "/usr/include/stdint.h" 3 4
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;

typedef unsigned int uint32_t;



typedef unsigned long int uint64_t;
# 66 "/usr/include/stdint.h" 3 4
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;

typedef long int int_least64_t;






typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;

typedef unsigned long int uint_least64_t;
# 91 "/usr/include/stdint.h" 3 4
typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 104 "/usr/include/stdint.h" 3 4
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 120 "/usr/include/stdint.h" 3 4
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 135 "/usr/include/stdint.h" 3 4
typedef long int intmax_t;
typedef unsigned long int uintmax_t;
# 4 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stdint.h" 2 3 4
# 29 "/usr/include/inttypes.h" 2 3 4






typedef int __gwchar_t;
# 274 "/usr/include/inttypes.h" 3 4





typedef struct
  {
    long int quot;
    long int rem;
  } imaxdiv_t;
# 298 "/usr/include/inttypes.h" 3 4
extern intmax_t imaxabs (intmax_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern imaxdiv_t imaxdiv (intmax_t __numer, intmax_t __denom)
      __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern intmax_t strtoimax (__const char *__restrict __nptr,
      char **__restrict __endptr, int __base) __attribute__ ((__nothrow__));


extern uintmax_t strtoumax (__const char *__restrict __nptr,
       char ** __restrict __endptr, int __base) __attribute__ ((__nothrow__));


extern intmax_t wcstoimax (__const __gwchar_t *__restrict __nptr,
      __gwchar_t **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__));


extern uintmax_t wcstoumax (__const __gwchar_t *__restrict __nptr,
       __gwchar_t ** __restrict __endptr, int __base)
     __attribute__ ((__nothrow__));





extern long int __strtol_internal (__const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, int __group)
  __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern __inline intmax_t
__attribute__ ((__nothrow__)) strtoimax (__const char *__restrict nptr, char **__restrict endptr, int base)

{
  return __strtol_internal (nptr, endptr, base, 0);
}

extern unsigned long int __strtoul_internal (__const char *
          __restrict __nptr,
          char ** __restrict __endptr,
          int __base, int __group)
  __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern __inline uintmax_t
__attribute__ ((__nothrow__)) strtoumax (__const char *__restrict nptr, char **__restrict endptr, int base)

{
  return __strtoul_internal (nptr, endptr, base, 0);
}

extern long int __wcstol_internal (__const __gwchar_t * __restrict __nptr,
       __gwchar_t **__restrict __endptr,
       int __base, int __group)
  __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern __inline intmax_t
__attribute__ ((__nothrow__)) wcstoimax (__const __gwchar_t *__restrict nptr, __gwchar_t **__restrict endptr, int base)

{
  return __wcstol_internal (nptr, endptr, base, 0);
}

extern unsigned long int __wcstoul_internal (__const __gwchar_t *
          __restrict __nptr,
          __gwchar_t **
          __restrict __endptr,
          int __base, int __group)
  __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));

extern __inline uintmax_t
__attribute__ ((__nothrow__)) wcstoumax (__const __gwchar_t *__restrict nptr, __gwchar_t **__restrict endptr, int base)

{
  return __wcstoul_internal (nptr, endptr, base, 0);
}
# 442 "/usr/include/inttypes.h" 3 4

# 33 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 1 3 4
# 34 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h" 1 3 4






# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 1 3 4
# 169 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 3 4
# 1 "/usr/include/limits.h" 1 3 4
# 145 "/usr/include/limits.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 1 3 4
# 157 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 1 3 4
# 39 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 3 4
# 1 "/usr/include/linux/limits.h" 1 3 4
# 40 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 2 3 4
# 158 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 2 3 4
# 146 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/posix2_lim.h" 1 3 4
# 150 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/xopen_lim.h" 1 3 4
# 34 "/usr/include/x86_64-linux-gnu/bits/xopen_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio_lim.h" 1 3 4
# 35 "/usr/include/x86_64-linux-gnu/bits/xopen_lim.h" 2 3 4
# 154 "/usr/include/limits.h" 2 3 4
# 170 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 2 3 4
# 8 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/syslimits.h" 2 3 4
# 35 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 2 3 4
# 34 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/time.h" 1 3 4
# 30 "/usr/include/time.h" 3 4








# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 39 "/usr/include/time.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/time.h" 1 3 4
# 43 "/usr/include/time.h" 2 3 4
# 131 "/usr/include/time.h" 3 4


struct tm
{
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;


  long int tm_gmtoff;
  __const char *tm_zone;




};








struct itimerspec
  {
    struct timespec it_interval;
    struct timespec it_value;
  };


struct sigevent;
# 180 "/usr/include/time.h" 3 4



extern clock_t clock (void) __attribute__ ((__nothrow__));


extern time_t time (time_t *__timer) __attribute__ ((__nothrow__));


extern double difftime (time_t __time1, time_t __time0)
     __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern time_t mktime (struct tm *__tp) __attribute__ ((__nothrow__));





extern size_t strftime (char *__restrict __s, size_t __maxsize,
   __const char *__restrict __format,
   __const struct tm *__restrict __tp) __attribute__ ((__nothrow__));





extern char *strptime (__const char *__restrict __s,
         __const char *__restrict __fmt, struct tm *__tp)
     __attribute__ ((__nothrow__));







extern size_t strftime_l (char *__restrict __s, size_t __maxsize,
     __const char *__restrict __format,
     __const struct tm *__restrict __tp,
     __locale_t __loc) __attribute__ ((__nothrow__));



extern char *strptime_l (__const char *__restrict __s,
    __const char *__restrict __fmt, struct tm *__tp,
    __locale_t __loc) __attribute__ ((__nothrow__));






extern struct tm *gmtime (__const time_t *__timer) __attribute__ ((__nothrow__));



extern struct tm *localtime (__const time_t *__timer) __attribute__ ((__nothrow__));





extern struct tm *gmtime_r (__const time_t *__restrict __timer,
       struct tm *__restrict __tp) __attribute__ ((__nothrow__));



extern struct tm *localtime_r (__const time_t *__restrict __timer,
          struct tm *__restrict __tp) __attribute__ ((__nothrow__));





extern char *asctime (__const struct tm *__tp) __attribute__ ((__nothrow__));


extern char *ctime (__const time_t *__timer) __attribute__ ((__nothrow__));







extern char *asctime_r (__const struct tm *__restrict __tp,
   char *__restrict __buf) __attribute__ ((__nothrow__));


extern char *ctime_r (__const time_t *__restrict __timer,
        char *__restrict __buf) __attribute__ ((__nothrow__));




extern char *__tzname[2];
extern int __daylight;
extern long int __timezone;




extern char *tzname[2];



extern void tzset (void) __attribute__ ((__nothrow__));



extern int daylight;
extern long int timezone;





extern int stime (__const time_t *__when) __attribute__ ((__nothrow__));
# 313 "/usr/include/time.h" 3 4
extern time_t timegm (struct tm *__tp) __attribute__ ((__nothrow__));


extern time_t timelocal (struct tm *__tp) __attribute__ ((__nothrow__));


extern int dysize (int __year) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
# 328 "/usr/include/time.h" 3 4
extern int nanosleep (__const struct timespec *__requested_time,
        struct timespec *__remaining);



extern int clock_getres (clockid_t __clock_id, struct timespec *__res) __attribute__ ((__nothrow__));


extern int clock_gettime (clockid_t __clock_id, struct timespec *__tp) __attribute__ ((__nothrow__));


extern int clock_settime (clockid_t __clock_id, __const struct timespec *__tp)
     __attribute__ ((__nothrow__));






extern int clock_nanosleep (clockid_t __clock_id, int __flags,
       __const struct timespec *__req,
       struct timespec *__rem);


extern int clock_getcpuclockid (pid_t __pid, clockid_t *__clock_id) __attribute__ ((__nothrow__));




extern int timer_create (clockid_t __clock_id,
    struct sigevent *__restrict __evp,
    timer_t *__restrict __timerid) __attribute__ ((__nothrow__));


extern int timer_delete (timer_t __timerid) __attribute__ ((__nothrow__));


extern int timer_settime (timer_t __timerid, int __flags,
     __const struct itimerspec *__restrict __value,
     struct itimerspec *__restrict __ovalue) __attribute__ ((__nothrow__));


extern int timer_gettime (timer_t __timerid, struct itimerspec *__value)
     __attribute__ ((__nothrow__));


extern int timer_getoverrun (timer_t __timerid) __attribute__ ((__nothrow__));
# 390 "/usr/include/time.h" 3 4
extern int getdate_err;
# 399 "/usr/include/time.h" 3 4
extern struct tm *getdate (__const char *__string);
# 413 "/usr/include/time.h" 3 4
extern int getdate_r (__const char *__restrict __string,
        struct tm *__restrict __resbufp);



# 35 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/ctype.h" 1 3 4
# 30 "/usr/include/ctype.h" 3 4

# 48 "/usr/include/ctype.h" 3 4
enum
{
  _ISupper = ((0) < 8 ? ((1 << (0)) << 8) : ((1 << (0)) >> 8)),
  _ISlower = ((1) < 8 ? ((1 << (1)) << 8) : ((1 << (1)) >> 8)),
  _ISalpha = ((2) < 8 ? ((1 << (2)) << 8) : ((1 << (2)) >> 8)),
  _ISdigit = ((3) < 8 ? ((1 << (3)) << 8) : ((1 << (3)) >> 8)),
  _ISxdigit = ((4) < 8 ? ((1 << (4)) << 8) : ((1 << (4)) >> 8)),
  _ISspace = ((5) < 8 ? ((1 << (5)) << 8) : ((1 << (5)) >> 8)),
  _ISprint = ((6) < 8 ? ((1 << (6)) << 8) : ((1 << (6)) >> 8)),
  _ISgraph = ((7) < 8 ? ((1 << (7)) << 8) : ((1 << (7)) >> 8)),
  _ISblank = ((8) < 8 ? ((1 << (8)) << 8) : ((1 << (8)) >> 8)),
  _IScntrl = ((9) < 8 ? ((1 << (9)) << 8) : ((1 << (9)) >> 8)),
  _ISpunct = ((10) < 8 ? ((1 << (10)) << 8) : ((1 << (10)) >> 8)),
  _ISalnum = ((11) < 8 ? ((1 << (11)) << 8) : ((1 << (11)) >> 8))
};
# 81 "/usr/include/ctype.h" 3 4
extern __const unsigned short int **__ctype_b_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
extern __const __int32_t **__ctype_tolower_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
extern __const __int32_t **__ctype_toupper_loc (void)
     __attribute__ ((__nothrow__)) __attribute__ ((__const));
# 96 "/usr/include/ctype.h" 3 4






extern int isalnum (int) __attribute__ ((__nothrow__));
extern int isalpha (int) __attribute__ ((__nothrow__));
extern int iscntrl (int) __attribute__ ((__nothrow__));
extern int isdigit (int) __attribute__ ((__nothrow__));
extern int islower (int) __attribute__ ((__nothrow__));
extern int isgraph (int) __attribute__ ((__nothrow__));
extern int isprint (int) __attribute__ ((__nothrow__));
extern int ispunct (int) __attribute__ ((__nothrow__));
extern int isspace (int) __attribute__ ((__nothrow__));
extern int isupper (int) __attribute__ ((__nothrow__));
extern int isxdigit (int) __attribute__ ((__nothrow__));



extern int tolower (int __c) __attribute__ ((__nothrow__));


extern int toupper (int __c) __attribute__ ((__nothrow__));








extern int isblank (int) __attribute__ ((__nothrow__));






extern int isctype (int __c, int __mask) __attribute__ ((__nothrow__));






extern int isascii (int __c) __attribute__ ((__nothrow__));



extern int toascii (int __c) __attribute__ ((__nothrow__));



extern int _toupper (int) __attribute__ ((__nothrow__));
extern int _tolower (int) __attribute__ ((__nothrow__));
# 190 "/usr/include/ctype.h" 3 4
extern __inline int
__attribute__ ((__nothrow__)) tolower (int __c)
{
  return __c >= -128 && __c < 256 ? (*__ctype_tolower_loc ())[__c] : __c;
}

extern __inline int
__attribute__ ((__nothrow__)) toupper (int __c)
{
  return __c >= -128 && __c < 256 ? (*__ctype_toupper_loc ())[__c] : __c;
}
# 247 "/usr/include/ctype.h" 3 4
extern int isalnum_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isalpha_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int iscntrl_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isdigit_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int islower_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isgraph_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isprint_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int ispunct_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isspace_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isupper_l (int, __locale_t) __attribute__ ((__nothrow__));
extern int isxdigit_l (int, __locale_t) __attribute__ ((__nothrow__));

extern int isblank_l (int, __locale_t) __attribute__ ((__nothrow__));



extern int __tolower_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int tolower_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));


extern int __toupper_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
extern int toupper_l (int __c, __locale_t __l) __attribute__ ((__nothrow__));
# 323 "/usr/include/ctype.h" 3 4

# 36 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/errno.h" 1 3 4
# 32 "/usr/include/errno.h" 3 4




# 1 "/usr/include/x86_64-linux-gnu/bits/errno.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/errno.h" 3 4
# 1 "/usr/include/linux/errno.h" 1 3 4



# 1 "/usr/include/x86_64-linux-gnu/asm/errno.h" 1 3 4
# 1 "/usr/include/asm-generic/errno.h" 1 3 4



# 1 "/usr/include/asm-generic/errno-base.h" 1 3 4
# 5 "/usr/include/asm-generic/errno.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/asm/errno.h" 2 3 4
# 5 "/usr/include/linux/errno.h" 2 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/errno.h" 2 3 4
# 47 "/usr/include/x86_64-linux-gnu/bits/errno.h" 3 4
extern int *__errno_location (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));
# 37 "/usr/include/errno.h" 2 3 4
# 55 "/usr/include/errno.h" 3 4
extern char *program_invocation_name, *program_invocation_short_name;




# 69 "/usr/include/errno.h" 3 4
typedef int error_t;
# 37 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/unistd.h" 1 3 4
# 28 "/usr/include/unistd.h" 3 4

# 203 "/usr/include/unistd.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/posix_opt.h" 1 3 4
# 204 "/usr/include/unistd.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/environments.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/environments.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/environments.h" 2 3 4
# 208 "/usr/include/unistd.h" 2 3 4
# 227 "/usr/include/unistd.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 228 "/usr/include/unistd.h" 2 3 4
# 275 "/usr/include/unistd.h" 3 4
typedef __socklen_t socklen_t;
# 288 "/usr/include/unistd.h" 3 4
extern int access (__const char *__name, int __type) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




extern int euidaccess (__const char *__name, int __type)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int eaccess (__const char *__name, int __type)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int faccessat (int __fd, __const char *__file, int __type, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2))) __attribute__ ((__warn_unused_result__));
# 334 "/usr/include/unistd.h" 3 4
extern __off64_t lseek (int __fd, __off64_t __offset, int __whence) __asm__ ("" "lseek64") __attribute__ ((__nothrow__))

             ;





extern __off64_t lseek64 (int __fd, __off64_t __offset, int __whence)
     __attribute__ ((__nothrow__));






extern int close (int __fd);






extern ssize_t read (int __fd, void *__buf, size_t __nbytes) __attribute__ ((__warn_unused_result__));





extern ssize_t write (int __fd, __const void *__buf, size_t __n) __attribute__ ((__warn_unused_result__));
# 385 "/usr/include/unistd.h" 3 4
extern ssize_t pread (int __fd, void *__buf, size_t __nbytes, __off64_t __offset) __asm__ ("" "pread64")

               __attribute__ ((__warn_unused_result__));
extern ssize_t pwrite (int __fd, __const void *__buf, size_t __nbytes, __off64_t __offset) __asm__ ("" "pwrite64")

                __attribute__ ((__warn_unused_result__));
# 401 "/usr/include/unistd.h" 3 4
extern ssize_t pread64 (int __fd, void *__buf, size_t __nbytes,
   __off64_t __offset) __attribute__ ((__warn_unused_result__));


extern ssize_t pwrite64 (int __fd, __const void *__buf, size_t __n,
    __off64_t __offset) __attribute__ ((__warn_unused_result__));







extern int pipe (int __pipedes[2]) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern int pipe2 (int __pipedes[2], int __flags) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
# 429 "/usr/include/unistd.h" 3 4
extern unsigned int alarm (unsigned int __seconds) __attribute__ ((__nothrow__));
# 441 "/usr/include/unistd.h" 3 4
extern unsigned int sleep (unsigned int __seconds);







extern __useconds_t ualarm (__useconds_t __value, __useconds_t __interval)
     __attribute__ ((__nothrow__));






extern int usleep (__useconds_t __useconds);
# 466 "/usr/include/unistd.h" 3 4
extern int pause (void);



extern int chown (__const char *__file, __uid_t __owner, __gid_t __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));



extern int fchown (int __fd, __uid_t __owner, __gid_t __group) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern int lchown (__const char *__file, __uid_t __owner, __gid_t __group)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));






extern int fchownat (int __fd, __const char *__file, __uid_t __owner,
       __gid_t __group, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2))) __attribute__ ((__warn_unused_result__));



extern int chdir (__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));



extern int fchdir (int __fd) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
# 508 "/usr/include/unistd.h" 3 4
extern char *getcwd (char *__buf, size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));





extern char *get_current_dir_name (void) __attribute__ ((__nothrow__));







extern char *getwd (char *__buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__deprecated__)) __attribute__ ((__warn_unused_result__));




extern int dup (int __fd) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));


extern int dup2 (int __fd, int __fd2) __attribute__ ((__nothrow__));




extern int dup3 (int __fd, int __fd2, int __flags) __attribute__ ((__nothrow__));



extern char **__environ;

extern char **environ;





extern int execve (__const char *__path, char *__const __argv[],
     char *__const __envp[]) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int fexecve (int __fd, char *__const __argv[], char *__const __envp[])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));




extern int execv (__const char *__path, char *__const __argv[])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int execle (__const char *__path, __const char *__arg, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int execl (__const char *__path, __const char *__arg, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int execvp (__const char *__file, char *__const __argv[])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int execlp (__const char *__file, __const char *__arg, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int execvpe (__const char *__file, char *__const __argv[],
      char *__const __envp[])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));





extern int nice (int __inc) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));




extern void _exit (int __status) __attribute__ ((__noreturn__));





# 1 "/usr/include/x86_64-linux-gnu/bits/confname.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/confname.h" 3 4
enum
  {
    _PC_LINK_MAX,

    _PC_MAX_CANON,

    _PC_MAX_INPUT,

    _PC_NAME_MAX,

    _PC_PATH_MAX,

    _PC_PIPE_BUF,

    _PC_CHOWN_RESTRICTED,

    _PC_NO_TRUNC,

    _PC_VDISABLE,

    _PC_SYNC_IO,

    _PC_ASYNC_IO,

    _PC_PRIO_IO,

    _PC_SOCK_MAXBUF,

    _PC_FILESIZEBITS,

    _PC_REC_INCR_XFER_SIZE,

    _PC_REC_MAX_XFER_SIZE,

    _PC_REC_MIN_XFER_SIZE,

    _PC_REC_XFER_ALIGN,

    _PC_ALLOC_SIZE_MIN,

    _PC_SYMLINK_MAX,

    _PC_2_SYMLINKS

  };


enum
  {
    _SC_ARG_MAX,

    _SC_CHILD_MAX,

    _SC_CLK_TCK,

    _SC_NGROUPS_MAX,

    _SC_OPEN_MAX,

    _SC_STREAM_MAX,

    _SC_TZNAME_MAX,

    _SC_JOB_CONTROL,

    _SC_SAVED_IDS,

    _SC_REALTIME_SIGNALS,

    _SC_PRIORITY_SCHEDULING,

    _SC_TIMERS,

    _SC_ASYNCHRONOUS_IO,

    _SC_PRIORITIZED_IO,

    _SC_SYNCHRONIZED_IO,

    _SC_FSYNC,

    _SC_MAPPED_FILES,

    _SC_MEMLOCK,

    _SC_MEMLOCK_RANGE,

    _SC_MEMORY_PROTECTION,

    _SC_MESSAGE_PASSING,

    _SC_SEMAPHORES,

    _SC_SHARED_MEMORY_OBJECTS,

    _SC_AIO_LISTIO_MAX,

    _SC_AIO_MAX,

    _SC_AIO_PRIO_DELTA_MAX,

    _SC_DELAYTIMER_MAX,

    _SC_MQ_OPEN_MAX,

    _SC_MQ_PRIO_MAX,

    _SC_VERSION,

    _SC_PAGESIZE,


    _SC_RTSIG_MAX,

    _SC_SEM_NSEMS_MAX,

    _SC_SEM_VALUE_MAX,

    _SC_SIGQUEUE_MAX,

    _SC_TIMER_MAX,




    _SC_BC_BASE_MAX,

    _SC_BC_DIM_MAX,

    _SC_BC_SCALE_MAX,

    _SC_BC_STRING_MAX,

    _SC_COLL_WEIGHTS_MAX,

    _SC_EQUIV_CLASS_MAX,

    _SC_EXPR_NEST_MAX,

    _SC_LINE_MAX,

    _SC_RE_DUP_MAX,

    _SC_CHARCLASS_NAME_MAX,


    _SC_2_VERSION,

    _SC_2_C_BIND,

    _SC_2_C_DEV,

    _SC_2_FORT_DEV,

    _SC_2_FORT_RUN,

    _SC_2_SW_DEV,

    _SC_2_LOCALEDEF,


    _SC_PII,

    _SC_PII_XTI,

    _SC_PII_SOCKET,

    _SC_PII_INTERNET,

    _SC_PII_OSI,

    _SC_POLL,

    _SC_SELECT,

    _SC_UIO_MAXIOV,

    _SC_IOV_MAX = _SC_UIO_MAXIOV,

    _SC_PII_INTERNET_STREAM,

    _SC_PII_INTERNET_DGRAM,

    _SC_PII_OSI_COTS,

    _SC_PII_OSI_CLTS,

    _SC_PII_OSI_M,

    _SC_T_IOV_MAX,



    _SC_THREADS,

    _SC_THREAD_SAFE_FUNCTIONS,

    _SC_GETGR_R_SIZE_MAX,

    _SC_GETPW_R_SIZE_MAX,

    _SC_LOGIN_NAME_MAX,

    _SC_TTY_NAME_MAX,

    _SC_THREAD_DESTRUCTOR_ITERATIONS,

    _SC_THREAD_KEYS_MAX,

    _SC_THREAD_STACK_MIN,

    _SC_THREAD_THREADS_MAX,

    _SC_THREAD_ATTR_STACKADDR,

    _SC_THREAD_ATTR_STACKSIZE,

    _SC_THREAD_PRIORITY_SCHEDULING,

    _SC_THREAD_PRIO_INHERIT,

    _SC_THREAD_PRIO_PROTECT,

    _SC_THREAD_PROCESS_SHARED,


    _SC_NPROCESSORS_CONF,

    _SC_NPROCESSORS_ONLN,

    _SC_PHYS_PAGES,

    _SC_AVPHYS_PAGES,

    _SC_ATEXIT_MAX,

    _SC_PASS_MAX,


    _SC_XOPEN_VERSION,

    _SC_XOPEN_XCU_VERSION,

    _SC_XOPEN_UNIX,

    _SC_XOPEN_CRYPT,

    _SC_XOPEN_ENH_I18N,

    _SC_XOPEN_SHM,


    _SC_2_CHAR_TERM,

    _SC_2_C_VERSION,

    _SC_2_UPE,


    _SC_XOPEN_XPG2,

    _SC_XOPEN_XPG3,

    _SC_XOPEN_XPG4,


    _SC_CHAR_BIT,

    _SC_CHAR_MAX,

    _SC_CHAR_MIN,

    _SC_INT_MAX,

    _SC_INT_MIN,

    _SC_LONG_BIT,

    _SC_WORD_BIT,

    _SC_MB_LEN_MAX,

    _SC_NZERO,

    _SC_SSIZE_MAX,

    _SC_SCHAR_MAX,

    _SC_SCHAR_MIN,

    _SC_SHRT_MAX,

    _SC_SHRT_MIN,

    _SC_UCHAR_MAX,

    _SC_UINT_MAX,

    _SC_ULONG_MAX,

    _SC_USHRT_MAX,


    _SC_NL_ARGMAX,

    _SC_NL_LANGMAX,

    _SC_NL_MSGMAX,

    _SC_NL_NMAX,

    _SC_NL_SETMAX,

    _SC_NL_TEXTMAX,


    _SC_XBS5_ILP32_OFF32,

    _SC_XBS5_ILP32_OFFBIG,

    _SC_XBS5_LP64_OFF64,

    _SC_XBS5_LPBIG_OFFBIG,


    _SC_XOPEN_LEGACY,

    _SC_XOPEN_REALTIME,

    _SC_XOPEN_REALTIME_THREADS,


    _SC_ADVISORY_INFO,

    _SC_BARRIERS,

    _SC_BASE,

    _SC_C_LANG_SUPPORT,

    _SC_C_LANG_SUPPORT_R,

    _SC_CLOCK_SELECTION,

    _SC_CPUTIME,

    _SC_THREAD_CPUTIME,

    _SC_DEVICE_IO,

    _SC_DEVICE_SPECIFIC,

    _SC_DEVICE_SPECIFIC_R,

    _SC_FD_MGMT,

    _SC_FIFO,

    _SC_PIPE,

    _SC_FILE_ATTRIBUTES,

    _SC_FILE_LOCKING,

    _SC_FILE_SYSTEM,

    _SC_MONOTONIC_CLOCK,

    _SC_MULTI_PROCESS,

    _SC_SINGLE_PROCESS,

    _SC_NETWORKING,

    _SC_READER_WRITER_LOCKS,

    _SC_SPIN_LOCKS,

    _SC_REGEXP,

    _SC_REGEX_VERSION,

    _SC_SHELL,

    _SC_SIGNALS,

    _SC_SPAWN,

    _SC_SPORADIC_SERVER,

    _SC_THREAD_SPORADIC_SERVER,

    _SC_SYSTEM_DATABASE,

    _SC_SYSTEM_DATABASE_R,

    _SC_TIMEOUTS,

    _SC_TYPED_MEMORY_OBJECTS,

    _SC_USER_GROUPS,

    _SC_USER_GROUPS_R,

    _SC_2_PBS,

    _SC_2_PBS_ACCOUNTING,

    _SC_2_PBS_LOCATE,

    _SC_2_PBS_MESSAGE,

    _SC_2_PBS_TRACK,

    _SC_SYMLOOP_MAX,

    _SC_STREAMS,

    _SC_2_PBS_CHECKPOINT,


    _SC_V6_ILP32_OFF32,

    _SC_V6_ILP32_OFFBIG,

    _SC_V6_LP64_OFF64,

    _SC_V6_LPBIG_OFFBIG,


    _SC_HOST_NAME_MAX,

    _SC_TRACE,

    _SC_TRACE_EVENT_FILTER,

    _SC_TRACE_INHERIT,

    _SC_TRACE_LOG,


    _SC_LEVEL1_ICACHE_SIZE,

    _SC_LEVEL1_ICACHE_ASSOC,

    _SC_LEVEL1_ICACHE_LINESIZE,

    _SC_LEVEL1_DCACHE_SIZE,

    _SC_LEVEL1_DCACHE_ASSOC,

    _SC_LEVEL1_DCACHE_LINESIZE,

    _SC_LEVEL2_CACHE_SIZE,

    _SC_LEVEL2_CACHE_ASSOC,

    _SC_LEVEL2_CACHE_LINESIZE,

    _SC_LEVEL3_CACHE_SIZE,

    _SC_LEVEL3_CACHE_ASSOC,

    _SC_LEVEL3_CACHE_LINESIZE,

    _SC_LEVEL4_CACHE_SIZE,

    _SC_LEVEL4_CACHE_ASSOC,

    _SC_LEVEL4_CACHE_LINESIZE,



    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,

    _SC_RAW_SOCKETS,


    _SC_V7_ILP32_OFF32,

    _SC_V7_ILP32_OFFBIG,

    _SC_V7_LP64_OFF64,

    _SC_V7_LPBIG_OFFBIG,


    _SC_SS_REPL_MAX,


    _SC_TRACE_EVENT_NAME_MAX,

    _SC_TRACE_NAME_MAX,

    _SC_TRACE_SYS_MAX,

    _SC_TRACE_USER_EVENT_MAX,


    _SC_XOPEN_STREAMS,


    _SC_THREAD_ROBUST_PRIO_INHERIT,

    _SC_THREAD_ROBUST_PRIO_PROTECT

  };


enum
  {
    _CS_PATH,


    _CS_V6_WIDTH_RESTRICTED_ENVS,



    _CS_GNU_LIBC_VERSION,

    _CS_GNU_LIBPTHREAD_VERSION,


    _CS_V5_WIDTH_RESTRICTED_ENVS,



    _CS_V7_WIDTH_RESTRICTED_ENVS,



    _CS_LFS_CFLAGS = 1000,

    _CS_LFS_LDFLAGS,

    _CS_LFS_LIBS,

    _CS_LFS_LINTFLAGS,

    _CS_LFS64_CFLAGS,

    _CS_LFS64_LDFLAGS,

    _CS_LFS64_LIBS,

    _CS_LFS64_LINTFLAGS,


    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,

    _CS_XBS5_ILP32_OFF32_LDFLAGS,

    _CS_XBS5_ILP32_OFF32_LIBS,

    _CS_XBS5_ILP32_OFF32_LINTFLAGS,

    _CS_XBS5_ILP32_OFFBIG_CFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LDFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LIBS,

    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,

    _CS_XBS5_LP64_OFF64_CFLAGS,

    _CS_XBS5_LP64_OFF64_LDFLAGS,

    _CS_XBS5_LP64_OFF64_LIBS,

    _CS_XBS5_LP64_OFF64_LINTFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_CFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LIBS,

    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V6_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LIBS,

    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V6_LP64_OFF64_CFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LIBS,

    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V7_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LIBS,

    _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V7_LP64_OFF64_CFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LIBS,

    _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,


    _CS_V6_ENV,

    _CS_V7_ENV

  };
# 607 "/usr/include/unistd.h" 2 3 4


extern long int pathconf (__const char *__path, int __name)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern long int fpathconf (int __fd, int __name) __attribute__ ((__nothrow__));


extern long int sysconf (int __name) __attribute__ ((__nothrow__));



extern size_t confstr (int __name, char *__buf, size_t __len) __attribute__ ((__nothrow__));




extern __pid_t getpid (void) __attribute__ ((__nothrow__));


extern __pid_t getppid (void) __attribute__ ((__nothrow__));




extern __pid_t getpgrp (void) __attribute__ ((__nothrow__));
# 643 "/usr/include/unistd.h" 3 4
extern __pid_t __getpgid (__pid_t __pid) __attribute__ ((__nothrow__));

extern __pid_t getpgid (__pid_t __pid) __attribute__ ((__nothrow__));






extern int setpgid (__pid_t __pid, __pid_t __pgid) __attribute__ ((__nothrow__));
# 669 "/usr/include/unistd.h" 3 4
extern int setpgrp (void) __attribute__ ((__nothrow__));
# 686 "/usr/include/unistd.h" 3 4
extern __pid_t setsid (void) __attribute__ ((__nothrow__));



extern __pid_t getsid (__pid_t __pid) __attribute__ ((__nothrow__));



extern __uid_t getuid (void) __attribute__ ((__nothrow__));


extern __uid_t geteuid (void) __attribute__ ((__nothrow__));


extern __gid_t getgid (void) __attribute__ ((__nothrow__));


extern __gid_t getegid (void) __attribute__ ((__nothrow__));




extern int getgroups (int __size, __gid_t __list[]) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));



extern int group_member (__gid_t __gid) __attribute__ ((__nothrow__));






extern int setuid (__uid_t __uid) __attribute__ ((__nothrow__));




extern int setreuid (__uid_t __ruid, __uid_t __euid) __attribute__ ((__nothrow__));




extern int seteuid (__uid_t __uid) __attribute__ ((__nothrow__));






extern int setgid (__gid_t __gid) __attribute__ ((__nothrow__));




extern int setregid (__gid_t __rgid, __gid_t __egid) __attribute__ ((__nothrow__));




extern int setegid (__gid_t __gid) __attribute__ ((__nothrow__));





extern int getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid)
     __attribute__ ((__nothrow__));



extern int getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid)
     __attribute__ ((__nothrow__));



extern int setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid)
     __attribute__ ((__nothrow__));



extern int setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid)
     __attribute__ ((__nothrow__));






extern __pid_t fork (void) __attribute__ ((__nothrow__));







extern __pid_t vfork (void) __attribute__ ((__nothrow__));





extern char *ttyname (int __fd) __attribute__ ((__nothrow__));



extern int ttyname_r (int __fd, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2))) __attribute__ ((__warn_unused_result__));



extern int isatty (int __fd) __attribute__ ((__nothrow__));





extern int ttyslot (void) __attribute__ ((__nothrow__));




extern int link (__const char *__from, __const char *__to)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__));




extern int linkat (int __fromfd, __const char *__from, int __tofd,
     __const char *__to, int __flags)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4))) __attribute__ ((__warn_unused_result__));




extern int symlink (__const char *__from, __const char *__to)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__));




extern ssize_t readlink (__const char *__restrict __path,
    char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__));




extern int symlinkat (__const char *__from, int __tofd,
        __const char *__to) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3))) __attribute__ ((__warn_unused_result__));


extern ssize_t readlinkat (int __fd, __const char *__restrict __path,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3))) __attribute__ ((__warn_unused_result__));



extern int unlink (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int unlinkat (int __fd, __const char *__name, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));



extern int rmdir (__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern __pid_t tcgetpgrp (int __fd) __attribute__ ((__nothrow__));


extern int tcsetpgrp (int __fd, __pid_t __pgrp_id) __attribute__ ((__nothrow__));






extern char *getlogin (void);







extern int getlogin_r (char *__name, size_t __name_len) __attribute__ ((__nonnull__ (1)));




extern int setlogin (__const char *__name) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 890 "/usr/include/unistd.h" 3 4
# 1 "/usr/include/getopt.h" 1 3 4
# 59 "/usr/include/getopt.h" 3 4
extern char *optarg;
# 73 "/usr/include/getopt.h" 3 4
extern int optind;




extern int opterr;



extern int optopt;
# 152 "/usr/include/getopt.h" 3 4
extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
       __attribute__ ((__nothrow__));
# 891 "/usr/include/unistd.h" 2 3 4







extern int gethostname (char *__name, size_t __len) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int sethostname (__const char *__name, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));



extern int sethostid (long int __id) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));





extern int getdomainname (char *__name, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern int setdomainname (__const char *__name, size_t __len)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int vhangup (void) __attribute__ ((__nothrow__));


extern int revoke (__const char *__file) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));







extern int profil (unsigned short int *__sample_buffer, size_t __size,
     size_t __offset, unsigned int __scale)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int acct (__const char *__name) __attribute__ ((__nothrow__));



extern char *getusershell (void) __attribute__ ((__nothrow__));
extern void endusershell (void) __attribute__ ((__nothrow__));
extern void setusershell (void) __attribute__ ((__nothrow__));





extern int daemon (int __nochdir, int __noclose) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));






extern int chroot (__const char *__path) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));



extern char *getpass (__const char *__prompt) __attribute__ ((__nonnull__ (1)));
# 976 "/usr/include/unistd.h" 3 4
extern int fsync (int __fd);






extern long int gethostid (void);


extern void sync (void) __attribute__ ((__nothrow__));





extern int getpagesize (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));




extern int getdtablesize (void) __attribute__ ((__nothrow__));
# 1011 "/usr/include/unistd.h" 3 4
extern int truncate (__const char *__file, __off64_t __length) __asm__ ("" "truncate64") __attribute__ ((__nothrow__))

                  __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));





extern int truncate64 (__const char *__file, __off64_t __length)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
# 1032 "/usr/include/unistd.h" 3 4
extern int ftruncate (int __fd, __off64_t __length) __asm__ ("" "ftruncate64") __attribute__ ((__nothrow__))
                   __attribute__ ((__warn_unused_result__));





extern int ftruncate64 (int __fd, __off64_t __length) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
# 1050 "/usr/include/unistd.h" 3 4
extern int brk (void *__addr) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));





extern void *sbrk (intptr_t __delta) __attribute__ ((__nothrow__));
# 1071 "/usr/include/unistd.h" 3 4
extern long int syscall (long int __sysno, ...) __attribute__ ((__nothrow__));
# 1097 "/usr/include/unistd.h" 3 4
extern int lockf (int __fd, int __cmd, __off64_t __len) __asm__ ("" "lockf64")
                  __attribute__ ((__warn_unused_result__));





extern int lockf64 (int __fd, int __cmd, __off64_t __len) __attribute__ ((__warn_unused_result__));
# 1125 "/usr/include/unistd.h" 3 4
extern int fdatasync (int __fildes);







extern char *crypt (__const char *__key, __const char *__salt)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern void encrypt (char *__libc_block, int __edflag) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern void swab (__const void *__restrict __from, void *__restrict __to,
    ssize_t __n) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));







extern char *ctermid (char *__s) __attribute__ ((__nothrow__));





# 1 "/usr/include/x86_64-linux-gnu/bits/unistd.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/unistd.h" 3 4
extern ssize_t __read_chk (int __fd, void *__buf, size_t __nbytes,
      size_t __buflen) __attribute__ ((__warn_unused_result__));
extern ssize_t __read_alias (int __fd, void *__buf, size_t __nbytes) __asm__ ("" "read")
                               __attribute__ ((__warn_unused_result__));
extern ssize_t __read_chk_warn (int __fd, void *__buf, size_t __nbytes, size_t __buflen) __asm__ ("" "__read_chk")


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("read called with bigger length than size of " "the destination buffer")))
                                  ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) ssize_t
read (int __fd, void *__buf, size_t __nbytes)
{
  if (__builtin_object_size (__buf, 0) != (size_t) -1)
    {
      if (!__builtin_constant_p (__nbytes))
 return __read_chk (__fd, __buf, __nbytes, __builtin_object_size (__buf, 0));

      if (__nbytes > __builtin_object_size (__buf, 0))
 return __read_chk_warn (__fd, __buf, __nbytes, __builtin_object_size (__buf, 0));
    }
  return __read_alias (__fd, __buf, __nbytes);
}


extern ssize_t __pread_chk (int __fd, void *__buf, size_t __nbytes,
       __off_t __offset, size_t __bufsize) __attribute__ ((__warn_unused_result__));
extern ssize_t __pread64_chk (int __fd, void *__buf, size_t __nbytes,
         __off64_t __offset, size_t __bufsize) __attribute__ ((__warn_unused_result__));
extern ssize_t __pread_alias (int __fd, void *__buf, size_t __nbytes, __off_t __offset) __asm__ ("" "pread")

                                 __attribute__ ((__warn_unused_result__));
extern ssize_t __pread64_alias (int __fd, void *__buf, size_t __nbytes, __off64_t __offset) __asm__ ("" "pread64")

                                     __attribute__ ((__warn_unused_result__));
extern ssize_t __pread_chk_warn (int __fd, void *__buf, size_t __nbytes, __off_t __offset, size_t __bufsize) __asm__ ("" "__pread_chk")


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("pread called with bigger length than size of " "the destination buffer")))
                                  ;
extern ssize_t __pread64_chk_warn (int __fd, void *__buf, size_t __nbytes, __off64_t __offset, size_t __bufsize) __asm__ ("" "__pread64_chk")



     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("pread64 called with bigger length than size of " "the destination buffer")))
                                  ;
# 87 "/usr/include/x86_64-linux-gnu/bits/unistd.h" 3 4
extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) ssize_t
pread (int __fd, void *__buf, size_t __nbytes, __off64_t __offset)
{
  if (__builtin_object_size (__buf, 0) != (size_t) -1)
    {
      if (!__builtin_constant_p (__nbytes))
 return __pread64_chk (__fd, __buf, __nbytes, __offset, __builtin_object_size (__buf, 0));

      if ( __nbytes > __builtin_object_size (__buf, 0))
 return __pread64_chk_warn (__fd, __buf, __nbytes, __offset,
       __builtin_object_size (__buf, 0));
    }

  return __pread64_alias (__fd, __buf, __nbytes, __offset);
}



extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) ssize_t
pread64 (int __fd, void *__buf, size_t __nbytes, __off64_t __offset)
{
  if (__builtin_object_size (__buf, 0) != (size_t) -1)
    {
      if (!__builtin_constant_p (__nbytes))
 return __pread64_chk (__fd, __buf, __nbytes, __offset, __builtin_object_size (__buf, 0));

      if ( __nbytes > __builtin_object_size (__buf, 0))
 return __pread64_chk_warn (__fd, __buf, __nbytes, __offset,
       __builtin_object_size (__buf, 0));
    }

  return __pread64_alias (__fd, __buf, __nbytes, __offset);
}




extern ssize_t __readlink_chk (__const char *__restrict __path,
          char *__restrict __buf, size_t __len,
          size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__));
extern ssize_t __readlink_alias (__const char *__restrict __path, char *__restrict __buf, size_t __len) __asm__ ("" "readlink") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__));
extern ssize_t __readlink_chk_warn (__const char *__restrict __path, char *__restrict __buf, size_t __len, size_t __buflen) __asm__ ("" "__readlink_chk") __attribute__ ((__nothrow__))



     __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("readlink called with bigger length " "than size of destination buffer")))
                                         ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__warn_unused_result__)) ssize_t
__attribute__ ((__nothrow__)) readlink (__const char *__restrict __path, char *__restrict __buf, size_t __len)

{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__len))
 return __readlink_chk (__path, __buf, __len, __builtin_object_size (__buf, 2 > 1));

      if ( __len > __builtin_object_size (__buf, 2 > 1))
 return __readlink_chk_warn (__path, __buf, __len, __builtin_object_size (__buf, 2 > 1));
    }
  return __readlink_alias (__path, __buf, __len);
}



extern ssize_t __readlinkat_chk (int __fd, __const char *__restrict __path,
     char *__restrict __buf, size_t __len,
     size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3))) __attribute__ ((__warn_unused_result__));
extern ssize_t __readlinkat_alias (int __fd, __const char *__restrict __path, char *__restrict __buf, size_t __len) __asm__ ("" "readlinkat") __attribute__ ((__nothrow__))



     __attribute__ ((__nonnull__ (2, 3))) __attribute__ ((__warn_unused_result__));
extern ssize_t __readlinkat_chk_warn (int __fd, __const char *__restrict __path, char *__restrict __buf, size_t __len, size_t __buflen) __asm__ ("" "__readlinkat_chk") __attribute__ ((__nothrow__))



     __attribute__ ((__nonnull__ (2, 3))) __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("readlinkat called with bigger " "length than size of destination " "buffer")))

                ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__nonnull__ (2, 3))) __attribute__ ((__warn_unused_result__)) ssize_t
__attribute__ ((__nothrow__)) readlinkat (int __fd, __const char *__restrict __path, char *__restrict __buf, size_t __len)

{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__len))
 return __readlinkat_chk (__fd, __path, __buf, __len, __builtin_object_size (__buf, 2 > 1));

      if (__len > __builtin_object_size (__buf, 2 > 1))
 return __readlinkat_chk_warn (__fd, __path, __buf, __len,
          __builtin_object_size (__buf, 2 > 1));
    }
  return __readlinkat_alias (__fd, __path, __buf, __len);
}


extern char *__getcwd_chk (char *__buf, size_t __size, size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern char *__getcwd_alias (char *__buf, size_t __size) __asm__ ("" "getcwd") __attribute__ ((__nothrow__))
                                              __attribute__ ((__warn_unused_result__));
extern char *__getcwd_chk_warn (char *__buf, size_t __size, size_t __buflen) __asm__ ("" "__getcwd_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("getcwd caller with bigger length than size of " "destination buffer")))
                              ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__warn_unused_result__)) char *
__attribute__ ((__nothrow__)) getcwd (char *__buf, size_t __size)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__size))
 return __getcwd_chk (__buf, __size, __builtin_object_size (__buf, 2 > 1));

      if (__size > __builtin_object_size (__buf, 2 > 1))
 return __getcwd_chk_warn (__buf, __size, __builtin_object_size (__buf, 2 > 1));
    }
  return __getcwd_alias (__buf, __size);
}


extern char *__getwd_chk (char *__buf, size_t buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern char *__getwd_warn (char *__buf) __asm__ ("" "getwd") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("please use getcwd instead, as getwd " "doesn't specify buffer size")))
                                         ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__deprecated__)) __attribute__ ((__warn_unused_result__)) char *
__attribute__ ((__nothrow__)) getwd (char *__buf)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    return __getwd_chk (__buf, __builtin_object_size (__buf, 2 > 1));
  return __getwd_warn (__buf);
}


extern size_t __confstr_chk (int __name, char *__buf, size_t __len,
        size_t __buflen) __attribute__ ((__nothrow__));
extern size_t __confstr_alias (int __name, char *__buf, size_t __len) __asm__ ("" "confstr") __attribute__ ((__nothrow__))
                             ;
extern size_t __confstr_chk_warn (int __name, char *__buf, size_t __len, size_t __buflen) __asm__ ("" "__confstr_chk") __attribute__ ((__nothrow__))


     __attribute__((__warning__ ("confstr called with bigger length than size of destination " "buffer")))
            ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) size_t
__attribute__ ((__nothrow__)) confstr (int __name, char *__buf, size_t __len)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__len))
 return __confstr_chk (__name, __buf, __len, __builtin_object_size (__buf, 2 > 1));

      if (__builtin_object_size (__buf, 2 > 1) < __len)
 return __confstr_chk_warn (__name, __buf, __len, __builtin_object_size (__buf, 2 > 1));
    }
  return __confstr_alias (__name, __buf, __len);
}


extern int __getgroups_chk (int __size, __gid_t __list[], size_t __listlen)
     __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern int __getgroups_alias (int __size, __gid_t __list[]) __asm__ ("" "getgroups") __attribute__ ((__nothrow__))
                 __attribute__ ((__warn_unused_result__));
extern int __getgroups_chk_warn (int __size, __gid_t __list[], size_t __listlen) __asm__ ("" "__getgroups_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("getgroups called with bigger group count than what " "can fit into destination buffer")))
                                           ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) getgroups (int __size, __gid_t __list[])
{
  if (__builtin_object_size (__list, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__size) || __size < 0)
 return __getgroups_chk (__size, __list, __builtin_object_size (__list, 2 > 1));

      if (__size * sizeof (__gid_t) > __builtin_object_size (__list, 2 > 1))
 return __getgroups_chk_warn (__size, __list, __builtin_object_size (__list, 2 > 1));
    }
  return __getgroups_alias (__size, __list);
}


extern int __ttyname_r_chk (int __fd, char *__buf, size_t __buflen,
       size_t __nreal) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
extern int __ttyname_r_alias (int __fd, char *__buf, size_t __buflen) __asm__ ("" "ttyname_r") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (2)));
extern int __ttyname_r_chk_warn (int __fd, char *__buf, size_t __buflen, size_t __nreal) __asm__ ("" "__ttyname_r_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (2))) __attribute__((__warning__ ("ttyname_r called with bigger buflen than " "size of destination buffer")))
                                  ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) ttyname_r (int __fd, char *__buf, size_t __buflen)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__buflen))
 return __ttyname_r_chk (__fd, __buf, __buflen, __builtin_object_size (__buf, 2 > 1));

      if (__buflen > __builtin_object_size (__buf, 2 > 1))
 return __ttyname_r_chk_warn (__fd, __buf, __buflen, __builtin_object_size (__buf, 2 > 1));
    }
  return __ttyname_r_alias (__fd, __buf, __buflen);
}



extern int __getlogin_r_chk (char *__buf, size_t __buflen, size_t __nreal)
     __attribute__ ((__nonnull__ (1)));
extern int __getlogin_r_alias (char *__buf, size_t __buflen) __asm__ ("" "getlogin_r")
                     __attribute__ ((__nonnull__ (1)));
extern int __getlogin_r_chk_warn (char *__buf, size_t __buflen, size_t __nreal) __asm__ ("" "__getlogin_r_chk")


     __attribute__ ((__nonnull__ (1))) __attribute__((__warning__ ("getlogin_r called with bigger buflen than " "size of destination buffer")))
                                  ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
getlogin_r (char *__buf, size_t __buflen)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__buflen))
 return __getlogin_r_chk (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));

      if (__buflen > __builtin_object_size (__buf, 2 > 1))
 return __getlogin_r_chk_warn (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));
    }
  return __getlogin_r_alias (__buf, __buflen);
}




extern int __gethostname_chk (char *__buf, size_t __buflen, size_t __nreal)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
extern int __gethostname_alias (char *__buf, size_t __buflen) __asm__ ("" "gethostname") __attribute__ ((__nothrow__))
                   __attribute__ ((__nonnull__ (1)));
extern int __gethostname_chk_warn (char *__buf, size_t __buflen, size_t __nreal) __asm__ ("" "__gethostname_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (1))) __attribute__((__warning__ ("gethostname called with bigger buflen than " "size of destination buffer")))
                                  ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) gethostname (char *__buf, size_t __buflen)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__buflen))
 return __gethostname_chk (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));

      if (__buflen > __builtin_object_size (__buf, 2 > 1))
 return __gethostname_chk_warn (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));
    }
  return __gethostname_alias (__buf, __buflen);
}




extern int __getdomainname_chk (char *__buf, size_t __buflen, size_t __nreal)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern int __getdomainname_alias (char *__buf, size_t __buflen) __asm__ ("" "getdomainname") __attribute__ ((__nothrow__))

                     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__));
extern int __getdomainname_chk_warn (char *__buf, size_t __buflen, size_t __nreal) __asm__ ("" "__getdomainname_chk") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (1))) __attribute__ ((__warn_unused_result__)) __attribute__((__warning__ ("getdomainname called with bigger " "buflen than size of destination " "buffer")))

                    ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__)) getdomainname (char *__buf, size_t __buflen)
{
  if (__builtin_object_size (__buf, 2 > 1) != (size_t) -1)
    {
      if (!__builtin_constant_p (__buflen))
 return __getdomainname_chk (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));

      if (__buflen > __builtin_object_size (__buf, 2 > 1))
 return __getdomainname_chk_warn (__buf, __buflen, __builtin_object_size (__buf, 2 > 1));
    }
  return __getdomainname_alias (__buf, __buflen);
}
# 1161 "/usr/include/unistd.h" 2 3 4



# 38 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/fcntl.h" 1 3 4
# 30 "/usr/include/fcntl.h" 3 4




# 1 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/uio.h" 1 3 4
# 44 "/usr/include/x86_64-linux-gnu/bits/uio.h" 3 4
struct iovec
  {
    void *iov_base;
    size_t iov_len;
  };
# 29 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 2 3 4
# 167 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 3 4
struct flock
  {
    short int l_type;
    short int l_whence;




    __off64_t l_start;
    __off64_t l_len;

    __pid_t l_pid;
  };


struct flock64
  {
    short int l_type;
    short int l_whence;
    __off64_t l_start;
    __off64_t l_len;
    __pid_t l_pid;
  };




enum __pid_type
  {
    F_OWNER_TID = 0,
    F_OWNER_PID,
    F_OWNER_PGRP,
    F_OWNER_GID = F_OWNER_PGRP
  };


struct f_owner_ex
  {
    enum __pid_type type;
    __pid_t pid;
  };
# 267 "/usr/include/x86_64-linux-gnu/bits/fcntl.h" 3 4





extern ssize_t readahead (int __fd, __off64_t __offset, size_t __count)
    __attribute__ ((__nothrow__));



extern int sync_file_range (int __fd, __off64_t __offset, __off64_t __count,
       unsigned int __flags);



extern ssize_t vmsplice (int __fdout, const struct iovec *__iov,
    size_t __count, unsigned int __flags);


extern ssize_t splice (int __fdin, __off64_t *__offin, int __fdout,
         __off64_t *__offout, size_t __len,
         unsigned int __flags);


extern ssize_t tee (int __fdin, int __fdout, size_t __len,
      unsigned int __flags);






extern int fallocate (int __fd, int __mode, __off64_t __offset, __off64_t __len) __asm__ ("" "fallocate64")

                     ;





extern int fallocate64 (int __fd, int __mode, __off64_t __offset,
   __off64_t __len);




# 35 "/usr/include/fcntl.h" 2 3 4






# 1 "/usr/include/x86_64-linux-gnu/bits/stat.h" 1 3 4
# 46 "/usr/include/x86_64-linux-gnu/bits/stat.h" 3 4
struct stat
  {
    __dev_t st_dev;




    __ino_t st_ino;







    __nlink_t st_nlink;
    __mode_t st_mode;

    __uid_t st_uid;
    __gid_t st_gid;

    int __pad0;

    __dev_t st_rdev;




    __off_t st_size;



    __blksize_t st_blksize;

    __blkcnt_t st_blocks;
# 91 "/usr/include/x86_64-linux-gnu/bits/stat.h" 3 4
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 106 "/usr/include/x86_64-linux-gnu/bits/stat.h" 3 4
    long int __unused[3];
# 115 "/usr/include/x86_64-linux-gnu/bits/stat.h" 3 4
  };



struct stat64
  {
    __dev_t st_dev;

    __ino64_t st_ino;
    __nlink_t st_nlink;
    __mode_t st_mode;






    __uid_t st_uid;
    __gid_t st_gid;

    int __pad0;
    __dev_t st_rdev;
    __off_t st_size;





    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;







    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 167 "/usr/include/x86_64-linux-gnu/bits/stat.h" 3 4
    long int __unused[3];



  };
# 42 "/usr/include/fcntl.h" 2 3 4
# 110 "/usr/include/fcntl.h" 3 4
extern int fcntl (int __fd, int __cmd, ...);
# 122 "/usr/include/fcntl.h" 3 4
extern int open (__const char *__file, int __oflag, ...) __asm__ ("" "open64")
     __attribute__ ((__nonnull__ (1)));





extern int open64 (__const char *__file, int __oflag, ...) __attribute__ ((__nonnull__ (1)));
# 147 "/usr/include/fcntl.h" 3 4
extern int openat (int __fd, __const char *__file, int __oflag, ...) __asm__ ("" "openat64")
                    __attribute__ ((__nonnull__ (2)));





extern int openat64 (int __fd, __const char *__file, int __oflag, ...)
     __attribute__ ((__nonnull__ (2)));
# 168 "/usr/include/fcntl.h" 3 4
extern int creat (__const char *__file, __mode_t __mode) __asm__ ("" "creat64")
                  __attribute__ ((__nonnull__ (1)));





extern int creat64 (__const char *__file, __mode_t __mode) __attribute__ ((__nonnull__ (1)));
# 215 "/usr/include/fcntl.h" 3 4
extern int posix_fadvise (int __fd, __off64_t __offset, __off64_t __len, int __advise) __asm__ ("" "posix_fadvise64") __attribute__ ((__nothrow__))

                      ;





extern int posix_fadvise64 (int __fd, __off64_t __offset, __off64_t __len,
       int __advise) __attribute__ ((__nothrow__));
# 236 "/usr/include/fcntl.h" 3 4
extern int posix_fallocate (int __fd, __off64_t __offset, __off64_t __len) __asm__ ("" "posix_fallocate64")

                           ;





extern int posix_fallocate64 (int __fd, __off64_t __offset, __off64_t __len);







# 1 "/usr/include/x86_64-linux-gnu/bits/fcntl2.h" 1 3 4
# 31 "/usr/include/x86_64-linux-gnu/bits/fcntl2.h" 3 4
extern int __open_2 (__const char *__path, int __oflag) __asm__ ("" "__open64_2")
                     __attribute__ ((__nonnull__ (1)));
extern int __open_alias (__const char *__path, int __oflag, ...) __asm__ ("" "open64")
                 __attribute__ ((__nonnull__ (1)));

extern void __open_too_many_args (void) __attribute__((__error__ ("open can be called either with 2 or 3 arguments, not more")))
                                                                  ;
extern void __open_missing_mode (void) __attribute__((__error__ ("open with O_CREAT in second argument needs 3 arguments")))
                                                               ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
open (__const char *__path, int __oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    __open_too_many_args ();

  if (__builtin_constant_p (__oflag))
    {
      if ((__oflag & 0100) != 0 && __builtin_va_arg_pack_len () < 1)
 {
   __open_missing_mode ();
   return __open_2 (__path, __oflag);
 }
      return __open_alias (__path, __oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return __open_2 (__path, __oflag);

  return __open_alias (__path, __oflag, __builtin_va_arg_pack ());
}



extern int __open64_2 (__const char *__path, int __oflag) __attribute__ ((__nonnull__ (1)));
extern int __open64_alias (__const char *__path, int __oflag, ...) __asm__ ("" "open64")
                   __attribute__ ((__nonnull__ (1)));
extern void __open64_too_many_args (void) __attribute__((__error__ ("open64 can be called either with 2 or 3 arguments, not more")))
                                                                    ;
extern void __open64_missing_mode (void) __attribute__((__error__ ("open64 with O_CREAT in second argument needs 3 arguments")))
                                                                 ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
open64 (__const char *__path, int __oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    __open64_too_many_args ();

  if (__builtin_constant_p (__oflag))
    {
      if ((__oflag & 0100) != 0 && __builtin_va_arg_pack_len () < 1)
 {
   __open64_missing_mode ();
   return __open64_2 (__path, __oflag);
 }
      return __open64_alias (__path, __oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return __open64_2 (__path, __oflag);

  return __open64_alias (__path, __oflag, __builtin_va_arg_pack ());
}
# 105 "/usr/include/x86_64-linux-gnu/bits/fcntl2.h" 3 4
extern int __openat_2 (int __fd, __const char *__path, int __oflag) __asm__ ("" "__openat64_2")

     __attribute__ ((__nonnull__ (2)));
extern int __openat_alias (int __fd, __const char *__path, int __oflag, ...) __asm__ ("" "openat64")

     __attribute__ ((__nonnull__ (2)));

extern void __openat_too_many_args (void) __attribute__((__error__ ("openat can be called either with 3 or 4 arguments, not more")))
                                                                    ;
extern void __openat_missing_mode (void) __attribute__((__error__ ("openat with O_CREAT in third argument needs 4 arguments")))
                                                                ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
openat (int __fd, __const char *__path, int __oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    __openat_too_many_args ();

  if (__builtin_constant_p (__oflag))
    {
      if ((__oflag & 0100) != 0 && __builtin_va_arg_pack_len () < 1)
 {
   __openat_missing_mode ();
   return __openat_2 (__fd, __path, __oflag);
 }
      return __openat_alias (__fd, __path, __oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return __openat_2 (__fd, __path, __oflag);

  return __openat_alias (__fd, __path, __oflag, __builtin_va_arg_pack ());
}



extern int __openat64_2 (int __fd, __const char *__path, int __oflag)
     __attribute__ ((__nonnull__ (2)));
extern int __openat64_alias (int __fd, __const char *__path, int __oflag, ...) __asm__ ("" "openat64")

     __attribute__ ((__nonnull__ (2)));
extern void __openat64_too_many_args (void) __attribute__((__error__ ("openat64 can be called either with 3 or 4 arguments, not more")))
                                                                      ;
extern void __openat64_missing_mode (void) __attribute__((__error__ ("openat64 with O_CREAT in third argument needs 4 arguments")))
                                                                  ;

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__artificial__)) int
openat64 (int __fd, __const char *__path, int __oflag, ...)
{
  if (__builtin_va_arg_pack_len () > 1)
    __openat64_too_many_args ();

  if (__builtin_constant_p (__oflag))
    {
      if ((__oflag & 0100) != 0 && __builtin_va_arg_pack_len () < 1)
 {
   __openat64_missing_mode ();
   return __openat64_2 (__fd, __path, __oflag);
 }
      return __openat64_alias (__fd, __path, __oflag, __builtin_va_arg_pack ());
    }

  if (__builtin_va_arg_pack_len () < 1)
    return __openat64_2 (__fd, __path, __oflag);

  return __openat64_alias (__fd, __path, __oflag, __builtin_va_arg_pack ());
}
# 253 "/usr/include/fcntl.h" 2 3 4



# 39 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/x86_64-linux-gnu/sys/stat.h" 1 3 4
# 105 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/stat.h" 1 3 4
# 108 "/usr/include/x86_64-linux-gnu/sys/stat.h" 2 3 4
# 219 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4
extern int stat (__const char *__restrict __file, struct stat *__restrict __buf) __asm__ ("" "stat64") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (1, 2)));
extern int fstat (int __fd, struct stat *__buf) __asm__ ("" "fstat64") __attribute__ ((__nothrow__))
     __attribute__ ((__nonnull__ (2)));






extern int stat64 (__const char *__restrict __file,
     struct stat64 *__restrict __buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));
extern int fstat64 (int __fd, struct stat64 *__buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
# 245 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4
extern int fstatat (int __fd, __const char *__restrict __file, struct stat *__restrict __buf, int __flag) __asm__ ("" "fstatat64") __attribute__ ((__nothrow__))


                 __attribute__ ((__nonnull__ (2, 3)));






extern int fstatat64 (int __fd, __const char *__restrict __file,
        struct stat64 *__restrict __buf, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
# 269 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4
extern int lstat (__const char *__restrict __file, struct stat *__restrict __buf) __asm__ ("" "lstat64") __attribute__ ((__nothrow__))


     __attribute__ ((__nonnull__ (1, 2)));





extern int lstat64 (__const char *__restrict __file,
      struct stat64 *__restrict __buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));





extern int chmod (__const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int lchmod (__const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




extern int fchmod (int __fd, __mode_t __mode) __attribute__ ((__nothrow__));





extern int fchmodat (int __fd, __const char *__file, __mode_t __mode,
       int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2))) __attribute__ ((__warn_unused_result__));






extern __mode_t umask (__mode_t __mask) __attribute__ ((__nothrow__));




extern __mode_t getumask (void) __attribute__ ((__nothrow__));



extern int mkdir (__const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int mkdirat (int __fd, __const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));






extern int mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int mknodat (int __fd, __const char *__path, __mode_t __mode,
      __dev_t __dev) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));





extern int mkfifo (__const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int mkfifoat (int __fd, __const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));





extern int utimensat (int __fd, __const char *__path,
        __const struct timespec __times[2],
        int __flags)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));




extern int futimens (int __fd, __const struct timespec __times[2]) __attribute__ ((__nothrow__));
# 412 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4
extern int __fxstat (int __ver, int __fildes, struct stat *__stat_buf) __asm__ ("" "__fxstat64") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (3)));
extern int __xstat (int __ver, __const char *__filename, struct stat *__stat_buf) __asm__ ("" "__xstat64") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (2, 3)));
extern int __lxstat (int __ver, __const char *__filename, struct stat *__stat_buf) __asm__ ("" "__lxstat64") __attribute__ ((__nothrow__))

     __attribute__ ((__nonnull__ (2, 3)));
extern int __fxstatat (int __ver, int __fildes, __const char *__filename, struct stat *__stat_buf, int __flag) __asm__ ("" "__fxstatat64") __attribute__ ((__nothrow__))


                    __attribute__ ((__nonnull__ (3, 4)));
# 434 "/usr/include/x86_64-linux-gnu/sys/stat.h" 3 4
extern int __fxstat64 (int __ver, int __fildes, struct stat64 *__stat_buf)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));
extern int __xstat64 (int __ver, __const char *__filename,
        struct stat64 *__stat_buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern int __lxstat64 (int __ver, __const char *__filename,
         struct stat64 *__stat_buf) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));
extern int __fxstatat64 (int __ver, int __fildes, __const char *__filename,
    struct stat64 *__stat_buf, int __flag)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 4)));

extern int __xmknod (int __ver, __const char *__path, __mode_t __mode,
       __dev_t *__dev) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 4)));

extern int __xmknodat (int __ver, int __fd, __const char *__path,
         __mode_t __mode, __dev_t *__dev)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3, 5)));




extern __inline int
__attribute__ ((__nothrow__)) stat (__const char *__path, struct stat *__statbuf)
{
  return __xstat (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) lstat (__const char *__path, struct stat *__statbuf)
{
  return __lxstat (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) fstat (int __fd, struct stat *__statbuf)
{
  return __fxstat (1, __fd, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) fstatat (int __fd, __const char *__filename, struct stat *__statbuf, int __flag)

{
  return __fxstatat (1, __fd, __filename, __statbuf, __flag);
}



extern __inline int
__attribute__ ((__nothrow__)) mknod (__const char *__path, __mode_t __mode, __dev_t __dev)
{
  return __xmknod (0, __path, __mode, &__dev);
}



extern __inline int
__attribute__ ((__nothrow__)) mknodat (int __fd, __const char *__path, __mode_t __mode, __dev_t __dev)

{
  return __xmknodat (0, __fd, __path, __mode, &__dev);
}





extern __inline int
__attribute__ ((__nothrow__)) stat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __xstat64 (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) lstat64 (__const char *__path, struct stat64 *__statbuf)
{
  return __lxstat64 (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) fstat64 (int __fd, struct stat64 *__statbuf)
{
  return __fxstat64 (1, __fd, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__)) fstatat64 (int __fd, __const char *__filename, struct stat64 *__statbuf, int __flag)

{
  return __fxstatat64 (1, __fd, __filename, __statbuf, __flag);
}







# 40 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/x86_64-linux-gnu/sys/time.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/sys/time.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/time.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/sys/time.h" 2 3 4
# 39 "/usr/include/x86_64-linux-gnu/sys/time.h" 3 4

# 57 "/usr/include/x86_64-linux-gnu/sys/time.h" 3 4
struct timezone
  {
    int tz_minuteswest;
    int tz_dsttime;
  };

typedef struct timezone *__restrict __timezone_ptr_t;
# 73 "/usr/include/x86_64-linux-gnu/sys/time.h" 3 4
extern int gettimeofday (struct timeval *__restrict __tv,
    __timezone_ptr_t __tz) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));




extern int settimeofday (__const struct timeval *__tv,
    __const struct timezone *__tz)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int adjtime (__const struct timeval *__delta,
      struct timeval *__olddelta) __attribute__ ((__nothrow__));




enum __itimer_which
  {

    ITIMER_REAL = 0,


    ITIMER_VIRTUAL = 1,



    ITIMER_PROF = 2

  };



struct itimerval
  {

    struct timeval it_interval;

    struct timeval it_value;
  };




typedef enum __itimer_which __itimer_which_t;






extern int getitimer (__itimer_which_t __which,
        struct itimerval *__value) __attribute__ ((__nothrow__));




extern int setitimer (__itimer_which_t __which,
        __const struct itimerval *__restrict __new,
        struct itimerval *__restrict __old) __attribute__ ((__nothrow__));




extern int utimes (__const char *__file, __const struct timeval __tvp[2])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int lutimes (__const char *__file, __const struct timeval __tvp[2])
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int futimes (int __fd, __const struct timeval __tvp[2]) __attribute__ ((__nothrow__));






extern int futimesat (int __fd, __const char *__file,
        __const struct timeval __tvp[2]) __attribute__ ((__nothrow__));
# 191 "/usr/include/x86_64-linux-gnu/sys/time.h" 3 4

# 41 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/assert.h" 1 3 4
# 68 "/usr/include/assert.h" 3 4



extern void __assert_fail (__const char *__assertion, __const char *__file,
      unsigned int __line, __const char *__function)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, __const char *__file,
      unsigned int __line,
      __const char *__function)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__)) __attribute__ ((__noreturn__));



# 42 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/usr/include/signal.h" 1 3 4
# 31 "/usr/include/signal.h" 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 1 3 4
# 104 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 3 4
extern int __sigismember (__const __sigset_t *, int);
extern int __sigaddset (__sigset_t *, int);
extern int __sigdelset (__sigset_t *, int);
# 118 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 3 4
extern __inline int __sigismember (__const __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return (__set->__val[__word] & __mask) ? 1 : 0; }
extern __inline int __sigaddset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] |= __mask), 0); }
extern __inline int __sigdelset ( __sigset_t *__set, int __sig) { unsigned long int __mask = (((unsigned long int) 1) << (((__sig) - 1) % (8 * sizeof (unsigned long int)))); unsigned long int __word = (((__sig) - 1) / (8 * sizeof (unsigned long int))); return ((__set->__val[__word] &= ~__mask), 0); }
# 34 "/usr/include/signal.h" 2 3 4







typedef __sig_atomic_t sig_atomic_t;

# 58 "/usr/include/signal.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/signum.h" 1 3 4
# 59 "/usr/include/signal.h" 2 3 4
# 79 "/usr/include/signal.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 2 3 4







typedef union sigval
  {
    int sival_int;
    void *sival_ptr;
  } sigval_t;
# 51 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 3 4
typedef struct siginfo
  {
    int si_signo;
    int si_errno;

    int si_code;

    union
      {
 int _pad[((128 / sizeof (int)) - 4)];


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
   } _kill;


 struct
   {
     int si_tid;
     int si_overrun;
     sigval_t si_sigval;
   } _timer;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     sigval_t si_sigval;
   } _rt;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     int si_status;
     __clock_t si_utime;
     __clock_t si_stime;
   } _sigchld;


 struct
   {
     void *si_addr;
   } _sigfault;


 struct
   {
     long int si_band;
     int si_fd;
   } _sigpoll;
      } _sifields;
  } siginfo_t;
# 129 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 3 4
enum
{
  SI_ASYNCNL = -60,

  SI_TKILL = -6,

  SI_SIGIO,

  SI_ASYNCIO,

  SI_MESGQ,

  SI_TIMER,

  SI_QUEUE,

  SI_USER,

  SI_KERNEL = 0x80

};



enum
{
  ILL_ILLOPC = 1,

  ILL_ILLOPN,

  ILL_ILLADR,

  ILL_ILLTRP,

  ILL_PRVOPC,

  ILL_PRVREG,

  ILL_COPROC,

  ILL_BADSTK

};


enum
{
  FPE_INTDIV = 1,

  FPE_INTOVF,

  FPE_FLTDIV,

  FPE_FLTOVF,

  FPE_FLTUND,

  FPE_FLTRES,

  FPE_FLTINV,

  FPE_FLTSUB

};


enum
{
  SEGV_MAPERR = 1,

  SEGV_ACCERR

};


enum
{
  BUS_ADRALN = 1,

  BUS_ADRERR,

  BUS_OBJERR

};


enum
{
  TRAP_BRKPT = 1,

  TRAP_TRACE

};


enum
{
  CLD_EXITED = 1,

  CLD_KILLED,

  CLD_DUMPED,

  CLD_TRAPPED,

  CLD_STOPPED,

  CLD_CONTINUED

};


enum
{
  POLL_IN = 1,

  POLL_OUT,

  POLL_MSG,

  POLL_ERR,

  POLL_PRI,

  POLL_HUP

};
# 273 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 3 4
typedef struct sigevent
  {
    sigval_t sigev_value;
    int sigev_signo;
    int sigev_notify;

    union
      {
 int _pad[((64 / sizeof (int)) - 4)];



 __pid_t _tid;

 struct
   {
     void (*_function) (sigval_t);
     void *_attribute;
   } _sigev_thread;
      } _sigev_un;
  } sigevent_t;






enum
{
  SIGEV_SIGNAL = 0,

  SIGEV_NONE,

  SIGEV_THREAD,


  SIGEV_THREAD_ID = 4

};
# 80 "/usr/include/signal.h" 2 3 4




typedef void (*__sighandler_t) (int);




extern __sighandler_t __sysv_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));

extern __sighandler_t sysv_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));







extern __sighandler_t signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));
# 113 "/usr/include/signal.h" 3 4





extern __sighandler_t bsd_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));






extern int kill (__pid_t __pid, int __sig) __attribute__ ((__nothrow__));






extern int killpg (__pid_t __pgrp, int __sig) __attribute__ ((__nothrow__));




extern int raise (int __sig) __attribute__ ((__nothrow__));




extern __sighandler_t ssignal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__));
extern int gsignal (int __sig) __attribute__ ((__nothrow__));




extern void psignal (int __sig, __const char *__s);




extern void psiginfo (__const siginfo_t *__pinfo, __const char *__s);
# 168 "/usr/include/signal.h" 3 4
extern int __sigpause (int __sig_or_mask, int __is_sig);
# 177 "/usr/include/signal.h" 3 4
extern int sigpause (int __sig) __asm__ ("__xpg_sigpause");
# 196 "/usr/include/signal.h" 3 4
extern int sigblock (int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));


extern int sigsetmask (int __mask) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));


extern int siggetmask (void) __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));
# 211 "/usr/include/signal.h" 3 4
typedef __sighandler_t sighandler_t;




typedef __sighandler_t sig_t;





extern int sigemptyset (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int sigfillset (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int sigaddset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int sigdelset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int sigismember (__const sigset_t *__set, int __signo)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int sigisemptyset (__const sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int sigandset (sigset_t *__set, __const sigset_t *__left,
        __const sigset_t *__right) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2, 3)));


extern int sigorset (sigset_t *__set, __const sigset_t *__left,
       __const sigset_t *__right) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2, 3)));




# 1 "/usr/include/x86_64-linux-gnu/bits/sigaction.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/sigaction.h" 3 4
struct sigaction
  {


    union
      {

 __sighandler_t sa_handler;

 void (*sa_sigaction) (int, siginfo_t *, void *);
      }
    __sigaction_handler;







    __sigset_t sa_mask;


    int sa_flags;


    void (*sa_restorer) (void);
  };
# 253 "/usr/include/signal.h" 2 3 4


extern int sigprocmask (int __how, __const sigset_t *__restrict __set,
   sigset_t *__restrict __oset) __attribute__ ((__nothrow__));






extern int sigsuspend (__const sigset_t *__set) __attribute__ ((__nonnull__ (1)));


extern int sigaction (int __sig, __const struct sigaction *__restrict __act,
        struct sigaction *__restrict __oact) __attribute__ ((__nothrow__));


extern int sigpending (sigset_t *__set) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int sigwait (__const sigset_t *__restrict __set, int *__restrict __sig)
     __attribute__ ((__nonnull__ (1, 2)));






extern int sigwaitinfo (__const sigset_t *__restrict __set,
   siginfo_t *__restrict __info) __attribute__ ((__nonnull__ (1)));






extern int sigtimedwait (__const sigset_t *__restrict __set,
    siginfo_t *__restrict __info,
    __const struct timespec *__restrict __timeout)
     __attribute__ ((__nonnull__ (1)));



extern int sigqueue (__pid_t __pid, int __sig, __const union sigval __val)
     __attribute__ ((__nothrow__));
# 310 "/usr/include/signal.h" 3 4
extern __const char *__const _sys_siglist[65];
extern __const char *__const sys_siglist[65];


struct sigvec
  {
    __sighandler_t sv_handler;
    int sv_mask;

    int sv_flags;

  };
# 334 "/usr/include/signal.h" 3 4
extern int sigvec (int __sig, __const struct sigvec *__vec,
     struct sigvec *__ovec) __attribute__ ((__nothrow__));



# 1 "/usr/include/x86_64-linux-gnu/bits/sigcontext.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/sigcontext.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/sigcontext.h" 2 3 4

struct _fpreg
{
  unsigned short significand[4];
  unsigned short exponent;
};

struct _fpxreg
{
  unsigned short significand[4];
  unsigned short exponent;
  unsigned short padding[3];
};

struct _xmmreg
{
  __uint32_t element[4];
};
# 109 "/usr/include/x86_64-linux-gnu/bits/sigcontext.h" 3 4
struct _fpstate
{

  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _fpxreg _st[8];
  struct _xmmreg _xmm[16];
  __uint32_t padding[24];
};

struct sigcontext
{
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long r11;
  unsigned long r12;
  unsigned long r13;
  unsigned long r14;
  unsigned long r15;
  unsigned long rdi;
  unsigned long rsi;
  unsigned long rbp;
  unsigned long rbx;
  unsigned long rdx;
  unsigned long rax;
  unsigned long rcx;
  unsigned long rsp;
  unsigned long rip;
  unsigned long eflags;
  unsigned short cs;
  unsigned short gs;
  unsigned short fs;
  unsigned short __pad0;
  unsigned long err;
  unsigned long trapno;
  unsigned long oldmask;
  unsigned long cr2;
  struct _fpstate * fpstate;
  unsigned long __reserved1 [8];
};
# 340 "/usr/include/signal.h" 2 3 4


extern int sigreturn (struct sigcontext *__scp) __attribute__ ((__nothrow__));






# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 350 "/usr/include/signal.h" 2 3 4




extern int siginterrupt (int __sig, int __interrupt) __attribute__ ((__nothrow__));

# 1 "/usr/include/x86_64-linux-gnu/bits/sigstack.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/sigstack.h" 3 4
struct sigstack
  {
    void *ss_sp;
    int ss_onstack;
  };



enum
{
  SS_ONSTACK = 1,

  SS_DISABLE

};
# 50 "/usr/include/x86_64-linux-gnu/bits/sigstack.h" 3 4
typedef struct sigaltstack
  {
    void *ss_sp;
    int ss_flags;
    size_t ss_size;
  } stack_t;
# 357 "/usr/include/signal.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/sys/ucontext.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/sys/ucontext.h" 3 4
# 1 "/usr/include/signal.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/sys/ucontext.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/sys/ucontext.h" 2 3 4
# 33 "/usr/include/x86_64-linux-gnu/sys/ucontext.h" 3 4
typedef long int greg_t;





typedef greg_t gregset_t[23];



enum
{
  REG_R8 = 0,

  REG_R9,

  REG_R10,

  REG_R11,

  REG_R12,

  REG_R13,

  REG_R14,

  REG_R15,

  REG_RDI,

  REG_RSI,

  REG_RBP,

  REG_RBX,

  REG_RDX,

  REG_RAX,

  REG_RCX,

  REG_RSP,

  REG_RIP,

  REG_EFL,

  REG_CSGSFS,

  REG_ERR,

  REG_TRAPNO,

  REG_OLDMASK,

  REG_CR2

};


struct _libc_fpxreg
{
  unsigned short int significand[4];
  unsigned short int exponent;
  unsigned short int padding[3];
};

struct _libc_xmmreg
{
  __uint32_t element[4];
};

struct _libc_fpstate
{

  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _libc_fpxreg _st[8];
  struct _libc_xmmreg _xmm[16];
  __uint32_t padding[24];
};


typedef struct _libc_fpstate *fpregset_t;


typedef struct
  {
    gregset_t gregs;

    fpregset_t fpregs;
    unsigned long __reserved1 [8];
} mcontext_t;


typedef struct ucontext
  {
    unsigned long int uc_flags;
    struct ucontext *uc_link;
    stack_t uc_stack;
    mcontext_t uc_mcontext;
    __sigset_t uc_sigmask;
    struct _libc_fpstate __fpregs_mem;
  } ucontext_t;
# 360 "/usr/include/signal.h" 2 3 4





extern int sigstack (struct sigstack *__ss, struct sigstack *__oss)
     __attribute__ ((__nothrow__)) __attribute__ ((__deprecated__));



extern int sigaltstack (__const struct sigaltstack *__restrict __ss,
   struct sigaltstack *__restrict __oss) __attribute__ ((__nothrow__));







extern int sighold (int __sig) __attribute__ ((__nothrow__));


extern int sigrelse (int __sig) __attribute__ ((__nothrow__));


extern int sigignore (int __sig) __attribute__ ((__nothrow__));


extern __sighandler_t sigset (int __sig, __sighandler_t __disp) __attribute__ ((__nothrow__));






# 1 "/usr/include/x86_64-linux-gnu/bits/sigthread.h" 1 3 4
# 31 "/usr/include/x86_64-linux-gnu/bits/sigthread.h" 3 4
extern int pthread_sigmask (int __how,
       __const __sigset_t *__restrict __newmask,
       __sigset_t *__restrict __oldmask)__attribute__ ((__nothrow__));


extern int pthread_kill (pthread_t __threadid, int __signo) __attribute__ ((__nothrow__));



extern int pthread_sigqueue (pthread_t __threadid, int __signo,
        const union sigval __value) __attribute__ ((__nothrow__));
# 396 "/usr/include/signal.h" 2 3 4






extern int __libc_current_sigrtmin (void) __attribute__ ((__nothrow__));

extern int __libc_current_sigrtmax (void) __attribute__ ((__nothrow__));




# 43 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/home/gabriel/repos/qemu/include/glib-compat.h" 1
# 17 "/home/gabriel/repos/qemu/include/glib-compat.h"
# 1 "/usr/include/glib-2.0/glib.h" 1
# 32 "/usr/include/glib-2.0/glib.h"
# 1 "/usr/include/glib-2.0/glib/galloca.h" 1
# 34 "/usr/include/glib-2.0/glib/galloca.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 34 "/usr/include/glib-2.0/glib/gtypes.h"
# 1 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h" 1
# 9 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
# 1 "/usr/include/glib-2.0/glib/gmacros.h" 1
# 40 "/usr/include/glib-2.0/glib/gmacros.h"
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 150 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 41 "/usr/include/glib-2.0/glib/gmacros.h" 2
# 10 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h" 2

# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 1 3 4
# 12 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h" 2
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/float.h" 1 3 4
# 13 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h" 2
# 22 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"

# 38 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
typedef signed char gint8;
typedef unsigned char guint8;
typedef signed short gint16;
typedef unsigned short guint16;



typedef signed int gint32;
typedef unsigned int guint32;





typedef signed long gint64;
typedef unsigned long guint64;
# 65 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
typedef signed long gssize;
typedef unsigned long gsize;
# 75 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
typedef gint64 goffset;
# 90 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
typedef signed long gintptr;
typedef unsigned long guintptr;
# 198 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"
typedef int GPid;
# 208 "/usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h"

# 35 "/usr/include/glib-2.0/glib/gtypes.h" 2
# 1 "/usr/include/glib-2.0/glib/gmacros.h" 1
# 36 "/usr/include/glib-2.0/glib/gtypes.h" 2
# 1 "/usr/include/glib-2.0/glib/gversionmacros.h" 1
# 37 "/usr/include/glib-2.0/glib/gtypes.h" 2



# 48 "/usr/include/glib-2.0/glib/gtypes.h"
typedef char gchar;
typedef short gshort;
typedef long glong;
typedef int gint;
typedef gint gboolean;

typedef unsigned char guchar;
typedef unsigned short gushort;
typedef unsigned long gulong;
typedef unsigned int guint;

typedef float gfloat;
typedef double gdouble;
# 79 "/usr/include/glib-2.0/glib/gtypes.h"
typedef void* gpointer;
typedef const void *gconstpointer;

typedef gint (*GCompareFunc) (gconstpointer a,
                                                 gconstpointer b);
typedef gint (*GCompareDataFunc) (gconstpointer a,
                                                 gconstpointer b,
       gpointer user_data);
typedef gboolean (*GEqualFunc) (gconstpointer a,
                                                 gconstpointer b);
typedef void (*GDestroyNotify) (gpointer data);
typedef void (*GFunc) (gpointer data,
                                                 gpointer user_data);
typedef guint (*GHashFunc) (gconstpointer key);
typedef void (*GHFunc) (gpointer key,
                                                 gpointer value,
                                                 gpointer user_data);
# 105 "/usr/include/glib-2.0/glib/gtypes.h"
typedef void (*GFreeFunc) (gpointer data);
# 119 "/usr/include/glib-2.0/glib/gtypes.h"
typedef const gchar * (*GTranslateFunc) (const gchar *str,
       gpointer data);
# 401 "/usr/include/glib-2.0/glib/gtypes.h"
typedef union _GDoubleIEEE754 GDoubleIEEE754;
typedef union _GFloatIEEE754 GFloatIEEE754;





union _GFloatIEEE754
{
  gfloat v_float;
  struct {
    guint mantissa : 23;
    guint biased_exponent : 8;
    guint sign : 1;
  } mpn;
};
union _GDoubleIEEE754
{
  gdouble v_double;
  struct {
    guint mantissa_low : 32;
    guint mantissa_high : 20;
    guint biased_exponent : 11;
    guint sign : 1;
  } mpn;
};
# 451 "/usr/include/glib-2.0/glib/gtypes.h"
typedef struct _GTimeVal GTimeVal;

struct _GTimeVal
{
  glong tv_sec;
  glong tv_usec;
};


# 35 "/usr/include/glib-2.0/glib/galloca.h" 2
# 33 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/garray.h" 1
# 34 "/usr/include/glib-2.0/glib/garray.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/garray.h" 2



typedef struct _GBytes GBytes;
typedef struct _GArray GArray;
typedef struct _GByteArray GByteArray;
typedef struct _GPtrArray GPtrArray;

struct _GArray
{
  gchar *data;
  guint len;
};

struct _GByteArray
{
  guint8 *data;
  guint len;
};

struct _GPtrArray
{
  gpointer *pdata;
  guint len;
};
# 71 "/usr/include/glib-2.0/glib/garray.h"
GArray* g_array_new (gboolean zero_terminated,
       gboolean clear_,
       guint element_size);
GArray* g_array_sized_new (gboolean zero_terminated,
       gboolean clear_,
       guint element_size,
       guint reserved_size);
gchar* g_array_free (GArray *array,
       gboolean free_segment);
GArray *g_array_ref (GArray *array);
void g_array_unref (GArray *array);
guint g_array_get_element_size (GArray *array);
GArray* g_array_append_vals (GArray *array,
       gconstpointer data,
       guint len);
GArray* g_array_prepend_vals (GArray *array,
       gconstpointer data,
       guint len);
GArray* g_array_insert_vals (GArray *array,
       guint index_,
       gconstpointer data,
       guint len);
GArray* g_array_set_size (GArray *array,
       guint length);
GArray* g_array_remove_index (GArray *array,
       guint index_);
GArray* g_array_remove_index_fast (GArray *array,
       guint index_);
GArray* g_array_remove_range (GArray *array,
       guint index_,
       guint length);
void g_array_sort (GArray *array,
       GCompareFunc compare_func);
void g_array_sort_with_data (GArray *array,
       GCompareDataFunc compare_func,
       gpointer user_data);
void g_array_set_clear_func (GArray *array,
                                   GDestroyNotify clear_func);






GPtrArray* g_ptr_array_new (void);
GPtrArray* g_ptr_array_new_with_free_func (GDestroyNotify element_free_func);
GPtrArray* g_ptr_array_sized_new (guint reserved_size);
GPtrArray* g_ptr_array_new_full (guint reserved_size,
        GDestroyNotify element_free_func);
gpointer* g_ptr_array_free (GPtrArray *array,
        gboolean free_seg);
GPtrArray* g_ptr_array_ref (GPtrArray *array);
void g_ptr_array_unref (GPtrArray *array);
void g_ptr_array_set_free_func (GPtrArray *array,
                                           GDestroyNotify element_free_func);
void g_ptr_array_set_size (GPtrArray *array,
        gint length);
gpointer g_ptr_array_remove_index (GPtrArray *array,
        guint index_);
gpointer g_ptr_array_remove_index_fast (GPtrArray *array,
        guint index_);
gboolean g_ptr_array_remove (GPtrArray *array,
        gpointer data);
gboolean g_ptr_array_remove_fast (GPtrArray *array,
        gpointer data);
void g_ptr_array_remove_range (GPtrArray *array,
        guint index_,
        guint length);
void g_ptr_array_add (GPtrArray *array,
        gpointer data);
void g_ptr_array_sort (GPtrArray *array,
        GCompareFunc compare_func);
void g_ptr_array_sort_with_data (GPtrArray *array,
        GCompareDataFunc compare_func,
        gpointer user_data);
void g_ptr_array_foreach (GPtrArray *array,
        GFunc func,
        gpointer user_data);






GByteArray* g_byte_array_new (void);
GByteArray* g_byte_array_new_take (guint8 *data,
                                            gsize len);
GByteArray* g_byte_array_sized_new (guint reserved_size);
guint8* g_byte_array_free (GByteArray *array,
         gboolean free_segment);
GBytes* g_byte_array_free_to_bytes (GByteArray *array);
GByteArray *g_byte_array_ref (GByteArray *array);
void g_byte_array_unref (GByteArray *array);
GByteArray* g_byte_array_append (GByteArray *array,
         const guint8 *data,
         guint len);
GByteArray* g_byte_array_prepend (GByteArray *array,
         const guint8 *data,
         guint len);
GByteArray* g_byte_array_set_size (GByteArray *array,
         guint length);
GByteArray* g_byte_array_remove_index (GByteArray *array,
         guint index_);
GByteArray* g_byte_array_remove_index_fast (GByteArray *array,
         guint index_);
GByteArray* g_byte_array_remove_range (GByteArray *array,
         guint index_,
         guint length);
void g_byte_array_sort (GByteArray *array,
         GCompareFunc compare_func);
void g_byte_array_sort_with_data (GByteArray *array,
         GCompareDataFunc compare_func,
         gpointer user_data);


# 34 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gasyncqueue.h" 1
# 34 "/usr/include/glib-2.0/glib/gasyncqueue.h"
# 1 "/usr/include/glib-2.0/glib/gthread.h" 1
# 34 "/usr/include/glib-2.0/glib/gthread.h"
# 1 "/usr/include/glib-2.0/glib/gatomic.h" 1
# 29 "/usr/include/glib-2.0/glib/gatomic.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 30 "/usr/include/glib-2.0/glib/gatomic.h" 2



gint g_atomic_int_get (volatile gint *atomic);
void g_atomic_int_set (volatile gint *atomic,
                                                               gint newval);
void g_atomic_int_inc (volatile gint *atomic);
gboolean g_atomic_int_dec_and_test (volatile gint *atomic);
gboolean g_atomic_int_compare_and_exchange (volatile gint *atomic,
                                                               gint oldval,
                                                               gint newval);
gint g_atomic_int_add (volatile gint *atomic,
                                                               gint val);

guint g_atomic_int_and (volatile guint *atomic,
                                                               guint val);

guint g_atomic_int_or (volatile guint *atomic,
                                                               guint val);
guint g_atomic_int_xor (volatile guint *atomic,
                                                               guint val);

gpointer g_atomic_pointer_get (volatile void *atomic);
void g_atomic_pointer_set (volatile void *atomic,
                                                               gpointer newval);
gboolean g_atomic_pointer_compare_and_exchange (volatile void *atomic,
                                                               gpointer oldval,
                                                               gpointer newval);
gssize g_atomic_pointer_add (volatile void *atomic,
                                                               gssize val);

gsize g_atomic_pointer_and (volatile void *atomic,
                                                               gsize val);

gsize g_atomic_pointer_or (volatile void *atomic,
                                                               gsize val);
gsize g_atomic_pointer_xor (volatile void *atomic,
                                                               gsize val);

__attribute__((__deprecated__("Use '" "g_atomic_add" "' instead")))
gint g_atomic_int_exchange_and_add (volatile gint *atomic,
                                                               gint val);


# 35 "/usr/include/glib-2.0/glib/gthread.h" 2
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 30 "/usr/include/glib-2.0/glib/gerror.h"
# 1 "/usr/include/glib-2.0/glib/gquark.h" 1
# 34 "/usr/include/glib-2.0/glib/gquark.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gquark.h" 2



typedef guint32 GQuark;



GQuark g_quark_try_string (const gchar *string);
GQuark g_quark_from_static_string (const gchar *string);
GQuark g_quark_from_string (const gchar *string);
const gchar * g_quark_to_string (GQuark quark) __attribute__((__const__));

const gchar * g_intern_string (const gchar *string);
const gchar * g_intern_static_string (const gchar *string);


# 31 "/usr/include/glib-2.0/glib/gerror.h" 2


# 43 "/usr/include/glib-2.0/glib/gerror.h"
typedef struct _GError GError;

struct _GError
{
  GQuark domain;
  gint code;
  gchar *message;
};

GError* g_error_new (GQuark domain,
                                gint code,
                                const gchar *format,
                                ...) __attribute__((__format__ (__printf__, 3, 4)));

GError* g_error_new_literal (GQuark domain,
                                gint code,
                                const gchar *message);
GError* g_error_new_valist (GQuark domain,
                                gint code,
                                const gchar *format,
                                va_list args);

void g_error_free (GError *error);
GError* g_error_copy (const GError *error);

gboolean g_error_matches (const GError *error,
                                GQuark domain,
                                gint code);




void g_set_error (GError **err,
                                GQuark domain,
                                gint code,
                                const gchar *format,
                                ...) __attribute__((__format__ (__printf__, 4, 5)));

void g_set_error_literal (GError **err,
                                GQuark domain,
                                gint code,
                                const gchar *message);



void g_propagate_error (GError **dest,
    GError *src);


void g_clear_error (GError **err);


void g_prefix_error (GError **err,
                                       const gchar *format,
                                       ...) __attribute__((__format__ (__printf__, 2, 3)));


void g_propagate_prefixed_error (GError **dest,
                                       GError *src,
                                       const gchar *format,
                                       ...) __attribute__((__format__ (__printf__, 3, 4)));


# 36 "/usr/include/glib-2.0/glib/gthread.h" 2




GQuark g_thread_error_quark (void);

typedef enum
{
  G_THREAD_ERROR_AGAIN
} GThreadError;

typedef gpointer (*GThreadFunc) (gpointer data);

typedef struct _GThread GThread;

typedef union _GMutex GMutex;
typedef struct _GRecMutex GRecMutex;
typedef struct _GRWLock GRWLock;
typedef struct _GCond GCond;
typedef struct _GPrivate GPrivate;
typedef struct _GOnce GOnce;

union _GMutex
{

  gpointer p;
  guint i[2];
};

struct _GRWLock
{

  gpointer p;
  guint i[2];
};

struct _GCond
{

  gpointer p;
  guint i[2];
};

struct _GRecMutex
{

  gpointer p;
  guint i[2];
};


struct _GPrivate
{

  gpointer p;
  GDestroyNotify notify;
  gpointer future[2];
};

typedef enum
{
  G_ONCE_STATUS_NOTCALLED,
  G_ONCE_STATUS_PROGRESS,
  G_ONCE_STATUS_READY
} GOnceStatus;


struct _GOnce
{
  volatile GOnceStatus status;
  volatile gpointer retval;
};
# 140 "/usr/include/glib-2.0/glib/gthread.h"
GThread * g_thread_ref (GThread *thread);
void g_thread_unref (GThread *thread);
GThread * g_thread_new (const gchar *name,
                                                 GThreadFunc func,
                                                 gpointer data);
GThread * g_thread_try_new (const gchar *name,
                                                 GThreadFunc func,
                                                 gpointer data,
                                                 GError **error);
GThread * g_thread_self (void);
void g_thread_exit (gpointer retval);
gpointer g_thread_join (GThread *thread);
void g_thread_yield (void);


void g_mutex_init (GMutex *mutex);
void g_mutex_clear (GMutex *mutex);
void g_mutex_lock (GMutex *mutex);
gboolean g_mutex_trylock (GMutex *mutex);
void g_mutex_unlock (GMutex *mutex);

void g_rw_lock_init (GRWLock *rw_lock);
void g_rw_lock_clear (GRWLock *rw_lock);
void g_rw_lock_writer_lock (GRWLock *rw_lock);
gboolean g_rw_lock_writer_trylock (GRWLock *rw_lock);
void g_rw_lock_writer_unlock (GRWLock *rw_lock);
void g_rw_lock_reader_lock (GRWLock *rw_lock);
gboolean g_rw_lock_reader_trylock (GRWLock *rw_lock);
void g_rw_lock_reader_unlock (GRWLock *rw_lock);

void g_rec_mutex_init (GRecMutex *rec_mutex);
void g_rec_mutex_clear (GRecMutex *rec_mutex);
void g_rec_mutex_lock (GRecMutex *rec_mutex);
gboolean g_rec_mutex_trylock (GRecMutex *rec_mutex);
void g_rec_mutex_unlock (GRecMutex *rec_mutex);

void g_cond_init (GCond *cond);
void g_cond_clear (GCond *cond);
void g_cond_wait (GCond *cond,
                                                 GMutex *mutex);
void g_cond_signal (GCond *cond);
void g_cond_broadcast (GCond *cond);
gboolean g_cond_wait_until (GCond *cond,
                                                 GMutex *mutex,
                                                 gint64 end_time);

gpointer g_private_get (GPrivate *key);
void g_private_set (GPrivate *key,
                                                 gpointer value);
void g_private_replace (GPrivate *key,
                                                 gpointer value);

gpointer g_once_impl (GOnce *once,
                                                 GThreadFunc func,
                                                 gpointer arg);
gboolean g_once_init_enter (volatile void *location);
void g_once_init_leave (volatile void *location,
                                                 gsize result);
# 229 "/usr/include/glib-2.0/glib/gthread.h"

# 35 "/usr/include/glib-2.0/glib/gasyncqueue.h" 2



typedef struct _GAsyncQueue GAsyncQueue;

GAsyncQueue *g_async_queue_new (void);
GAsyncQueue *g_async_queue_new_full (GDestroyNotify item_free_func);
void g_async_queue_lock (GAsyncQueue *queue);
void g_async_queue_unlock (GAsyncQueue *queue);
GAsyncQueue *g_async_queue_ref (GAsyncQueue *queue);
void g_async_queue_unref (GAsyncQueue *queue);

__attribute__((__deprecated__("Use '" "g_async_queue_ref" "' instead")))
void g_async_queue_ref_unlocked (GAsyncQueue *queue);

__attribute__((__deprecated__("Use '" "g_async_queue_unref" "' instead")))
void g_async_queue_unref_and_unlock (GAsyncQueue *queue);

void g_async_queue_push (GAsyncQueue *queue,
                                                 gpointer data);
void g_async_queue_push_unlocked (GAsyncQueue *queue,
                                                 gpointer data);
void g_async_queue_push_sorted (GAsyncQueue *queue,
                                                 gpointer data,
                                                 GCompareDataFunc func,
                                                 gpointer user_data);
void g_async_queue_push_sorted_unlocked (GAsyncQueue *queue,
                                                 gpointer data,
                                                 GCompareDataFunc func,
                                                 gpointer user_data);
gpointer g_async_queue_pop (GAsyncQueue *queue);
gpointer g_async_queue_pop_unlocked (GAsyncQueue *queue);
gpointer g_async_queue_try_pop (GAsyncQueue *queue);
gpointer g_async_queue_try_pop_unlocked (GAsyncQueue *queue);
gpointer g_async_queue_timeout_pop (GAsyncQueue *queue,
                                                 guint64 timeout);
gpointer g_async_queue_timeout_pop_unlocked (GAsyncQueue *queue,
                                                 guint64 timeout);
gint g_async_queue_length (GAsyncQueue *queue);
gint g_async_queue_length_unlocked (GAsyncQueue *queue);
void g_async_queue_sort (GAsyncQueue *queue,
                                                 GCompareDataFunc func,
                                                 gpointer user_data);
void g_async_queue_sort_unlocked (GAsyncQueue *queue,
                                                 GCompareDataFunc func,
                                                 gpointer user_data);

__attribute__((__deprecated__("Use '" "g_async_queue_timeout_pop" "' instead")))
gpointer g_async_queue_timed_pop (GAsyncQueue *queue,
                                                 GTimeVal *end_time);
__attribute__((__deprecated__("Use '" "g_async_queue_timeout_pop_unlocked" "' instead")))
gpointer g_async_queue_timed_pop_unlocked (GAsyncQueue *queue,
                                                 GTimeVal *end_time);


# 35 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gatomic.h" 1
# 36 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gbacktrace.h" 1
# 34 "/usr/include/glib-2.0/glib/gbacktrace.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gbacktrace.h" 2




void g_on_error_query (const gchar *prg_name);
void g_on_error_stack_trace (const gchar *prg_name);
# 62 "/usr/include/glib-2.0/glib/gbacktrace.h"

# 37 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gbase64.h" 1
# 28 "/usr/include/glib-2.0/glib/gbase64.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 29 "/usr/include/glib-2.0/glib/gbase64.h" 2



gsize g_base64_encode_step (const guchar *in,
                                 gsize len,
                                 gboolean break_lines,
                                 gchar *out,
                                 gint *state,
                                 gint *save);
gsize g_base64_encode_close (gboolean break_lines,
                                 gchar *out,
                                 gint *state,
                                 gint *save);
gchar* g_base64_encode (const guchar *data,
                                 gsize len) __attribute__((__malloc__));
gsize g_base64_decode_step (const gchar *in,
                                 gsize len,
                                 guchar *out,
                                 gint *state,
                                 guint *save);
guchar *g_base64_decode (const gchar *text,
                                 gsize *out_len) __attribute__((__malloc__));
guchar *g_base64_decode_inplace (gchar *text,
                                 gsize *out_len);



# 38 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gbitlock.h" 1
# 26 "/usr/include/glib-2.0/glib/gbitlock.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 27 "/usr/include/glib-2.0/glib/gbitlock.h" 2







void g_bit_lock (volatile gint *address,
                                           gint lock_bit);
gboolean g_bit_trylock (volatile gint *address,
                                           gint lock_bit);
void g_bit_unlock (volatile gint *address,
                                           gint lock_bit);

void g_pointer_bit_lock (volatile void *address,
                                           gint lock_bit);
gboolean g_pointer_bit_trylock (volatile void *address,
                                           gint lock_bit);
void g_pointer_bit_unlock (volatile void *address,
                                           gint lock_bit);
# 70 "/usr/include/glib-2.0/glib/gbitlock.h"

# 39 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gbookmarkfile.h" 1
# 27 "/usr/include/glib-2.0/glib/gbookmarkfile.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 28 "/usr/include/glib-2.0/glib/gbookmarkfile.h" 2



# 57 "/usr/include/glib-2.0/glib/gbookmarkfile.h"
typedef enum
{
  G_BOOKMARK_FILE_ERROR_INVALID_URI,
  G_BOOKMARK_FILE_ERROR_INVALID_VALUE,
  G_BOOKMARK_FILE_ERROR_APP_NOT_REGISTERED,
  G_BOOKMARK_FILE_ERROR_URI_NOT_FOUND,
  G_BOOKMARK_FILE_ERROR_READ,
  G_BOOKMARK_FILE_ERROR_UNKNOWN_ENCODING,
  G_BOOKMARK_FILE_ERROR_WRITE,
  G_BOOKMARK_FILE_ERROR_FILE_NOT_FOUND
} GBookmarkFileError;

GQuark g_bookmark_file_error_quark (void);







typedef struct _GBookmarkFile GBookmarkFile;

GBookmarkFile *g_bookmark_file_new (void);
void g_bookmark_file_free (GBookmarkFile *bookmark);

gboolean g_bookmark_file_load_from_file (GBookmarkFile *bookmark,
          const gchar *filename,
          GError **error);
gboolean g_bookmark_file_load_from_data (GBookmarkFile *bookmark,
          const gchar *data,
          gsize length,
          GError **error);
gboolean g_bookmark_file_load_from_data_dirs (GBookmarkFile *bookmark,
          const gchar *file,
          gchar **full_path,
          GError **error);
gchar * g_bookmark_file_to_data (GBookmarkFile *bookmark,
          gsize *length,
          GError **error) __attribute__((__malloc__));
gboolean g_bookmark_file_to_file (GBookmarkFile *bookmark,
          const gchar *filename,
          GError **error);

void g_bookmark_file_set_title (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *title);
gchar * g_bookmark_file_get_title (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error) __attribute__((__malloc__));
void g_bookmark_file_set_description (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *description);
gchar * g_bookmark_file_get_description (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error) __attribute__((__malloc__));
void g_bookmark_file_set_mime_type (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *mime_type);
gchar * g_bookmark_file_get_mime_type (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error) __attribute__((__malloc__));
void g_bookmark_file_set_groups (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar **groups,
          gsize length);
void g_bookmark_file_add_group (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *group);
gboolean g_bookmark_file_has_group (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *group,
          GError **error);
gchar ** g_bookmark_file_get_groups (GBookmarkFile *bookmark,
          const gchar *uri,
          gsize *length,
          GError **error) __attribute__((__malloc__));
void g_bookmark_file_add_application (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *name,
          const gchar *exec);
gboolean g_bookmark_file_has_application (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *name,
          GError **error);
gchar ** g_bookmark_file_get_applications (GBookmarkFile *bookmark,
          const gchar *uri,
          gsize *length,
          GError **error) __attribute__((__malloc__));
gboolean g_bookmark_file_set_app_info (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *name,
          const gchar *exec,
          gint count,
          time_t stamp,
          GError **error);
gboolean g_bookmark_file_get_app_info (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *name,
          gchar **exec,
          guint *count,
          time_t *stamp,
          GError **error);
void g_bookmark_file_set_is_private (GBookmarkFile *bookmark,
          const gchar *uri,
          gboolean is_private);
gboolean g_bookmark_file_get_is_private (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error);
void g_bookmark_file_set_icon (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *href,
          const gchar *mime_type);
gboolean g_bookmark_file_get_icon (GBookmarkFile *bookmark,
          const gchar *uri,
          gchar **href,
          gchar **mime_type,
          GError **error);
void g_bookmark_file_set_added (GBookmarkFile *bookmark,
          const gchar *uri,
          time_t added);
time_t g_bookmark_file_get_added (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error);
void g_bookmark_file_set_modified (GBookmarkFile *bookmark,
          const gchar *uri,
          time_t modified);
time_t g_bookmark_file_get_modified (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error);
void g_bookmark_file_set_visited (GBookmarkFile *bookmark,
          const gchar *uri,
          time_t visited);
time_t g_bookmark_file_get_visited (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error);
gboolean g_bookmark_file_has_item (GBookmarkFile *bookmark,
          const gchar *uri);
gint g_bookmark_file_get_size (GBookmarkFile *bookmark);
gchar ** g_bookmark_file_get_uris (GBookmarkFile *bookmark,
          gsize *length) __attribute__((__malloc__));
gboolean g_bookmark_file_remove_group (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *group,
          GError **error);
gboolean g_bookmark_file_remove_application (GBookmarkFile *bookmark,
          const gchar *uri,
          const gchar *name,
          GError **error);
gboolean g_bookmark_file_remove_item (GBookmarkFile *bookmark,
          const gchar *uri,
          GError **error);
gboolean g_bookmark_file_move_item (GBookmarkFile *bookmark,
          const gchar *old_uri,
          const gchar *new_uri,
          GError **error);


# 40 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gbytes.h" 1
# 31 "/usr/include/glib-2.0/glib/gbytes.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 32 "/usr/include/glib-2.0/glib/gbytes.h" 2
# 1 "/usr/include/glib-2.0/glib/garray.h" 1
# 33 "/usr/include/glib-2.0/glib/gbytes.h" 2



GBytes * g_bytes_new (gconstpointer data,
                                                 gsize size);

GBytes * g_bytes_new_take (gpointer data,
                                                 gsize size);

GBytes * g_bytes_new_static (gconstpointer data,
                                                 gsize size);

GBytes * g_bytes_new_with_free_func (gconstpointer data,
                                                 gsize size,
                                                 GDestroyNotify free_func,
                                                 gpointer user_data);

GBytes * g_bytes_new_from_bytes (GBytes *bytes,
                                                 gsize offset,
                                                 gsize length);

gconstpointer g_bytes_get_data (GBytes *bytes,
                                                 gsize *size);

gsize g_bytes_get_size (GBytes *bytes);

GBytes * g_bytes_ref (GBytes *bytes);

void g_bytes_unref (GBytes *bytes);

gpointer g_bytes_unref_to_data (GBytes *bytes,
                                                 gsize *size);

GByteArray * g_bytes_unref_to_array (GBytes *bytes);

guint g_bytes_hash (gconstpointer bytes);

gboolean g_bytes_equal (gconstpointer bytes1,
                                                 gconstpointer bytes2);

gint g_bytes_compare (gconstpointer bytes1,
                                                 gconstpointer bytes2);


# 41 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gcharset.h" 1
# 28 "/usr/include/glib-2.0/glib/gcharset.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 29 "/usr/include/glib-2.0/glib/gcharset.h" 2



gboolean g_get_charset (const char **charset);
gchar * g_get_codeset (void);

const gchar * const * g_get_language_names (void);
gchar ** g_get_locale_variants (const gchar *locale);


# 42 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gchecksum.h" 1
# 28 "/usr/include/glib-2.0/glib/gchecksum.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 29 "/usr/include/glib-2.0/glib/gchecksum.h" 2


# 46 "/usr/include/glib-2.0/glib/gchecksum.h"
typedef enum {
  G_CHECKSUM_MD5,
  G_CHECKSUM_SHA1,
  G_CHECKSUM_SHA256
} GChecksumType;
# 61 "/usr/include/glib-2.0/glib/gchecksum.h"
typedef struct _GChecksum GChecksum;

gssize g_checksum_type_get_length (GChecksumType checksum_type);

GChecksum * g_checksum_new (GChecksumType checksum_type);
void g_checksum_reset (GChecksum *checksum);
GChecksum * g_checksum_copy (const GChecksum *checksum);
void g_checksum_free (GChecksum *checksum);
void g_checksum_update (GChecksum *checksum,
                                                     const guchar *data,
                                                     gssize length);
const gchar * g_checksum_get_string (GChecksum *checksum);
void g_checksum_get_digest (GChecksum *checksum,
                                                     guint8 *buffer,
                                                     gsize *digest_len);

gchar *g_compute_checksum_for_data (GChecksumType checksum_type,
                                                     const guchar *data,
                                                     gsize length);
gchar *g_compute_checksum_for_string (GChecksumType checksum_type,
                                                     const gchar *str,
                                                     gssize length);


# 43 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gconvert.h" 1
# 34 "/usr/include/glib-2.0/glib/gconvert.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 35 "/usr/include/glib-2.0/glib/gconvert.h" 2


# 50 "/usr/include/glib-2.0/glib/gconvert.h"
typedef enum
{
  G_CONVERT_ERROR_NO_CONVERSION,
  G_CONVERT_ERROR_ILLEGAL_SEQUENCE,
  G_CONVERT_ERROR_FAILED,
  G_CONVERT_ERROR_PARTIAL_INPUT,
  G_CONVERT_ERROR_BAD_URI,
  G_CONVERT_ERROR_NOT_ABSOLUTE_PATH
} GConvertError;
# 68 "/usr/include/glib-2.0/glib/gconvert.h"
GQuark g_convert_error_quark (void);
# 77 "/usr/include/glib-2.0/glib/gconvert.h"
typedef struct _GIConv *GIConv;

GIConv g_iconv_open (const gchar *to_codeset,
         const gchar *from_codeset);
gsize g_iconv (GIConv converter,
         gchar **inbuf,
         gsize *inbytes_left,
         gchar **outbuf,
         gsize *outbytes_left);
gint g_iconv_close (GIConv converter);


gchar* g_convert (const gchar *str,
    gssize len,
    const gchar *to_codeset,
    const gchar *from_codeset,
    gsize *bytes_read,
    gsize *bytes_written,
    GError **error) __attribute__((__malloc__));
gchar* g_convert_with_iconv (const gchar *str,
    gssize len,
    GIConv converter,
    gsize *bytes_read,
    gsize *bytes_written,
    GError **error) __attribute__((__malloc__));
gchar* g_convert_with_fallback (const gchar *str,
    gssize len,
    const gchar *to_codeset,
    const gchar *from_codeset,
    const gchar *fallback,
    gsize *bytes_read,
    gsize *bytes_written,
    GError **error) __attribute__((__malloc__));




gchar* g_locale_to_utf8 (const gchar *opsysstring,
      gssize len,
      gsize *bytes_read,
      gsize *bytes_written,
      GError **error) __attribute__((__malloc__));
gchar* g_locale_from_utf8 (const gchar *utf8string,
      gssize len,
      gsize *bytes_read,
      gsize *bytes_written,
      GError **error) __attribute__((__malloc__));
# 137 "/usr/include/glib-2.0/glib/gconvert.h"
gchar* g_filename_to_utf8 (const gchar *opsysstring,
        gssize len,
        gsize *bytes_read,
        gsize *bytes_written,
        GError **error) __attribute__((__malloc__));
gchar* g_filename_from_utf8 (const gchar *utf8string,
        gssize len,
        gsize *bytes_read,
        gsize *bytes_written,
        GError **error) __attribute__((__malloc__));

gchar *g_filename_from_uri (const gchar *uri,
       gchar **hostname,
       GError **error) __attribute__((__malloc__));

gchar *g_filename_to_uri (const gchar *filename,
       const gchar *hostname,
       GError **error) __attribute__((__malloc__));
gchar *g_filename_display_name (const gchar *filename) __attribute__((__malloc__));
gboolean g_get_filename_charsets (const gchar ***charsets);

gchar *g_filename_display_basename (const gchar *filename) __attribute__((__malloc__));

gchar **g_uri_list_extract_uris (const gchar *uri_list) __attribute__((__malloc__));


# 44 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gdataset.h" 1
# 34 "/usr/include/glib-2.0/glib/gdataset.h"
# 1 "/usr/include/glib-2.0/glib/gquark.h" 1
# 35 "/usr/include/glib-2.0/glib/gdataset.h" 2



typedef struct _GData GData;

typedef void (*GDataForeachFunc) (GQuark key_id,
                                                 gpointer data,
                                                 gpointer user_data);



void g_datalist_init (GData **datalist);
void g_datalist_clear (GData **datalist);
gpointer g_datalist_id_get_data (GData **datalist,
      GQuark key_id);
void g_datalist_id_set_data_full (GData **datalist,
      GQuark key_id,
      gpointer data,
      GDestroyNotify destroy_func);
gpointer g_datalist_id_remove_no_notify (GData **datalist,
      GQuark key_id);
void g_datalist_foreach (GData **datalist,
      GDataForeachFunc func,
      gpointer user_data);
# 69 "/usr/include/glib-2.0/glib/gdataset.h"
void g_datalist_set_flags (GData **datalist,
      guint flags);
void g_datalist_unset_flags (GData **datalist,
      guint flags);
guint g_datalist_get_flags (GData **datalist);
# 91 "/usr/include/glib-2.0/glib/gdataset.h"
void g_dataset_destroy (gconstpointer dataset_location);
gpointer g_dataset_id_get_data (gconstpointer dataset_location,
                                         GQuark key_id);
gpointer g_datalist_get_data (GData **datalist,
       const gchar *key);
void g_dataset_id_set_data_full (gconstpointer dataset_location,
                                         GQuark key_id,
                                         gpointer data,
                                         GDestroyNotify destroy_func);
gpointer g_dataset_id_remove_no_notify (gconstpointer dataset_location,
                                         GQuark key_id);
void g_dataset_foreach (gconstpointer dataset_location,
                                         GDataForeachFunc func,
                                         gpointer user_data);
# 120 "/usr/include/glib-2.0/glib/gdataset.h"

# 45 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gdate.h" 1
# 36 "/usr/include/glib-2.0/glib/gdate.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 37 "/usr/include/glib-2.0/glib/gdate.h" 2
# 1 "/usr/include/glib-2.0/glib/gquark.h" 1
# 38 "/usr/include/glib-2.0/glib/gdate.h" 2


# 50 "/usr/include/glib-2.0/glib/gdate.h"
typedef gint32 GTime;
typedef guint16 GDateYear;
typedef guint8 GDateDay;
typedef struct _GDate GDate;


typedef enum
{
  G_DATE_DAY = 0,
  G_DATE_MONTH = 1,
  G_DATE_YEAR = 2
} GDateDMY;


typedef enum
{
  G_DATE_BAD_WEEKDAY = 0,
  G_DATE_MONDAY = 1,
  G_DATE_TUESDAY = 2,
  G_DATE_WEDNESDAY = 3,
  G_DATE_THURSDAY = 4,
  G_DATE_FRIDAY = 5,
  G_DATE_SATURDAY = 6,
  G_DATE_SUNDAY = 7
} GDateWeekday;
typedef enum
{
  G_DATE_BAD_MONTH = 0,
  G_DATE_JANUARY = 1,
  G_DATE_FEBRUARY = 2,
  G_DATE_MARCH = 3,
  G_DATE_APRIL = 4,
  G_DATE_MAY = 5,
  G_DATE_JUNE = 6,
  G_DATE_JULY = 7,
  G_DATE_AUGUST = 8,
  G_DATE_SEPTEMBER = 9,
  G_DATE_OCTOBER = 10,
  G_DATE_NOVEMBER = 11,
  G_DATE_DECEMBER = 12
} GDateMonth;
# 101 "/usr/include/glib-2.0/glib/gdate.h"
struct _GDate
{
  guint julian_days : 32;





  guint julian : 1;
  guint dmy : 1;


  guint day : 6;
  guint month : 4;
  guint year : 16;
};





GDate* g_date_new (void);
GDate* g_date_new_dmy (GDateDay day,
                                           GDateMonth month,
                                           GDateYear year);
GDate* g_date_new_julian (guint32 julian_day);
void g_date_free (GDate *date);






gboolean g_date_valid (const GDate *date);
gboolean g_date_valid_day (GDateDay day) __attribute__((__const__));
gboolean g_date_valid_month (GDateMonth month) __attribute__((__const__));
gboolean g_date_valid_year (GDateYear year) __attribute__((__const__));
gboolean g_date_valid_weekday (GDateWeekday weekday) __attribute__((__const__));
gboolean g_date_valid_julian (guint32 julian_date) __attribute__((__const__));
gboolean g_date_valid_dmy (GDateDay day,
                                           GDateMonth month,
                                           GDateYear year) __attribute__((__const__));

GDateWeekday g_date_get_weekday (const GDate *date);
GDateMonth g_date_get_month (const GDate *date);
GDateYear g_date_get_year (const GDate *date);
GDateDay g_date_get_day (const GDate *date);
guint32 g_date_get_julian (const GDate *date);
guint g_date_get_day_of_year (const GDate *date);






guint g_date_get_monday_week_of_year (const GDate *date);
guint g_date_get_sunday_week_of_year (const GDate *date);
guint g_date_get_iso8601_week_of_year (const GDate *date);





void g_date_clear (GDate *date,
                                           guint n_dates);





void g_date_set_parse (GDate *date,
                                           const gchar *str);
void g_date_set_time_t (GDate *date,
        time_t timet);
void g_date_set_time_val (GDate *date,
        GTimeVal *timeval);

__attribute__((__deprecated__("Use '" "g_date_set_time_t" "' instead")))
void g_date_set_time (GDate *date,
                                           GTime time_);

void g_date_set_month (GDate *date,
                                           GDateMonth month);
void g_date_set_day (GDate *date,
                                           GDateDay day);
void g_date_set_year (GDate *date,
                                           GDateYear year);
void g_date_set_dmy (GDate *date,
                                           GDateDay day,
                                           GDateMonth month,
                                           GDateYear y);
void g_date_set_julian (GDate *date,
                                           guint32 julian_date);
gboolean g_date_is_first_of_month (const GDate *date);
gboolean g_date_is_last_of_month (const GDate *date);


void g_date_add_days (GDate *date,
                                           guint n_days);
void g_date_subtract_days (GDate *date,
                                           guint n_days);


void g_date_add_months (GDate *date,
                                           guint n_months);
void g_date_subtract_months (GDate *date,
                                           guint n_months);


void g_date_add_years (GDate *date,
                                           guint n_years);
void g_date_subtract_years (GDate *date,
                                           guint n_years);
gboolean g_date_is_leap_year (GDateYear year) __attribute__((__const__));
guint8 g_date_get_days_in_month (GDateMonth month,
                                           GDateYear year) __attribute__((__const__));
guint8 g_date_get_monday_weeks_in_year (GDateYear year) __attribute__((__const__));
guint8 g_date_get_sunday_weeks_in_year (GDateYear year) __attribute__((__const__));



gint g_date_days_between (const GDate *date1,
        const GDate *date2);


gint g_date_compare (const GDate *lhs,
                                           const GDate *rhs);
void g_date_to_struct_tm (const GDate *date,
                                           struct tm *tm);

void g_date_clamp (GDate *date,
        const GDate *min_date,
        const GDate *max_date);


void g_date_order (GDate *date1, GDate *date2);




gsize g_date_strftime (gchar *s,
                                           gsize slen,
                                           const gchar *format,
                                           const GDate *date);
# 262 "/usr/include/glib-2.0/glib/gdate.h"

# 46 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gdatetime.h" 1
# 33 "/usr/include/glib-2.0/glib/gdatetime.h"
# 1 "/usr/include/glib-2.0/glib/gtimezone.h" 1
# 29 "/usr/include/glib-2.0/glib/gtimezone.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 30 "/usr/include/glib-2.0/glib/gtimezone.h" 2



typedef struct _GTimeZone GTimeZone;
# 50 "/usr/include/glib-2.0/glib/gtimezone.h"
typedef enum
{
  G_TIME_TYPE_STANDARD,
  G_TIME_TYPE_DAYLIGHT,
  G_TIME_TYPE_UNIVERSAL
} GTimeType;

GTimeZone * g_time_zone_new (const gchar *identifier);
GTimeZone * g_time_zone_new_utc (void);
GTimeZone * g_time_zone_new_local (void);

GTimeZone * g_time_zone_ref (GTimeZone *tz);
void g_time_zone_unref (GTimeZone *tz);

gint g_time_zone_find_interval (GTimeZone *tz,
                                                                         GTimeType type,
                                                                         gint64 time_);

gint g_time_zone_adjust_time (GTimeZone *tz,
                                                                         GTimeType type,
                                                                         gint64 *time_);

const gchar * g_time_zone_get_abbreviation (GTimeZone *tz,
                                                                         gint interval);
gint32 g_time_zone_get_offset (GTimeZone *tz,
                                                                         gint interval);
gboolean g_time_zone_is_dst (GTimeZone *tz,
                                                                         gint interval);


# 34 "/usr/include/glib-2.0/glib/gdatetime.h" 2


# 89 "/usr/include/glib-2.0/glib/gdatetime.h"
typedef gint64 GTimeSpan;
# 99 "/usr/include/glib-2.0/glib/gdatetime.h"
typedef struct _GDateTime GDateTime;

void g_date_time_unref (GDateTime *datetime);
GDateTime * g_date_time_ref (GDateTime *datetime);

GDateTime * g_date_time_new_now (GTimeZone *tz);
GDateTime * g_date_time_new_now_local (void);
GDateTime * g_date_time_new_now_utc (void);

GDateTime * g_date_time_new_from_unix_local (gint64 t);
GDateTime * g_date_time_new_from_unix_utc (gint64 t);

GDateTime * g_date_time_new_from_timeval_local (const GTimeVal *tv);
GDateTime * g_date_time_new_from_timeval_utc (const GTimeVal *tv);

GDateTime * g_date_time_new (GTimeZone *tz,
                                                                         gint year,
                                                                         gint month,
                                                                         gint day,
                                                                         gint hour,
                                                                         gint minute,
                                                                         gdouble seconds);
GDateTime * g_date_time_new_local (gint year,
                                                                         gint month,
                                                                         gint day,
                                                                         gint hour,
                                                                         gint minute,
                                                                         gdouble seconds);
GDateTime * g_date_time_new_utc (gint year,
                                                                         gint month,
                                                                         gint day,
                                                                         gint hour,
                                                                         gint minute,
                                                                         gdouble seconds);

__attribute__((warn_unused_result))
GDateTime * g_date_time_add (GDateTime *datetime,
                                                                         GTimeSpan timespan);

__attribute__((warn_unused_result))
GDateTime * g_date_time_add_years (GDateTime *datetime,
                                                                         gint years);
__attribute__((warn_unused_result))
GDateTime * g_date_time_add_months (GDateTime *datetime,
                                                                         gint months);
__attribute__((warn_unused_result))
GDateTime * g_date_time_add_weeks (GDateTime *datetime,
                                                                         gint weeks);
__attribute__((warn_unused_result))
GDateTime * g_date_time_add_days (GDateTime *datetime,
                                                                         gint days);

__attribute__((warn_unused_result))
GDateTime * g_date_time_add_hours (GDateTime *datetime,
                                                                         gint hours);
__attribute__((warn_unused_result))
GDateTime * g_date_time_add_minutes (GDateTime *datetime,
                                                                         gint minutes);
__attribute__((warn_unused_result))
GDateTime * g_date_time_add_seconds (GDateTime *datetime,
                                                                         gdouble seconds);

__attribute__((warn_unused_result))
GDateTime * g_date_time_add_full (GDateTime *datetime,
                                                                         gint years,
                                                                         gint months,
                                                                         gint days,
                                                                         gint hours,
                                                                         gint minutes,
                                                                         gdouble seconds);

gint g_date_time_compare (gconstpointer dt1,
                                                                         gconstpointer dt2);
GTimeSpan g_date_time_difference (GDateTime *end,
                                                                         GDateTime *begin);
guint g_date_time_hash (gconstpointer datetime);
gboolean g_date_time_equal (gconstpointer dt1,
                                                                         gconstpointer dt2);

void g_date_time_get_ymd (GDateTime *datetime,
                                                                         gint *year,
                                                                         gint *month,
                                                                         gint *day);

gint g_date_time_get_year (GDateTime *datetime);
gint g_date_time_get_month (GDateTime *datetime);
gint g_date_time_get_day_of_month (GDateTime *datetime);

gint g_date_time_get_week_numbering_year (GDateTime *datetime);
gint g_date_time_get_week_of_year (GDateTime *datetime);
gint g_date_time_get_day_of_week (GDateTime *datetime);

gint g_date_time_get_day_of_year (GDateTime *datetime);

gint g_date_time_get_hour (GDateTime *datetime);
gint g_date_time_get_minute (GDateTime *datetime);
gint g_date_time_get_second (GDateTime *datetime);
gint g_date_time_get_microsecond (GDateTime *datetime);
gdouble g_date_time_get_seconds (GDateTime *datetime);

gint64 g_date_time_to_unix (GDateTime *datetime);
gboolean g_date_time_to_timeval (GDateTime *datetime,
                                                                         GTimeVal *tv);

GTimeSpan g_date_time_get_utc_offset (GDateTime *datetime);
const gchar * g_date_time_get_timezone_abbreviation (GDateTime *datetime);
gboolean g_date_time_is_daylight_savings (GDateTime *datetime);

GDateTime * g_date_time_to_timezone (GDateTime *datetime,
                                                                         GTimeZone *tz);
GDateTime * g_date_time_to_local (GDateTime *datetime);
GDateTime * g_date_time_to_utc (GDateTime *datetime);

gchar * g_date_time_format (GDateTime *datetime,
                                                                         const gchar *format) __attribute__((__malloc__));


# 47 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gdir.h" 1
# 31 "/usr/include/glib-2.0/glib/gdir.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 32 "/usr/include/glib-2.0/glib/gdir.h" 2



typedef struct _GDir GDir;
# 45 "/usr/include/glib-2.0/glib/gdir.h"
GDir * g_dir_open (const gchar *path,
            guint flags,
            GError **error);
const gchar * g_dir_read_name (GDir *dir);
void g_dir_rewind (GDir *dir);
void g_dir_close (GDir *dir);


# 48 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/genviron.h" 1
# 34 "/usr/include/glib-2.0/glib/genviron.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/genviron.h" 2


# 46 "/usr/include/glib-2.0/glib/genviron.h"
const gchar * g_getenv (const gchar *variable);
gboolean g_setenv (const gchar *variable,
                                  const gchar *value,
                                  gboolean overwrite);
void g_unsetenv (const gchar *variable);
gchar ** g_listenv (void);

gchar ** g_get_environ (void);
const gchar * g_environ_getenv (gchar **envp,
                                  const gchar *variable);
gchar ** g_environ_setenv (gchar **envp,
                                  const gchar *variable,
                                  const gchar *value,
                                  gboolean overwrite) __attribute__((warn_unused_result));
gchar ** g_environ_unsetenv (gchar **envp,
                                  const gchar *variable) __attribute__((warn_unused_result));


# 49 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 50 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gfileutils.h" 1
# 28 "/usr/include/glib-2.0/glib/gfileutils.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 29 "/usr/include/glib-2.0/glib/gfileutils.h" 2





typedef enum
{
  G_FILE_ERROR_EXIST,
  G_FILE_ERROR_ISDIR,
  G_FILE_ERROR_ACCES,
  G_FILE_ERROR_NAMETOOLONG,
  G_FILE_ERROR_NOENT,
  G_FILE_ERROR_NOTDIR,
  G_FILE_ERROR_NXIO,
  G_FILE_ERROR_NODEV,
  G_FILE_ERROR_ROFS,
  G_FILE_ERROR_TXTBSY,
  G_FILE_ERROR_FAULT,
  G_FILE_ERROR_LOOP,
  G_FILE_ERROR_NOSPC,
  G_FILE_ERROR_NOMEM,
  G_FILE_ERROR_MFILE,
  G_FILE_ERROR_NFILE,
  G_FILE_ERROR_BADF,
  G_FILE_ERROR_INVAL,
  G_FILE_ERROR_PIPE,
  G_FILE_ERROR_AGAIN,
  G_FILE_ERROR_INTR,
  G_FILE_ERROR_IO,
  G_FILE_ERROR_PERM,
  G_FILE_ERROR_NOSYS,
  G_FILE_ERROR_FAILED
} GFileError;





typedef enum
{
  G_FILE_TEST_IS_REGULAR = 1 << 0,
  G_FILE_TEST_IS_SYMLINK = 1 << 1,
  G_FILE_TEST_IS_DIR = 1 << 2,
  G_FILE_TEST_IS_EXECUTABLE = 1 << 3,
  G_FILE_TEST_EXISTS = 1 << 4
} GFileTest;

GQuark g_file_error_quark (void);

GFileError g_file_error_from_errno (gint err_no);
# 89 "/usr/include/glib-2.0/glib/gfileutils.h"
gboolean g_file_test (const gchar *filename,
                              GFileTest test);
gboolean g_file_get_contents (const gchar *filename,
                              gchar **contents,
                              gsize *length,
                              GError **error);
gboolean g_file_set_contents (const gchar *filename,
                              const gchar *contents,
                              gssize length,
                              GError **error);
gchar *g_file_read_link (const gchar *filename,
                              GError **error);



gchar *g_mkdtemp (gchar *tmpl);

gchar *g_mkdtemp_full (gchar *tmpl,
                               gint mode);


gint g_mkstemp (gchar *tmpl);
gint g_mkstemp_full (gchar *tmpl,
                               gint flags,
                               gint mode);


gint g_file_open_tmp (const gchar *tmpl,
                               gchar **name_used,
                               GError **error);

gchar *g_dir_make_tmp (const gchar *tmpl,
                               GError **error);

gchar *g_build_path (const gchar *separator,
                               const gchar *first_element,
                               ...) __attribute__((__malloc__)) __attribute__((__sentinel__));
gchar *g_build_pathv (const gchar *separator,
                               gchar **args) __attribute__((__malloc__));

gchar *g_build_filename (const gchar *first_element,
                               ...) __attribute__((__malloc__)) __attribute__((__sentinel__));
gchar *g_build_filenamev (gchar **args) __attribute__((__malloc__));

gint g_mkdir_with_parents (const gchar *pathname,
                               gint mode);
# 158 "/usr/include/glib-2.0/glib/gfileutils.h"
gboolean g_path_is_absolute (const gchar *file_name);
const gchar *g_path_skip_root (const gchar *file_name);

__attribute__((__deprecated__("Use '" "g_path_get_basename" "' instead")))
const gchar *g_basename (const gchar *file_name);
# 173 "/usr/include/glib-2.0/glib/gfileutils.h"
gchar *g_get_current_dir (void);
gchar *g_path_get_basename (const gchar *file_name) __attribute__((__malloc__));
gchar *g_path_get_dirname (const gchar *file_name) __attribute__((__malloc__));


# 51 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/ggettext.h" 1
# 34 "/usr/include/glib-2.0/glib/ggettext.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/ggettext.h" 2



const gchar *g_strip_context (const gchar *msgid,
                              const gchar *msgval) __attribute__((__format_arg__ (1)));

const gchar *g_dgettext (const gchar *domain,
                              const gchar *msgid) __attribute__((__format_arg__ (2)));
const gchar *g_dcgettext (const gchar *domain,
                              const gchar *msgid,
                              gint category) __attribute__((__format_arg__ (2)));
const gchar *g_dngettext (const gchar *domain,
                              const gchar *msgid,
                              const gchar *msgid_plural,
                              gulong n) __attribute__((__format_arg__ (3)));
const gchar *g_dpgettext (const gchar *domain,
                              const gchar *msgctxtid,
                              gsize msgidoffset) __attribute__((__format_arg__ (2)));
const gchar *g_dpgettext2 (const gchar *domain,
                              const gchar *context,
                              const gchar *msgid) __attribute__((__format_arg__ (3)));


# 52 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/ghash.h" 1
# 34 "/usr/include/glib-2.0/glib/ghash.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/ghash.h" 2
# 1 "/usr/include/glib-2.0/glib/glist.h" 1
# 34 "/usr/include/glib-2.0/glib/glist.h"
# 1 "/usr/include/glib-2.0/glib/gmem.h" 1
# 34 "/usr/include/glib-2.0/glib/gmem.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gmem.h" 2


# 51 "/usr/include/glib-2.0/glib/gmem.h"
typedef struct _GMemVTable GMemVTable;
# 70 "/usr/include/glib-2.0/glib/gmem.h"
void g_free (gpointer mem);

gpointer g_malloc (gsize n_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_malloc0 (gsize n_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_realloc (gpointer mem,
      gsize n_bytes) __attribute__((warn_unused_result));
gpointer g_try_malloc (gsize n_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_try_malloc0 (gsize n_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_try_realloc (gpointer mem,
      gsize n_bytes) __attribute__((warn_unused_result));

gpointer g_malloc_n (gsize n_blocks,
      gsize n_block_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1,2)));
gpointer g_malloc0_n (gsize n_blocks,
      gsize n_block_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1,2)));
gpointer g_realloc_n (gpointer mem,
      gsize n_blocks,
      gsize n_block_bytes) __attribute__((warn_unused_result));
gpointer g_try_malloc_n (gsize n_blocks,
      gsize n_block_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1,2)));
gpointer g_try_malloc0_n (gsize n_blocks,
      gsize n_block_bytes) __attribute__((__malloc__)) __attribute__((__alloc_size__(1,2)));
gpointer g_try_realloc_n (gpointer mem,
      gsize n_blocks,
      gsize n_block_bytes) __attribute__((warn_unused_result));
# 240 "/usr/include/glib-2.0/glib/gmem.h"
struct _GMemVTable {
  gpointer (*malloc) (gsize n_bytes);
  gpointer (*realloc) (gpointer mem,
      gsize n_bytes);
  void (*free) (gpointer mem);

  gpointer (*calloc) (gsize n_blocks,
      gsize n_block_bytes);
  gpointer (*try_malloc) (gsize n_bytes);
  gpointer (*try_realloc) (gpointer mem,
      gsize n_bytes);
};
void g_mem_set_vtable (GMemVTable *vtable);
gboolean g_mem_is_system_malloc (void);

extern gboolean g_mem_gc_friendly;



extern GMemVTable *glib_mem_profiler_table;
void g_mem_profile (void);


# 35 "/usr/include/glib-2.0/glib/glist.h" 2



typedef struct _GList GList;

struct _GList
{
  gpointer data;
  GList *next;
  GList *prev;
};



GList* g_list_alloc (void) __attribute__((warn_unused_result));
void g_list_free (GList *list);
void g_list_free_1 (GList *list);

void g_list_free_full (GList *list,
      GDestroyNotify free_func);
GList* g_list_append (GList *list,
      gpointer data) __attribute__((warn_unused_result));
GList* g_list_prepend (GList *list,
      gpointer data) __attribute__((warn_unused_result));
GList* g_list_insert (GList *list,
      gpointer data,
      gint position) __attribute__((warn_unused_result));
GList* g_list_insert_sorted (GList *list,
      gpointer data,
      GCompareFunc func) __attribute__((warn_unused_result));
GList* g_list_insert_sorted_with_data (GList *list,
      gpointer data,
      GCompareDataFunc func,
      gpointer user_data) __attribute__((warn_unused_result));
GList* g_list_insert_before (GList *list,
      GList *sibling,
      gpointer data) __attribute__((warn_unused_result));
GList* g_list_concat (GList *list1,
      GList *list2) __attribute__((warn_unused_result));
GList* g_list_remove (GList *list,
      gconstpointer data) __attribute__((warn_unused_result));
GList* g_list_remove_all (GList *list,
      gconstpointer data) __attribute__((warn_unused_result));
GList* g_list_remove_link (GList *list,
      GList *llink) __attribute__((warn_unused_result));
GList* g_list_delete_link (GList *list,
      GList *link_) __attribute__((warn_unused_result));
GList* g_list_reverse (GList *list) __attribute__((warn_unused_result));
GList* g_list_copy (GList *list) __attribute__((warn_unused_result));
GList* g_list_nth (GList *list,
      guint n);
GList* g_list_nth_prev (GList *list,
      guint n);
GList* g_list_find (GList *list,
      gconstpointer data);
GList* g_list_find_custom (GList *list,
      gconstpointer data,
      GCompareFunc func);
gint g_list_position (GList *list,
      GList *llink);
gint g_list_index (GList *list,
      gconstpointer data);
GList* g_list_last (GList *list);
GList* g_list_first (GList *list);
guint g_list_length (GList *list);
void g_list_foreach (GList *list,
      GFunc func,
      gpointer user_data);
GList* g_list_sort (GList *list,
      GCompareFunc compare_func) __attribute__((warn_unused_result));
GList* g_list_sort_with_data (GList *list,
      GCompareDataFunc compare_func,
      gpointer user_data) __attribute__((warn_unused_result));
gpointer g_list_nth_data (GList *list,
      guint n);






# 36 "/usr/include/glib-2.0/glib/ghash.h" 2



typedef struct _GHashTable GHashTable;

typedef gboolean (*GHRFunc) (gpointer key,
                               gpointer value,
                               gpointer user_data);

typedef struct _GHashTableIter GHashTableIter;

struct _GHashTableIter
{

  gpointer dummy1;
  gpointer dummy2;
  gpointer dummy3;
  int dummy4;
  gboolean dummy5;
  gpointer dummy6;
};

GHashTable* g_hash_table_new (GHashFunc hash_func,
                                            GEqualFunc key_equal_func);
GHashTable* g_hash_table_new_full (GHashFunc hash_func,
                                            GEqualFunc key_equal_func,
                                            GDestroyNotify key_destroy_func,
                                            GDestroyNotify value_destroy_func);
void g_hash_table_destroy (GHashTable *hash_table);
void g_hash_table_insert (GHashTable *hash_table,
                                            gpointer key,
                                            gpointer value);
void g_hash_table_replace (GHashTable *hash_table,
                                            gpointer key,
                                            gpointer value);
void g_hash_table_add (GHashTable *hash_table,
                                            gpointer key);
gboolean g_hash_table_remove (GHashTable *hash_table,
                                            gconstpointer key);
void g_hash_table_remove_all (GHashTable *hash_table);
gboolean g_hash_table_steal (GHashTable *hash_table,
                                            gconstpointer key);
void g_hash_table_steal_all (GHashTable *hash_table);
gpointer g_hash_table_lookup (GHashTable *hash_table,
                                            gconstpointer key);
gboolean g_hash_table_contains (GHashTable *hash_table,
                                            gconstpointer key);
gboolean g_hash_table_lookup_extended (GHashTable *hash_table,
                                            gconstpointer lookup_key,
                                            gpointer *orig_key,
                                            gpointer *value);
void g_hash_table_foreach (GHashTable *hash_table,
                                            GHFunc func,
                                            gpointer user_data);
gpointer g_hash_table_find (GHashTable *hash_table,
                                            GHRFunc predicate,
                                            gpointer user_data);
guint g_hash_table_foreach_remove (GHashTable *hash_table,
                                            GHRFunc func,
                                            gpointer user_data);
guint g_hash_table_foreach_steal (GHashTable *hash_table,
                                            GHRFunc func,
                                            gpointer user_data);
guint g_hash_table_size (GHashTable *hash_table);
GList * g_hash_table_get_keys (GHashTable *hash_table);
GList * g_hash_table_get_values (GHashTable *hash_table);

void g_hash_table_iter_init (GHashTableIter *iter,
                                            GHashTable *hash_table);
gboolean g_hash_table_iter_next (GHashTableIter *iter,
                                            gpointer *key,
                                            gpointer *value);
GHashTable* g_hash_table_iter_get_hash_table (GHashTableIter *iter);
void g_hash_table_iter_remove (GHashTableIter *iter);

void g_hash_table_iter_replace (GHashTableIter *iter,
                                            gpointer value);
void g_hash_table_iter_steal (GHashTableIter *iter);

GHashTable* g_hash_table_ref (GHashTable *hash_table);
void g_hash_table_unref (GHashTable *hash_table);
# 125 "/usr/include/glib-2.0/glib/ghash.h"
gboolean g_str_equal (gconstpointer v1,
                         gconstpointer v2);
guint g_str_hash (gconstpointer v);

gboolean g_int_equal (gconstpointer v1,
                         gconstpointer v2);
guint g_int_hash (gconstpointer v);

gboolean g_int64_equal (gconstpointer v1,
                         gconstpointer v2);
guint g_int64_hash (gconstpointer v);

gboolean g_double_equal (gconstpointer v1,
                         gconstpointer v2);
guint g_double_hash (gconstpointer v);

guint g_direct_hash (gconstpointer v) __attribute__((__const__));
gboolean g_direct_equal (gconstpointer v1,
                         gconstpointer v2) __attribute__((__const__));


# 53 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/ghmac.h" 1
# 28 "/usr/include/glib-2.0/glib/ghmac.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 29 "/usr/include/glib-2.0/glib/ghmac.h" 2
# 1 "/usr/include/glib-2.0/glib/gchecksum.h" 1
# 30 "/usr/include/glib-2.0/glib/ghmac.h" 2


# 42 "/usr/include/glib-2.0/glib/ghmac.h"
typedef struct _GHmac GHmac;


GHmac * g_hmac_new (GChecksumType digest_type,
                                                     const guchar *key,
                                                     gsize key_len);

GHmac * g_hmac_copy (const GHmac *hmac);

GHmac * g_hmac_ref (GHmac *hmac);

void g_hmac_unref (GHmac *hmac);

void g_hmac_update (GHmac *hmac,
                                                     const guchar *data,
                                                     gssize length);

const gchar * g_hmac_get_string (GHmac *hmac);

void g_hmac_get_digest (GHmac *hmac,
                                                     guint8 *buffer,
                                                     gsize *digest_len);


gchar *g_compute_hmac_for_data (GChecksumType digest_type,
                                                     const guchar *key,
                                                     gsize key_len,
                                                     const guchar *data,
                                                     gsize length);

gchar *g_compute_hmac_for_string (GChecksumType digest_type,
                                                     const guchar *key,
                                                     gsize key_len,
                                                     const gchar *str,
                                                     gssize length);


# 54 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/ghook.h" 1
# 34 "/usr/include/glib-2.0/glib/ghook.h"
# 1 "/usr/include/glib-2.0/glib/gmem.h" 1
# 35 "/usr/include/glib-2.0/glib/ghook.h" 2





typedef struct _GHook GHook;
typedef struct _GHookList GHookList;

typedef gint (*GHookCompareFunc) (GHook *new_hook,
       GHook *sibling);
typedef gboolean (*GHookFindFunc) (GHook *hook,
       gpointer data);
typedef void (*GHookMarshaller) (GHook *hook,
       gpointer marshal_data);
typedef gboolean (*GHookCheckMarshaller) (GHook *hook,
       gpointer marshal_data);
typedef void (*GHookFunc) (gpointer data);
typedef gboolean (*GHookCheckFunc) (gpointer data);
typedef void (*GHookFinalizeFunc) (GHookList *hook_list,
       GHook *hook);
typedef enum
{
  G_HOOK_FLAG_ACTIVE = 1 << 0,
  G_HOOK_FLAG_IN_CALL = 1 << 1,
  G_HOOK_FLAG_MASK = 0x0f
} GHookFlagMask;




struct _GHookList
{
  gulong seq_id;
  guint hook_size : 16;
  guint is_setup : 1;
  GHook *hooks;
  gpointer dummy3;
  GHookFinalizeFunc finalize_hook;
  gpointer dummy[2];
};
struct _GHook
{
  gpointer data;
  GHook *next;
  GHook *prev;
  guint ref_count;
  gulong hook_id;
  guint flags;
  gpointer func;
  GDestroyNotify destroy;
};
# 106 "/usr/include/glib-2.0/glib/ghook.h"
void g_hook_list_init (GHookList *hook_list,
      guint hook_size);
void g_hook_list_clear (GHookList *hook_list);
GHook* g_hook_alloc (GHookList *hook_list);
void g_hook_free (GHookList *hook_list,
      GHook *hook);
GHook * g_hook_ref (GHookList *hook_list,
      GHook *hook);
void g_hook_unref (GHookList *hook_list,
      GHook *hook);
gboolean g_hook_destroy (GHookList *hook_list,
      gulong hook_id);
void g_hook_destroy_link (GHookList *hook_list,
      GHook *hook);
void g_hook_prepend (GHookList *hook_list,
      GHook *hook);
void g_hook_insert_before (GHookList *hook_list,
      GHook *sibling,
      GHook *hook);
void g_hook_insert_sorted (GHookList *hook_list,
      GHook *hook,
      GHookCompareFunc func);
GHook* g_hook_get (GHookList *hook_list,
      gulong hook_id);
GHook* g_hook_find (GHookList *hook_list,
      gboolean need_valids,
      GHookFindFunc func,
      gpointer data);
GHook* g_hook_find_data (GHookList *hook_list,
      gboolean need_valids,
      gpointer data);
GHook* g_hook_find_func (GHookList *hook_list,
      gboolean need_valids,
      gpointer func);
GHook* g_hook_find_func_data (GHookList *hook_list,
      gboolean need_valids,
      gpointer func,
      gpointer data);

GHook* g_hook_first_valid (GHookList *hook_list,
      gboolean may_be_in_call);



GHook* g_hook_next_valid (GHookList *hook_list,
      GHook *hook,
      gboolean may_be_in_call);

gint g_hook_compare_ids (GHook *new_hook,
      GHook *sibling);





void g_hook_list_invoke (GHookList *hook_list,
      gboolean may_recurse);



void g_hook_list_invoke_check (GHookList *hook_list,
      gboolean may_recurse);


void g_hook_list_marshal (GHookList *hook_list,
      gboolean may_recurse,
      GHookMarshaller marshaller,
      gpointer marshal_data);
void g_hook_list_marshal_check (GHookList *hook_list,
      gboolean may_recurse,
      GHookCheckMarshaller marshaller,
      gpointer marshal_data);


# 55 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/ghostutils.h" 1
# 27 "/usr/include/glib-2.0/glib/ghostutils.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 28 "/usr/include/glib-2.0/glib/ghostutils.h" 2



gboolean g_hostname_is_non_ascii (const gchar *hostname);
gboolean g_hostname_is_ascii_encoded (const gchar *hostname);
gboolean g_hostname_is_ip_address (const gchar *hostname);

gchar *g_hostname_to_ascii (const gchar *hostname);
gchar *g_hostname_to_unicode (const gchar *hostname);


# 56 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/giochannel.h" 1
# 34 "/usr/include/glib-2.0/glib/giochannel.h"
# 1 "/usr/include/glib-2.0/glib/gconvert.h" 1
# 35 "/usr/include/glib-2.0/glib/giochannel.h" 2
# 1 "/usr/include/glib-2.0/glib/gmain.h" 1
# 27 "/usr/include/glib-2.0/glib/gmain.h"
# 1 "/usr/include/glib-2.0/glib/gpoll.h" 1
# 27 "/usr/include/glib-2.0/glib/gpoll.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 28 "/usr/include/glib-2.0/glib/gpoll.h" 2


# 61 "/usr/include/glib-2.0/glib/gpoll.h"
typedef struct _GPollFD GPollFD;
# 76 "/usr/include/glib-2.0/glib/gpoll.h"
typedef gint (*GPollFunc) (GPollFD *ufds,
                                 guint nfsd,
                                 gint timeout_);
# 93 "/usr/include/glib-2.0/glib/gpoll.h"
struct _GPollFD
{



  gint fd;

  gushort events;
  gushort revents;
};
# 114 "/usr/include/glib-2.0/glib/gpoll.h"
gint g_poll (GPollFD *fds,
      guint nfds,
      gint timeout);


# 28 "/usr/include/glib-2.0/glib/gmain.h" 2
# 1 "/usr/include/glib-2.0/glib/gslist.h" 1
# 34 "/usr/include/glib-2.0/glib/gslist.h"
# 1 "/usr/include/glib-2.0/glib/gmem.h" 1
# 35 "/usr/include/glib-2.0/glib/gslist.h" 2



typedef struct _GSList GSList;

struct _GSList
{
  gpointer data;
  GSList *next;
};



GSList* g_slist_alloc (void) __attribute__((warn_unused_result));
void g_slist_free (GSList *list);
void g_slist_free_1 (GSList *list);

void g_slist_free_full (GSList *list,
       GDestroyNotify free_func);
GSList* g_slist_append (GSList *list,
       gpointer data) __attribute__((warn_unused_result));
GSList* g_slist_prepend (GSList *list,
       gpointer data) __attribute__((warn_unused_result));
GSList* g_slist_insert (GSList *list,
       gpointer data,
       gint position) __attribute__((warn_unused_result));
GSList* g_slist_insert_sorted (GSList *list,
       gpointer data,
       GCompareFunc func) __attribute__((warn_unused_result));
GSList* g_slist_insert_sorted_with_data (GSList *list,
       gpointer data,
       GCompareDataFunc func,
       gpointer user_data) __attribute__((warn_unused_result));
GSList* g_slist_insert_before (GSList *slist,
       GSList *sibling,
       gpointer data) __attribute__((warn_unused_result));
GSList* g_slist_concat (GSList *list1,
       GSList *list2) __attribute__((warn_unused_result));
GSList* g_slist_remove (GSList *list,
       gconstpointer data) __attribute__((warn_unused_result));
GSList* g_slist_remove_all (GSList *list,
       gconstpointer data) __attribute__((warn_unused_result));
GSList* g_slist_remove_link (GSList *list,
       GSList *link_) __attribute__((warn_unused_result));
GSList* g_slist_delete_link (GSList *list,
       GSList *link_) __attribute__((warn_unused_result));
GSList* g_slist_reverse (GSList *list) __attribute__((warn_unused_result));
GSList* g_slist_copy (GSList *list) __attribute__((warn_unused_result));
GSList* g_slist_nth (GSList *list,
       guint n);
GSList* g_slist_find (GSList *list,
       gconstpointer data);
GSList* g_slist_find_custom (GSList *list,
       gconstpointer data,
       GCompareFunc func);
gint g_slist_position (GSList *list,
       GSList *llink);
gint g_slist_index (GSList *list,
       gconstpointer data);
GSList* g_slist_last (GSList *list);
guint g_slist_length (GSList *list);
void g_slist_foreach (GSList *list,
       GFunc func,
       gpointer user_data);
GSList* g_slist_sort (GSList *list,
       GCompareFunc compare_func) __attribute__((warn_unused_result));
GSList* g_slist_sort_with_data (GSList *list,
       GCompareDataFunc compare_func,
       gpointer user_data) __attribute__((warn_unused_result));
gpointer g_slist_nth_data (GSList *list,
       guint n);




# 29 "/usr/include/glib-2.0/glib/gmain.h" 2
# 1 "/usr/include/glib-2.0/glib/gthread.h" 1
# 30 "/usr/include/glib-2.0/glib/gmain.h" 2









typedef struct _GMainContext GMainContext;







typedef struct _GMainLoop GMainLoop;







typedef struct _GSource GSource;
typedef struct _GSourcePrivate GSourcePrivate;
# 68 "/usr/include/glib-2.0/glib/gmain.h"
typedef struct _GSourceCallbackFuncs GSourceCallbackFuncs;
# 113 "/usr/include/glib-2.0/glib/gmain.h"
typedef struct _GSourceFuncs GSourceFuncs;
# 137 "/usr/include/glib-2.0/glib/gmain.h"
typedef gboolean (*GSourceFunc) (gpointer user_data);
# 148 "/usr/include/glib-2.0/glib/gmain.h"
typedef void (*GChildWatchFunc) (GPid pid,
                                       gint status,
                                       gpointer user_data);
struct _GSource
{

  gpointer callback_data;
  GSourceCallbackFuncs *callback_funcs;

  GSourceFuncs *source_funcs;
  guint ref_count;

  GMainContext *context;

  gint priority;
  guint flags;
  guint source_id;

  GSList *poll_fds;

  GSource *prev;
  GSource *next;

  char *name;

  GSourcePrivate *priv;
};

struct _GSourceCallbackFuncs
{
  void (*ref) (gpointer cb_data);
  void (*unref) (gpointer cb_data);
  void (*get) (gpointer cb_data,
                 GSource *source,
                 GSourceFunc *func,
                 gpointer *data);
};







typedef void (*GSourceDummyMarshal) (void);

struct _GSourceFuncs
{
  gboolean (*prepare) (GSource *source,
                        gint *timeout_);
  gboolean (*check) (GSource *source);
  gboolean (*dispatch) (GSource *source,
                        GSourceFunc callback,
                        gpointer user_data);
  void (*finalize) (GSource *source);



  GSourceFunc closure_callback;
  GSourceDummyMarshal closure_marshal;
};
# 285 "/usr/include/glib-2.0/glib/gmain.h"
GMainContext *g_main_context_new (void);
GMainContext *g_main_context_ref (GMainContext *context);
void g_main_context_unref (GMainContext *context);
GMainContext *g_main_context_default (void);

gboolean g_main_context_iteration (GMainContext *context,
                                        gboolean may_block);
gboolean g_main_context_pending (GMainContext *context);



GSource *g_main_context_find_source_by_id (GMainContext *context,
                                                             guint source_id);
GSource *g_main_context_find_source_by_user_data (GMainContext *context,
                                                             gpointer user_data);
GSource *g_main_context_find_source_by_funcs_user_data (GMainContext *context,
                                                             GSourceFuncs *funcs,
                                                             gpointer user_data);



void g_main_context_wakeup (GMainContext *context);
gboolean g_main_context_acquire (GMainContext *context);
void g_main_context_release (GMainContext *context);
gboolean g_main_context_is_owner (GMainContext *context);
gboolean g_main_context_wait (GMainContext *context,
                                 GCond *cond,
                                 GMutex *mutex);

gboolean g_main_context_prepare (GMainContext *context,
                                  gint *priority);
gint g_main_context_query (GMainContext *context,
                                  gint max_priority,
                                  gint *timeout_,
                                  GPollFD *fds,
                                  gint n_fds);
gint g_main_context_check (GMainContext *context,
                                  gint max_priority,
                                  GPollFD *fds,
                                  gint n_fds);
void g_main_context_dispatch (GMainContext *context);

void g_main_context_set_poll_func (GMainContext *context,
                                       GPollFunc func);
GPollFunc g_main_context_get_poll_func (GMainContext *context);



void g_main_context_add_poll (GMainContext *context,
                                     GPollFD *fd,
                                     gint priority);
void g_main_context_remove_poll (GMainContext *context,
                                     GPollFD *fd);

gint g_main_depth (void);
GSource *g_main_current_source (void);



void g_main_context_push_thread_default (GMainContext *context);
void g_main_context_pop_thread_default (GMainContext *context);
GMainContext *g_main_context_get_thread_default (void);
GMainContext *g_main_context_ref_thread_default (void);



GMainLoop *g_main_loop_new (GMainContext *context,
                                   gboolean is_running);
void g_main_loop_run (GMainLoop *loop);
void g_main_loop_quit (GMainLoop *loop);
GMainLoop *g_main_loop_ref (GMainLoop *loop);
void g_main_loop_unref (GMainLoop *loop);
gboolean g_main_loop_is_running (GMainLoop *loop);
GMainContext *g_main_loop_get_context (GMainLoop *loop);



GSource *g_source_new (GSourceFuncs *source_funcs,
                                   guint struct_size);
GSource *g_source_ref (GSource *source);
void g_source_unref (GSource *source);

guint g_source_attach (GSource *source,
                                   GMainContext *context);
void g_source_destroy (GSource *source);

void g_source_set_priority (GSource *source,
                                   gint priority);
gint g_source_get_priority (GSource *source);
void g_source_set_can_recurse (GSource *source,
                                   gboolean can_recurse);
gboolean g_source_get_can_recurse (GSource *source);
guint g_source_get_id (GSource *source);

GMainContext *g_source_get_context (GSource *source);

void g_source_set_callback (GSource *source,
                                   GSourceFunc func,
                                   gpointer data,
                                   GDestroyNotify notify);

void g_source_set_funcs (GSource *source,
                                   GSourceFuncs *funcs);
gboolean g_source_is_destroyed (GSource *source);

void g_source_set_name (GSource *source,
                                              const char *name);
const char * g_source_get_name (GSource *source);
void g_source_set_name_by_id (guint tag,
                                              const char *name);



void g_source_set_callback_indirect (GSource *source,
                                     gpointer callback_data,
                                     GSourceCallbackFuncs *callback_funcs);

void g_source_add_poll (GSource *source,
           GPollFD *fd);
void g_source_remove_poll (GSource *source,
           GPollFD *fd);

void g_source_add_child_source (GSource *source,
           GSource *child_source);
void g_source_remove_child_source (GSource *source,
           GSource *child_source);

__attribute__((__deprecated__("Use '" "g_source_get_time" "' instead")))
void g_source_get_current_time (GSource *source,
                                    GTimeVal *timeval);

gint64 g_source_get_time (GSource *source);







GSource *g_idle_source_new (void);
GSource *g_child_watch_source_new (GPid pid);
GSource *g_timeout_source_new (guint interval);
GSource *g_timeout_source_new_seconds (guint interval);



void g_get_current_time (GTimeVal *result);
gint64 g_get_monotonic_time (void);
gint64 g_get_real_time (void);



gboolean g_source_remove (guint tag);
gboolean g_source_remove_by_user_data (gpointer user_data);
gboolean g_source_remove_by_funcs_user_data (GSourceFuncs *funcs,
                                              gpointer user_data);


guint g_timeout_add_full (gint priority,
                                     guint interval,
                                     GSourceFunc function,
                                     gpointer data,
                                     GDestroyNotify notify);
guint g_timeout_add (guint interval,
                                     GSourceFunc function,
                                     gpointer data);
guint g_timeout_add_seconds_full (gint priority,
                                     guint interval,
                                     GSourceFunc function,
                                     gpointer data,
                                     GDestroyNotify notify);
guint g_timeout_add_seconds (guint interval,
                                     GSourceFunc function,
                                     gpointer data);
guint g_child_watch_add_full (gint priority,
                                     GPid pid,
                                     GChildWatchFunc function,
                                     gpointer data,
                                     GDestroyNotify notify);
guint g_child_watch_add (GPid pid,
                                     GChildWatchFunc function,
                                     gpointer data);
guint g_idle_add (GSourceFunc function,
                                     gpointer data);
guint g_idle_add_full (gint priority,
                                     GSourceFunc function,
                                     gpointer data,
                                     GDestroyNotify notify);
gboolean g_idle_remove_by_data (gpointer data);

void g_main_context_invoke_full (GMainContext *context,
                                     gint priority,
                                     GSourceFunc function,
                                     gpointer data,
                                     GDestroyNotify notify);
void g_main_context_invoke (GMainContext *context,
                                     GSourceFunc function,
                                     gpointer data);


extern GSourceFuncs g_timeout_funcs;
extern GSourceFuncs g_child_watch_funcs;
extern GSourceFuncs g_idle_funcs;


# 36 "/usr/include/glib-2.0/glib/giochannel.h" 2
# 1 "/usr/include/glib-2.0/glib/gstring.h" 1
# 34 "/usr/include/glib-2.0/glib/gstring.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gstring.h" 2
# 1 "/usr/include/glib-2.0/glib/gunicode.h" 1
# 29 "/usr/include/glib-2.0/glib/gunicode.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 30 "/usr/include/glib-2.0/glib/gunicode.h" 2
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 31 "/usr/include/glib-2.0/glib/gunicode.h" 2


# 61 "/usr/include/glib-2.0/glib/gunicode.h"
typedef guint32 gunichar;
# 79 "/usr/include/glib-2.0/glib/gunicode.h"
typedef guint16 gunichar2;
# 118 "/usr/include/glib-2.0/glib/gunicode.h"
typedef enum
{
  G_UNICODE_CONTROL,
  G_UNICODE_FORMAT,
  G_UNICODE_UNASSIGNED,
  G_UNICODE_PRIVATE_USE,
  G_UNICODE_SURROGATE,
  G_UNICODE_LOWERCASE_LETTER,
  G_UNICODE_MODIFIER_LETTER,
  G_UNICODE_OTHER_LETTER,
  G_UNICODE_TITLECASE_LETTER,
  G_UNICODE_UPPERCASE_LETTER,
  G_UNICODE_SPACING_MARK,
  G_UNICODE_ENCLOSING_MARK,
  G_UNICODE_NON_SPACING_MARK,
  G_UNICODE_DECIMAL_NUMBER,
  G_UNICODE_LETTER_NUMBER,
  G_UNICODE_OTHER_NUMBER,
  G_UNICODE_CONNECT_PUNCTUATION,
  G_UNICODE_DASH_PUNCTUATION,
  G_UNICODE_CLOSE_PUNCTUATION,
  G_UNICODE_FINAL_PUNCTUATION,
  G_UNICODE_INITIAL_PUNCTUATION,
  G_UNICODE_OTHER_PUNCTUATION,
  G_UNICODE_OPEN_PUNCTUATION,
  G_UNICODE_CURRENCY_SYMBOL,
  G_UNICODE_MODIFIER_SYMBOL,
  G_UNICODE_MATH_SYMBOL,
  G_UNICODE_OTHER_SYMBOL,
  G_UNICODE_LINE_SEPARATOR,
  G_UNICODE_PARAGRAPH_SEPARATOR,
  G_UNICODE_SPACE_SEPARATOR
} GUnicodeType;
# 214 "/usr/include/glib-2.0/glib/gunicode.h"
typedef enum
{
  G_UNICODE_BREAK_MANDATORY,
  G_UNICODE_BREAK_CARRIAGE_RETURN,
  G_UNICODE_BREAK_LINE_FEED,
  G_UNICODE_BREAK_COMBINING_MARK,
  G_UNICODE_BREAK_SURROGATE,
  G_UNICODE_BREAK_ZERO_WIDTH_SPACE,
  G_UNICODE_BREAK_INSEPARABLE,
  G_UNICODE_BREAK_NON_BREAKING_GLUE,
  G_UNICODE_BREAK_CONTINGENT,
  G_UNICODE_BREAK_SPACE,
  G_UNICODE_BREAK_AFTER,
  G_UNICODE_BREAK_BEFORE,
  G_UNICODE_BREAK_BEFORE_AND_AFTER,
  G_UNICODE_BREAK_HYPHEN,
  G_UNICODE_BREAK_NON_STARTER,
  G_UNICODE_BREAK_OPEN_PUNCTUATION,
  G_UNICODE_BREAK_CLOSE_PUNCTUATION,
  G_UNICODE_BREAK_QUOTATION,
  G_UNICODE_BREAK_EXCLAMATION,
  G_UNICODE_BREAK_IDEOGRAPHIC,
  G_UNICODE_BREAK_NUMERIC,
  G_UNICODE_BREAK_INFIX_SEPARATOR,
  G_UNICODE_BREAK_SYMBOL,
  G_UNICODE_BREAK_ALPHABETIC,
  G_UNICODE_BREAK_PREFIX,
  G_UNICODE_BREAK_POSTFIX,
  G_UNICODE_BREAK_COMPLEX_CONTEXT,
  G_UNICODE_BREAK_AMBIGUOUS,
  G_UNICODE_BREAK_UNKNOWN,
  G_UNICODE_BREAK_NEXT_LINE,
  G_UNICODE_BREAK_WORD_JOINER,
  G_UNICODE_BREAK_HANGUL_L_JAMO,
  G_UNICODE_BREAK_HANGUL_V_JAMO,
  G_UNICODE_BREAK_HANGUL_T_JAMO,
  G_UNICODE_BREAK_HANGUL_LV_SYLLABLE,
  G_UNICODE_BREAK_HANGUL_LVT_SYLLABLE,
  G_UNICODE_BREAK_CLOSE_PARANTHESIS,
  G_UNICODE_BREAK_CONDITIONAL_JAPANESE_STARTER,
  G_UNICODE_BREAK_HEBREW_LETTER
} GUnicodeBreakType;
# 387 "/usr/include/glib-2.0/glib/gunicode.h"
typedef enum
{
  G_UNICODE_SCRIPT_INVALID_CODE = -1,
  G_UNICODE_SCRIPT_COMMON = 0,
  G_UNICODE_SCRIPT_INHERITED,
  G_UNICODE_SCRIPT_ARABIC,
  G_UNICODE_SCRIPT_ARMENIAN,
  G_UNICODE_SCRIPT_BENGALI,
  G_UNICODE_SCRIPT_BOPOMOFO,
  G_UNICODE_SCRIPT_CHEROKEE,
  G_UNICODE_SCRIPT_COPTIC,
  G_UNICODE_SCRIPT_CYRILLIC,
  G_UNICODE_SCRIPT_DESERET,
  G_UNICODE_SCRIPT_DEVANAGARI,
  G_UNICODE_SCRIPT_ETHIOPIC,
  G_UNICODE_SCRIPT_GEORGIAN,
  G_UNICODE_SCRIPT_GOTHIC,
  G_UNICODE_SCRIPT_GREEK,
  G_UNICODE_SCRIPT_GUJARATI,
  G_UNICODE_SCRIPT_GURMUKHI,
  G_UNICODE_SCRIPT_HAN,
  G_UNICODE_SCRIPT_HANGUL,
  G_UNICODE_SCRIPT_HEBREW,
  G_UNICODE_SCRIPT_HIRAGANA,
  G_UNICODE_SCRIPT_KANNADA,
  G_UNICODE_SCRIPT_KATAKANA,
  G_UNICODE_SCRIPT_KHMER,
  G_UNICODE_SCRIPT_LAO,
  G_UNICODE_SCRIPT_LATIN,
  G_UNICODE_SCRIPT_MALAYALAM,
  G_UNICODE_SCRIPT_MONGOLIAN,
  G_UNICODE_SCRIPT_MYANMAR,
  G_UNICODE_SCRIPT_OGHAM,
  G_UNICODE_SCRIPT_OLD_ITALIC,
  G_UNICODE_SCRIPT_ORIYA,
  G_UNICODE_SCRIPT_RUNIC,
  G_UNICODE_SCRIPT_SINHALA,
  G_UNICODE_SCRIPT_SYRIAC,
  G_UNICODE_SCRIPT_TAMIL,
  G_UNICODE_SCRIPT_TELUGU,
  G_UNICODE_SCRIPT_THAANA,
  G_UNICODE_SCRIPT_THAI,
  G_UNICODE_SCRIPT_TIBETAN,
  G_UNICODE_SCRIPT_CANADIAN_ABORIGINAL,
  G_UNICODE_SCRIPT_YI,
  G_UNICODE_SCRIPT_TAGALOG,
  G_UNICODE_SCRIPT_HANUNOO,
  G_UNICODE_SCRIPT_BUHID,
  G_UNICODE_SCRIPT_TAGBANWA,


  G_UNICODE_SCRIPT_BRAILLE,
  G_UNICODE_SCRIPT_CYPRIOT,
  G_UNICODE_SCRIPT_LIMBU,
  G_UNICODE_SCRIPT_OSMANYA,
  G_UNICODE_SCRIPT_SHAVIAN,
  G_UNICODE_SCRIPT_LINEAR_B,
  G_UNICODE_SCRIPT_TAI_LE,
  G_UNICODE_SCRIPT_UGARITIC,


  G_UNICODE_SCRIPT_NEW_TAI_LUE,
  G_UNICODE_SCRIPT_BUGINESE,
  G_UNICODE_SCRIPT_GLAGOLITIC,
  G_UNICODE_SCRIPT_TIFINAGH,
  G_UNICODE_SCRIPT_SYLOTI_NAGRI,
  G_UNICODE_SCRIPT_OLD_PERSIAN,
  G_UNICODE_SCRIPT_KHAROSHTHI,


  G_UNICODE_SCRIPT_UNKNOWN,
  G_UNICODE_SCRIPT_BALINESE,
  G_UNICODE_SCRIPT_CUNEIFORM,
  G_UNICODE_SCRIPT_PHOENICIAN,
  G_UNICODE_SCRIPT_PHAGS_PA,
  G_UNICODE_SCRIPT_NKO,


  G_UNICODE_SCRIPT_KAYAH_LI,
  G_UNICODE_SCRIPT_LEPCHA,
  G_UNICODE_SCRIPT_REJANG,
  G_UNICODE_SCRIPT_SUNDANESE,
  G_UNICODE_SCRIPT_SAURASHTRA,
  G_UNICODE_SCRIPT_CHAM,
  G_UNICODE_SCRIPT_OL_CHIKI,
  G_UNICODE_SCRIPT_VAI,
  G_UNICODE_SCRIPT_CARIAN,
  G_UNICODE_SCRIPT_LYCIAN,
  G_UNICODE_SCRIPT_LYDIAN,


  G_UNICODE_SCRIPT_AVESTAN,
  G_UNICODE_SCRIPT_BAMUM,
  G_UNICODE_SCRIPT_EGYPTIAN_HIEROGLYPHS,
  G_UNICODE_SCRIPT_IMPERIAL_ARAMAIC,
  G_UNICODE_SCRIPT_INSCRIPTIONAL_PAHLAVI,
  G_UNICODE_SCRIPT_INSCRIPTIONAL_PARTHIAN,
  G_UNICODE_SCRIPT_JAVANESE,
  G_UNICODE_SCRIPT_KAITHI,
  G_UNICODE_SCRIPT_LISU,
  G_UNICODE_SCRIPT_MEETEI_MAYEK,
  G_UNICODE_SCRIPT_OLD_SOUTH_ARABIAN,
  G_UNICODE_SCRIPT_OLD_TURKIC,
  G_UNICODE_SCRIPT_SAMARITAN,
  G_UNICODE_SCRIPT_TAI_THAM,
  G_UNICODE_SCRIPT_TAI_VIET,


  G_UNICODE_SCRIPT_BATAK,
  G_UNICODE_SCRIPT_BRAHMI,
  G_UNICODE_SCRIPT_MANDAIC,


  G_UNICODE_SCRIPT_CHAKMA,
  G_UNICODE_SCRIPT_MEROITIC_CURSIVE,
  G_UNICODE_SCRIPT_MEROITIC_HIEROGLYPHS,
  G_UNICODE_SCRIPT_MIAO,
  G_UNICODE_SCRIPT_SHARADA,
  G_UNICODE_SCRIPT_SORA_SOMPENG,
  G_UNICODE_SCRIPT_TAKRI
} GUnicodeScript;

guint32 g_unicode_script_to_iso15924 (GUnicodeScript script);
GUnicodeScript g_unicode_script_from_iso15924 (guint32 iso15924);



gboolean g_unichar_isalnum (gunichar c) __attribute__((__const__));
gboolean g_unichar_isalpha (gunichar c) __attribute__((__const__));
gboolean g_unichar_iscntrl (gunichar c) __attribute__((__const__));
gboolean g_unichar_isdigit (gunichar c) __attribute__((__const__));
gboolean g_unichar_isgraph (gunichar c) __attribute__((__const__));
gboolean g_unichar_islower (gunichar c) __attribute__((__const__));
gboolean g_unichar_isprint (gunichar c) __attribute__((__const__));
gboolean g_unichar_ispunct (gunichar c) __attribute__((__const__));
gboolean g_unichar_isspace (gunichar c) __attribute__((__const__));
gboolean g_unichar_isupper (gunichar c) __attribute__((__const__));
gboolean g_unichar_isxdigit (gunichar c) __attribute__((__const__));
gboolean g_unichar_istitle (gunichar c) __attribute__((__const__));
gboolean g_unichar_isdefined (gunichar c) __attribute__((__const__));
gboolean g_unichar_iswide (gunichar c) __attribute__((__const__));
gboolean g_unichar_iswide_cjk(gunichar c) __attribute__((__const__));
gboolean g_unichar_iszerowidth(gunichar c) __attribute__((__const__));
gboolean g_unichar_ismark (gunichar c) __attribute__((__const__));



gunichar g_unichar_toupper (gunichar c) __attribute__((__const__));
gunichar g_unichar_tolower (gunichar c) __attribute__((__const__));
gunichar g_unichar_totitle (gunichar c) __attribute__((__const__));



gint g_unichar_digit_value (gunichar c) __attribute__((__const__));

gint g_unichar_xdigit_value (gunichar c) __attribute__((__const__));


GUnicodeType g_unichar_type (gunichar c) __attribute__((__const__));


GUnicodeBreakType g_unichar_break_type (gunichar c) __attribute__((__const__));


gint g_unichar_combining_class (gunichar uc) __attribute__((__const__));

gboolean g_unichar_get_mirror_char (gunichar ch,
                                    gunichar *mirrored_ch);

GUnicodeScript g_unichar_get_script (gunichar ch) __attribute__((__const__));


gboolean g_unichar_validate (gunichar ch) __attribute__((__const__));


gboolean g_unichar_compose (gunichar a,
                            gunichar b,
                            gunichar *ch);
gboolean g_unichar_decompose (gunichar ch,
                              gunichar *a,
                              gunichar *b);

gsize g_unichar_fully_decompose (gunichar ch,
                                 gboolean compat,
                                 gunichar *result,
                                 gsize result_len);
# 589 "/usr/include/glib-2.0/glib/gunicode.h"
void g_unicode_canonical_ordering (gunichar *string,
                                   gsize len);


__attribute__((__deprecated__))
gunichar *g_unicode_canonical_decomposition (gunichar ch,
                                             gsize *result_len) __attribute__((__malloc__));



extern const gchar * const g_utf8_skip;
# 614 "/usr/include/glib-2.0/glib/gunicode.h"
gunichar g_utf8_get_char (const gchar *p) __attribute__((__pure__));
gunichar g_utf8_get_char_validated (const gchar *p,
                                    gssize max_len) __attribute__((__pure__));

gchar* g_utf8_offset_to_pointer (const gchar *str,
                                   glong offset) __attribute__((__pure__));
glong g_utf8_pointer_to_offset (const gchar *str,
                                   const gchar *pos) __attribute__((__pure__));
gchar* g_utf8_prev_char (const gchar *p) __attribute__((__pure__));
gchar* g_utf8_find_next_char (const gchar *p,
                                   const gchar *end) __attribute__((__pure__));
gchar* g_utf8_find_prev_char (const gchar *str,
                                   const gchar *p) __attribute__((__pure__));

glong g_utf8_strlen (const gchar *p,
                                   gssize max) __attribute__((__pure__));


gchar *g_utf8_substring (const gchar *str,
                                   glong start_pos,
                                   glong end_pos) __attribute__((__malloc__));

gchar *g_utf8_strncpy (gchar *dest,
                                   const gchar *src,
                                   gsize n);



gchar* g_utf8_strchr (const gchar *p,
                       gssize len,
                       gunichar c);
gchar* g_utf8_strrchr (const gchar *p,
                       gssize len,
                       gunichar c);
gchar* g_utf8_strreverse (const gchar *str,
                          gssize len);

gunichar2 *g_utf8_to_utf16 (const gchar *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));
gunichar * g_utf8_to_ucs4 (const gchar *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));
gunichar * g_utf8_to_ucs4_fast (const gchar *str,
                                glong len,
                                glong *items_written) __attribute__((__malloc__));
gunichar * g_utf16_to_ucs4 (const gunichar2 *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));
gchar* g_utf16_to_utf8 (const gunichar2 *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));
gunichar2 *g_ucs4_to_utf16 (const gunichar *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));
gchar* g_ucs4_to_utf8 (const gunichar *str,
                                glong len,
                                glong *items_read,
                                glong *items_written,
                                GError **error) __attribute__((__malloc__));

gint g_unichar_to_utf8 (gunichar c,
                             gchar *outbuf);

gboolean g_utf8_validate (const gchar *str,
                          gssize max_len,
                          const gchar **end);

gchar *g_utf8_strup (const gchar *str,
                       gssize len) __attribute__((__malloc__));
gchar *g_utf8_strdown (const gchar *str,
                       gssize len) __attribute__((__malloc__));
gchar *g_utf8_casefold (const gchar *str,
                        gssize len) __attribute__((__malloc__));
# 723 "/usr/include/glib-2.0/glib/gunicode.h"
typedef enum {
  G_NORMALIZE_DEFAULT,
  G_NORMALIZE_NFD = G_NORMALIZE_DEFAULT,
  G_NORMALIZE_DEFAULT_COMPOSE,
  G_NORMALIZE_NFC = G_NORMALIZE_DEFAULT_COMPOSE,
  G_NORMALIZE_ALL,
  G_NORMALIZE_NFKD = G_NORMALIZE_ALL,
  G_NORMALIZE_ALL_COMPOSE,
  G_NORMALIZE_NFKC = G_NORMALIZE_ALL_COMPOSE
} GNormalizeMode;

gchar *g_utf8_normalize (const gchar *str,
                         gssize len,
                         GNormalizeMode mode) __attribute__((__malloc__));

gint g_utf8_collate (const gchar *str1,
                           const gchar *str2) __attribute__((__pure__));
gchar *g_utf8_collate_key (const gchar *str,
                           gssize len) __attribute__((__malloc__));
gchar *g_utf8_collate_key_for_filename (const gchar *str,
                                        gssize len) __attribute__((__malloc__));




gchar *_g_utf8_make_valid (const gchar *name);


# 36 "/usr/include/glib-2.0/glib/gstring.h" 2
# 1 "/usr/include/glib-2.0/glib/gutils.h" 1
# 34 "/usr/include/glib-2.0/glib/gutils.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gutils.h" 2



# 96 "/usr/include/glib-2.0/glib/gutils.h"
const gchar * g_get_user_name (void);
const gchar * g_get_real_name (void);
const gchar * g_get_home_dir (void);
const gchar * g_get_tmp_dir (void);
const gchar * g_get_host_name (void);
gchar * g_get_prgname (void);
void g_set_prgname (const gchar *prgname);
const gchar * g_get_application_name (void);
void g_set_application_name (const gchar *application_name);

void g_reload_user_special_dirs_cache (void);
const gchar * g_get_user_data_dir (void);
const gchar * g_get_user_config_dir (void);
const gchar * g_get_user_cache_dir (void);
const gchar * const * g_get_system_data_dirs (void);
# 130 "/usr/include/glib-2.0/glib/gutils.h"
const gchar * const * g_get_system_config_dirs (void);

const gchar * g_get_user_runtime_dir (void);
# 156 "/usr/include/glib-2.0/glib/gutils.h"
typedef enum {
  G_USER_DIRECTORY_DESKTOP,
  G_USER_DIRECTORY_DOCUMENTS,
  G_USER_DIRECTORY_DOWNLOAD,
  G_USER_DIRECTORY_MUSIC,
  G_USER_DIRECTORY_PICTURES,
  G_USER_DIRECTORY_PUBLIC_SHARE,
  G_USER_DIRECTORY_TEMPLATES,
  G_USER_DIRECTORY_VIDEOS,

  G_USER_N_DIRECTORIES
} GUserDirectory;

const gchar * g_get_user_special_dir (GUserDirectory directory);
# 179 "/usr/include/glib-2.0/glib/gutils.h"
typedef struct _GDebugKey GDebugKey;
struct _GDebugKey
{
  const gchar *key;
  guint value;
};



guint g_parse_debug_string (const gchar *string,
         const GDebugKey *keys,
         guint nkeys);

gint g_snprintf (gchar *string,
         gulong n,
         gchar const *format,
         ...) __attribute__((__format__ (__printf__, 3, 4)));
gint g_vsnprintf (gchar *string,
         gulong n,
         gchar const *format,
         va_list args);

void g_nullify_pointer (gpointer *nullify_location);

typedef enum
{
  G_FORMAT_SIZE_DEFAULT = 0,
  G_FORMAT_SIZE_LONG_FORMAT = 1 << 0,
  G_FORMAT_SIZE_IEC_UNITS = 1 << 1
} GFormatSizeFlags;


gchar *g_format_size_full (guint64 size,
                             GFormatSizeFlags flags);

gchar *g_format_size (guint64 size);

__attribute__((__deprecated__("Use '" "g_format_size" "' instead")))
gchar *g_format_size_for_display (goffset size);
# 227 "/usr/include/glib-2.0/glib/gutils.h"
typedef void (*GVoidFunc) (void);
# 238 "/usr/include/glib-2.0/glib/gutils.h"
__attribute__((__deprecated__))
void g_atexit (GVoidFunc func);
# 263 "/usr/include/glib-2.0/glib/gutils.h"
gchar* g_find_program_in_path (const gchar *program);



static __inline __attribute__ ((unused)) gint g_bit_nth_lsf (gulong mask,
           gint nth_bit) __attribute__((__const__));
static __inline __attribute__ ((unused)) gint g_bit_nth_msf (gulong mask,
           gint nth_bit) __attribute__((__const__));
static __inline __attribute__ ((unused)) guint g_bit_storage (gulong number) __attribute__((__const__));




static __inline __attribute__ ((unused)) gint
g_bit_nth_lsf (gulong mask,
        gint nth_bit)
{
  if ((__builtin_expect (__extension__ ({ int _g_boolean_var_; if (nth_bit < -1) _g_boolean_var_ = 1; else _g_boolean_var_ = 0; _g_boolean_var_; }), 0)))
    nth_bit = -1;
  while (nth_bit < ((8 * 8) - 1))
    {
      nth_bit++;
      if (mask & (1UL << nth_bit))
 return nth_bit;
    }
  return -1;
}
static __inline __attribute__ ((unused)) gint
g_bit_nth_msf (gulong mask,
        gint nth_bit)
{
  if (nth_bit < 0 || (__builtin_expect (__extension__ ({ int _g_boolean_var_; if (nth_bit > 8 * 8) _g_boolean_var_ = 1; else _g_boolean_var_ = 0; _g_boolean_var_; }), 0)))
    nth_bit = 8 * 8;
  while (nth_bit > 0)
    {
      nth_bit--;
      if (mask & (1UL << nth_bit))
 return nth_bit;
    }
  return -1;
}
static __inline __attribute__ ((unused)) guint
g_bit_storage (gulong number)
{

  return (__builtin_expect (__extension__ ({ int _g_boolean_var_; if (number) _g_boolean_var_ = 1; else _g_boolean_var_ = 0; _g_boolean_var_; }), 1)) ?
    ((8 * 8U - 1) ^ (guint) __builtin_clzl(number)) + 1 : 1;
# 321 "/usr/include/glib-2.0/glib/gutils.h"
}



# 37 "/usr/include/glib-2.0/glib/gstring.h" 2



typedef struct _GString GString;

struct _GString
{
  gchar *str;
  gsize len;
  gsize allocated_len;
};

GString* g_string_new (const gchar *init);
GString* g_string_new_len (const gchar *init,
                                         gssize len);
GString* g_string_sized_new (gsize dfl_size);
gchar* g_string_free (GString *string,
                                         gboolean free_segment);
gboolean g_string_equal (const GString *v,
                                         const GString *v2);
guint g_string_hash (const GString *str);
GString* g_string_assign (GString *string,
                                         const gchar *rval);
GString* g_string_truncate (GString *string,
                                         gsize len);
GString* g_string_set_size (GString *string,
                                         gsize len);
GString* g_string_insert_len (GString *string,
                                         gssize pos,
                                         const gchar *val,
                                         gssize len);
GString* g_string_append (GString *string,
                                         const gchar *val);
GString* g_string_append_len (GString *string,
                                         const gchar *val,
                                         gssize len);
GString* g_string_append_c (GString *string,
                                         gchar c);
GString* g_string_append_unichar (GString *string,
                                         gunichar wc);
GString* g_string_prepend (GString *string,
                                         const gchar *val);
GString* g_string_prepend_c (GString *string,
                                         gchar c);
GString* g_string_prepend_unichar (GString *string,
                                         gunichar wc);
GString* g_string_prepend_len (GString *string,
                                         const gchar *val,
                                         gssize len);
GString* g_string_insert (GString *string,
                                         gssize pos,
                                         const gchar *val);
GString* g_string_insert_c (GString *string,
                                         gssize pos,
                                         gchar c);
GString* g_string_insert_unichar (GString *string,
                                         gssize pos,
                                         gunichar wc);
GString* g_string_overwrite (GString *string,
                                         gsize pos,
                                         const gchar *val);
GString* g_string_overwrite_len (GString *string,
                                         gsize pos,
                                         const gchar *val,
                                         gssize len);
GString* g_string_erase (GString *string,
                                         gssize pos,
                                         gssize len);
GString* g_string_ascii_down (GString *string);
GString* g_string_ascii_up (GString *string);
void g_string_vprintf (GString *string,
                                         const gchar *format,
                                         va_list args);
void g_string_printf (GString *string,
                                         const gchar *format,
                                         ...) __attribute__((__format__ (__printf__, 2, 3)));
void g_string_append_vprintf (GString *string,
                                         const gchar *format,
                                         va_list args);
void g_string_append_printf (GString *string,
                                         const gchar *format,
                                         ...) __attribute__((__format__ (__printf__, 2, 3)));
GString* g_string_append_uri_escaped (GString *string,
                                          const gchar *unescaped,
                                          const gchar *reserved_chars_allowed,
                                          gboolean allow_utf8);



static inline GString*
g_string_append_c_inline (GString *gstring,
                          gchar c)
{
  if (gstring->len + 1 < gstring->allocated_len)
    {
      gstring->str[gstring->len++] = c;
      gstring->str[gstring->len] = 0;
    }
  else
    g_string_insert_c (gstring, -1, c);
  return gstring;
}




__attribute__((__deprecated__))
GString *g_string_down (GString *string);
__attribute__((__deprecated__))
GString *g_string_up (GString *string);







# 37 "/usr/include/glib-2.0/glib/giochannel.h" 2






typedef struct _GIOChannel GIOChannel;
typedef struct _GIOFuncs GIOFuncs;

typedef enum
{
  G_IO_ERROR_NONE,
  G_IO_ERROR_AGAIN,
  G_IO_ERROR_INVAL,
  G_IO_ERROR_UNKNOWN
} GIOError;



typedef enum
{

  G_IO_CHANNEL_ERROR_FBIG,
  G_IO_CHANNEL_ERROR_INVAL,
  G_IO_CHANNEL_ERROR_IO,
  G_IO_CHANNEL_ERROR_ISDIR,
  G_IO_CHANNEL_ERROR_NOSPC,
  G_IO_CHANNEL_ERROR_NXIO,
  G_IO_CHANNEL_ERROR_OVERFLOW,
  G_IO_CHANNEL_ERROR_PIPE,

  G_IO_CHANNEL_ERROR_FAILED
} GIOChannelError;

typedef enum
{
  G_IO_STATUS_ERROR,
  G_IO_STATUS_NORMAL,
  G_IO_STATUS_EOF,
  G_IO_STATUS_AGAIN
} GIOStatus;

typedef enum
{
  G_SEEK_CUR,
  G_SEEK_SET,
  G_SEEK_END
} GSeekType;

typedef enum
{
  G_IO_IN =1,
  G_IO_OUT =4,
  G_IO_PRI =2,
  G_IO_ERR =8,
  G_IO_HUP =16,
  G_IO_NVAL =32
} GIOCondition;

typedef enum
{
  G_IO_FLAG_APPEND = 1 << 0,
  G_IO_FLAG_NONBLOCK = 1 << 1,
  G_IO_FLAG_IS_READABLE = 1 << 2,
  G_IO_FLAG_IS_WRITABLE = 1 << 3,
  G_IO_FLAG_IS_SEEKABLE = 1 << 4,
  G_IO_FLAG_MASK = (1 << 5) - 1,
  G_IO_FLAG_GET_MASK = G_IO_FLAG_MASK,
  G_IO_FLAG_SET_MASK = G_IO_FLAG_APPEND | G_IO_FLAG_NONBLOCK
} GIOFlags;




struct _GIOChannel
{

  gint ref_count;
  GIOFuncs *funcs;

  gchar *encoding;
  GIConv read_cd;
  GIConv write_cd;
  gchar *line_term;
  guint line_term_len;

  gsize buf_size;
  GString *read_buf;
  GString *encoded_read_buf;
  GString *write_buf;
  gchar partial_write_buf[6];



  guint use_buffer : 1;
  guint do_encode : 1;
  guint close_on_unref : 1;
  guint is_readable : 1;
  guint is_writeable : 1;
  guint is_seekable : 1;

  gpointer reserved1;
  gpointer reserved2;
};

typedef gboolean (*GIOFunc) (GIOChannel *source,
        GIOCondition condition,
        gpointer data);
struct _GIOFuncs
{
  GIOStatus (*io_read) (GIOChannel *channel,
             gchar *buf,
      gsize count,
      gsize *bytes_read,
      GError **err);
  GIOStatus (*io_write) (GIOChannel *channel,
      const gchar *buf,
      gsize count,
      gsize *bytes_written,
      GError **err);
  GIOStatus (*io_seek) (GIOChannel *channel,
      gint64 offset,
      GSeekType type,
      GError **err);
  GIOStatus (*io_close) (GIOChannel *channel,
      GError **err);
  GSource* (*io_create_watch) (GIOChannel *channel,
      GIOCondition condition);
  void (*io_free) (GIOChannel *channel);
  GIOStatus (*io_set_flags) (GIOChannel *channel,
                                  GIOFlags flags,
      GError **err);
  GIOFlags (*io_get_flags) (GIOChannel *channel);
};

void g_io_channel_init (GIOChannel *channel);
GIOChannel *g_io_channel_ref (GIOChannel *channel);
void g_io_channel_unref (GIOChannel *channel);

__attribute__((__deprecated__("Use '" "g_io_channel_read_for" "' instead")))
GIOError g_io_channel_read (GIOChannel *channel,
                                 gchar *buf,
                                 gsize count,
                                 gsize *bytes_read);

__attribute__((__deprecated__("Use '" "g_io_channel_write_chars" "' instead")))
GIOError g_io_channel_write (GIOChannel *channel,
                                 const gchar *buf,
                                 gsize count,
                                 gsize *bytes_written);

__attribute__((__deprecated__("Use '" "g_io_channel_seek_position" "' instead")))
GIOError g_io_channel_seek (GIOChannel *channel,
                                 gint64 offset,
                                 GSeekType type);

__attribute__((__deprecated__("Use '" "g_io_channel_shutdown" "' instead")))
void g_io_channel_close (GIOChannel *channel);

GIOStatus g_io_channel_shutdown (GIOChannel *channel,
     gboolean flush,
     GError **err);
guint g_io_add_watch_full (GIOChannel *channel,
     gint priority,
     GIOCondition condition,
     GIOFunc func,
     gpointer user_data,
     GDestroyNotify notify);
GSource * g_io_create_watch (GIOChannel *channel,
     GIOCondition condition);
guint g_io_add_watch (GIOChannel *channel,
     GIOCondition condition,
     GIOFunc func,
     gpointer user_data);




void g_io_channel_set_buffer_size (GIOChannel *channel,
        gsize size);
gsize g_io_channel_get_buffer_size (GIOChannel *channel);
GIOCondition g_io_channel_get_buffer_condition (GIOChannel *channel);
GIOStatus g_io_channel_set_flags (GIOChannel *channel,
        GIOFlags flags,
        GError **error);
GIOFlags g_io_channel_get_flags (GIOChannel *channel);
void g_io_channel_set_line_term (GIOChannel *channel,
        const gchar *line_term,
        gint length);
const gchar * g_io_channel_get_line_term (GIOChannel *channel,
        gint *length);
void g_io_channel_set_buffered (GIOChannel *channel,
        gboolean buffered);
gboolean g_io_channel_get_buffered (GIOChannel *channel);
GIOStatus g_io_channel_set_encoding (GIOChannel *channel,
        const gchar *encoding,
        GError **error);
const gchar * g_io_channel_get_encoding (GIOChannel *channel);
void g_io_channel_set_close_on_unref (GIOChannel *channel,
        gboolean do_close);
gboolean g_io_channel_get_close_on_unref (GIOChannel *channel);


GIOStatus g_io_channel_flush (GIOChannel *channel,
        GError **error);
GIOStatus g_io_channel_read_line (GIOChannel *channel,
        gchar **str_return,
        gsize *length,
        gsize *terminator_pos,
        GError **error);
GIOStatus g_io_channel_read_line_string (GIOChannel *channel,
        GString *buffer,
        gsize *terminator_pos,
        GError **error);
GIOStatus g_io_channel_read_to_end (GIOChannel *channel,
        gchar **str_return,
        gsize *length,
        GError **error);
GIOStatus g_io_channel_read_chars (GIOChannel *channel,
        gchar *buf,
        gsize count,
        gsize *bytes_read,
        GError **error);
GIOStatus g_io_channel_read_unichar (GIOChannel *channel,
        gunichar *thechar,
        GError **error);
GIOStatus g_io_channel_write_chars (GIOChannel *channel,
        const gchar *buf,
        gssize count,
        gsize *bytes_written,
        GError **error);
GIOStatus g_io_channel_write_unichar (GIOChannel *channel,
        gunichar thechar,
        GError **error);
GIOStatus g_io_channel_seek_position (GIOChannel *channel,
        gint64 offset,
        GSeekType type,
        GError **error);




GIOChannel* g_io_channel_new_file (const gchar *filename,
        const gchar *mode,
        GError **error);



GQuark g_io_channel_error_quark (void);
GIOChannelError g_io_channel_error_from_errno (gint en);
# 306 "/usr/include/glib-2.0/glib/giochannel.h"
GIOChannel* g_io_channel_unix_new (int fd);
gint g_io_channel_unix_get_fd (GIOChannel *channel);



extern GSourceFuncs g_io_watch_funcs;
# 372 "/usr/include/glib-2.0/glib/giochannel.h"

# 57 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gkeyfile.h" 1
# 30 "/usr/include/glib-2.0/glib/gkeyfile.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 31 "/usr/include/glib-2.0/glib/gkeyfile.h" 2



typedef enum
{
  G_KEY_FILE_ERROR_UNKNOWN_ENCODING,
  G_KEY_FILE_ERROR_PARSE,
  G_KEY_FILE_ERROR_NOT_FOUND,
  G_KEY_FILE_ERROR_KEY_NOT_FOUND,
  G_KEY_FILE_ERROR_GROUP_NOT_FOUND,
  G_KEY_FILE_ERROR_INVALID_VALUE
} GKeyFileError;



GQuark g_key_file_error_quark (void);

typedef struct _GKeyFile GKeyFile;

typedef enum
{
  G_KEY_FILE_NONE = 0,
  G_KEY_FILE_KEEP_COMMENTS = 1 << 0,
  G_KEY_FILE_KEEP_TRANSLATIONS = 1 << 1
} GKeyFileFlags;

GKeyFile *g_key_file_new (void);
GKeyFile *g_key_file_ref (GKeyFile *key_file);
void g_key_file_unref (GKeyFile *key_file);
void g_key_file_free (GKeyFile *key_file);
void g_key_file_set_list_separator (GKeyFile *key_file,
          gchar separator);
gboolean g_key_file_load_from_file (GKeyFile *key_file,
          const gchar *file,
          GKeyFileFlags flags,
          GError **error);
gboolean g_key_file_load_from_data (GKeyFile *key_file,
          const gchar *data,
          gsize length,
          GKeyFileFlags flags,
          GError **error);
gboolean g_key_file_load_from_dirs (GKeyFile *key_file,
          const gchar *file,
          const gchar **search_dirs,
          gchar **full_path,
          GKeyFileFlags flags,
          GError **error);
gboolean g_key_file_load_from_data_dirs (GKeyFile *key_file,
          const gchar *file,
          gchar **full_path,
          GKeyFileFlags flags,
          GError **error);
gchar *g_key_file_to_data (GKeyFile *key_file,
          gsize *length,
          GError **error) __attribute__((__malloc__));
gchar *g_key_file_get_start_group (GKeyFile *key_file) __attribute__((__malloc__));
gchar **g_key_file_get_groups (GKeyFile *key_file,
          gsize *length) __attribute__((__malloc__));
gchar **g_key_file_get_keys (GKeyFile *key_file,
          const gchar *group_name,
          gsize *length,
          GError **error) __attribute__((__malloc__));
gboolean g_key_file_has_group (GKeyFile *key_file,
          const gchar *group_name);
gboolean g_key_file_has_key (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
gchar *g_key_file_get_value (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_value (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *value);
gchar *g_key_file_get_string (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_string (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *string);
gchar *g_key_file_get_locale_string (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *locale,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_locale_string (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *locale,
          const gchar *string);
gboolean g_key_file_get_boolean (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
void g_key_file_set_boolean (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gboolean value);
gint g_key_file_get_integer (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
void g_key_file_set_integer (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gint value);
gint64 g_key_file_get_int64 (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
void g_key_file_set_int64 (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gint64 value);
guint64 g_key_file_get_uint64 (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
void g_key_file_set_uint64 (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          guint64 value);
gdouble g_key_file_get_double (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             GError **error);
void g_key_file_set_double (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             gdouble value);
gchar **g_key_file_get_string_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gsize *length,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_string_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar * const list[],
          gsize length);
gchar **g_key_file_get_locale_string_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *locale,
          gsize *length,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_locale_string_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          const gchar *locale,
          const gchar * const list[],
          gsize length);
gboolean *g_key_file_get_boolean_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gsize *length,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_boolean_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gboolean list[],
          gsize length);
gint *g_key_file_get_integer_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gsize *length,
          GError **error) __attribute__((__malloc__));
void g_key_file_set_double_list (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             gdouble list[],
                                             gsize length);
gdouble *g_key_file_get_double_list (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             gsize *length,
                                             GError **error) __attribute__((__malloc__));
void g_key_file_set_integer_list (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          gint list[],
          gsize length);
gboolean g_key_file_set_comment (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             const gchar *comment,
                                             GError **error);
gchar *g_key_file_get_comment (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
                                             GError **error) __attribute__((__malloc__));

gboolean g_key_file_remove_comment (GKeyFile *key_file,
                                             const gchar *group_name,
                                             const gchar *key,
          GError **error);
gboolean g_key_file_remove_key (GKeyFile *key_file,
          const gchar *group_name,
          const gchar *key,
          GError **error);
gboolean g_key_file_remove_group (GKeyFile *key_file,
          const gchar *group_name,
          GError **error);
# 269 "/usr/include/glib-2.0/glib/gkeyfile.h"

# 58 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/glist.h" 1
# 59 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmacros.h" 1
# 60 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmain.h" 1
# 61 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmappedfile.h" 1
# 29 "/usr/include/glib-2.0/glib/gmappedfile.h"
# 1 "/usr/include/glib-2.0/glib/gbytes.h" 1
# 30 "/usr/include/glib-2.0/glib/gmappedfile.h" 2
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 31 "/usr/include/glib-2.0/glib/gmappedfile.h" 2



typedef struct _GMappedFile GMappedFile;

GMappedFile *g_mapped_file_new (const gchar *filename,
             gboolean writable,
             GError **error) __attribute__((__malloc__));
GMappedFile *g_mapped_file_new_from_fd (gint fd,
      gboolean writable,
      GError **error) __attribute__((__malloc__));
gsize g_mapped_file_get_length (GMappedFile *file);
gchar *g_mapped_file_get_contents (GMappedFile *file);
GMappedFile *g_mapped_file_ref (GMappedFile *file);
void g_mapped_file_unref (GMappedFile *file);

__attribute__((__deprecated__("Use '" "g_mapped_file_unref" "' instead")))
void g_mapped_file_free (GMappedFile *file);


# 62 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmarkup.h" 1
# 30 "/usr/include/glib-2.0/glib/gmarkup.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 31 "/usr/include/glib-2.0/glib/gmarkup.h" 2
# 1 "/usr/include/glib-2.0/glib/gslist.h" 1
# 32 "/usr/include/glib-2.0/glib/gmarkup.h" 2


# 51 "/usr/include/glib-2.0/glib/gmarkup.h"
typedef enum
{
  G_MARKUP_ERROR_BAD_UTF8,
  G_MARKUP_ERROR_EMPTY,
  G_MARKUP_ERROR_PARSE,



  G_MARKUP_ERROR_UNKNOWN_ELEMENT,
  G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE,
  G_MARKUP_ERROR_INVALID_CONTENT,
  G_MARKUP_ERROR_MISSING_ATTRIBUTE
} GMarkupError;
# 74 "/usr/include/glib-2.0/glib/gmarkup.h"
GQuark g_markup_error_quark (void);
# 92 "/usr/include/glib-2.0/glib/gmarkup.h"
typedef enum
{
  G_MARKUP_DO_NOT_USE_THIS_UNSUPPORTED_FLAG = 1 << 0,
  G_MARKUP_TREAT_CDATA_AS_TEXT = 1 << 1,
  G_MARKUP_PREFIX_ERROR_POSITION = 1 << 2
} GMarkupParseFlags;
# 108 "/usr/include/glib-2.0/glib/gmarkup.h"
typedef struct _GMarkupParseContext GMarkupParseContext;
typedef struct _GMarkupParser GMarkupParser;
# 138 "/usr/include/glib-2.0/glib/gmarkup.h"
struct _GMarkupParser
{

  void (*start_element) (GMarkupParseContext *context,
                          const gchar *element_name,
                          const gchar **attribute_names,
                          const gchar **attribute_values,
                          gpointer user_data,
                          GError **error);


  void (*end_element) (GMarkupParseContext *context,
                          const gchar *element_name,
                          gpointer user_data,
                          GError **error);



  void (*text) (GMarkupParseContext *context,
                          const gchar *text,
                          gsize text_len,
                          gpointer user_data,
                          GError **error);






  void (*passthrough) (GMarkupParseContext *context,
                          const gchar *passthrough_text,
                          gsize text_len,
                          gpointer user_data,
                          GError **error);




  void (*error) (GMarkupParseContext *context,
                          GError *error,
                          gpointer user_data);
};

GMarkupParseContext *g_markup_parse_context_new (const GMarkupParser *parser,
                                                   GMarkupParseFlags flags,
                                                   gpointer user_data,
                                                   GDestroyNotify user_data_dnotify);
void g_markup_parse_context_free (GMarkupParseContext *context);
gboolean g_markup_parse_context_parse (GMarkupParseContext *context,
                                                   const gchar *text,
                                                   gssize text_len,
                                                   GError **error);
void g_markup_parse_context_push (GMarkupParseContext *context,
                                                   const GMarkupParser *parser,
                                                   gpointer user_data);
gpointer g_markup_parse_context_pop (GMarkupParseContext *context);

gboolean g_markup_parse_context_end_parse (GMarkupParseContext *context,
                                                       GError **error);
const gchar * g_markup_parse_context_get_element (GMarkupParseContext *context);
const GSList * g_markup_parse_context_get_element_stack (GMarkupParseContext *context);


void g_markup_parse_context_get_position (GMarkupParseContext *context,
                                                          gint *line_number,
                                                          gint *char_number);
gpointer g_markup_parse_context_get_user_data (GMarkupParseContext *context);


gchar* g_markup_escape_text (const gchar *text,
                             gssize length);

gchar *g_markup_printf_escaped (const char *format,
    ...) __attribute__((__format__ (__printf__, 1, 2)));
gchar *g_markup_vprintf_escaped (const char *format,
     va_list args);

typedef enum
{
  G_MARKUP_COLLECT_INVALID,
  G_MARKUP_COLLECT_STRING,
  G_MARKUP_COLLECT_STRDUP,
  G_MARKUP_COLLECT_BOOLEAN,
  G_MARKUP_COLLECT_TRISTATE,

  G_MARKUP_COLLECT_OPTIONAL = (1 << 16)
} GMarkupCollectType;



gboolean g_markup_collect_attributes (const gchar *element_name,
                                        const gchar **attribute_names,
                                        const gchar **attribute_values,
                                        GError **error,
                                        GMarkupCollectType first_type,
                                        const gchar *first_attr,
                                        ...);


# 63 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmem.h" 1
# 64 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gmessages.h" 1
# 35 "/usr/include/glib-2.0/glib/gmessages.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 36 "/usr/include/glib-2.0/glib/gmessages.h" 2
# 1 "/usr/include/glib-2.0/glib/gmacros.h" 1
# 37 "/usr/include/glib-2.0/glib/gmessages.h" 2




       
# 42 "/usr/include/glib-2.0/glib/gmessages.h" 3






gsize g_printf_string_upper_bound (const gchar* format,
         va_list args);
# 58 "/usr/include/glib-2.0/glib/gmessages.h" 3
typedef enum
{

  G_LOG_FLAG_RECURSION = 1 << 0,
  G_LOG_FLAG_FATAL = 1 << 1,


  G_LOG_LEVEL_ERROR = 1 << 2,
  G_LOG_LEVEL_CRITICAL = 1 << 3,
  G_LOG_LEVEL_WARNING = 1 << 4,
  G_LOG_LEVEL_MESSAGE = 1 << 5,
  G_LOG_LEVEL_INFO = 1 << 6,
  G_LOG_LEVEL_DEBUG = 1 << 7,

  G_LOG_LEVEL_MASK = ~(G_LOG_FLAG_RECURSION | G_LOG_FLAG_FATAL)
} GLogLevelFlags;




typedef void (*GLogFunc) (const gchar *log_domain,
                                                 GLogLevelFlags log_level,
                                                 const gchar *message,
                                                 gpointer user_data);



guint g_log_set_handler (const gchar *log_domain,
                                         GLogLevelFlags log_levels,
                                         GLogFunc log_func,
                                         gpointer user_data);
void g_log_remove_handler (const gchar *log_domain,
                                         guint handler_id);
void g_log_default_handler (const gchar *log_domain,
                                         GLogLevelFlags log_level,
                                         const gchar *message,
                                         gpointer unused_data);
GLogFunc g_log_set_default_handler (GLogFunc log_func,
        gpointer user_data);
void g_log (const gchar *log_domain,
                                         GLogLevelFlags log_level,
                                         const gchar *format,
                                         ...) __attribute__((__format__ (__printf__, 3, 4)));
void g_logv (const gchar *log_domain,
                                         GLogLevelFlags log_level,
                                         const gchar *format,
                                         va_list args);
GLogLevelFlags g_log_set_fatal_mask (const gchar *log_domain,
                                         GLogLevelFlags fatal_mask);
GLogLevelFlags g_log_set_always_fatal (GLogLevelFlags fatal_mask);


__attribute__((visibility("hidden"))) void _g_log_fallback_handler (const gchar *log_domain,
       GLogLevelFlags log_level,
       const gchar *message,
       gpointer unused_data);


void g_return_if_fail_warning (const char *log_domain,
          const char *pretty_function,
          const char *expression);
void g_warn_message (const char *domain,
                               const char *file,
                               int line,
                               const char *func,
                               const char *warnexpr);
__attribute__((__deprecated__))
void g_assert_warning (const char *log_domain,
          const char *file,
          const int line,
                 const char *pretty_function,
                 const char *expression) __attribute__((__noreturn__));
# 235 "/usr/include/glib-2.0/glib/gmessages.h" 3
typedef void (*GPrintFunc) (const gchar *string);
void g_print (const gchar *format,
                                         ...) __attribute__((__format__ (__printf__, 1, 2)));
GPrintFunc g_set_print_handler (GPrintFunc func);
void g_printerr (const gchar *format,
                                         ...) __attribute__((__format__ (__printf__, 1, 2)));
GPrintFunc g_set_printerr_handler (GPrintFunc func);
# 402 "/usr/include/glib-2.0/glib/gmessages.h" 3

# 65 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gnode.h" 1
# 34 "/usr/include/glib-2.0/glib/gnode.h"
# 1 "/usr/include/glib-2.0/glib/gmem.h" 1
# 35 "/usr/include/glib-2.0/glib/gnode.h" 2



typedef struct _GNode GNode;


typedef enum
{
  G_TRAVERSE_LEAVES = 1 << 0,
  G_TRAVERSE_NON_LEAVES = 1 << 1,
  G_TRAVERSE_ALL = G_TRAVERSE_LEAVES | G_TRAVERSE_NON_LEAVES,
  G_TRAVERSE_MASK = 0x03,
  G_TRAVERSE_LEAFS = G_TRAVERSE_LEAVES,
  G_TRAVERSE_NON_LEAFS = G_TRAVERSE_NON_LEAVES
} GTraverseFlags;


typedef enum
{
  G_IN_ORDER,
  G_PRE_ORDER,
  G_POST_ORDER,
  G_LEVEL_ORDER
} GTraverseType;

typedef gboolean (*GNodeTraverseFunc) (GNode *node,
       gpointer data);
typedef void (*GNodeForeachFunc) (GNode *node,
       gpointer data);
# 77 "/usr/include/glib-2.0/glib/gnode.h"
typedef gpointer (*GCopyFunc) (gconstpointer src,
                                                 gpointer data);



struct _GNode
{
  gpointer data;
  GNode *next;
  GNode *prev;
  GNode *parent;
  GNode *children;
};
# 115 "/usr/include/glib-2.0/glib/gnode.h"
GNode* g_node_new (gpointer data);
void g_node_destroy (GNode *root);
void g_node_unlink (GNode *node);
GNode* g_node_copy_deep (GNode *node,
     GCopyFunc copy_func,
     gpointer data);
GNode* g_node_copy (GNode *node);
GNode* g_node_insert (GNode *parent,
     gint position,
     GNode *node);
GNode* g_node_insert_before (GNode *parent,
     GNode *sibling,
     GNode *node);
GNode* g_node_insert_after (GNode *parent,
     GNode *sibling,
     GNode *node);
GNode* g_node_prepend (GNode *parent,
     GNode *node);
guint g_node_n_nodes (GNode *root,
     GTraverseFlags flags);
GNode* g_node_get_root (GNode *node);
gboolean g_node_is_ancestor (GNode *node,
     GNode *descendant);
guint g_node_depth (GNode *node);
GNode* g_node_find (GNode *root,
     GTraverseType order,
     GTraverseFlags flags,
     gpointer data);
# 226 "/usr/include/glib-2.0/glib/gnode.h"
void g_node_traverse (GNode *root,
     GTraverseType order,
     GTraverseFlags flags,
     gint max_depth,
     GNodeTraverseFunc func,
     gpointer data);






guint g_node_max_height (GNode *root);

void g_node_children_foreach (GNode *node,
      GTraverseFlags flags,
      GNodeForeachFunc func,
      gpointer data);
void g_node_reverse_children (GNode *node);
guint g_node_n_children (GNode *node);
GNode* g_node_nth_child (GNode *node,
      guint n);
GNode* g_node_last_child (GNode *node);
GNode* g_node_find_child (GNode *node,
      GTraverseFlags flags,
      gpointer data);
gint g_node_child_position (GNode *node,
      GNode *child);
gint g_node_child_index (GNode *node,
      gpointer data);

GNode* g_node_first_sibling (GNode *node);
GNode* g_node_last_sibling (GNode *node);
# 296 "/usr/include/glib-2.0/glib/gnode.h"

# 66 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/goption.h" 1
# 28 "/usr/include/glib-2.0/glib/goption.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 29 "/usr/include/glib-2.0/glib/goption.h" 2
# 1 "/usr/include/glib-2.0/glib/gquark.h" 1
# 30 "/usr/include/glib-2.0/glib/goption.h" 2


# 40 "/usr/include/glib-2.0/glib/goption.h"
typedef struct _GOptionContext GOptionContext;
# 53 "/usr/include/glib-2.0/glib/goption.h"
typedef struct _GOptionGroup GOptionGroup;
typedef struct _GOptionEntry GOptionEntry;
# 82 "/usr/include/glib-2.0/glib/goption.h"
typedef enum
{
  G_OPTION_FLAG_HIDDEN = 1 << 0,
  G_OPTION_FLAG_IN_MAIN = 1 << 1,
  G_OPTION_FLAG_REVERSE = 1 << 2,
  G_OPTION_FLAG_NO_ARG = 1 << 3,
  G_OPTION_FLAG_FILENAME = 1 << 4,
  G_OPTION_FLAG_OPTIONAL_ARG = 1 << 5,
  G_OPTION_FLAG_NOALIAS = 1 << 6
} GOptionFlags;
# 118 "/usr/include/glib-2.0/glib/goption.h"
typedef enum
{
  G_OPTION_ARG_NONE,
  G_OPTION_ARG_STRING,
  G_OPTION_ARG_INT,
  G_OPTION_ARG_CALLBACK,
  G_OPTION_ARG_FILENAME,
  G_OPTION_ARG_STRING_ARRAY,
  G_OPTION_ARG_FILENAME_ARRAY,
  G_OPTION_ARG_DOUBLE,
  G_OPTION_ARG_INT64
} GOptionArg;
# 148 "/usr/include/glib-2.0/glib/goption.h"
typedef gboolean (*GOptionArgFunc) (const gchar *option_name,
        const gchar *value,
        gpointer data,
        GError **error);
# 166 "/usr/include/glib-2.0/glib/goption.h"
typedef gboolean (*GOptionParseFunc) (GOptionContext *context,
          GOptionGroup *group,
          gpointer data,
          GError **error);
# 181 "/usr/include/glib-2.0/glib/goption.h"
typedef void (*GOptionErrorFunc) (GOptionContext *context,
      GOptionGroup *group,
      gpointer data,
      GError **error);
# 205 "/usr/include/glib-2.0/glib/goption.h"
typedef enum
{
  G_OPTION_ERROR_UNKNOWN_OPTION,
  G_OPTION_ERROR_BAD_VALUE,
  G_OPTION_ERROR_FAILED
} GOptionError;

GQuark g_option_error_quark (void);
# 279 "/usr/include/glib-2.0/glib/goption.h"
struct _GOptionEntry
{
  const gchar *long_name;
  gchar short_name;
  gint flags;

  GOptionArg arg;
  gpointer arg_data;

  const gchar *description;
  const gchar *arg_description;
};
# 310 "/usr/include/glib-2.0/glib/goption.h"
GOptionContext *g_option_context_new (const gchar *parameter_string);
void g_option_context_set_summary (GOptionContext *context,
                                                   const gchar *summary);
const gchar * g_option_context_get_summary (GOptionContext *context);
void g_option_context_set_description (GOptionContext *context,
                                                   const gchar *description);
const gchar * g_option_context_get_description (GOptionContext *context);
void g_option_context_free (GOptionContext *context);
void g_option_context_set_help_enabled (GOptionContext *context,
         gboolean help_enabled);
gboolean g_option_context_get_help_enabled (GOptionContext *context);
void g_option_context_set_ignore_unknown_options (GOptionContext *context,
            gboolean ignore_unknown);
gboolean g_option_context_get_ignore_unknown_options (GOptionContext *context);

void g_option_context_add_main_entries (GOptionContext *context,
         const GOptionEntry *entries,
         const gchar *translation_domain);
gboolean g_option_context_parse (GOptionContext *context,
         gint *argc,
         gchar ***argv,
         GError **error);
void g_option_context_set_translate_func (GOptionContext *context,
           GTranslateFunc func,
           gpointer data,
           GDestroyNotify destroy_notify);
void g_option_context_set_translation_domain (GOptionContext *context,
        const gchar *domain);

void g_option_context_add_group (GOptionContext *context,
       GOptionGroup *group);
void g_option_context_set_main_group (GOptionContext *context,
            GOptionGroup *group);
GOptionGroup *g_option_context_get_main_group (GOptionContext *context);
gchar *g_option_context_get_help (GOptionContext *context,
                                               gboolean main_help,
                                               GOptionGroup *group);

GOptionGroup *g_option_group_new (const gchar *name,
           const gchar *description,
           const gchar *help_description,
           gpointer user_data,
           GDestroyNotify destroy);
void g_option_group_set_parse_hooks (GOptionGroup *group,
           GOptionParseFunc pre_parse_func,
           GOptionParseFunc post_parse_func);
void g_option_group_set_error_hook (GOptionGroup *group,
           GOptionErrorFunc error_func);
void g_option_group_free (GOptionGroup *group);
void g_option_group_add_entries (GOptionGroup *group,
           const GOptionEntry *entries);
void g_option_group_set_translate_func (GOptionGroup *group,
           GTranslateFunc func,
           gpointer data,
           GDestroyNotify destroy_notify);
void g_option_group_set_translation_domain (GOptionGroup *group,
           const gchar *domain);


# 67 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gpattern.h" 1
# 27 "/usr/include/glib-2.0/glib/gpattern.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 28 "/usr/include/glib-2.0/glib/gpattern.h" 2




typedef struct _GPatternSpec GPatternSpec;

GPatternSpec* g_pattern_spec_new (const gchar *pattern);
void g_pattern_spec_free (GPatternSpec *pspec);
gboolean g_pattern_spec_equal (GPatternSpec *pspec1,
     GPatternSpec *pspec2);
gboolean g_pattern_match (GPatternSpec *pspec,
     guint string_length,
     const gchar *string,
     const gchar *string_reversed);
gboolean g_pattern_match_string (GPatternSpec *pspec,
     const gchar *string);
gboolean g_pattern_match_simple (const gchar *pattern,
     const gchar *string);


# 68 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gpoll.h" 1
# 69 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gprimes.h" 1
# 34 "/usr/include/glib-2.0/glib/gprimes.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gprimes.h" 2


# 47 "/usr/include/glib-2.0/glib/gprimes.h"
guint g_spaced_primes_closest (guint num) __attribute__((__const__));


# 70 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gqsort.h" 1
# 34 "/usr/include/glib-2.0/glib/gqsort.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gqsort.h" 2



void g_qsort_with_data (gconstpointer pbase,
   gint total_elems,
   gsize size,
   GCompareDataFunc compare_func,
   gpointer user_data);


# 71 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gquark.h" 1
# 72 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gqueue.h" 1
# 34 "/usr/include/glib-2.0/glib/gqueue.h"
# 1 "/usr/include/glib-2.0/glib/glist.h" 1
# 35 "/usr/include/glib-2.0/glib/gqueue.h" 2



typedef struct _GQueue GQueue;
# 49 "/usr/include/glib-2.0/glib/gqueue.h"
struct _GQueue
{
  GList *head;
  GList *tail;
  guint length;
};
# 74 "/usr/include/glib-2.0/glib/gqueue.h"
GQueue* g_queue_new (void);
void g_queue_free (GQueue *queue);
void g_queue_free_full (GQueue *queue,
    GDestroyNotify free_func);
void g_queue_init (GQueue *queue);
void g_queue_clear (GQueue *queue);
gboolean g_queue_is_empty (GQueue *queue);
guint g_queue_get_length (GQueue *queue);
void g_queue_reverse (GQueue *queue);
GQueue * g_queue_copy (GQueue *queue);
void g_queue_foreach (GQueue *queue,
                                 GFunc func,
                                 gpointer user_data);
GList * g_queue_find (GQueue *queue,
                                 gconstpointer data);
GList * g_queue_find_custom (GQueue *queue,
                                 gconstpointer data,
                                 GCompareFunc func);
void g_queue_sort (GQueue *queue,
                                 GCompareDataFunc compare_func,
                                 gpointer user_data);

void g_queue_push_head (GQueue *queue,
                                 gpointer data);
void g_queue_push_tail (GQueue *queue,
                                 gpointer data);
void g_queue_push_nth (GQueue *queue,
                                 gpointer data,
                                 gint n);
gpointer g_queue_pop_head (GQueue *queue);
gpointer g_queue_pop_tail (GQueue *queue);
gpointer g_queue_pop_nth (GQueue *queue,
                                 guint n);
gpointer g_queue_peek_head (GQueue *queue);
gpointer g_queue_peek_tail (GQueue *queue);
gpointer g_queue_peek_nth (GQueue *queue,
                                 guint n);
gint g_queue_index (GQueue *queue,
                                 gconstpointer data);
gboolean g_queue_remove (GQueue *queue,
                                 gconstpointer data);
guint g_queue_remove_all (GQueue *queue,
                                 gconstpointer data);
void g_queue_insert_before (GQueue *queue,
                                 GList *sibling,
                                 gpointer data);
void g_queue_insert_after (GQueue *queue,
                                 GList *sibling,
                                 gpointer data);
void g_queue_insert_sorted (GQueue *queue,
                                 gpointer data,
                                 GCompareDataFunc func,
                                 gpointer user_data);

void g_queue_push_head_link (GQueue *queue,
                                 GList *link_);
void g_queue_push_tail_link (GQueue *queue,
                                 GList *link_);
void g_queue_push_nth_link (GQueue *queue,
                                 gint n,
                                 GList *link_);
GList* g_queue_pop_head_link (GQueue *queue);
GList* g_queue_pop_tail_link (GQueue *queue);
GList* g_queue_pop_nth_link (GQueue *queue,
                                 guint n);
GList* g_queue_peek_head_link (GQueue *queue);
GList* g_queue_peek_tail_link (GQueue *queue);
GList* g_queue_peek_nth_link (GQueue *queue,
                                 guint n);
gint g_queue_link_index (GQueue *queue,
                                 GList *link_);
void g_queue_unlink (GQueue *queue,
                                 GList *link_);
void g_queue_delete_link (GQueue *queue,
                                 GList *link_);


# 73 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/grand.h" 1
# 34 "/usr/include/glib-2.0/glib/grand.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/grand.h" 2



typedef struct _GRand GRand;
# 49 "/usr/include/glib-2.0/glib/grand.h"
GRand* g_rand_new_with_seed (guint32 seed);
GRand* g_rand_new_with_seed_array (const guint32 *seed,
        guint seed_length);
GRand* g_rand_new (void);
void g_rand_free (GRand *rand_);
GRand* g_rand_copy (GRand *rand_);
void g_rand_set_seed (GRand *rand_,
          guint32 seed);
void g_rand_set_seed_array (GRand *rand_,
          const guint32 *seed,
          guint seed_length);



guint32 g_rand_int (GRand *rand_);
gint32 g_rand_int_range (GRand *rand_,
          gint32 begin,
          gint32 end);
gdouble g_rand_double (GRand *rand_);
gdouble g_rand_double_range (GRand *rand_,
          gdouble begin,
          gdouble end);
void g_random_set_seed (guint32 seed);



guint32 g_random_int (void);
gint32 g_random_int_range (gint32 begin,
          gint32 end);
gdouble g_random_double (void);
gdouble g_random_double_range (gdouble begin,
          gdouble end);



# 74 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gregex.h" 1
# 29 "/usr/include/glib-2.0/glib/gregex.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 30 "/usr/include/glib-2.0/glib/gregex.h" 2
# 1 "/usr/include/glib-2.0/glib/gstring.h" 1
# 31 "/usr/include/glib-2.0/glib/gregex.h" 2


# 117 "/usr/include/glib-2.0/glib/gregex.h"
typedef enum
{
  G_REGEX_ERROR_COMPILE,
  G_REGEX_ERROR_OPTIMIZE,
  G_REGEX_ERROR_REPLACE,
  G_REGEX_ERROR_MATCH,
  G_REGEX_ERROR_INTERNAL,


  G_REGEX_ERROR_STRAY_BACKSLASH = 101,
  G_REGEX_ERROR_MISSING_CONTROL_CHAR = 102,
  G_REGEX_ERROR_UNRECOGNIZED_ESCAPE = 103,
  G_REGEX_ERROR_QUANTIFIERS_OUT_OF_ORDER = 104,
  G_REGEX_ERROR_QUANTIFIER_TOO_BIG = 105,
  G_REGEX_ERROR_UNTERMINATED_CHARACTER_CLASS = 106,
  G_REGEX_ERROR_INVALID_ESCAPE_IN_CHARACTER_CLASS = 107,
  G_REGEX_ERROR_RANGE_OUT_OF_ORDER = 108,
  G_REGEX_ERROR_NOTHING_TO_REPEAT = 109,
  G_REGEX_ERROR_UNRECOGNIZED_CHARACTER = 112,
  G_REGEX_ERROR_POSIX_NAMED_CLASS_OUTSIDE_CLASS = 113,
  G_REGEX_ERROR_UNMATCHED_PARENTHESIS = 114,
  G_REGEX_ERROR_INEXISTENT_SUBPATTERN_REFERENCE = 115,
  G_REGEX_ERROR_UNTERMINATED_COMMENT = 118,
  G_REGEX_ERROR_EXPRESSION_TOO_LARGE = 120,
  G_REGEX_ERROR_MEMORY_ERROR = 121,
  G_REGEX_ERROR_VARIABLE_LENGTH_LOOKBEHIND = 125,
  G_REGEX_ERROR_MALFORMED_CONDITION = 126,
  G_REGEX_ERROR_TOO_MANY_CONDITIONAL_BRANCHES = 127,
  G_REGEX_ERROR_ASSERTION_EXPECTED = 128,
  G_REGEX_ERROR_UNKNOWN_POSIX_CLASS_NAME = 130,
  G_REGEX_ERROR_POSIX_COLLATING_ELEMENTS_NOT_SUPPORTED = 131,
  G_REGEX_ERROR_HEX_CODE_TOO_LARGE = 134,
  G_REGEX_ERROR_INVALID_CONDITION = 135,
  G_REGEX_ERROR_SINGLE_BYTE_MATCH_IN_LOOKBEHIND = 136,
  G_REGEX_ERROR_INFINITE_LOOP = 140,
  G_REGEX_ERROR_MISSING_SUBPATTERN_NAME_TERMINATOR = 142,
  G_REGEX_ERROR_DUPLICATE_SUBPATTERN_NAME = 143,
  G_REGEX_ERROR_MALFORMED_PROPERTY = 146,
  G_REGEX_ERROR_UNKNOWN_PROPERTY = 147,
  G_REGEX_ERROR_SUBPATTERN_NAME_TOO_LONG = 148,
  G_REGEX_ERROR_TOO_MANY_SUBPATTERNS = 149,
  G_REGEX_ERROR_INVALID_OCTAL_VALUE = 151,
  G_REGEX_ERROR_TOO_MANY_BRANCHES_IN_DEFINE = 154,
  G_REGEX_ERROR_DEFINE_REPETION = 155,
  G_REGEX_ERROR_INCONSISTENT_NEWLINE_OPTIONS = 156,
  G_REGEX_ERROR_MISSING_BACK_REFERENCE = 157
} GRegexError;
# 176 "/usr/include/glib-2.0/glib/gregex.h"
GQuark g_regex_error_quark (void);
# 243 "/usr/include/glib-2.0/glib/gregex.h"
typedef enum
{
  G_REGEX_CASELESS = 1 << 0,
  G_REGEX_MULTILINE = 1 << 1,
  G_REGEX_DOTALL = 1 << 2,
  G_REGEX_EXTENDED = 1 << 3,
  G_REGEX_ANCHORED = 1 << 4,
  G_REGEX_DOLLAR_ENDONLY = 1 << 5,
  G_REGEX_UNGREEDY = 1 << 9,
  G_REGEX_RAW = 1 << 11,
  G_REGEX_NO_AUTO_CAPTURE = 1 << 12,
  G_REGEX_OPTIMIZE = 1 << 13,
  G_REGEX_DUPNAMES = 1 << 19,
  G_REGEX_NEWLINE_CR = 1 << 20,
  G_REGEX_NEWLINE_LF = 1 << 21,
  G_REGEX_NEWLINE_CRLF = G_REGEX_NEWLINE_CR | G_REGEX_NEWLINE_LF
} GRegexCompileFlags;
# 306 "/usr/include/glib-2.0/glib/gregex.h"
typedef enum
{
  G_REGEX_MATCH_ANCHORED = 1 << 4,
  G_REGEX_MATCH_NOTBOL = 1 << 7,
  G_REGEX_MATCH_NOTEOL = 1 << 8,
  G_REGEX_MATCH_NOTEMPTY = 1 << 10,
  G_REGEX_MATCH_PARTIAL = 1 << 15,
  G_REGEX_MATCH_NEWLINE_CR = 1 << 20,
  G_REGEX_MATCH_NEWLINE_LF = 1 << 21,
  G_REGEX_MATCH_NEWLINE_CRLF = G_REGEX_MATCH_NEWLINE_CR | G_REGEX_MATCH_NEWLINE_LF,
  G_REGEX_MATCH_NEWLINE_ANY = 1 << 22
} GRegexMatchFlags;
# 327 "/usr/include/glib-2.0/glib/gregex.h"
typedef struct _GRegex GRegex;


typedef struct _GMatchInfo GMatchInfo;
# 349 "/usr/include/glib-2.0/glib/gregex.h"
typedef gboolean (*GRegexEvalCallback) (const GMatchInfo *match_info,
       GString *result,
       gpointer user_data);


GRegex *g_regex_new (const gchar *pattern,
       GRegexCompileFlags compile_options,
       GRegexMatchFlags match_options,
       GError **error);
GRegex *g_regex_ref (GRegex *regex);
void g_regex_unref (GRegex *regex);
const gchar *g_regex_get_pattern (const GRegex *regex);
gint g_regex_get_max_backref (const GRegex *regex);
gint g_regex_get_capture_count (const GRegex *regex);
gint g_regex_get_string_number (const GRegex *regex,
       const gchar *name);
gchar *g_regex_escape_string (const gchar *string,
       gint length);
gchar *g_regex_escape_nul (const gchar *string,
       gint length);

GRegexCompileFlags g_regex_get_compile_flags (const GRegex *regex);
GRegexMatchFlags g_regex_get_match_flags (const GRegex *regex);


gboolean g_regex_match_simple (const gchar *pattern,
       const gchar *string,
       GRegexCompileFlags compile_options,
       GRegexMatchFlags match_options);
gboolean g_regex_match (const GRegex *regex,
       const gchar *string,
       GRegexMatchFlags match_options,
       GMatchInfo **match_info);
gboolean g_regex_match_full (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       GRegexMatchFlags match_options,
       GMatchInfo **match_info,
       GError **error);
gboolean g_regex_match_all (const GRegex *regex,
       const gchar *string,
       GRegexMatchFlags match_options,
       GMatchInfo **match_info);
gboolean g_regex_match_all_full (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       GRegexMatchFlags match_options,
       GMatchInfo **match_info,
       GError **error);


gchar **g_regex_split_simple (const gchar *pattern,
       const gchar *string,
       GRegexCompileFlags compile_options,
       GRegexMatchFlags match_options);
gchar **g_regex_split (const GRegex *regex,
       const gchar *string,
       GRegexMatchFlags match_options);
gchar **g_regex_split_full (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       GRegexMatchFlags match_options,
       gint max_tokens,
       GError **error);


gchar *g_regex_replace (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       const gchar *replacement,
       GRegexMatchFlags match_options,
       GError **error);
gchar *g_regex_replace_literal (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       const gchar *replacement,
       GRegexMatchFlags match_options,
       GError **error);
gchar *g_regex_replace_eval (const GRegex *regex,
       const gchar *string,
       gssize string_len,
       gint start_position,
       GRegexMatchFlags match_options,
       GRegexEvalCallback eval,
       gpointer user_data,
       GError **error);
gboolean g_regex_check_replacement (const gchar *replacement,
       gboolean *has_references,
       GError **error);


GRegex *g_match_info_get_regex (const GMatchInfo *match_info);
const gchar *g_match_info_get_string (const GMatchInfo *match_info);

GMatchInfo *g_match_info_ref (GMatchInfo *match_info);
void g_match_info_unref (GMatchInfo *match_info);
void g_match_info_free (GMatchInfo *match_info);
gboolean g_match_info_next (GMatchInfo *match_info,
       GError **error);
gboolean g_match_info_matches (const GMatchInfo *match_info);
gint g_match_info_get_match_count (const GMatchInfo *match_info);
gboolean g_match_info_is_partial_match (const GMatchInfo *match_info);
gchar *g_match_info_expand_references(const GMatchInfo *match_info,
       const gchar *string_to_expand,
       GError **error);
gchar *g_match_info_fetch (const GMatchInfo *match_info,
       gint match_num);
gboolean g_match_info_fetch_pos (const GMatchInfo *match_info,
       gint match_num,
       gint *start_pos,
       gint *end_pos);
gchar *g_match_info_fetch_named (const GMatchInfo *match_info,
       const gchar *name);
gboolean g_match_info_fetch_named_pos (const GMatchInfo *match_info,
       const gchar *name,
       gint *start_pos,
       gint *end_pos);
gchar **g_match_info_fetch_all (const GMatchInfo *match_info);


# 75 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gscanner.h" 1
# 34 "/usr/include/glib-2.0/glib/gscanner.h"
# 1 "/usr/include/glib-2.0/glib/gdataset.h" 1
# 35 "/usr/include/glib-2.0/glib/gscanner.h" 2
# 1 "/usr/include/glib-2.0/glib/ghash.h" 1
# 36 "/usr/include/glib-2.0/glib/gscanner.h" 2



typedef struct _GScanner GScanner;
typedef struct _GScannerConfig GScannerConfig;
typedef union _GTokenValue GTokenValue;

typedef void (*GScannerMsgFunc) (GScanner *scanner,
       gchar *message,
       gboolean error);
# 64 "/usr/include/glib-2.0/glib/gscanner.h"
typedef enum
{
  G_ERR_UNKNOWN,
  G_ERR_UNEXP_EOF,
  G_ERR_UNEXP_EOF_IN_STRING,
  G_ERR_UNEXP_EOF_IN_COMMENT,
  G_ERR_NON_DIGIT_IN_CONST,
  G_ERR_DIGIT_RADIX,
  G_ERR_FLOAT_RADIX,
  G_ERR_FLOAT_MALFORMED
} GErrorType;


typedef enum
{
  G_TOKEN_EOF = 0,

  G_TOKEN_LEFT_PAREN = '(',
  G_TOKEN_RIGHT_PAREN = ')',
  G_TOKEN_LEFT_CURLY = '{',
  G_TOKEN_RIGHT_CURLY = '}',
  G_TOKEN_LEFT_BRACE = '[',
  G_TOKEN_RIGHT_BRACE = ']',
  G_TOKEN_EQUAL_SIGN = '=',
  G_TOKEN_COMMA = ',',

  G_TOKEN_NONE = 256,

  G_TOKEN_ERROR,

  G_TOKEN_CHAR,
  G_TOKEN_BINARY,
  G_TOKEN_OCTAL,
  G_TOKEN_INT,
  G_TOKEN_HEX,
  G_TOKEN_FLOAT,
  G_TOKEN_STRING,

  G_TOKEN_SYMBOL,
  G_TOKEN_IDENTIFIER,
  G_TOKEN_IDENTIFIER_NULL,

  G_TOKEN_COMMENT_SINGLE,
  G_TOKEN_COMMENT_MULTI,


  G_TOKEN_LAST
} GTokenType;

union _GTokenValue
{
  gpointer v_symbol;
  gchar *v_identifier;
  gulong v_binary;
  gulong v_octal;
  gulong v_int;
  guint64 v_int64;
  gdouble v_float;
  gulong v_hex;
  gchar *v_string;
  gchar *v_comment;
  guchar v_char;
  guint v_error;
};

struct _GScannerConfig
{


  gchar *cset_skip_characters;
  gchar *cset_identifier_first;
  gchar *cset_identifier_nth;
  gchar *cpair_comment_single;



  guint case_sensitive : 1;




  guint skip_comment_multi : 1;
  guint skip_comment_single : 1;
  guint scan_comment_multi : 1;
  guint scan_identifier : 1;
  guint scan_identifier_1char : 1;
  guint scan_identifier_NULL : 1;
  guint scan_symbols : 1;
  guint scan_binary : 1;
  guint scan_octal : 1;
  guint scan_float : 1;
  guint scan_hex : 1;
  guint scan_hex_dollar : 1;
  guint scan_string_sq : 1;
  guint scan_string_dq : 1;
  guint numbers_2_int : 1;
  guint int_2_float : 1;
  guint identifier_2_string : 1;
  guint char_2_token : 1;
  guint symbol_2_token : 1;
  guint scope_0_fallback : 1;
  guint store_int64 : 1;


  guint padding_dummy;
};

struct _GScanner
{

  gpointer user_data;
  guint max_parse_errors;


  guint parse_errors;


  const gchar *input_name;


  GData *qdata;


  GScannerConfig *config;


  GTokenType token;
  GTokenValue value;
  guint line;
  guint position;


  GTokenType next_token;
  GTokenValue next_value;
  guint next_line;
  guint next_position;



  GHashTable *symbol_table;
  gint input_fd;
  const gchar *text;
  const gchar *text_end;
  gchar *buffer;
  guint scope_id;



  GScannerMsgFunc msg_handler;
};

GScanner* g_scanner_new (const GScannerConfig *config_templ);
void g_scanner_destroy (GScanner *scanner);
void g_scanner_input_file (GScanner *scanner,
       gint input_fd);
void g_scanner_sync_file_offset (GScanner *scanner);
void g_scanner_input_text (GScanner *scanner,
       const gchar *text,
       guint text_len);
GTokenType g_scanner_get_next_token (GScanner *scanner);
GTokenType g_scanner_peek_next_token (GScanner *scanner);
GTokenType g_scanner_cur_token (GScanner *scanner);
GTokenValue g_scanner_cur_value (GScanner *scanner);
guint g_scanner_cur_line (GScanner *scanner);
guint g_scanner_cur_position (GScanner *scanner);
gboolean g_scanner_eof (GScanner *scanner);
guint g_scanner_set_scope (GScanner *scanner,
       guint scope_id);
void g_scanner_scope_add_symbol (GScanner *scanner,
       guint scope_id,
       const gchar *symbol,
       gpointer value);
void g_scanner_scope_remove_symbol (GScanner *scanner,
       guint scope_id,
       const gchar *symbol);
gpointer g_scanner_scope_lookup_symbol (GScanner *scanner,
       guint scope_id,
       const gchar *symbol);
void g_scanner_scope_foreach_symbol (GScanner *scanner,
       guint scope_id,
       GHFunc func,
       gpointer user_data);
gpointer g_scanner_lookup_symbol (GScanner *scanner,
       const gchar *symbol);
void g_scanner_unexp_token (GScanner *scanner,
       GTokenType expected_token,
       const gchar *identifier_spec,
       const gchar *symbol_spec,
       const gchar *symbol_name,
       const gchar *message,
       gint is_error);
void g_scanner_error (GScanner *scanner,
       const gchar *format,
       ...) __attribute__((__format__ (__printf__, 2, 3)));
void g_scanner_warn (GScanner *scanner,
       const gchar *format,
       ...) __attribute__((__format__ (__printf__, 2, 3)));
# 282 "/usr/include/glib-2.0/glib/gscanner.h"

# 76 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gsequence.h" 1
# 28 "/usr/include/glib-2.0/glib/gsequence.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 29 "/usr/include/glib-2.0/glib/gsequence.h" 2



typedef struct _GSequence GSequence;
typedef struct _GSequenceNode GSequenceIter;

typedef gint (* GSequenceIterCompareFunc) (GSequenceIter *a,
                                           GSequenceIter *b,
                                           gpointer data);



GSequence * g_sequence_new (GDestroyNotify data_destroy);
void g_sequence_free (GSequence *seq);
gint g_sequence_get_length (GSequence *seq);
void g_sequence_foreach (GSequence *seq,
                                              GFunc func,
                                              gpointer user_data);
void g_sequence_foreach_range (GSequenceIter *begin,
                                              GSequenceIter *end,
                                              GFunc func,
                                              gpointer user_data);
void g_sequence_sort (GSequence *seq,
                                              GCompareDataFunc cmp_func,
                                              gpointer cmp_data);
void g_sequence_sort_iter (GSequence *seq,
                                              GSequenceIterCompareFunc cmp_func,
                                              gpointer cmp_data);



GSequenceIter *g_sequence_get_begin_iter (GSequence *seq);
GSequenceIter *g_sequence_get_end_iter (GSequence *seq);
GSequenceIter *g_sequence_get_iter_at_pos (GSequence *seq,
                                              gint pos);
GSequenceIter *g_sequence_append (GSequence *seq,
                                              gpointer data);
GSequenceIter *g_sequence_prepend (GSequence *seq,
                                              gpointer data);
GSequenceIter *g_sequence_insert_before (GSequenceIter *iter,
                                              gpointer data);
void g_sequence_move (GSequenceIter *src,
                                              GSequenceIter *dest);
void g_sequence_swap (GSequenceIter *a,
                                              GSequenceIter *b);
GSequenceIter *g_sequence_insert_sorted (GSequence *seq,
                                              gpointer data,
                                              GCompareDataFunc cmp_func,
                                              gpointer cmp_data);
GSequenceIter *g_sequence_insert_sorted_iter (GSequence *seq,
                                              gpointer data,
                                              GSequenceIterCompareFunc iter_cmp,
                                              gpointer cmp_data);
void g_sequence_sort_changed (GSequenceIter *iter,
                                              GCompareDataFunc cmp_func,
                                              gpointer cmp_data);
void g_sequence_sort_changed_iter (GSequenceIter *iter,
                                              GSequenceIterCompareFunc iter_cmp,
                                              gpointer cmp_data);
void g_sequence_remove (GSequenceIter *iter);
void g_sequence_remove_range (GSequenceIter *begin,
                                              GSequenceIter *end);
void g_sequence_move_range (GSequenceIter *dest,
                                              GSequenceIter *begin,
                                              GSequenceIter *end);
GSequenceIter *g_sequence_search (GSequence *seq,
                                              gpointer data,
                                              GCompareDataFunc cmp_func,
                                              gpointer cmp_data);
GSequenceIter *g_sequence_search_iter (GSequence *seq,
                                              gpointer data,
                                              GSequenceIterCompareFunc iter_cmp,
                                              gpointer cmp_data);
GSequenceIter *g_sequence_lookup (GSequence *seq,
                                              gpointer data,
                                              GCompareDataFunc cmp_func,
                                              gpointer cmp_data);
GSequenceIter *g_sequence_lookup_iter (GSequence *seq,
                                              gpointer data,
                                              GSequenceIterCompareFunc iter_cmp,
                                              gpointer cmp_data);



gpointer g_sequence_get (GSequenceIter *iter);
void g_sequence_set (GSequenceIter *iter,
                                              gpointer data);


gboolean g_sequence_iter_is_begin (GSequenceIter *iter);
gboolean g_sequence_iter_is_end (GSequenceIter *iter);
GSequenceIter *g_sequence_iter_next (GSequenceIter *iter);
GSequenceIter *g_sequence_iter_prev (GSequenceIter *iter);
gint g_sequence_iter_get_position (GSequenceIter *iter);
GSequenceIter *g_sequence_iter_move (GSequenceIter *iter,
                                              gint delta);
GSequence * g_sequence_iter_get_sequence (GSequenceIter *iter);



gint g_sequence_iter_compare (GSequenceIter *a,
                                              GSequenceIter *b);
GSequenceIter *g_sequence_range_get_midpoint (GSequenceIter *begin,
                                              GSequenceIter *end);


# 77 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gshell.h" 1
# 28 "/usr/include/glib-2.0/glib/gshell.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 29 "/usr/include/glib-2.0/glib/gshell.h" 2





typedef enum
{

  G_SHELL_ERROR_BAD_QUOTING,

  G_SHELL_ERROR_EMPTY_STRING,
  G_SHELL_ERROR_FAILED
} GShellError;

GQuark g_shell_error_quark (void);

gchar* g_shell_quote (const gchar *unquoted_string);
gchar* g_shell_unquote (const gchar *quoted_string,
                             GError **error);
gboolean g_shell_parse_argv (const gchar *command_line,
                             gint *argcp,
                             gchar ***argvp,
                             GError **error);


# 78 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gslice.h" 1
# 27 "/usr/include/glib-2.0/glib/gslice.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 28 "/usr/include/glib-2.0/glib/gslice.h" 2





gpointer g_slice_alloc (gsize block_size) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_slice_alloc0 (gsize block_size) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
gpointer g_slice_copy (gsize block_size,
                                         gconstpointer mem_block) __attribute__((__malloc__)) __attribute__((__alloc_size__(1)));
void g_slice_free1 (gsize block_size,
      gpointer mem_block);
void g_slice_free_chain_with_offset (gsize block_size,
      gpointer mem_chain,
      gsize next_offset);
# 72 "/usr/include/glib-2.0/glib/gslice.h"
typedef enum {
  G_SLICE_CONFIG_ALWAYS_MALLOC = 1,
  G_SLICE_CONFIG_BYPASS_MAGAZINES,
  G_SLICE_CONFIG_WORKING_SET_MSECS,
  G_SLICE_CONFIG_COLOR_INCREMENT,
  G_SLICE_CONFIG_CHUNK_SIZES,
  G_SLICE_CONFIG_CONTENTION_COUNTER
} GSliceConfig;
void g_slice_set_config (GSliceConfig ckey, gint64 value);
gint64 g_slice_get_config (GSliceConfig ckey);
gint64* g_slice_get_config_state (GSliceConfig ckey, gint64 address, guint *n_values);


# 79 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gslist.h" 1
# 80 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gspawn.h" 1
# 28 "/usr/include/glib-2.0/glib/gspawn.h"
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 29 "/usr/include/glib-2.0/glib/gspawn.h" 2


# 70 "/usr/include/glib-2.0/glib/gspawn.h"
typedef enum
{
  G_SPAWN_ERROR_FORK,
  G_SPAWN_ERROR_READ,
  G_SPAWN_ERROR_CHDIR,
  G_SPAWN_ERROR_ACCES,
  G_SPAWN_ERROR_PERM,
  G_SPAWN_ERROR_TOO_BIG,

  G_SPAWN_ERROR_2BIG = G_SPAWN_ERROR_TOO_BIG,

  G_SPAWN_ERROR_NOEXEC,
  G_SPAWN_ERROR_NAMETOOLONG,
  G_SPAWN_ERROR_NOENT,
  G_SPAWN_ERROR_NOMEM,
  G_SPAWN_ERROR_NOTDIR,
  G_SPAWN_ERROR_LOOP,
  G_SPAWN_ERROR_TXTBUSY,
  G_SPAWN_ERROR_IO,
  G_SPAWN_ERROR_NFILE,
  G_SPAWN_ERROR_MFILE,
  G_SPAWN_ERROR_INVAL,
  G_SPAWN_ERROR_ISDIR,
  G_SPAWN_ERROR_LIBBAD,
  G_SPAWN_ERROR_FAILED


} GSpawnError;
# 135 "/usr/include/glib-2.0/glib/gspawn.h"
typedef void (* GSpawnChildSetupFunc) (gpointer user_data);
# 161 "/usr/include/glib-2.0/glib/gspawn.h"
typedef enum
{
  G_SPAWN_LEAVE_DESCRIPTORS_OPEN = 1 << 0,
  G_SPAWN_DO_NOT_REAP_CHILD = 1 << 1,

  G_SPAWN_SEARCH_PATH = 1 << 2,

  G_SPAWN_STDOUT_TO_DEV_NULL = 1 << 3,
  G_SPAWN_STDERR_TO_DEV_NULL = 1 << 4,
  G_SPAWN_CHILD_INHERITS_STDIN = 1 << 5,
  G_SPAWN_FILE_AND_ARGV_ZERO = 1 << 6
} GSpawnFlags;

GQuark g_spawn_error_quark (void);
# 186 "/usr/include/glib-2.0/glib/gspawn.h"
gboolean g_spawn_async (const gchar *working_directory,
                        gchar **argv,
                        gchar **envp,
                        GSpawnFlags flags,
                        GSpawnChildSetupFunc child_setup,
                        gpointer user_data,
                        GPid *child_pid,
                        GError **error);





gboolean g_spawn_async_with_pipes (const gchar *working_directory,
                                   gchar **argv,
                                   gchar **envp,
                                   GSpawnFlags flags,
                                   GSpawnChildSetupFunc child_setup,
                                   gpointer user_data,
                                   GPid *child_pid,
                                   gint *standard_input,
                                   gint *standard_output,
                                   gint *standard_error,
                                   GError **error);






gboolean g_spawn_sync (const gchar *working_directory,
                               gchar **argv,
                               gchar **envp,
                               GSpawnFlags flags,
                               GSpawnChildSetupFunc child_setup,
                               gpointer user_data,
                               gchar **standard_output,
                               gchar **standard_error,
                               gint *exit_status,
                               GError **error);

gboolean g_spawn_command_line_sync (const gchar *command_line,
                                     gchar **standard_output,
                                     gchar **standard_error,
                                     gint *exit_status,
                                     GError **error);
gboolean g_spawn_command_line_async (const gchar *command_line,
                                     GError **error);

void g_spawn_close_pid (GPid pid);


# 81 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gstrfuncs.h" 1
# 35 "/usr/include/glib-2.0/glib/gstrfuncs.h"
# 1 "/usr/include/glib-2.0/glib/gmacros.h" 1
# 36 "/usr/include/glib-2.0/glib/gstrfuncs.h" 2
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 37 "/usr/include/glib-2.0/glib/gstrfuncs.h" 2




typedef enum {
  G_ASCII_ALNUM = 1 << 0,
  G_ASCII_ALPHA = 1 << 1,
  G_ASCII_CNTRL = 1 << 2,
  G_ASCII_DIGIT = 1 << 3,
  G_ASCII_GRAPH = 1 << 4,
  G_ASCII_LOWER = 1 << 5,
  G_ASCII_PRINT = 1 << 6,
  G_ASCII_PUNCT = 1 << 7,
  G_ASCII_SPACE = 1 << 8,
  G_ASCII_UPPER = 1 << 9,
  G_ASCII_XDIGIT = 1 << 10
} GAsciiType;

extern const guint16 * const g_ascii_table;
# 90 "/usr/include/glib-2.0/glib/gstrfuncs.h"
gchar g_ascii_tolower (gchar c) __attribute__((__const__));
gchar g_ascii_toupper (gchar c) __attribute__((__const__));

gint g_ascii_digit_value (gchar c) __attribute__((__const__));
gint g_ascii_xdigit_value (gchar c) __attribute__((__const__));





gchar* g_strdelimit (gchar *string,
     const gchar *delimiters,
     gchar new_delimiter);
gchar* g_strcanon (gchar *string,
     const gchar *valid_chars,
     gchar substitutor);
const gchar * g_strerror (gint errnum) __attribute__((__const__));
const gchar * g_strsignal (gint signum) __attribute__((__const__));
gchar * g_strreverse (gchar *string);
gsize g_strlcpy (gchar *dest,
     const gchar *src,
     gsize dest_size);
gsize g_strlcat (gchar *dest,
     const gchar *src,
     gsize dest_size);
gchar * g_strstr_len (const gchar *haystack,
     gssize haystack_len,
     const gchar *needle);
gchar * g_strrstr (const gchar *haystack,
     const gchar *needle);
gchar * g_strrstr_len (const gchar *haystack,
     gssize haystack_len,
     const gchar *needle);

gboolean g_str_has_suffix (const gchar *str,
     const gchar *suffix);
gboolean g_str_has_prefix (const gchar *str,
     const gchar *prefix);



gdouble g_strtod (const gchar *nptr,
     gchar **endptr);
gdouble g_ascii_strtod (const gchar *nptr,
     gchar **endptr);
guint64 g_ascii_strtoull (const gchar *nptr,
     gchar **endptr,
     guint base);
gint64 g_ascii_strtoll (const gchar *nptr,
     gchar **endptr,
     guint base);




gchar * g_ascii_dtostr (gchar *buffer,
     gint buf_len,
     gdouble d);
gchar * g_ascii_formatd (gchar *buffer,
     gint buf_len,
     const gchar *format,
     gdouble d);


gchar* g_strchug (gchar *string);

gchar* g_strchomp (gchar *string);



gint g_ascii_strcasecmp (const gchar *s1,
        const gchar *s2);
gint g_ascii_strncasecmp (const gchar *s1,
        const gchar *s2,
        gsize n);
gchar* g_ascii_strdown (const gchar *str,
        gssize len) __attribute__((__malloc__));
gchar* g_ascii_strup (const gchar *str,
        gssize len) __attribute__((__malloc__));


__attribute__((__deprecated__))
gint g_strcasecmp (const gchar *s1,
                                        const gchar *s2);
__attribute__((__deprecated__))
gint g_strncasecmp (const gchar *s1,
                                        const gchar *s2,
                                        guint n);
__attribute__((__deprecated__))
gchar* g_strdown (gchar *string);
__attribute__((__deprecated__))
gchar* g_strup (gchar *string);





gchar* g_strdup (const gchar *str) __attribute__((__malloc__));
gchar* g_strdup_printf (const gchar *format,
     ...) __attribute__((__format__ (__printf__, 1, 2))) __attribute__((__malloc__));
gchar* g_strdup_vprintf (const gchar *format,
     va_list args) __attribute__((__malloc__));
gchar* g_strndup (const gchar *str,
     gsize n) __attribute__((__malloc__));
gchar* g_strnfill (gsize length,
     gchar fill_char) __attribute__((__malloc__));
gchar* g_strconcat (const gchar *string1,
     ...) __attribute__((__malloc__)) __attribute__((__sentinel__));
gchar* g_strjoin (const gchar *separator,
     ...) __attribute__((__malloc__)) __attribute__((__sentinel__));





gchar* g_strcompress (const gchar *source) __attribute__((__malloc__));
# 215 "/usr/include/glib-2.0/glib/gstrfuncs.h"
gchar* g_strescape (const gchar *source,
     const gchar *exceptions) __attribute__((__malloc__));

gpointer g_memdup (gconstpointer mem,
     guint byte_size) __attribute__((__malloc__)) __attribute__((__alloc_size__(2)));
# 230 "/usr/include/glib-2.0/glib/gstrfuncs.h"
gchar** g_strsplit (const gchar *string,
     const gchar *delimiter,
     gint max_tokens) __attribute__((__malloc__));
gchar ** g_strsplit_set (const gchar *string,
     const gchar *delimiters,
     gint max_tokens) __attribute__((__malloc__));
gchar* g_strjoinv (const gchar *separator,
     gchar **str_array) __attribute__((__malloc__));
void g_strfreev (gchar **str_array);
gchar** g_strdupv (gchar **str_array) __attribute__((__malloc__));
guint g_strv_length (gchar **str_array);

gchar* g_stpcpy (gchar *dest,
                                        const char *src);


# 82 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gstring.h" 1
# 83 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gstringchunk.h" 1
# 34 "/usr/include/glib-2.0/glib/gstringchunk.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gstringchunk.h" 2



typedef struct _GStringChunk GStringChunk;

GStringChunk* g_string_chunk_new (gsize size);
void g_string_chunk_free (GStringChunk *chunk);
void g_string_chunk_clear (GStringChunk *chunk);
gchar* g_string_chunk_insert (GStringChunk *chunk,
                                           const gchar *string);
gchar* g_string_chunk_insert_len (GStringChunk *chunk,
                                           const gchar *string,
                                           gssize len);
gchar* g_string_chunk_insert_const (GStringChunk *chunk,
                                           const gchar *string);


# 84 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtestutils.h" 1
# 28 "/usr/include/glib-2.0/glib/gtestutils.h"
# 1 "/usr/include/glib-2.0/glib/gmessages.h" 1
# 29 "/usr/include/glib-2.0/glib/gtestutils.h" 2
# 1 "/usr/include/glib-2.0/glib/gstring.h" 1
# 30 "/usr/include/glib-2.0/glib/gtestutils.h" 2
# 1 "/usr/include/glib-2.0/glib/gerror.h" 1
# 31 "/usr/include/glib-2.0/glib/gtestutils.h" 2
# 1 "/usr/include/glib-2.0/glib/gslist.h" 1
# 32 "/usr/include/glib-2.0/glib/gtestutils.h" 2



typedef struct GTestCase GTestCase;
typedef struct GTestSuite GTestSuite;
typedef void (*GTestFunc) (void);
typedef void (*GTestDataFunc) (gconstpointer user_data);
typedef void (*GTestFixtureFunc) (gpointer fixture,
                                  gconstpointer user_data);
# 79 "/usr/include/glib-2.0/glib/gtestutils.h"
int g_strcmp0 (const char *str1,
                                         const char *str2);


void g_test_minimized_result (double minimized_quantity,
                                         const char *format,
                                         ...) __attribute__((__format__ (__printf__, 2, 3)));
void g_test_maximized_result (double maximized_quantity,
                                         const char *format,
                                         ...) __attribute__((__format__ (__printf__, 2, 3)));


void g_test_init (int *argc,
                                         char ***argv,
                                         ...);
# 103 "/usr/include/glib-2.0/glib/gtestutils.h"
int g_test_run (void);

void g_test_add_func (const char *testpath,
                                         GTestFunc test_func);

void g_test_add_data_func (const char *testpath,
                                         gconstpointer test_data,
                                         GTestDataFunc test_func);

void g_test_fail (void);
# 128 "/usr/include/glib-2.0/glib/gtestutils.h"
void g_test_message (const char *format,
                                         ...) __attribute__((__format__ (__printf__, 1, 2)));
void g_test_bug_base (const char *uri_pattern);
void g_test_bug (const char *bug_uri_snippet);

void g_test_timer_start (void);
double g_test_timer_elapsed (void);
double g_test_timer_last (void);


void g_test_queue_free (gpointer gfree_pointer);
void g_test_queue_destroy (GDestroyNotify destroy_func,
                                         gpointer destroy_data);



typedef enum {
  G_TEST_TRAP_SILENCE_STDOUT = 1 << 7,
  G_TEST_TRAP_SILENCE_STDERR = 1 << 8,
  G_TEST_TRAP_INHERIT_STDIN = 1 << 9
} GTestTrapFlags;
gboolean g_test_trap_fork (guint64 usec_timeout,
                                         GTestTrapFlags test_trap_flags);
gboolean g_test_trap_has_passed (void);
gboolean g_test_trap_reached_timeout (void);
# 162 "/usr/include/glib-2.0/glib/gtestutils.h"
gint32 g_test_rand_int (void);
gint32 g_test_rand_int_range (gint32 begin,
                                         gint32 end);
double g_test_rand_double (void);
double g_test_rand_double_range (double range_start,
                                         double range_end);


GTestCase* g_test_create_case (const char *test_name,
                                         gsize data_size,
                                         gconstpointer test_data,
                                         GTestFixtureFunc data_setup,
                                         GTestFixtureFunc data_test,
                                         GTestFixtureFunc data_teardown);
GTestSuite* g_test_create_suite (const char *suite_name);
GTestSuite* g_test_get_root (void);
void g_test_suite_add (GTestSuite *suite,
                                         GTestCase *test_case);
void g_test_suite_add_suite (GTestSuite *suite,
                                         GTestSuite *nestedsuite);
int g_test_run_suite (GTestSuite *suite);


void g_test_trap_assertions (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         guint64 assertion_flags,
                                         const char *pattern);
void g_assertion_message (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         const char *message) __attribute__((__noreturn__));
void g_assertion_message_expr (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         const char *expr) __attribute__((__noreturn__));
void g_assertion_message_cmpstr (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         const char *expr,
                                         const char *arg1,
                                         const char *cmp,
                                         const char *arg2) __attribute__((__noreturn__));
void g_assertion_message_cmpnum (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         const char *expr,
                                         long double arg1,
                                         const char *cmp,
                                         long double arg2,
                                         char numtype) __attribute__((__noreturn__));
void g_assertion_message_error (const char *domain,
                                         const char *file,
                                         int line,
                                         const char *func,
                                         const char *expr,
                                         const GError *error,
                                         GQuark error_domain,
                                         int error_code) __attribute__((__noreturn__));
void g_test_add_vtable (const char *testpath,
                                         gsize data_size,
                                         gconstpointer test_data,
                                         GTestFixtureFunc data_setup,
                                         GTestFixtureFunc data_test,
                                         GTestFixtureFunc data_teardown);
typedef struct {
  gboolean test_initialized;
  gboolean test_quick;
  gboolean test_perf;
  gboolean test_verbose;
  gboolean test_quiet;
  gboolean test_undefined;
} GTestConfig;
extern const GTestConfig * const g_test_config_vars;


typedef enum {
  G_TEST_LOG_NONE,
  G_TEST_LOG_ERROR,
  G_TEST_LOG_START_BINARY,
  G_TEST_LOG_LIST_CASE,
  G_TEST_LOG_SKIP_CASE,
  G_TEST_LOG_START_CASE,
  G_TEST_LOG_STOP_CASE,
  G_TEST_LOG_MIN_RESULT,
  G_TEST_LOG_MAX_RESULT,
  G_TEST_LOG_MESSAGE
} GTestLogType;

typedef struct {
  GTestLogType log_type;
  guint n_strings;
  gchar **strings;
  guint n_nums;
  long double *nums;
} GTestLogMsg;
typedef struct {

  GString *data;
  GSList *msgs;
} GTestLogBuffer;

const char* g_test_log_type_name (GTestLogType log_type);
GTestLogBuffer* g_test_log_buffer_new (void);
void g_test_log_buffer_free (GTestLogBuffer *tbuffer);
void g_test_log_buffer_push (GTestLogBuffer *tbuffer,
                                         guint n_bytes,
                                         const guint8 *bytes);
GTestLogMsg* g_test_log_buffer_pop (GTestLogBuffer *tbuffer);
void g_test_log_msg_free (GTestLogMsg *tmsg);
# 291 "/usr/include/glib-2.0/glib/gtestutils.h"
typedef gboolean (*GTestLogFatalFunc) (const gchar *log_domain,
                                                 GLogLevelFlags log_level,
                                                 const gchar *message,
                                                 gpointer user_data);
void
g_test_log_set_fatal_handler (GTestLogFatalFunc log_func,
                                         gpointer user_data);


# 85 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gthread.h" 1
# 86 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gthreadpool.h" 1
# 34 "/usr/include/glib-2.0/glib/gthreadpool.h"
# 1 "/usr/include/glib-2.0/glib/gthread.h" 1
# 35 "/usr/include/glib-2.0/glib/gthreadpool.h" 2



typedef struct _GThreadPool GThreadPool;




struct _GThreadPool
{
  GFunc func;
  gpointer user_data;
  gboolean exclusive;
};

GThreadPool * g_thread_pool_new (GFunc func,
                                                 gpointer user_data,
                                                 gint max_threads,
                                                 gboolean exclusive,
                                                 GError **error);
void g_thread_pool_free (GThreadPool *pool,
                                                 gboolean immediate,
                                                 gboolean wait_);
gboolean g_thread_pool_push (GThreadPool *pool,
                                                 gpointer data,
                                                 GError **error);
guint g_thread_pool_unprocessed (GThreadPool *pool);
void g_thread_pool_set_sort_function (GThreadPool *pool,
                                                 GCompareDataFunc func,
                                                 gpointer user_data);
gboolean g_thread_pool_set_max_threads (GThreadPool *pool,
                                                 gint max_threads,
                                                 GError **error);
gint g_thread_pool_get_max_threads (GThreadPool *pool);
guint g_thread_pool_get_num_threads (GThreadPool *pool);

void g_thread_pool_set_max_unused_threads (gint max_threads);
gint g_thread_pool_get_max_unused_threads (void);
guint g_thread_pool_get_num_unused_threads (void);
void g_thread_pool_stop_unused_threads (void);
void g_thread_pool_set_max_idle_time (guint interval);
guint g_thread_pool_get_max_idle_time (void);


# 87 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtimer.h" 1
# 34 "/usr/include/glib-2.0/glib/gtimer.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gtimer.h" 2







typedef struct _GTimer GTimer;



GTimer* g_timer_new (void);
void g_timer_destroy (GTimer *timer);
void g_timer_start (GTimer *timer);
void g_timer_stop (GTimer *timer);
void g_timer_reset (GTimer *timer);
void g_timer_continue (GTimer *timer);
gdouble g_timer_elapsed (GTimer *timer,
      gulong *microseconds);

void g_usleep (gulong microseconds);

void g_time_val_add (GTimeVal *time_,
                                  glong microseconds);
gboolean g_time_val_from_iso8601 (const gchar *iso_date,
      GTimeVal *time_);
gchar* g_time_val_to_iso8601 (GTimeVal *time_) __attribute__((__malloc__));


# 88 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtimezone.h" 1
# 89 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtrashstack.h" 1
# 34 "/usr/include/glib-2.0/glib/gtrashstack.h"
# 1 "/usr/include/glib-2.0/glib/gutils.h" 1
# 35 "/usr/include/glib-2.0/glib/gtrashstack.h" 2



typedef struct _GTrashStack GTrashStack;
struct _GTrashStack
{
  GTrashStack *next;
};

static __inline __attribute__ ((unused)) void g_trash_stack_push (GTrashStack **stack_p,
                                              gpointer data_p);
static __inline __attribute__ ((unused)) gpointer g_trash_stack_pop (GTrashStack **stack_p);
static __inline __attribute__ ((unused)) gpointer g_trash_stack_peek (GTrashStack **stack_p);
static __inline __attribute__ ((unused)) guint g_trash_stack_height (GTrashStack **stack_p);



static __inline __attribute__ ((unused)) void
g_trash_stack_push (GTrashStack **stack_p,
                    gpointer data_p)
{
  GTrashStack *data = (GTrashStack *) data_p;

  data->next = *stack_p;
  *stack_p = data;
}
static __inline __attribute__ ((unused)) gpointer
g_trash_stack_pop (GTrashStack **stack_p)
{
  GTrashStack *data;

  data = *stack_p;
  if (data)
    {
      *stack_p = data->next;



      data->next = ((void *)0);
    }

  return data;
}
static __inline __attribute__ ((unused)) gpointer
g_trash_stack_peek (GTrashStack **stack_p)
{
  GTrashStack *data;

  data = *stack_p;

  return data;
}
static __inline __attribute__ ((unused)) guint
g_trash_stack_height (GTrashStack **stack_p)
{
  GTrashStack *data;
  guint i = 0;

  for (data = *stack_p; data; data = data->next)
    i++;

  return i;
}




# 90 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtree.h" 1
# 34 "/usr/include/glib-2.0/glib/gtree.h"
# 1 "/usr/include/glib-2.0/glib/gnode.h" 1
# 35 "/usr/include/glib-2.0/glib/gtree.h" 2



typedef struct _GTree GTree;

typedef gboolean (*GTraverseFunc) (gpointer key,
                                   gpointer value,
                                   gpointer data);



GTree* g_tree_new (GCompareFunc key_compare_func);
GTree* g_tree_new_with_data (GCompareDataFunc key_compare_func,
                                 gpointer key_compare_data);
GTree* g_tree_new_full (GCompareDataFunc key_compare_func,
                                 gpointer key_compare_data,
                                 GDestroyNotify key_destroy_func,
                                 GDestroyNotify value_destroy_func);
GTree* g_tree_ref (GTree *tree);
void g_tree_unref (GTree *tree);
void g_tree_destroy (GTree *tree);
void g_tree_insert (GTree *tree,
                                 gpointer key,
                                 gpointer value);
void g_tree_replace (GTree *tree,
                                 gpointer key,
                                 gpointer value);
gboolean g_tree_remove (GTree *tree,
                                 gconstpointer key);
gboolean g_tree_steal (GTree *tree,
                                 gconstpointer key);
gpointer g_tree_lookup (GTree *tree,
                                 gconstpointer key);
gboolean g_tree_lookup_extended (GTree *tree,
                                 gconstpointer lookup_key,
                                 gpointer *orig_key,
                                 gpointer *value);
void g_tree_foreach (GTree *tree,
                                 GTraverseFunc func,
                                 gpointer user_data);

__attribute__((__deprecated__))
void g_tree_traverse (GTree *tree,
                                 GTraverseFunc traverse_func,
                                 GTraverseType traverse_type,
                                 gpointer user_data);

gpointer g_tree_search (GTree *tree,
                                 GCompareFunc search_func,
                                 gconstpointer user_data);
gint g_tree_height (GTree *tree);
gint g_tree_nnodes (GTree *tree);


# 91 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 92 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gunicode.h" 1
# 93 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gurifuncs.h" 1
# 30 "/usr/include/glib-2.0/glib/gurifuncs.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 31 "/usr/include/glib-2.0/glib/gurifuncs.h" 2


# 69 "/usr/include/glib-2.0/glib/gurifuncs.h"
char * g_uri_unescape_string (const char *escaped_string,
          const char *illegal_characters);
char * g_uri_unescape_segment (const char *escaped_string,
          const char *escaped_string_end,
          const char *illegal_characters);
char * g_uri_parse_scheme (const char *uri);
char * g_uri_escape_string (const char *unescaped,
          const char *reserved_chars_allowed,
          gboolean allow_utf8);


# 94 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gutils.h" 1
# 95 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gvarianttype.h" 1
# 30 "/usr/include/glib-2.0/glib/gvarianttype.h"
# 1 "/usr/include/glib-2.0/glib/gmessages.h" 1
# 31 "/usr/include/glib-2.0/glib/gvarianttype.h" 2
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 32 "/usr/include/glib-2.0/glib/gvarianttype.h" 2


# 44 "/usr/include/glib-2.0/glib/gvarianttype.h"
typedef struct _GVariantType GVariantType;
# 297 "/usr/include/glib-2.0/glib/gvarianttype.h"
gboolean g_variant_type_string_is_valid (const gchar *type_string);
gboolean g_variant_type_string_scan (const gchar *string,
                                                                         const gchar *limit,
                                                                         const gchar **endptr);


void g_variant_type_free (GVariantType *type);
GVariantType * g_variant_type_copy (const GVariantType *type);
GVariantType * g_variant_type_new (const gchar *type_string);


gsize g_variant_type_get_string_length (const GVariantType *type);
const gchar * g_variant_type_peek_string (const GVariantType *type);
gchar * g_variant_type_dup_string (const GVariantType *type);


gboolean g_variant_type_is_definite (const GVariantType *type);
gboolean g_variant_type_is_container (const GVariantType *type);
gboolean g_variant_type_is_basic (const GVariantType *type);
gboolean g_variant_type_is_maybe (const GVariantType *type);
gboolean g_variant_type_is_array (const GVariantType *type);
gboolean g_variant_type_is_tuple (const GVariantType *type);
gboolean g_variant_type_is_dict_entry (const GVariantType *type);
gboolean g_variant_type_is_variant (const GVariantType *type);


guint g_variant_type_hash (gconstpointer type);
gboolean g_variant_type_equal (gconstpointer type1,
                                                                         gconstpointer type2);


gboolean g_variant_type_is_subtype_of (const GVariantType *type,
                                                                         const GVariantType *supertype);


const GVariantType * g_variant_type_element (const GVariantType *type);
const GVariantType * g_variant_type_first (const GVariantType *type);
const GVariantType * g_variant_type_next (const GVariantType *type);
gsize g_variant_type_n_items (const GVariantType *type);
const GVariantType * g_variant_type_key (const GVariantType *type);
const GVariantType * g_variant_type_value (const GVariantType *type);


GVariantType * g_variant_type_new_array (const GVariantType *element);
GVariantType * g_variant_type_new_maybe (const GVariantType *element);
GVariantType * g_variant_type_new_tuple (const GVariantType * const *items,
                                                                         gint length);
GVariantType * g_variant_type_new_dict_entry (const GVariantType *key,
                                                                         const GVariantType *value);


const GVariantType * g_variant_type_checked_ (const gchar *);


# 96 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gvariant.h" 1
# 30 "/usr/include/glib-2.0/glib/gvariant.h"
# 1 "/usr/include/glib-2.0/glib/gvarianttype.h" 1
# 31 "/usr/include/glib-2.0/glib/gvariant.h" 2
# 1 "/usr/include/glib-2.0/glib/gstring.h" 1
# 32 "/usr/include/glib-2.0/glib/gvariant.h" 2



typedef struct _GVariant GVariant;

typedef enum
{
  G_VARIANT_CLASS_BOOLEAN = 'b',
  G_VARIANT_CLASS_BYTE = 'y',
  G_VARIANT_CLASS_INT16 = 'n',
  G_VARIANT_CLASS_UINT16 = 'q',
  G_VARIANT_CLASS_INT32 = 'i',
  G_VARIANT_CLASS_UINT32 = 'u',
  G_VARIANT_CLASS_INT64 = 'x',
  G_VARIANT_CLASS_UINT64 = 't',
  G_VARIANT_CLASS_HANDLE = 'h',
  G_VARIANT_CLASS_DOUBLE = 'd',
  G_VARIANT_CLASS_STRING = 's',
  G_VARIANT_CLASS_OBJECT_PATH = 'o',
  G_VARIANT_CLASS_SIGNATURE = 'g',
  G_VARIANT_CLASS_VARIANT = 'v',
  G_VARIANT_CLASS_MAYBE = 'm',
  G_VARIANT_CLASS_ARRAY = 'a',
  G_VARIANT_CLASS_TUPLE = '(',
  G_VARIANT_CLASS_DICT_ENTRY = '{'
} GVariantClass;

void g_variant_unref (GVariant *value);
GVariant * g_variant_ref (GVariant *value);
GVariant * g_variant_ref_sink (GVariant *value);
gboolean g_variant_is_floating (GVariant *value);
GVariant * g_variant_take_ref (GVariant *value);

const GVariantType * g_variant_get_type (GVariant *value);
const gchar * g_variant_get_type_string (GVariant *value);
gboolean g_variant_is_of_type (GVariant *value,
                                                                         const GVariantType *type);
gboolean g_variant_is_container (GVariant *value);
GVariantClass g_variant_classify (GVariant *value);
GVariant * g_variant_new_boolean (gboolean value);
GVariant * g_variant_new_byte (guchar value);
GVariant * g_variant_new_int16 (gint16 value);
GVariant * g_variant_new_uint16 (guint16 value);
GVariant * g_variant_new_int32 (gint32 value);
GVariant * g_variant_new_uint32 (guint32 value);
GVariant * g_variant_new_int64 (gint64 value);
GVariant * g_variant_new_uint64 (guint64 value);
GVariant * g_variant_new_handle (gint32 value);
GVariant * g_variant_new_double (gdouble value);
GVariant * g_variant_new_string (const gchar *string);
GVariant * g_variant_new_object_path (const gchar *object_path);
gboolean g_variant_is_object_path (const gchar *string);
GVariant * g_variant_new_signature (const gchar *signature);
gboolean g_variant_is_signature (const gchar *string);
GVariant * g_variant_new_variant (GVariant *value);
GVariant * g_variant_new_strv (const gchar * const *strv,
                                                                         gssize length);

GVariant * g_variant_new_objv (const gchar * const *strv,
                                                                         gssize length);
GVariant * g_variant_new_bytestring (const gchar *string);
GVariant * g_variant_new_bytestring_array (const gchar * const *strv,
                                                                         gssize length);
GVariant * g_variant_new_fixed_array (const GVariantType *element_type,
                                                                         gconstpointer elements,
                                                                         gsize n_elements,
                                                                         gsize element_size);
gboolean g_variant_get_boolean (GVariant *value);
guchar g_variant_get_byte (GVariant *value);
gint16 g_variant_get_int16 (GVariant *value);
guint16 g_variant_get_uint16 (GVariant *value);
gint32 g_variant_get_int32 (GVariant *value);
guint32 g_variant_get_uint32 (GVariant *value);
gint64 g_variant_get_int64 (GVariant *value);
guint64 g_variant_get_uint64 (GVariant *value);
gint32 g_variant_get_handle (GVariant *value);
gdouble g_variant_get_double (GVariant *value);
GVariant * g_variant_get_variant (GVariant *value);
const gchar * g_variant_get_string (GVariant *value,
                                                                         gsize *length);
gchar * g_variant_dup_string (GVariant *value,
                                                                         gsize *length);
const gchar ** g_variant_get_strv (GVariant *value,
                                                                         gsize *length);
gchar ** g_variant_dup_strv (GVariant *value,
                                                                         gsize *length);

const gchar ** g_variant_get_objv (GVariant *value,
                                                                         gsize *length);
gchar ** g_variant_dup_objv (GVariant *value,
                                                                         gsize *length);
const gchar * g_variant_get_bytestring (GVariant *value);
gchar * g_variant_dup_bytestring (GVariant *value,
                                                                         gsize *length);
const gchar ** g_variant_get_bytestring_array (GVariant *value,
                                                                         gsize *length);
gchar ** g_variant_dup_bytestring_array (GVariant *value,
                                                                         gsize *length);

GVariant * g_variant_new_maybe (const GVariantType *child_type,
                                                                         GVariant *child);
GVariant * g_variant_new_array (const GVariantType *child_type,
                                                                         GVariant * const *children,
                                                                         gsize n_children);
GVariant * g_variant_new_tuple (GVariant * const *children,
                                                                         gsize n_children);
GVariant * g_variant_new_dict_entry (GVariant *key,
                                                                         GVariant *value);

GVariant * g_variant_get_maybe (GVariant *value);
gsize g_variant_n_children (GVariant *value);
void g_variant_get_child (GVariant *value,
                                                                         gsize index_,
                                                                         const gchar *format_string,
                                                                         ...);
GVariant * g_variant_get_child_value (GVariant *value,
                                                                         gsize index_);
gboolean g_variant_lookup (GVariant *dictionary,
                                                                         const gchar *key,
                                                                         const gchar *format_string,
                                                                         ...);
GVariant * g_variant_lookup_value (GVariant *dictionary,
                                                                         const gchar *key,
                                                                         const GVariantType *expected_type);
gconstpointer g_variant_get_fixed_array (GVariant *value,
                                                                         gsize *n_elements,
                                                                         gsize element_size);

gsize g_variant_get_size (GVariant *value);
gconstpointer g_variant_get_data (GVariant *value);
void g_variant_store (GVariant *value,
                                                                         gpointer data);

gchar * g_variant_print (GVariant *value,
                                                                         gboolean type_annotate);
GString * g_variant_print_string (GVariant *value,
                                                                         GString *string,
                                                                         gboolean type_annotate);

guint g_variant_hash (gconstpointer value);
gboolean g_variant_equal (gconstpointer one,
                                                                         gconstpointer two);

GVariant * g_variant_get_normal_form (GVariant *value);
gboolean g_variant_is_normal_form (GVariant *value);
GVariant * g_variant_byteswap (GVariant *value);
GVariant * g_variant_new_from_data (const GVariantType *type,
                                                                         gconstpointer data,
                                                                         gsize size,
                                                                         gboolean trusted,
                                                                         GDestroyNotify notify,
                                                                         gpointer user_data);

typedef struct _GVariantIter GVariantIter;
struct _GVariantIter {

  gsize x[16];
};

GVariantIter * g_variant_iter_new (GVariant *value);
gsize g_variant_iter_init (GVariantIter *iter,
                                                                         GVariant *value);
GVariantIter * g_variant_iter_copy (GVariantIter *iter);
gsize g_variant_iter_n_children (GVariantIter *iter);
void g_variant_iter_free (GVariantIter *iter);
GVariant * g_variant_iter_next_value (GVariantIter *iter);
gboolean g_variant_iter_next (GVariantIter *iter,
                                                                         const gchar *format_string,
                                                                         ...);
gboolean g_variant_iter_loop (GVariantIter *iter,
                                                                         const gchar *format_string,
                                                                         ...);


typedef struct _GVariantBuilder GVariantBuilder;
struct _GVariantBuilder {

  gsize x[16];
};

typedef enum
{
  G_VARIANT_PARSE_ERROR_FAILED,
  G_VARIANT_PARSE_ERROR_BASIC_TYPE_EXPECTED,
  G_VARIANT_PARSE_ERROR_CANNOT_INFER_TYPE,
  G_VARIANT_PARSE_ERROR_DEFINITE_TYPE_EXPECTED,
  G_VARIANT_PARSE_ERROR_INPUT_NOT_AT_END,
  G_VARIANT_PARSE_ERROR_INVALID_CHARACTER,
  G_VARIANT_PARSE_ERROR_INVALID_FORMAT_STRING,
  G_VARIANT_PARSE_ERROR_INVALID_OBJECT_PATH,
  G_VARIANT_PARSE_ERROR_INVALID_SIGNATURE,
  G_VARIANT_PARSE_ERROR_INVALID_TYPE_STRING,
  G_VARIANT_PARSE_ERROR_NO_COMMON_TYPE,
  G_VARIANT_PARSE_ERROR_NUMBER_OUT_OF_RANGE,
  G_VARIANT_PARSE_ERROR_NUMBER_TOO_BIG,
  G_VARIANT_PARSE_ERROR_TYPE_ERROR,
  G_VARIANT_PARSE_ERROR_UNEXPECTED_TOKEN,
  G_VARIANT_PARSE_ERROR_UNKNOWN_KEYWORD,
  G_VARIANT_PARSE_ERROR_UNTERMINATED_STRING_CONSTANT,
  G_VARIANT_PARSE_ERROR_VALUE_EXPECTED
} GVariantParseError;


GQuark g_variant_parser_get_error_quark (void);

GVariantBuilder * g_variant_builder_new (const GVariantType *type);
void g_variant_builder_unref (GVariantBuilder *builder);
GVariantBuilder * g_variant_builder_ref (GVariantBuilder *builder);
void g_variant_builder_init (GVariantBuilder *builder,
                                                                         const GVariantType *type);
GVariant * g_variant_builder_end (GVariantBuilder *builder);
void g_variant_builder_clear (GVariantBuilder *builder);
void g_variant_builder_open (GVariantBuilder *builder,
                                                                         const GVariantType *type);
void g_variant_builder_close (GVariantBuilder *builder);
void g_variant_builder_add_value (GVariantBuilder *builder,
                                                                         GVariant *value);
void g_variant_builder_add (GVariantBuilder *builder,
                                                                         const gchar *format_string,
                                                                         ...);
void g_variant_builder_add_parsed (GVariantBuilder *builder,
                                                                         const gchar *format,
                                                                         ...);

GVariant * g_variant_new (const gchar *format_string,
                                                                         ...);
void g_variant_get (GVariant *value,
                                                                         const gchar *format_string,
                                                                         ...);
GVariant * g_variant_new_va (const gchar *format_string,
                                                                         const gchar **endptr,
                                                                         va_list *app);
void g_variant_get_va (GVariant *value,
                                                                         const gchar *format_string,
                                                                         const gchar **endptr,
                                                                         va_list *app);


GVariant * g_variant_parse (const GVariantType *type,
                                                                         const gchar *text,
                                                                         const gchar *limit,
                                                                         const gchar **endptr,
                                                                         GError **error);
GVariant * g_variant_new_parsed (const gchar *format,
                                                                         ...);
GVariant * g_variant_new_parsed_va (const gchar *format,
                                                                         va_list *app);

gint g_variant_compare (gconstpointer one,
                                                                         gconstpointer two);

# 97 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gversion.h" 1
# 34 "/usr/include/glib-2.0/glib/gversion.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/gversion.h" 2



extern const guint glib_major_version;
extern const guint glib_minor_version;
extern const guint glib_micro_version;
extern const guint glib_interface_age;
extern const guint glib_binary_age;

const gchar * glib_check_version (guint required_major,
                                  guint required_minor,
                                  guint required_micro);








# 98 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/gversionmacros.h" 1
# 99 "/usr/include/glib-2.0/glib.h" 2




# 1 "/usr/include/glib-2.0/glib/deprecated/gallocator.h" 1
# 25 "/usr/include/glib-2.0/glib/deprecated/gallocator.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 26 "/usr/include/glib-2.0/glib/deprecated/gallocator.h" 2



typedef struct _GAllocator GAllocator;
typedef struct _GMemChunk GMemChunk;
# 44 "/usr/include/glib-2.0/glib/deprecated/gallocator.h"
__attribute__((__deprecated__))
GMemChunk * g_mem_chunk_new (const gchar *name,
                                         gint atom_size,
                                         gsize area_size,
                                         gint type);
__attribute__((__deprecated__))
void g_mem_chunk_destroy (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
gpointer g_mem_chunk_alloc (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
gpointer g_mem_chunk_alloc0 (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
void g_mem_chunk_free (GMemChunk *mem_chunk,
                                         gpointer mem);
__attribute__((__deprecated__))
void g_mem_chunk_clean (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
void g_mem_chunk_reset (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
void g_mem_chunk_print (GMemChunk *mem_chunk);
__attribute__((__deprecated__))
void g_mem_chunk_info (void);
__attribute__((__deprecated__))
void g_blow_chunks (void);


__attribute__((__deprecated__))
GAllocator * g_allocator_new (const gchar *name,
                                         guint n_preallocs);
__attribute__((__deprecated__))
void g_allocator_free (GAllocator *allocator);
__attribute__((__deprecated__))
void g_list_push_allocator (GAllocator *allocator);
__attribute__((__deprecated__))
void g_list_pop_allocator (void);
__attribute__((__deprecated__))
void g_slist_push_allocator (GAllocator *allocator);
__attribute__((__deprecated__))
void g_slist_pop_allocator (void);
__attribute__((__deprecated__))
void g_node_push_allocator (GAllocator *allocator);
__attribute__((__deprecated__))
void g_node_pop_allocator (void);


# 104 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/deprecated/gcache.h" 1
# 34 "/usr/include/glib-2.0/glib/deprecated/gcache.h"
# 1 "/usr/include/glib-2.0/glib/glist.h" 1
# 35 "/usr/include/glib-2.0/glib/deprecated/gcache.h" 2



typedef struct _GCache GCache;

typedef gpointer (*GCacheNewFunc) (gpointer key);
typedef gpointer (*GCacheDupFunc) (gpointer value);
typedef void (*GCacheDestroyFunc) (gpointer value);



__attribute__((__deprecated__))
GCache* g_cache_new (GCacheNewFunc value_new_func,
                                GCacheDestroyFunc value_destroy_func,
                                GCacheDupFunc key_dup_func,
                                GCacheDestroyFunc key_destroy_func,
                                GHashFunc hash_key_func,
                                GHashFunc hash_value_func,
                                GEqualFunc key_equal_func);
__attribute__((__deprecated__))
void g_cache_destroy (GCache *cache);
__attribute__((__deprecated__))
gpointer g_cache_insert (GCache *cache,
                                gpointer key);
__attribute__((__deprecated__))
void g_cache_remove (GCache *cache,
                                gconstpointer value);
__attribute__((__deprecated__))
void g_cache_key_foreach (GCache *cache,
                                GHFunc func,
                                gpointer user_data);
__attribute__((__deprecated__))
void g_cache_value_foreach (GCache *cache,
                                GHFunc func,
                                gpointer user_data);


# 105 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/deprecated/gcompletion.h" 1
# 34 "/usr/include/glib-2.0/glib/deprecated/gcompletion.h"
# 1 "/usr/include/glib-2.0/glib/glist.h" 1
# 35 "/usr/include/glib-2.0/glib/deprecated/gcompletion.h" 2



typedef struct _GCompletion GCompletion;

typedef gchar* (*GCompletionFunc) (gpointer);




typedef gint (*GCompletionStrncmpFunc) (const gchar *s1,
                                        const gchar *s2,
                                        gsize n);

struct _GCompletion
{
  GList* items;
  GCompletionFunc func;

  gchar* prefix;
  GList* cache;
  GCompletionStrncmpFunc strncmp_func;
};

__attribute__((__deprecated__))
GCompletion* g_completion_new (GCompletionFunc func);
__attribute__((__deprecated__))
void g_completion_add_items (GCompletion* cmp,
                                         GList* items);
__attribute__((__deprecated__))
void g_completion_remove_items (GCompletion* cmp,
                                         GList* items);
__attribute__((__deprecated__))
void g_completion_clear_items (GCompletion* cmp);
__attribute__((__deprecated__))
GList* g_completion_complete (GCompletion* cmp,
                                         const gchar* prefix,
                                         gchar** new_prefix);
__attribute__((__deprecated__))
GList* g_completion_complete_utf8 (GCompletion *cmp,
                                         const gchar* prefix,
                                         gchar** new_prefix);
__attribute__((__deprecated__))
void g_completion_set_compare (GCompletion *cmp,
                                         GCompletionStrncmpFunc strncmp_func);
__attribute__((__deprecated__))
void g_completion_free (GCompletion* cmp);


# 106 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/deprecated/gmain.h" 1
# 34 "/usr/include/glib-2.0/glib/deprecated/gmain.h"
# 1 "/usr/include/glib-2.0/glib/gmain.h" 1
# 35 "/usr/include/glib-2.0/glib/deprecated/gmain.h" 2


# 136 "/usr/include/glib-2.0/glib/deprecated/gmain.h"

# 107 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/deprecated/grel.h" 1
# 34 "/usr/include/glib-2.0/glib/deprecated/grel.h"
# 1 "/usr/include/glib-2.0/glib/gtypes.h" 1
# 35 "/usr/include/glib-2.0/glib/deprecated/grel.h" 2



typedef struct _GRelation GRelation;
typedef struct _GTuples GTuples;

struct _GTuples
{
  guint len;
};
# 69 "/usr/include/glib-2.0/glib/deprecated/grel.h"
__attribute__((__deprecated__))
GRelation* g_relation_new (gint fields);
__attribute__((__deprecated__))
void g_relation_destroy (GRelation *relation);
__attribute__((__deprecated__))
void g_relation_index (GRelation *relation,
                               gint field,
                               GHashFunc hash_func,
                               GEqualFunc key_equal_func);
__attribute__((__deprecated__))
void g_relation_insert (GRelation *relation,
                               ...);
__attribute__((__deprecated__))
gint g_relation_delete (GRelation *relation,
                               gconstpointer key,
                               gint field);
__attribute__((__deprecated__))
GTuples* g_relation_select (GRelation *relation,
                               gconstpointer key,
                               gint field);
__attribute__((__deprecated__))
gint g_relation_count (GRelation *relation,
                               gconstpointer key,
                               gint field);
__attribute__((__deprecated__))
gboolean g_relation_exists (GRelation *relation,
                               ...);
__attribute__((__deprecated__))
void g_relation_print (GRelation *relation);
__attribute__((__deprecated__))
void g_tuples_destroy (GTuples *tuples);
__attribute__((__deprecated__))
gpointer g_tuples_index (GTuples *tuples,
                               gint index_,
                               gint field);


# 108 "/usr/include/glib-2.0/glib.h" 2
# 1 "/usr/include/glib-2.0/glib/deprecated/gthread.h" 1
# 34 "/usr/include/glib-2.0/glib/deprecated/gthread.h"
# 1 "/usr/include/glib-2.0/glib/gthread.h" 1
# 35 "/usr/include/glib-2.0/glib/deprecated/gthread.h" 2



typedef enum
{
  G_THREAD_PRIORITY_LOW,
  G_THREAD_PRIORITY_NORMAL,
  G_THREAD_PRIORITY_HIGH,
  G_THREAD_PRIORITY_URGENT
} GThreadPriority;

struct _GThread
{

  GThreadFunc func;
  gpointer data;
  gboolean joinable;
  GThreadPriority priority;
};

typedef struct _GThreadFunctions GThreadFunctions;
struct _GThreadFunctions
{
  GMutex* (*mutex_new) (void);
  void (*mutex_lock) (GMutex *mutex);
  gboolean (*mutex_trylock) (GMutex *mutex);
  void (*mutex_unlock) (GMutex *mutex);
  void (*mutex_free) (GMutex *mutex);
  GCond* (*cond_new) (void);
  void (*cond_signal) (GCond *cond);
  void (*cond_broadcast) (GCond *cond);
  void (*cond_wait) (GCond *cond,
                                   GMutex *mutex);
  gboolean (*cond_timed_wait) (GCond *cond,
                                   GMutex *mutex,
                                   GTimeVal *end_time);
  void (*cond_free) (GCond *cond);
  GPrivate* (*private_new) (GDestroyNotify destructor);
  gpointer (*private_get) (GPrivate *private_key);
  void (*private_set) (GPrivate *private_key,
                                   gpointer data);
  void (*thread_create) (GThreadFunc func,
                                   gpointer data,
                                   gulong stack_size,
                                   gboolean joinable,
                                   gboolean bound,
                                   GThreadPriority priority,
                                   gpointer thread,
                                   GError **error);
  void (*thread_yield) (void);
  void (*thread_join) (gpointer thread);
  void (*thread_exit) (void);
  void (*thread_set_priority)(gpointer thread,
                                   GThreadPriority priority);
  void (*thread_self) (gpointer thread);
  gboolean (*thread_equal) (gpointer thread1,
                                   gpointer thread2);
};

extern GThreadFunctions g_thread_functions_for_glib_use;
extern gboolean g_thread_use_default_impl;

extern guint64 (*g_thread_gettime) (void);


GThread *g_thread_create (GThreadFunc func,
                                gpointer data,
                                gboolean joinable,
                                GError **error);


GThread *g_thread_create_full (GThreadFunc func,
                                gpointer data,
                                gulong stack_size,
                                gboolean joinable,
                                gboolean bound,
                                GThreadPriority priority,
                                GError **error);


void g_thread_set_priority (GThread *thread,
                                GThreadPriority priority);


void g_thread_foreach (GFunc thread_func,
                                gpointer user_data);


# 1 "/usr/include/pthread.h" 1 3 4
# 25 "/usr/include/pthread.h" 3 4
# 1 "/usr/include/sched.h" 1 3 4
# 30 "/usr/include/sched.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 31 "/usr/include/sched.h" 2 3 4
# 43 "/usr/include/sched.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/sched.h" 1 3 4
# 74 "/usr/include/x86_64-linux-gnu/bits/sched.h" 3 4
struct sched_param
  {
    int __sched_priority;
  };





extern int clone (int (*__fn) (void *__arg), void *__child_stack,
    int __flags, void *__arg, ...) __attribute__ ((__nothrow__));


extern int unshare (int __flags) __attribute__ ((__nothrow__));


extern int sched_getcpu (void) __attribute__ ((__nothrow__));










struct __sched_param
  {
    int __sched_priority;
  };
# 116 "/usr/include/x86_64-linux-gnu/bits/sched.h" 3 4
typedef unsigned long int __cpu_mask;






typedef struct
{
  __cpu_mask __bits[1024 / (8 * sizeof (__cpu_mask))];
} cpu_set_t;
# 199 "/usr/include/x86_64-linux-gnu/bits/sched.h" 3 4


extern int __sched_cpucount (size_t __setsize, const cpu_set_t *__setp)
  __attribute__ ((__nothrow__));
extern cpu_set_t *__sched_cpualloc (size_t __count) __attribute__ ((__nothrow__)) __attribute__ ((__warn_unused_result__));
extern void __sched_cpufree (cpu_set_t *__set) __attribute__ ((__nothrow__));


# 44 "/usr/include/sched.h" 2 3 4







extern int sched_setparam (__pid_t __pid, __const struct sched_param *__param)
     __attribute__ ((__nothrow__));


extern int sched_getparam (__pid_t __pid, struct sched_param *__param) __attribute__ ((__nothrow__));


extern int sched_setscheduler (__pid_t __pid, int __policy,
          __const struct sched_param *__param) __attribute__ ((__nothrow__));


extern int sched_getscheduler (__pid_t __pid) __attribute__ ((__nothrow__));


extern int sched_yield (void) __attribute__ ((__nothrow__));


extern int sched_get_priority_max (int __algorithm) __attribute__ ((__nothrow__));


extern int sched_get_priority_min (int __algorithm) __attribute__ ((__nothrow__));


extern int sched_rr_get_interval (__pid_t __pid, struct timespec *__t) __attribute__ ((__nothrow__));
# 118 "/usr/include/sched.h" 3 4
extern int sched_setaffinity (__pid_t __pid, size_t __cpusetsize,
         __const cpu_set_t *__cpuset) __attribute__ ((__nothrow__));


extern int sched_getaffinity (__pid_t __pid, size_t __cpusetsize,
         cpu_set_t *__cpuset) __attribute__ ((__nothrow__));



# 26 "/usr/include/pthread.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/setjmp.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/setjmp.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/setjmp.h" 2 3 4




typedef long int __jmp_buf[8];
# 30 "/usr/include/pthread.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 31 "/usr/include/pthread.h" 2 3 4



enum
{
  PTHREAD_CREATE_JOINABLE,

  PTHREAD_CREATE_DETACHED

};



enum
{
  PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_ADAPTIVE_NP

  ,
  PTHREAD_MUTEX_NORMAL = PTHREAD_MUTEX_TIMED_NP,
  PTHREAD_MUTEX_RECURSIVE = PTHREAD_MUTEX_RECURSIVE_NP,
  PTHREAD_MUTEX_ERRORCHECK = PTHREAD_MUTEX_ERRORCHECK_NP,
  PTHREAD_MUTEX_DEFAULT = PTHREAD_MUTEX_NORMAL



  , PTHREAD_MUTEX_FAST_NP = PTHREAD_MUTEX_TIMED_NP

};




enum
{
  PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_STALLED_NP = PTHREAD_MUTEX_STALLED,
  PTHREAD_MUTEX_ROBUST,
  PTHREAD_MUTEX_ROBUST_NP = PTHREAD_MUTEX_ROBUST
};





enum
{
  PTHREAD_PRIO_NONE,
  PTHREAD_PRIO_INHERIT,
  PTHREAD_PRIO_PROTECT
};
# 115 "/usr/include/pthread.h" 3 4
enum
{
  PTHREAD_RWLOCK_PREFER_READER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NP,
  PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP,
  PTHREAD_RWLOCK_DEFAULT_NP = PTHREAD_RWLOCK_PREFER_READER_NP
};
# 147 "/usr/include/pthread.h" 3 4
enum
{
  PTHREAD_INHERIT_SCHED,

  PTHREAD_EXPLICIT_SCHED

};



enum
{
  PTHREAD_SCOPE_SYSTEM,

  PTHREAD_SCOPE_PROCESS

};



enum
{
  PTHREAD_PROCESS_PRIVATE,

  PTHREAD_PROCESS_SHARED

};
# 182 "/usr/include/pthread.h" 3 4
struct _pthread_cleanup_buffer
{
  void (*__routine) (void *);
  void *__arg;
  int __canceltype;
  struct _pthread_cleanup_buffer *__prev;
};


enum
{
  PTHREAD_CANCEL_ENABLE,

  PTHREAD_CANCEL_DISABLE

};
enum
{
  PTHREAD_CANCEL_DEFERRED,

  PTHREAD_CANCEL_ASYNCHRONOUS

};
# 220 "/usr/include/pthread.h" 3 4





extern int pthread_create (pthread_t *__restrict __newthread,
      __const pthread_attr_t *__restrict __attr,
      void *(*__start_routine) (void *),
      void *__restrict __arg) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3)));





extern void pthread_exit (void *__retval) __attribute__ ((__noreturn__));







extern int pthread_join (pthread_t __th, void **__thread_return);




extern int pthread_tryjoin_np (pthread_t __th, void **__thread_return) __attribute__ ((__nothrow__));







extern int pthread_timedjoin_np (pthread_t __th, void **__thread_return,
     __const struct timespec *__abstime);






extern int pthread_detach (pthread_t __th) __attribute__ ((__nothrow__));



extern pthread_t pthread_self (void) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int pthread_equal (pthread_t __thread1, pthread_t __thread2) __attribute__ ((__nothrow__));







extern int pthread_attr_init (pthread_attr_t *__attr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_attr_destroy (pthread_attr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_attr_getdetachstate (__const pthread_attr_t *__attr,
     int *__detachstate)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setdetachstate (pthread_attr_t *__attr,
     int __detachstate)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_attr_getguardsize (__const pthread_attr_t *__attr,
          size_t *__guardsize)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setguardsize (pthread_attr_t *__attr,
          size_t __guardsize)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_attr_getschedparam (__const pthread_attr_t *__restrict
           __attr,
           struct sched_param *__restrict __param)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setschedparam (pthread_attr_t *__restrict __attr,
           __const struct sched_param *__restrict
           __param) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_getschedpolicy (__const pthread_attr_t *__restrict
     __attr, int *__restrict __policy)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setschedpolicy (pthread_attr_t *__attr, int __policy)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_attr_getinheritsched (__const pthread_attr_t *__restrict
      __attr, int *__restrict __inherit)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setinheritsched (pthread_attr_t *__attr,
      int __inherit)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_attr_getscope (__const pthread_attr_t *__restrict __attr,
      int *__restrict __scope)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_attr_setscope (pthread_attr_t *__attr, int __scope)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_attr_getstackaddr (__const pthread_attr_t *__restrict
          __attr, void **__restrict __stackaddr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2))) __attribute__ ((__deprecated__));





extern int pthread_attr_setstackaddr (pthread_attr_t *__attr,
          void *__stackaddr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1))) __attribute__ ((__deprecated__));


extern int pthread_attr_getstacksize (__const pthread_attr_t *__restrict
          __attr, size_t *__restrict __stacksize)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int pthread_attr_setstacksize (pthread_attr_t *__attr,
          size_t __stacksize)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_attr_getstack (__const pthread_attr_t *__restrict __attr,
      void **__restrict __stackaddr,
      size_t *__restrict __stacksize)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2, 3)));




extern int pthread_attr_setstack (pthread_attr_t *__attr, void *__stackaddr,
      size_t __stacksize) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int pthread_attr_setaffinity_np (pthread_attr_t *__attr,
     size_t __cpusetsize,
     __const cpu_set_t *__cpuset)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3)));



extern int pthread_attr_getaffinity_np (__const pthread_attr_t *__attr,
     size_t __cpusetsize,
     cpu_set_t *__cpuset)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3)));





extern int pthread_getattr_np (pthread_t __th, pthread_attr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));







extern int pthread_setschedparam (pthread_t __target_thread, int __policy,
      __const struct sched_param *__param)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));


extern int pthread_getschedparam (pthread_t __target_thread,
      int *__restrict __policy,
      struct sched_param *__restrict __param)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2, 3)));


extern int pthread_setschedprio (pthread_t __target_thread, int __prio)
     __attribute__ ((__nothrow__));




extern int pthread_getname_np (pthread_t __target_thread, char *__buf,
          size_t __buflen)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));


extern int pthread_setname_np (pthread_t __target_thread, __const char *__name)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));





extern int pthread_getconcurrency (void) __attribute__ ((__nothrow__));


extern int pthread_setconcurrency (int __level) __attribute__ ((__nothrow__));







extern int pthread_yield (void) __attribute__ ((__nothrow__));




extern int pthread_setaffinity_np (pthread_t __th, size_t __cpusetsize,
       __const cpu_set_t *__cpuset)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));


extern int pthread_getaffinity_np (pthread_t __th, size_t __cpusetsize,
       cpu_set_t *__cpuset)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (3)));
# 478 "/usr/include/pthread.h" 3 4
extern int pthread_once (pthread_once_t *__once_control,
    void (*__init_routine) (void)) __attribute__ ((__nonnull__ (1, 2)));
# 490 "/usr/include/pthread.h" 3 4
extern int pthread_setcancelstate (int __state, int *__oldstate);



extern int pthread_setcanceltype (int __type, int *__oldtype);


extern int pthread_cancel (pthread_t __th);




extern void pthread_testcancel (void);




typedef struct
{
  struct
  {
    __jmp_buf __cancel_jmp_buf;
    int __mask_was_saved;
  } __cancel_jmp_buf[1];
  void *__pad[4];
} __pthread_unwind_buf_t __attribute__ ((__aligned__));
# 524 "/usr/include/pthread.h" 3 4
struct __pthread_cleanup_frame
{
  void (*__cancel_routine) (void *);
  void *__cancel_arg;
  int __do_it;
  int __cancel_type;
};
# 664 "/usr/include/pthread.h" 3 4
extern void __pthread_register_cancel (__pthread_unwind_buf_t *__buf)
     ;
# 676 "/usr/include/pthread.h" 3 4
extern void __pthread_unregister_cancel (__pthread_unwind_buf_t *__buf)
  ;
# 699 "/usr/include/pthread.h" 3 4
extern void __pthread_register_cancel_defer (__pthread_unwind_buf_t *__buf)
     ;
# 712 "/usr/include/pthread.h" 3 4
extern void __pthread_unregister_cancel_restore (__pthread_unwind_buf_t *__buf)
  ;



extern void __pthread_unwind_next (__pthread_unwind_buf_t *__buf)
     __attribute__ ((__noreturn__))

     __attribute__ ((__weak__))

     ;



struct __jmp_buf_tag;
extern int __sigsetjmp (struct __jmp_buf_tag *__env, int __savemask) __attribute__ ((__nothrow__));





extern int pthread_mutex_init (pthread_mutex_t *__mutex,
          __const pthread_mutexattr_t *__mutexattr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutex_destroy (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutex_trylock (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutex_lock (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_mutex_timedlock (pthread_mutex_t *__restrict __mutex,
        __const struct timespec *__restrict
        __abstime) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_mutex_unlock (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_mutex_getprioceiling (__const pthread_mutex_t *
      __restrict __mutex,
      int *__restrict __prioceiling)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_mutex_setprioceiling (pthread_mutex_t *__restrict __mutex,
      int __prioceiling,
      int *__restrict __old_ceiling)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 3)));




extern int pthread_mutex_consistent (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern int pthread_mutex_consistent_np (pthread_mutex_t *__mutex)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 790 "/usr/include/pthread.h" 3 4
extern int pthread_mutexattr_init (pthread_mutexattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutexattr_destroy (pthread_mutexattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutexattr_getpshared (__const pthread_mutexattr_t *
      __restrict __attr,
      int *__restrict __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_mutexattr_setpshared (pthread_mutexattr_t *__attr,
      int __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_mutexattr_gettype (__const pthread_mutexattr_t *__restrict
          __attr, int *__restrict __kind)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));




extern int pthread_mutexattr_settype (pthread_mutexattr_t *__attr, int __kind)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_mutexattr_getprotocol (__const pthread_mutexattr_t *
       __restrict __attr,
       int *__restrict __protocol)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_mutexattr_setprotocol (pthread_mutexattr_t *__attr,
       int __protocol)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_mutexattr_getprioceiling (__const pthread_mutexattr_t *
          __restrict __attr,
          int *__restrict __prioceiling)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_mutexattr_setprioceiling (pthread_mutexattr_t *__attr,
          int __prioceiling)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_mutexattr_getrobust (__const pthread_mutexattr_t *__attr,
     int *__robustness)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));

extern int pthread_mutexattr_getrobust_np (__const pthread_mutexattr_t *__attr,
        int *__robustness)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_mutexattr_setrobust (pthread_mutexattr_t *__attr,
     int __robustness)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));

extern int pthread_mutexattr_setrobust_np (pthread_mutexattr_t *__attr,
        int __robustness)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 872 "/usr/include/pthread.h" 3 4
extern int pthread_rwlock_init (pthread_rwlock_t *__restrict __rwlock,
    __const pthread_rwlockattr_t *__restrict
    __attr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlock_destroy (pthread_rwlock_t *__rwlock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlock_rdlock (pthread_rwlock_t *__rwlock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlock_tryrdlock (pthread_rwlock_t *__rwlock)
  __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_rwlock_timedrdlock (pthread_rwlock_t *__restrict __rwlock,
           __const struct timespec *__restrict
           __abstime) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_rwlock_wrlock (pthread_rwlock_t *__rwlock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlock_trywrlock (pthread_rwlock_t *__rwlock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_rwlock_timedwrlock (pthread_rwlock_t *__restrict __rwlock,
           __const struct timespec *__restrict
           __abstime) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));



extern int pthread_rwlock_unlock (pthread_rwlock_t *__rwlock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));





extern int pthread_rwlockattr_init (pthread_rwlockattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlockattr_destroy (pthread_rwlockattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlockattr_getpshared (__const pthread_rwlockattr_t *
       __restrict __attr,
       int *__restrict __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_rwlockattr_setpshared (pthread_rwlockattr_t *__attr,
       int __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_rwlockattr_getkind_np (__const pthread_rwlockattr_t *
       __restrict __attr,
       int *__restrict __pref)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_rwlockattr_setkind_np (pthread_rwlockattr_t *__attr,
       int __pref) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));







extern int pthread_cond_init (pthread_cond_t *__restrict __cond,
         __const pthread_condattr_t *__restrict
         __cond_attr) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_cond_destroy (pthread_cond_t *__cond)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_cond_signal (pthread_cond_t *__cond)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_cond_broadcast (pthread_cond_t *__cond)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int pthread_cond_wait (pthread_cond_t *__restrict __cond,
         pthread_mutex_t *__restrict __mutex)
     __attribute__ ((__nonnull__ (1, 2)));
# 984 "/usr/include/pthread.h" 3 4
extern int pthread_cond_timedwait (pthread_cond_t *__restrict __cond,
       pthread_mutex_t *__restrict __mutex,
       __const struct timespec *__restrict
       __abstime) __attribute__ ((__nonnull__ (1, 2, 3)));




extern int pthread_condattr_init (pthread_condattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_condattr_destroy (pthread_condattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_condattr_getpshared (__const pthread_condattr_t *
     __restrict __attr,
     int *__restrict __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_condattr_setpshared (pthread_condattr_t *__attr,
     int __pshared) __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_condattr_getclock (__const pthread_condattr_t *
          __restrict __attr,
          __clockid_t *__restrict __clock_id)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_condattr_setclock (pthread_condattr_t *__attr,
          __clockid_t __clock_id)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 1028 "/usr/include/pthread.h" 3 4
extern int pthread_spin_init (pthread_spinlock_t *__lock, int __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_spin_destroy (pthread_spinlock_t *__lock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_spin_lock (pthread_spinlock_t *__lock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_spin_trylock (pthread_spinlock_t *__lock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_spin_unlock (pthread_spinlock_t *__lock)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));






extern int pthread_barrier_init (pthread_barrier_t *__restrict __barrier,
     __const pthread_barrierattr_t *__restrict
     __attr, unsigned int __count)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_barrier_destroy (pthread_barrier_t *__barrier)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_barrier_wait (pthread_barrier_t *__barrier)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));



extern int pthread_barrierattr_init (pthread_barrierattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_barrierattr_destroy (pthread_barrierattr_t *__attr)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_barrierattr_getpshared (__const pthread_barrierattr_t *
        __restrict __attr,
        int *__restrict __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1, 2)));


extern int pthread_barrierattr_setpshared (pthread_barrierattr_t *__attr,
        int __pshared)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));
# 1095 "/usr/include/pthread.h" 3 4
extern int pthread_key_create (pthread_key_t *__key,
          void (*__destr_function) (void *))
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (1)));


extern int pthread_key_delete (pthread_key_t __key) __attribute__ ((__nothrow__));


extern void *pthread_getspecific (pthread_key_t __key) __attribute__ ((__nothrow__));


extern int pthread_setspecific (pthread_key_t __key,
    __const void *__pointer) __attribute__ ((__nothrow__)) ;




extern int pthread_getcpuclockid (pthread_t __thread_id,
      __clockid_t *__clock_id)
     __attribute__ ((__nothrow__)) __attribute__ ((__nonnull__ (2)));
# 1129 "/usr/include/pthread.h" 3 4
extern int pthread_atfork (void (*__prepare) (void),
      void (*__parent) (void),
      void (*__child) (void)) __attribute__ ((__nothrow__));




extern __inline int
__attribute__ ((__nothrow__)) pthread_equal (pthread_t __thread1, pthread_t __thread2)
{
  return __thread1 == __thread2;
}



# 124 "/usr/include/glib-2.0/glib/deprecated/gthread.h" 2




typedef struct
{
  GMutex *mutex;


  pthread_mutex_t unused;

} GStaticMutex;
# 144 "/usr/include/glib-2.0/glib/deprecated/gthread.h"

void g_static_mutex_init (GStaticMutex *mutex);

void g_static_mutex_free (GStaticMutex *mutex);
GMutex *g_static_mutex_get_mutex_impl (GStaticMutex *mutex);

typedef struct _GStaticRecMutex GStaticRecMutex;
struct _GStaticRecMutex
{

  GStaticMutex mutex;
  guint depth;


  union {



    pthread_t owner;

    gdouble dummy;
  } unused;
};



void g_static_rec_mutex_init (GStaticRecMutex *mutex);


void g_static_rec_mutex_lock (GStaticRecMutex *mutex);


gboolean g_static_rec_mutex_trylock (GStaticRecMutex *mutex);


void g_static_rec_mutex_unlock (GStaticRecMutex *mutex);


void g_static_rec_mutex_lock_full (GStaticRecMutex *mutex,
                                         guint depth);


guint g_static_rec_mutex_unlock_full (GStaticRecMutex *mutex);


void g_static_rec_mutex_free (GStaticRecMutex *mutex);

typedef struct _GStaticRWLock GStaticRWLock;
struct _GStaticRWLock
{

  GStaticMutex mutex;
  GCond *read_cond;
  GCond *write_cond;
  guint read_counter;
  gboolean have_writer;
  guint want_to_read;
  guint want_to_write;
};




void g_static_rw_lock_init (GStaticRWLock *lock);


void g_static_rw_lock_reader_lock (GStaticRWLock *lock);


gboolean g_static_rw_lock_reader_trylock (GStaticRWLock *lock);


void g_static_rw_lock_reader_unlock (GStaticRWLock *lock);


void g_static_rw_lock_writer_lock (GStaticRWLock *lock);


gboolean g_static_rw_lock_writer_trylock (GStaticRWLock *lock);


void g_static_rw_lock_writer_unlock (GStaticRWLock *lock);


void g_static_rw_lock_free (GStaticRWLock *lock);


GPrivate * g_private_new (GDestroyNotify notify);

typedef struct _GStaticPrivate GStaticPrivate;
struct _GStaticPrivate
{

  guint index;
};



void g_static_private_init (GStaticPrivate *private_key);


gpointer g_static_private_get (GStaticPrivate *private_key);


void g_static_private_set (GStaticPrivate *private_key,
                                          gpointer data,
                                          GDestroyNotify notify);


void g_static_private_free (GStaticPrivate *private_key);


gboolean g_once_init_enter_impl (volatile gsize *location);


void g_thread_init (gpointer vtable);

void g_thread_init_with_errorcheck_mutexes (gpointer vtable);


gboolean g_thread_get_initialized (void);

extern gboolean g_threads_got_initialized;




GMutex * g_mutex_new (void);

void g_mutex_free (GMutex *mutex);

GCond * g_cond_new (void);

void g_cond_free (GCond *cond);

gboolean g_cond_timed_wait (GCond *cond,
                                         GMutex *mutex,
                                         GTimeVal *timeval);


# 109 "/usr/include/glib-2.0/glib.h" 2
# 18 "/home/gabriel/repos/qemu/include/glib-compat.h" 2
# 44 "/home/gabriel/repos/qemu/include/qemu-common.h" 2






# 1 "/home/gabriel/repos/qemu/include/sysemu/os-posix.h" 1
# 29 "/home/gabriel/repos/qemu/include/sysemu/os-posix.h"
void os_set_line_buffering(void);
void os_set_proc_name(const char *s);
void os_setup_signal_handling(void);
void os_daemonize(void);
void os_setup_post(void);

typedef struct timeval qemu_timeval;
# 46 "/home/gabriel/repos/qemu/include/sysemu/os-posix.h"
typedef struct timespec qemu_timespec;
int qemu_utimens(const char *path, const qemu_timespec *times);

_Bool is_daemonized(void);
# 51 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 98 "/home/gabriel/repos/qemu/include/qemu-common.h"
# 1 "/usr/include/x86_64-linux-gnu/sys/uio.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/uio.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/sys/uio.h" 2 3 4
# 40 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4
extern ssize_t readv (int __fd, __const struct iovec *__iovec, int __count)
  __attribute__ ((__warn_unused_result__));
# 51 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4
extern ssize_t writev (int __fd, __const struct iovec *__iovec, int __count)
  __attribute__ ((__warn_unused_result__));
# 82 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4
extern ssize_t preadv (int __fd, __const struct iovec *__iovec, int __count, __off64_t __offset) __asm__ ("" "preadv64")

                __attribute__ ((__warn_unused_result__));
extern ssize_t pwritev (int __fd, __const struct iovec *__iovec, int __count, __off64_t __offset) __asm__ ("" "pwritev64")

                 __attribute__ ((__warn_unused_result__));
# 104 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4
extern ssize_t preadv64 (int __fd, __const struct iovec *__iovec, int __count,
    __off64_t __offset) __attribute__ ((__warn_unused_result__));
# 116 "/usr/include/x86_64-linux-gnu/sys/uio.h" 3 4
extern ssize_t pwritev64 (int __fd, __const struct iovec *__iovec, int __count,
     __off64_t __offset) __attribute__ ((__warn_unused_result__));




# 99 "/home/gabriel/repos/qemu/include/qemu-common.h" 2


typedef int (*fprintf_function)(FILE *f, const char *fmt, ...)
    __attribute__((format(gnu_printf, 2, 3)));
# 122 "/home/gabriel/repos/qemu/include/qemu-common.h"
void configure_icount(const char *option);
extern int use_icount;




# 1 "/home/gabriel/repos/qemu/include/qemu/osdep.h" 1




# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 6 "/home/gabriel/repos/qemu/include/qemu/osdep.h" 2







# 1 "/usr/include/x86_64-linux-gnu/sys/wait.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/sys/wait.h" 3 4



# 1 "/usr/include/x86_64-linux-gnu/sys/resource.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/sys/resource.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/resource.h" 1 3 4
# 33 "/usr/include/x86_64-linux-gnu/bits/resource.h" 3 4
enum __rlimit_resource
{

  RLIMIT_CPU = 0,



  RLIMIT_FSIZE = 1,



  RLIMIT_DATA = 2,



  RLIMIT_STACK = 3,



  RLIMIT_CORE = 4,






  __RLIMIT_RSS = 5,



  RLIMIT_NOFILE = 7,
  __RLIMIT_OFILE = RLIMIT_NOFILE,




  RLIMIT_AS = 9,



  __RLIMIT_NPROC = 6,



  __RLIMIT_MEMLOCK = 8,



  __RLIMIT_LOCKS = 10,



  __RLIMIT_SIGPENDING = 11,



  __RLIMIT_MSGQUEUE = 12,





  __RLIMIT_NICE = 13,




  __RLIMIT_RTPRIO = 14,





  __RLIMIT_RTTIME = 15,


  __RLIMIT_NLIMITS = 16,
  __RLIM_NLIMITS = __RLIMIT_NLIMITS


};
# 135 "/usr/include/x86_64-linux-gnu/bits/resource.h" 3 4
typedef __rlim64_t rlim_t;


typedef __rlim64_t rlim64_t;


struct rlimit
  {

    rlim_t rlim_cur;

    rlim_t rlim_max;
  };


struct rlimit64
  {

    rlim64_t rlim_cur;

    rlim64_t rlim_max;
 };



enum __rusage_who
{

  RUSAGE_SELF = 0,



  RUSAGE_CHILDREN = -1



  ,

  RUSAGE_THREAD = 1




};


# 1 "/usr/include/x86_64-linux-gnu/bits/time.h" 1 3 4
# 182 "/usr/include/x86_64-linux-gnu/bits/resource.h" 2 3 4


struct rusage
  {

    struct timeval ru_utime;

    struct timeval ru_stime;

    long int ru_maxrss;


    long int ru_ixrss;

    long int ru_idrss;

    long int ru_isrss;


    long int ru_minflt;

    long int ru_majflt;

    long int ru_nswap;


    long int ru_inblock;

    long int ru_oublock;

    long int ru_msgsnd;

    long int ru_msgrcv;

    long int ru_nsignals;



    long int ru_nvcsw;


    long int ru_nivcsw;
  };







enum __priority_which
{
  PRIO_PROCESS = 0,

  PRIO_PGRP = 1,

  PRIO_USER = 2

};



# 253 "/usr/include/x86_64-linux-gnu/bits/resource.h" 3 4
extern int prlimit (__pid_t __pid, enum __rlimit_resource __resource, __const struct rlimit *__new_limit, struct rlimit *__old_limit) __asm__ ("" "prlimit64") __attribute__ ((__nothrow__))


                                                ;





extern int prlimit64 (__pid_t __pid, enum __rlimit_resource __resource,
        __const struct rlimit64 *__new_limit,
        struct rlimit64 *__old_limit) __attribute__ ((__nothrow__));




# 26 "/usr/include/x86_64-linux-gnu/sys/resource.h" 2 3 4













typedef enum __rlimit_resource __rlimit_resource_t;
typedef enum __rusage_who __rusage_who_t;
typedef enum __priority_which __priority_which_t;
# 55 "/usr/include/x86_64-linux-gnu/sys/resource.h" 3 4
extern int getrlimit (__rlimit_resource_t __resource, struct rlimit *__rlimits) __asm__ ("" "getrlimit64") __attribute__ ((__nothrow__))
                                                  ;





extern int getrlimit64 (__rlimit_resource_t __resource,
   struct rlimit64 *__rlimits) __attribute__ ((__nothrow__));
# 74 "/usr/include/x86_64-linux-gnu/sys/resource.h" 3 4
extern int setrlimit (__rlimit_resource_t __resource, __const struct rlimit *__rlimits) __asm__ ("" "setrlimit64") __attribute__ ((__nothrow__))

                  ;





extern int setrlimit64 (__rlimit_resource_t __resource,
   __const struct rlimit64 *__rlimits) __attribute__ ((__nothrow__));




extern int getrusage (__rusage_who_t __who, struct rusage *__usage) __attribute__ ((__nothrow__));





extern int getpriority (__priority_which_t __which, id_t __who) __attribute__ ((__nothrow__));



extern int setpriority (__priority_which_t __which, id_t __who, int __prio)
     __attribute__ ((__nothrow__));


# 33 "/usr/include/x86_64-linux-gnu/sys/wait.h" 2 3 4
# 102 "/usr/include/x86_64-linux-gnu/sys/wait.h" 3 4
typedef enum
{
  P_ALL,
  P_PID,
  P_PGID
} idtype_t;
# 116 "/usr/include/x86_64-linux-gnu/sys/wait.h" 3 4
extern __pid_t wait (__WAIT_STATUS __stat_loc);
# 139 "/usr/include/x86_64-linux-gnu/sys/wait.h" 3 4
extern __pid_t waitpid (__pid_t __pid, int *__stat_loc, int __options);



# 1 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/siginfo.h" 2 3 4
# 144 "/usr/include/x86_64-linux-gnu/sys/wait.h" 2 3 4
# 155 "/usr/include/x86_64-linux-gnu/sys/wait.h" 3 4
extern int waitid (idtype_t __idtype, __id_t __id, siginfo_t *__infop,
     int __options);





struct rusage;






extern __pid_t wait3 (__WAIT_STATUS __stat_loc, int __options,
        struct rusage * __usage) __attribute__ ((__nothrow__));




extern __pid_t wait4 (__pid_t __pid, __WAIT_STATUS __stat_loc, int __options,
        struct rusage *__usage) __attribute__ ((__nothrow__));




# 14 "/home/gabriel/repos/qemu/include/qemu/osdep.h" 2
# 92 "/home/gabriel/repos/qemu/include/qemu/osdep.h"
int qemu_daemon(int nochdir, int noclose);
void *qemu_memalign(size_t alignment, size_t size);
void *qemu_vmalloc(size_t size);
void qemu_vfree(void *ptr);
# 144 "/home/gabriel/repos/qemu/include/qemu/osdep.h"
int qemu_madvise(void *addr, size_t len, int advice);

int qemu_open(const char *name, int flags, ...);
int qemu_close(int fd);
# 157 "/home/gabriel/repos/qemu/include/qemu/osdep.h"
int qemu_create_pidfile(const char *filename);
int qemu_get_thread_id(void);
# 177 "/home/gabriel/repos/qemu/include/qemu/osdep.h"
void qemu_set_cloexec(int fd);

void qemu_set_version(const char *);
const char *qemu_get_version(void);

void fips_set_state(_Bool requested);
_Bool fips_get_state(void);
# 129 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 1



# 1 "./config-host.h" 1
# 5 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 2

# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include-fixed/limits.h" 1 3 4
# 7 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 2
# 1 "/home/gabriel/repos/qemu/include/fpu/softfloat.h" 1
# 46 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
# 1 "./config-host.h" 1
# 47 "/home/gabriel/repos/qemu/include/fpu/softfloat.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/osdep.h" 1
# 48 "/home/gabriel/repos/qemu/include/fpu/softfloat.h" 2
# 57 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
typedef uint8_t flag;
typedef uint8_t uint8;
typedef int8_t int8;
typedef unsigned int uint32;
typedef signed int int32;
typedef uint64_t uint64;
typedef int64_t int64;
# 75 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
enum {
    float_relation_less = -1,
    float_relation_equal = 0,
    float_relation_greater = 1,
    float_relation_unordered = 2
};
# 111 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
typedef uint16_t float16;
typedef uint32_t float32;
typedef uint64_t float64;
# 124 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
typedef struct {
    uint64_t low;
    uint16_t high;
} floatx80;


typedef struct {



    uint64_t low, high;

} float128;






enum {
    float_tininess_after_rounding = 0,
    float_tininess_before_rounding = 1
};




enum {
    float_round_nearest_even = 0,
    float_round_down = 1,
    float_round_up = 2,
    float_round_to_zero = 3
};




enum {
    float_flag_invalid = 1,
    float_flag_divbyzero = 4,
    float_flag_overflow = 8,
    float_flag_underflow = 16,
    float_flag_inexact = 32,
    float_flag_input_denormal = 64,
    float_flag_output_denormal = 128
};

typedef struct float_status {
    signed char float_detect_tininess;
    signed char float_rounding_mode;
    signed char float_exception_flags;
    signed char floatx80_rounding_precision;

    flag flush_to_zero;

    flag flush_inputs_to_zero;
    flag default_nan_mode;
} float_status;

void set_float_rounding_mode(int val , float_status *status);
void set_float_exception_flags(int val , float_status *status);
static __attribute__ (( always_inline )) __inline__ void set_float_detect_tininess(int val , float_status *status)
{
    status->float_detect_tininess = val;
}
static __attribute__ (( always_inline )) __inline__ void set_flush_to_zero(flag val , float_status *status)
{
    status->flush_to_zero = val;
}
static __attribute__ (( always_inline )) __inline__ void set_flush_inputs_to_zero(flag val , float_status *status)
{
    status->flush_inputs_to_zero = val;
}
static __attribute__ (( always_inline )) __inline__ void set_default_nan_mode(flag val , float_status *status)
{
    status->default_nan_mode = val;
}
static __attribute__ (( always_inline )) __inline__ int get_float_exception_flags(float_status *status)
{
    return status->float_exception_flags;
}
void set_floatx80_rounding_precision(int val , float_status *status);





void float_raise( int8 flags , float_status *status);







enum {
    float_muladd_negate_c = 1,
    float_muladd_negate_product = 2,
    float_muladd_negate_result = 4,
};




float32 int32_to_float32( int32 , float_status *status );
float64 int32_to_float64( int32 , float_status *status );
float32 uint32_to_float32( uint32 , float_status *status );
float64 uint32_to_float64( uint32 , float_status *status );
floatx80 int32_to_floatx80( int32 , float_status *status );
float128 int32_to_float128( int32 , float_status *status );
float32 int64_to_float32( int64 , float_status *status );
float32 uint64_to_float32( uint64 , float_status *status );
float64 int64_to_float64( int64 , float_status *status );
float64 uint64_to_float64( uint64 , float_status *status );
floatx80 int64_to_floatx80( int64 , float_status *status );
float128 int64_to_float128( int64 , float_status *status );
float128 uint64_to_float128( uint64 , float_status *status );




float16 float32_to_float16( float32, flag , float_status *status );
float32 float16_to_float32( float16, flag , float_status *status );




int float16_is_quiet_nan( float16 );
int float16_is_signaling_nan( float16 );
float16 float16_maybe_silence_nan( float16 );

static __attribute__ (( always_inline )) __inline__ int float16_is_any_nan(float16 a)
{
    return (((a) & ~0x8000) > 0x7c00);
}




extern const float16 float16_default_nan;




int_fast16_t float32_to_int16_round_to_zero(float32 , float_status *status);
uint_fast16_t float32_to_uint16_round_to_zero(float32 , float_status *status);
int32 float32_to_int32( float32 , float_status *status );
int32 float32_to_int32_round_to_zero( float32 , float_status *status );
uint32 float32_to_uint32( float32 , float_status *status );
uint32 float32_to_uint32_round_to_zero( float32 , float_status *status );
int64 float32_to_int64( float32 , float_status *status );
int64 float32_to_int64_round_to_zero( float32 , float_status *status );
float64 float32_to_float64( float32 , float_status *status );
floatx80 float32_to_floatx80( float32 , float_status *status );
float128 float32_to_float128( float32 , float_status *status );




float32 float32_round_to_int( float32 , float_status *status );
float32 float32_add( float32, float32 , float_status *status );
float32 float32_sub( float32, float32 , float_status *status );
float32 float32_mul( float32, float32 , float_status *status );
float32 float32_div( float32, float32 , float_status *status );
float32 float32_rem( float32, float32 , float_status *status );
float32 float32_muladd(float32, float32, float32, int , float_status *status);
float32 float32_sqrt( float32 , float_status *status );
float32 float32_exp2( float32 , float_status *status );
float32 float32_log2( float32 , float_status *status );
int float32_eq( float32, float32 , float_status *status );
int float32_le( float32, float32 , float_status *status );
int float32_lt( float32, float32 , float_status *status );
int float32_unordered( float32, float32 , float_status *status );
int float32_eq_quiet( float32, float32 , float_status *status );
int float32_le_quiet( float32, float32 , float_status *status );
int float32_lt_quiet( float32, float32 , float_status *status );
int float32_unordered_quiet( float32, float32 , float_status *status );
int float32_compare( float32, float32 , float_status *status );
int float32_compare_quiet( float32, float32 , float_status *status );
float32 float32_min(float32, float32 , float_status *status);
float32 float32_max(float32, float32 , float_status *status);
int float32_is_quiet_nan( float32 );
int float32_is_signaling_nan( float32 );
float32 float32_maybe_silence_nan( float32 );
float32 float32_scalbn( float32, int , float_status *status );

static __attribute__ (( always_inline )) __inline__ float32 float32_abs(float32 a)
{



    return ((a) & 0x7fffffff);
}

static __attribute__ (( always_inline )) __inline__ float32 float32_chs(float32 a)
{



    return ((a) ^ 0x80000000);
}

static __attribute__ (( always_inline )) __inline__ int float32_is_infinity(float32 a)
{
    return ((a) & 0x7fffffff) == 0x7f800000;
}

static __attribute__ (( always_inline )) __inline__ int float32_is_neg(float32 a)
{
    return (a) >> 31;
}

static __attribute__ (( always_inline )) __inline__ int float32_is_zero(float32 a)
{
    return ((a) & 0x7fffffff) == 0;
}

static __attribute__ (( always_inline )) __inline__ int float32_is_any_nan(float32 a)
{
    return (((a) & ~(1 << 31)) > 0x7f800000UL);
}

static __attribute__ (( always_inline )) __inline__ int float32_is_zero_or_denormal(float32 a)
{
    return ((a) & 0x7f800000) == 0;
}

static __attribute__ (( always_inline )) __inline__ float32 float32_set_sign(float32 a, int sign)
{
    return (((a) & 0x7fffffff) | (sign << 31));
}
# 367 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
extern const float32 float32_default_nan;




int_fast16_t float64_to_int16_round_to_zero(float64 , float_status *status);
uint_fast16_t float64_to_uint16_round_to_zero(float64 , float_status *status);
int32 float64_to_int32( float64 , float_status *status );
int32 float64_to_int32_round_to_zero( float64 , float_status *status );
uint32 float64_to_uint32( float64 , float_status *status );
uint32 float64_to_uint32_round_to_zero( float64 , float_status *status );
int64 float64_to_int64( float64 , float_status *status );
int64 float64_to_int64_round_to_zero( float64 , float_status *status );
uint64 float64_to_uint64 (float64 a , float_status *status);
uint64 float64_to_uint64_round_to_zero (float64 a , float_status *status);
float32 float64_to_float32( float64 , float_status *status );
floatx80 float64_to_floatx80( float64 , float_status *status );
float128 float64_to_float128( float64 , float_status *status );




float64 float64_round_to_int( float64 , float_status *status );
float64 float64_trunc_to_int( float64 , float_status *status );
float64 float64_add( float64, float64 , float_status *status );
float64 float64_sub( float64, float64 , float_status *status );
float64 float64_mul( float64, float64 , float_status *status );
float64 float64_div( float64, float64 , float_status *status );
float64 float64_rem( float64, float64 , float_status *status );
float64 float64_muladd(float64, float64, float64, int , float_status *status);
float64 float64_sqrt( float64 , float_status *status );
float64 float64_log2( float64 , float_status *status );
int float64_eq( float64, float64 , float_status *status );
int float64_le( float64, float64 , float_status *status );
int float64_lt( float64, float64 , float_status *status );
int float64_unordered( float64, float64 , float_status *status );
int float64_eq_quiet( float64, float64 , float_status *status );
int float64_le_quiet( float64, float64 , float_status *status );
int float64_lt_quiet( float64, float64 , float_status *status );
int float64_unordered_quiet( float64, float64 , float_status *status );
int float64_compare( float64, float64 , float_status *status );
int float64_compare_quiet( float64, float64 , float_status *status );
float64 float64_min(float64, float64 , float_status *status);
float64 float64_max(float64, float64 , float_status *status);
int float64_is_quiet_nan( float64 a );
int float64_is_signaling_nan( float64 );
float64 float64_maybe_silence_nan( float64 );
float64 float64_scalbn( float64, int , float_status *status );

static __attribute__ (( always_inline )) __inline__ float64 float64_abs(float64 a)
{



    return ((a) & 0x7fffffffffffffffLL);
}

static __attribute__ (( always_inline )) __inline__ float64 float64_chs(float64 a)
{



    return ((a) ^ 0x8000000000000000LL);
}

static __attribute__ (( always_inline )) __inline__ int float64_is_infinity(float64 a)
{
    return ((a) & 0x7fffffffffffffffLL ) == 0x7ff0000000000000LL;
}

static __attribute__ (( always_inline )) __inline__ int float64_is_neg(float64 a)
{
    return (a) >> 63;
}

static __attribute__ (( always_inline )) __inline__ int float64_is_zero(float64 a)
{
    return ((a) & 0x7fffffffffffffffLL) == 0;
}

static __attribute__ (( always_inline )) __inline__ int float64_is_any_nan(float64 a)
{
    return (((a) & ~(1ULL << 63)) > 0x7ff0000000000000ULL);
}

static __attribute__ (( always_inline )) __inline__ int float64_is_zero_or_denormal(float64 a)
{
    return ((a) & 0x7ff0000000000000LL) == 0;
}

static __attribute__ (( always_inline )) __inline__ float64 float64_set_sign(float64 a, int sign)
{
    return (((a) & 0x7fffffffffffffffULL) | ((int64_t)sign << 63))
                                                ;
}
# 473 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
extern const float64 float64_default_nan;




int32 floatx80_to_int32( floatx80 , float_status *status );
int32 floatx80_to_int32_round_to_zero( floatx80 , float_status *status );
int64 floatx80_to_int64( floatx80 , float_status *status );
int64 floatx80_to_int64_round_to_zero( floatx80 , float_status *status );
float32 floatx80_to_float32( floatx80 , float_status *status );
float64 floatx80_to_float64( floatx80 , float_status *status );
float128 floatx80_to_float128( floatx80 , float_status *status );




floatx80 floatx80_round_to_int( floatx80 , float_status *status );
floatx80 floatx80_add( floatx80, floatx80 , float_status *status );
floatx80 floatx80_sub( floatx80, floatx80 , float_status *status );
floatx80 floatx80_mul( floatx80, floatx80 , float_status *status );
floatx80 floatx80_div( floatx80, floatx80 , float_status *status );
floatx80 floatx80_rem( floatx80, floatx80 , float_status *status );
floatx80 floatx80_sqrt( floatx80 , float_status *status );
int floatx80_eq( floatx80, floatx80 , float_status *status );
int floatx80_le( floatx80, floatx80 , float_status *status );
int floatx80_lt( floatx80, floatx80 , float_status *status );
int floatx80_unordered( floatx80, floatx80 , float_status *status );
int floatx80_eq_quiet( floatx80, floatx80 , float_status *status );
int floatx80_le_quiet( floatx80, floatx80 , float_status *status );
int floatx80_lt_quiet( floatx80, floatx80 , float_status *status );
int floatx80_unordered_quiet( floatx80, floatx80 , float_status *status );
int floatx80_compare( floatx80, floatx80 , float_status *status );
int floatx80_compare_quiet( floatx80, floatx80 , float_status *status );
int floatx80_is_quiet_nan( floatx80 );
int floatx80_is_signaling_nan( floatx80 );
floatx80 floatx80_maybe_silence_nan( floatx80 );
floatx80 floatx80_scalbn( floatx80, int , float_status *status );

static __attribute__ (( always_inline )) __inline__ floatx80 floatx80_abs(floatx80 a)
{
    a.high &= 0x7fff;
    return a;
}

static __attribute__ (( always_inline )) __inline__ floatx80 floatx80_chs(floatx80 a)
{
    a.high ^= 0x8000;
    return a;
}

static __attribute__ (( always_inline )) __inline__ int floatx80_is_infinity(floatx80 a)
{
    return (a.high & 0x7fff) == 0x7fff && a.low == 0x8000000000000000LL;
}

static __attribute__ (( always_inline )) __inline__ int floatx80_is_neg(floatx80 a)
{
    return a.high >> 15;
}

static __attribute__ (( always_inline )) __inline__ int floatx80_is_zero(floatx80 a)
{
    return (a.high & 0x7fff) == 0 && a.low == 0;
}

static __attribute__ (( always_inline )) __inline__ int floatx80_is_zero_or_denormal(floatx80 a)
{
    return (a.high & 0x7fff) == 0;
}

static __attribute__ (( always_inline )) __inline__ int floatx80_is_any_nan(floatx80 a)
{
    return ((a.high & 0x7fff) == 0x7fff) && (a.low<<1);
}
# 558 "/home/gabriel/repos/qemu/include/fpu/softfloat.h"
extern const floatx80 floatx80_default_nan;




int32 float128_to_int32( float128 , float_status *status );
int32 float128_to_int32_round_to_zero( float128 , float_status *status );
int64 float128_to_int64( float128 , float_status *status );
int64 float128_to_int64_round_to_zero( float128 , float_status *status );
float32 float128_to_float32( float128 , float_status *status );
float64 float128_to_float64( float128 , float_status *status );
floatx80 float128_to_floatx80( float128 , float_status *status );




float128 float128_round_to_int( float128 , float_status *status );
float128 float128_add( float128, float128 , float_status *status );
float128 float128_sub( float128, float128 , float_status *status );
float128 float128_mul( float128, float128 , float_status *status );
float128 float128_div( float128, float128 , float_status *status );
float128 float128_rem( float128, float128 , float_status *status );
float128 float128_sqrt( float128 , float_status *status );
int float128_eq( float128, float128 , float_status *status );
int float128_le( float128, float128 , float_status *status );
int float128_lt( float128, float128 , float_status *status );
int float128_unordered( float128, float128 , float_status *status );
int float128_eq_quiet( float128, float128 , float_status *status );
int float128_le_quiet( float128, float128 , float_status *status );
int float128_lt_quiet( float128, float128 , float_status *status );
int float128_unordered_quiet( float128, float128 , float_status *status );
int float128_compare( float128, float128 , float_status *status );
int float128_compare_quiet( float128, float128 , float_status *status );
int float128_is_quiet_nan( float128 );
int float128_is_signaling_nan( float128 );
float128 float128_maybe_silence_nan( float128 );
float128 float128_scalbn( float128, int , float_status *status );

static __attribute__ (( always_inline )) __inline__ float128 float128_abs(float128 a)
{
    a.high &= 0x7fffffffffffffffLL;
    return a;
}

static __attribute__ (( always_inline )) __inline__ float128 float128_chs(float128 a)
{
    a.high ^= 0x8000000000000000LL;
    return a;
}

static __attribute__ (( always_inline )) __inline__ int float128_is_infinity(float128 a)
{
    return (a.high & 0x7fffffffffffffffLL) == 0x7fff000000000000LL && a.low == 0;
}

static __attribute__ (( always_inline )) __inline__ int float128_is_neg(float128 a)
{
    return a.high >> 63;
}

static __attribute__ (( always_inline )) __inline__ int float128_is_zero(float128 a)
{
    return (a.high & 0x7fffffffffffffffLL) == 0 && a.low == 0;
}

static __attribute__ (( always_inline )) __inline__ int float128_is_zero_or_denormal(float128 a)
{
    return (a.high & 0x7fff000000000000LL) == 0;
}

static __attribute__ (( always_inline )) __inline__ int float128_is_any_nan(float128 a)
{
    return ((a.high >> 48) & 0x7fff) == 0x7fff &&
        ((a.low != 0) || ((a.high & 0xffffffffffffLL) != 0));
}






extern const float128 float128_default_nan;
# 8 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 2






# 1 "/usr/include/byteswap.h" 1 3 4
# 23 "/usr/include/byteswap.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 1 3 4
# 24 "/usr/include/byteswap.h" 2 3 4
# 15 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 2

static __attribute__ (( always_inline )) __inline__ uint16_t bswap16(uint16_t x)
{
    return (__extension__ ({ register unsigned short int __v, __x = (unsigned short int) (x); if (__builtin_constant_p (__x)) __v = ((unsigned short int) ((((__x) >> 8) & 0xff) | (((__x) & 0xff) << 8))); else __asm__ ("rorw $8, %w0" : "=r" (__v) : "0" (__x) : "cc"); __v; }));
}

static __attribute__ (( always_inline )) __inline__ uint32_t bswap32(uint32_t x)
{
    return (__extension__ ({ register unsigned int __v, __x = (x); if (__builtin_constant_p (__x)) __v = ((((__x) & 0xff000000) >> 24) | (((__x) & 0x00ff0000) >> 8) | (((__x) & 0x0000ff00) << 8) | (((__x) & 0x000000ff) << 24)); else __asm__ ("bswap %0" : "=r" (__v) : "0" (__x)); __v; }));
}

static __attribute__ (( always_inline )) __inline__ uint64_t bswap64(uint64_t x)
{
    return (__extension__ ({ register unsigned long __v, __x = (x); if (__builtin_constant_p (__x)) __v = ((((__x) & 0xff00000000000000ull) >> 56) | (((__x) & 0x00ff000000000000ull) >> 40) | (((__x) & 0x0000ff0000000000ull) >> 24) | (((__x) & 0x000000ff00000000ull) >> 8) | (((__x) & 0x00000000ff000000ull) << 8) | (((__x) & 0x0000000000ff0000ull) << 24) | (((__x) & 0x000000000000ff00ull) << 40) | (((__x) & 0x00000000000000ffull) << 56)); else __asm__ ("bswap %q0" : "=r" (__v) : "0" (__x)); __v; }));
}
# 58 "/home/gabriel/repos/qemu/include/qemu/bswap.h"
static __attribute__ (( always_inline )) __inline__ void bswap16s(uint16_t *s)
{
    *s = bswap16(*s);
}

static __attribute__ (( always_inline )) __inline__ void bswap32s(uint32_t *s)
{
    *s = bswap32(*s);
}

static __attribute__ (( always_inline )) __inline__ void bswap64s(uint64_t *s)
{
    *s = bswap64(*s);
}
# 116 "/home/gabriel/repos/qemu/include/qemu/bswap.h"
static __attribute__ (( always_inline )) __inline__ uint16_t be16_to_cpu(uint16_t v){ return bswap16(v);}static __attribute__ (( always_inline )) __inline__ uint16_t cpu_to_be16(uint16_t v){ return bswap16(v);}static __attribute__ (( always_inline )) __inline__ void be16_to_cpus(uint16_t *p){ do { *p = bswap16(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be16s(uint16_t *p){ do { *p = bswap16(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ uint16_t be16_to_cpup(const uint16_t *p){ return be16_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be16w(uint16_t *p, uint16_t v){ *p = cpu_to_be16(v);}
static __attribute__ (( always_inline )) __inline__ uint32_t be32_to_cpu(uint32_t v){ return bswap32(v);}static __attribute__ (( always_inline )) __inline__ uint32_t cpu_to_be32(uint32_t v){ return bswap32(v);}static __attribute__ (( always_inline )) __inline__ void be32_to_cpus(uint32_t *p){ do { *p = bswap32(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be32s(uint32_t *p){ do { *p = bswap32(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ uint32_t be32_to_cpup(const uint32_t *p){ return be32_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be32w(uint32_t *p, uint32_t v){ *p = cpu_to_be32(v);}
static __attribute__ (( always_inline )) __inline__ uint64_t be64_to_cpu(uint64_t v){ return bswap64(v);}static __attribute__ (( always_inline )) __inline__ uint64_t cpu_to_be64(uint64_t v){ return bswap64(v);}static __attribute__ (( always_inline )) __inline__ void be64_to_cpus(uint64_t *p){ do { *p = bswap64(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be64s(uint64_t *p){ do { *p = bswap64(*p); } while(0);}static __attribute__ (( always_inline )) __inline__ uint64_t be64_to_cpup(const uint64_t *p){ return be64_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_be64w(uint64_t *p, uint64_t v){ *p = cpu_to_be64(v);}

static __attribute__ (( always_inline )) __inline__ uint16_t le16_to_cpu(uint16_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ uint16_t cpu_to_le16(uint16_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ void le16_to_cpus(uint16_t *p){ ;}static __attribute__ (( always_inline )) __inline__ void cpu_to_le16s(uint16_t *p){ ;}static __attribute__ (( always_inline )) __inline__ uint16_t le16_to_cpup(const uint16_t *p){ return le16_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_le16w(uint16_t *p, uint16_t v){ *p = cpu_to_le16(v);}
static __attribute__ (( always_inline )) __inline__ uint32_t le32_to_cpu(uint32_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ uint32_t cpu_to_le32(uint32_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ void le32_to_cpus(uint32_t *p){ ;}static __attribute__ (( always_inline )) __inline__ void cpu_to_le32s(uint32_t *p){ ;}static __attribute__ (( always_inline )) __inline__ uint32_t le32_to_cpup(const uint32_t *p){ return le32_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_le32w(uint32_t *p, uint32_t v){ *p = cpu_to_le32(v);}
static __attribute__ (( always_inline )) __inline__ uint64_t le64_to_cpu(uint64_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ uint64_t cpu_to_le64(uint64_t v){ return (v);}static __attribute__ (( always_inline )) __inline__ void le64_to_cpus(uint64_t *p){ ;}static __attribute__ (( always_inline )) __inline__ void cpu_to_le64s(uint64_t *p){ ;}static __attribute__ (( always_inline )) __inline__ uint64_t le64_to_cpup(const uint64_t *p){ return le64_to_cpu(*p);}static __attribute__ (( always_inline )) __inline__ void cpu_to_le64w(uint64_t *p, uint64_t v){ *p = cpu_to_le64(v);}


static __attribute__ (( always_inline )) __inline__ uint32_t qemu_bswap_len(uint32_t value, int len)
{
    return bswap32(value) >> (32 - 8 * len);
}



typedef union {
    float32 f;
    uint32_t l;
} CPU_FloatU;

typedef union {
    float64 d;






    struct {
        uint32_t lower;
        uint32_t upper;
    } l;

    uint64_t ll;
} CPU_DoubleU;

typedef union {
     floatx80 d;
     struct {
         uint64_t lower;
         uint16_t upper;
     } l;
} CPU_LDoubleU;

typedef union {
    float128 q;
# 175 "/home/gabriel/repos/qemu/include/qemu/bswap.h"
    struct {
        uint32_t lowest;
        uint32_t lower;
        uint32_t upper;
        uint32_t upmost;
    } l;
    struct {
        uint64_t lower;
        uint64_t upper;
    } ll;

} CPU_QuadU;
# 220 "/home/gabriel/repos/qemu/include/qemu/bswap.h"
static __attribute__ (( always_inline )) __inline__ int ldub_p(const void *ptr)
{
    return *(uint8_t *)ptr;
}

static __attribute__ (( always_inline )) __inline__ int ldsb_p(const void *ptr)
{
    return *(int8_t *)ptr;
}

static __attribute__ (( always_inline )) __inline__ void stb_p(void *ptr, int v)
{
    *(uint8_t *)ptr = v;
}





static __attribute__ (( always_inline )) __inline__ int lduw_p(const void *ptr)
{
    uint16_t r;
    memcpy(&r, ptr, sizeof(r));
    return r;
}

static __attribute__ (( always_inline )) __inline__ int ldsw_p(const void *ptr)
{
    int16_t r;
    memcpy(&r, ptr, sizeof(r));
    return r;
}

static __attribute__ (( always_inline )) __inline__ void stw_p(void *ptr, uint16_t v)
{
    memcpy(ptr, &v, sizeof(v));
}

static __attribute__ (( always_inline )) __inline__ int ldl_p(const void *ptr)
{
    int32_t r;
    memcpy(&r, ptr, sizeof(r));
    return r;
}

static __attribute__ (( always_inline )) __inline__ void stl_p(void *ptr, uint32_t v)
{
    memcpy(ptr, &v, sizeof(v));
}

static __attribute__ (( always_inline )) __inline__ uint64_t ldq_p(const void *ptr)
{
    uint64_t r;
    memcpy(&r, ptr, sizeof(r));
    return r;
}

static __attribute__ (( always_inline )) __inline__ void stq_p(void *ptr, uint64_t v)
{
    memcpy(ptr, &v, sizeof(v));
}

static __attribute__ (( always_inline )) __inline__ int lduw_le_p(const void *ptr)
{
    return (uint16_t)(lduw_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ int ldsw_le_p(const void *ptr)
{
    return (int16_t)(lduw_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ int ldl_le_p(const void *ptr)
{
    return (ldl_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ uint64_t ldq_le_p(const void *ptr)
{
    return (ldq_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ void stw_le_p(void *ptr, int v)
{
    stw_p(ptr, (v));
}

static __attribute__ (( always_inline )) __inline__ void stl_le_p(void *ptr, int v)
{
    stl_p(ptr, (v));
}

static __attribute__ (( always_inline )) __inline__ void stq_le_p(void *ptr, uint64_t v)
{
    stq_p(ptr, (v));
}



static __attribute__ (( always_inline )) __inline__ float32 ldfl_le_p(const void *ptr)
{
    CPU_FloatU u;
    u.l = ldl_le_p(ptr);
    return u.f;
}

static __attribute__ (( always_inline )) __inline__ void stfl_le_p(void *ptr, float32 v)
{
    CPU_FloatU u;
    u.f = v;
    stl_le_p(ptr, u.l);
}

static __attribute__ (( always_inline )) __inline__ float64 ldfq_le_p(const void *ptr)
{
    CPU_DoubleU u;
    u.ll = ldq_le_p(ptr);
    return u.d;
}

static __attribute__ (( always_inline )) __inline__ void stfq_le_p(void *ptr, float64 v)
{
    CPU_DoubleU u;
    u.d = v;
    stq_le_p(ptr, u.ll);
}

static __attribute__ (( always_inline )) __inline__ int lduw_be_p(const void *ptr)
{
    return (uint16_t)bswap16(lduw_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ int ldsw_be_p(const void *ptr)
{
    return (int16_t)bswap16(lduw_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ int ldl_be_p(const void *ptr)
{
    return bswap32(ldl_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ uint64_t ldq_be_p(const void *ptr)
{
    return bswap64(ldq_p(ptr));
}

static __attribute__ (( always_inline )) __inline__ void stw_be_p(void *ptr, int v)
{
    stw_p(ptr, bswap16(v));
}

static __attribute__ (( always_inline )) __inline__ void stl_be_p(void *ptr, int v)
{
    stl_p(ptr, bswap32(v));
}

static __attribute__ (( always_inline )) __inline__ void stq_be_p(void *ptr, uint64_t v)
{
    stq_p(ptr, bswap64(v));
}



static __attribute__ (( always_inline )) __inline__ float32 ldfl_be_p(const void *ptr)
{
    CPU_FloatU u;
    u.l = ldl_be_p(ptr);
    return u.f;
}

static __attribute__ (( always_inline )) __inline__ void stfl_be_p(void *ptr, float32 v)
{
    CPU_FloatU u;
    u.f = v;
    stl_be_p(ptr, u.l);
}

static __attribute__ (( always_inline )) __inline__ float64 ldfq_be_p(const void *ptr)
{
    CPU_DoubleU u;
    u.ll = ldq_be_p(ptr);
    return u.d;
}

static __attribute__ (( always_inline )) __inline__ void stfq_be_p(void *ptr, float64 v)
{
    CPU_DoubleU u;
    u.d = v;
    stq_be_p(ptr, u.ll);
}



static __attribute__ (( always_inline )) __inline__ void cpu_to_le16wu(uint16_t *p, uint16_t v)
{
    stw_le_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ void cpu_to_le32wu(uint32_t *p, uint32_t v)
{
    stl_le_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ uint16_t le16_to_cpupu(const uint16_t *p)
{
    return lduw_le_p(p);
}

static __attribute__ (( always_inline )) __inline__ uint32_t le32_to_cpupu(const uint32_t *p)
{
    return ldl_le_p(p);
}

static __attribute__ (( always_inline )) __inline__ uint32_t be32_to_cpupu(const uint32_t *p)
{
    return ldl_be_p(p);
}

static __attribute__ (( always_inline )) __inline__ void cpu_to_be16wu(uint16_t *p, uint16_t v)
{
    stw_be_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ void cpu_to_be32wu(uint32_t *p, uint32_t v)
{
    stl_be_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ void cpu_to_be64wu(uint64_t *p, uint64_t v)
{
    stq_be_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ void cpu_to_32wu(uint32_t *p, uint32_t v)
{
    stl_p(p, v);
}

static __attribute__ (( always_inline )) __inline__ unsigned long leul_to_cpu(unsigned long v)
{





    return (v);



}
# 130 "/home/gabriel/repos/qemu/include/qemu-common.h" 2
# 142 "/home/gabriel/repos/qemu/include/qemu-common.h"
void qemu_get_timedate(struct tm *tm, int offset);
int qemu_timedate_diff(struct tm *tm);
# 170 "/home/gabriel/repos/qemu/include/qemu-common.h"
static __attribute__ (( always_inline )) __inline__ _Bool is_help_option(const char *s)
{
    return !__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (s) && __builtin_constant_p ("?") && (__s1_len = __builtin_strlen (s), __s2_len = __builtin_strlen ("?"), (!((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("?") + 1) - (size_t)(const void *)("?") == 1) || __s2_len >= 4)) ? __builtin_strcmp (s, "?") : (__builtin_constant_p (s) && ((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) && (__s1_len = __builtin_strlen (s), __s1_len < 4) ? (__builtin_constant_p ("?") && ((size_t)(const void *)(("?") + 1) - (size_t)(const void *)("?") == 1) ? __builtin_strcmp (s, "?") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("?"); register int __result = (((__const unsigned char *) (__const char *) (s))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (s))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (s))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (s))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("?") && ((size_t)(const void *)(("?") + 1) - (size_t)(const void *)("?") == 1) && (__s2_len = __builtin_strlen ("?"), __s2_len < 4) ? (__builtin_constant_p (s) && ((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) ? __builtin_strcmp (s, "?") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (s); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("?"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("?"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("?"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("?"))[3]); } } __result; }))) : __builtin_strcmp (s, "?")))); }) || !__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (s) && __builtin_constant_p ("help") && (__s1_len = __builtin_strlen (s), __s2_len = __builtin_strlen ("help"), (!((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("help") + 1) - (size_t)(const void *)("help") == 1) || __s2_len >= 4)) ? __builtin_strcmp (s, "help") : (__builtin_constant_p (s) && ((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) && (__s1_len = __builtin_strlen (s), __s1_len < 4) ? (__builtin_constant_p ("help") && ((size_t)(const void *)(("help") + 1) - (size_t)(const void *)("help") == 1) ? __builtin_strcmp (s, "help") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("help"); register int __result = (((__const unsigned char *) (__const char *) (s))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (s))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (s))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (s))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("help") && ((size_t)(const void *)(("help") + 1) - (size_t)(const void *)("help") == 1) && (__s2_len = __builtin_strlen ("help"), __s2_len < 4) ? (__builtin_constant_p (s) && ((size_t)(const void *)((s) + 1) - (size_t)(const void *)(s) == 1) ? __builtin_strcmp (s, "help") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (s); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("help"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("help"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("help"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("help"))[3]); } } __result; }))) : __builtin_strcmp (s, "help")))); });
}


void pstrcpy(char *buf, int buf_size, const char *str);
void strpadcpy(char *buf, int buf_size, const char *str, char pad);
char *pstrcat(char *buf, int buf_size, const char *s);
int strstart(const char *str, const char *val, const char **ptr);
int stristart(const char *str, const char *val, const char **ptr);
int qemu_strnlen(const char *s, int max_len);
time_t mktimegm(struct tm *tm);
int qemu_fls(int i);
int qemu_fdatasync(int fd);
int fcntl_setfl(int fd, int flag);
int qemu_parse_fd(const char *param);

int parse_uint(const char *s, unsigned long long *value, char **endptr,
               int base);
int parse_uint_full(const char *s, unsigned long long *value, int base);
# 204 "/home/gabriel/repos/qemu/include/qemu-common.h"
int64_t strtosz(const char *nptr, char **end);
int64_t strtosz_suffix(const char *nptr, char **end, const char default_suffix);
int64_t strtosz_suffix_unit(const char *nptr, char **end,
                            const char default_suffix, int64_t unit);


void init_paths(const char *prefix);
const char *path(const char *pathname);
# 229 "/home/gabriel/repos/qemu/include/qemu-common.h"
void *qemu_oom_check(void *ptr);

ssize_t qemu_write_full(int fd, const void *buf, size_t count)
    __attribute__((warn_unused_result));
ssize_t qemu_send_full(int fd, const void *buf, size_t count, int flags)
    __attribute__((warn_unused_result));
ssize_t qemu_recv_full(int fd, void *buf, size_t count, int flags)
    __attribute__((warn_unused_result));


int qemu_pipe(int pipefd[2]);
# 263 "/home/gabriel/repos/qemu/include/qemu-common.h"
void __attribute__ ((__noreturn__)) hw_error(const char *fmt, ...) __attribute__((format(gnu_printf, 1, 2)));

struct ParallelIOArg {
    void *buffer;
    int count;
};

typedef int (*DMA_transfer_handler) (void *opaque, int nchan, int pos, int size);

typedef uint64_t pcibus_t;

typedef enum LostTickPolicy {
    LOST_TICK_DISCARD,
    LOST_TICK_DELAY,
    LOST_TICK_MERGE,
    LOST_TICK_SLEW,
    LOST_TICK_MAX
} LostTickPolicy;

typedef struct PCIHostDeviceAddress {
    unsigned int domain;
    unsigned int bus;
    unsigned int slot;
    unsigned int function;
} PCIHostDeviceAddress;

void tcg_exec_init(unsigned long tb_size);
_Bool tcg_enabled(void);

void cpu_exec_init_all(void);


void cpu_save(QEMUFile *f, void *opaque);
int cpu_load(QEMUFile *f, void *opaque, int version_id);


void qemu_cpu_kick_self(void);


struct qemu_work_item {
    struct qemu_work_item *next;
    void (*func)(void *data);
    void *data;
    int done;
};






void qemu_init_vcpu(void *env);
# 325 "/home/gabriel/repos/qemu/include/qemu-common.h"
ssize_t qemu_co_sendv_recvv(int sockfd, struct iovec *iov, unsigned iov_cnt,
                            size_t offset, size_t bytes, _Bool do_send);
# 335 "/home/gabriel/repos/qemu/include/qemu-common.h"
ssize_t qemu_co_send_recv(int sockfd, void *buf, size_t bytes, _Bool do_send);





typedef struct QEMUIOVector {
    struct iovec *iov;
    int niov;
    int nalloc;
    size_t size;
} QEMUIOVector;

void qemu_iovec_init(QEMUIOVector *qiov, int alloc_hint);
void qemu_iovec_init_external(QEMUIOVector *qiov, struct iovec *iov, int niov);
void qemu_iovec_add(QEMUIOVector *qiov, void *base, size_t len);
void qemu_iovec_concat(QEMUIOVector *dst,
                       QEMUIOVector *src, size_t soffset, size_t sbytes);
void qemu_iovec_concat_iov(QEMUIOVector *dst,
                           struct iovec *src_iov, unsigned int src_cnt,
                           size_t soffset, size_t sbytes);
void qemu_iovec_destroy(QEMUIOVector *qiov);
void qemu_iovec_reset(QEMUIOVector *qiov);
size_t qemu_iovec_to_buf(QEMUIOVector *qiov, size_t offset,
                         void *buf, size_t bytes);
size_t qemu_iovec_from_buf(QEMUIOVector *qiov, size_t offset,
                           const void *buf, size_t bytes);
size_t qemu_iovec_memset(QEMUIOVector *qiov, size_t offset,
                         int fillc, size_t bytes);

_Bool buffer_is_zero(const void *buf, size_t len);

void qemu_progress_init(int enabled, float min_skip);
void qemu_progress_end(void);
void qemu_progress_print(float delta, int max);
const char *qemu_get_vm_name(void);



char *qemu_find_file(int type, const char *name);


void os_setup_early_signal_handling(void);
char *os_find_datadir(const char *argv0);
void os_parse_cmd_args(int index, const char *optarg);
void os_pidfile_error(void);


static __attribute__ (( always_inline )) __inline__ uint8_t to_bcd(uint8_t val)
{
    return ((val / 10) << 4) | (val % 10);
}

static __attribute__ (( always_inline )) __inline__ uint8_t from_bcd(uint8_t val)
{
    return ((val >> 4) * 10) + (val & 0x0f);
}


static __attribute__ (( always_inline )) __inline__ uint64_t muldiv64(uint64_t a, uint32_t b, uint32_t c)
{
    union {
        uint64_t ll;
        struct {



            uint32_t low, high;

        } l;
    } u, res;
    uint64_t rl, rh;

    u.ll = a;
    rl = (uint64_t)u.l.low * (uint64_t)b;
    rh = (uint64_t)u.l.high * (uint64_t)b;
    rh += (rl >> 32);
    res.l.high = rh / c;
    res.l.low = (((rh % c) << 32) + (rl & 0xffffffff)) / c;
    return res.ll;
}







static __attribute__ (( always_inline )) __inline__ _Bool is_power_of_2(uint64_t value)
{
    if (!value) {
        return 0;
    }

    return !(value & (value - 1));
}


int64_t pow2floor(int64_t value);

# 1 "/home/gabriel/repos/qemu/include/qemu/module.h" 1
# 23 "/home/gabriel/repos/qemu/include/qemu/module.h"
typedef enum {
    MODULE_INIT_BLOCK,
    MODULE_INIT_MACHINE,
    MODULE_INIT_QAPI,
    MODULE_INIT_QOM,
    MODULE_INIT_MAX
} module_init_type;






void register_module_init(void (*fn)(void), module_init_type type);

void module_call_init(module_init_type type);
# 436 "/home/gabriel/repos/qemu/include/qemu-common.h" 2






int uleb128_encode_small(uint8_t *out, uint32_t n);
int uleb128_decode_small(const uint8_t *in, uint32_t *n);


int mod_utf8_codepoint(const char *s, size_t n, char **end);





void hexdump(const char *buf, FILE *fp, const char *prefix, size_t size);
# 478 "/home/gabriel/repos/qemu/include/qemu-common.h"
static __attribute__ (( always_inline )) __inline__ _Bool
can_use_buffer_find_nonzero_offset(const void *buf, size_t len)
{
    return (len % (8
                   * sizeof(unsigned long)) == 0
            && ((uintptr_t) buf) % sizeof(unsigned long) == 0);
}
size_t buffer_find_nonzero_offset(const void *buf, size_t len);
# 6 "/home/gabriel/repos/qemu/include/hw/hw.h" 2


# 1 "/home/gabriel/repos/qemu/include/exec/cpu-common.h" 1





# 1 "/home/gabriel/repos/qemu/include/exec/hwaddr.h" 1
# 12 "/home/gabriel/repos/qemu/include/exec/hwaddr.h"
typedef uint64_t hwaddr;
# 7 "/home/gabriel/repos/qemu/include/exec/cpu-common.h" 2


# 1 "/home/gabriel/repos/qemu/include/exec/poison.h" 1







       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

       
       

       
       
       

       
       
       
       

       
       

       
       
       
       
       
       
       
       

       
       
       
       
       
       
       
       
       
       
       
       
# 10 "/home/gabriel/repos/qemu/include/exec/cpu-common.h" 2


# 1 "/home/gabriel/repos/qemu/include/qemu/bswap.h" 1
# 13 "/home/gabriel/repos/qemu/include/exec/cpu-common.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/queue.h" 1
# 81 "/home/gabriel/repos/qemu/include/qemu/queue.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/atomic.h" 1
# 82 "/home/gabriel/repos/qemu/include/qemu/queue.h" 2
# 14 "/home/gabriel/repos/qemu/include/exec/cpu-common.h" 2
# 22 "/home/gabriel/repos/qemu/include/exec/cpu-common.h"
typedef struct CPUListState {
    fprintf_function cpu_fprintf;
    FILE *file;
} CPUListState;



enum device_endian {
    DEVICE_NATIVE_ENDIAN,
    DEVICE_BIG_ENDIAN,
    DEVICE_LITTLE_ENDIAN,
};







typedef uintptr_t ram_addr_t;






typedef void CPUWriteMemoryFunc(void *opaque, hwaddr addr, uint32_t value);
typedef uint32_t CPUReadMemoryFunc(void *opaque, hwaddr addr);

void qemu_ram_remap(ram_addr_t addr, ram_addr_t length);

void *qemu_get_ram_ptr(ram_addr_t addr);
void qemu_put_ram_ptr(void *addr);

int qemu_ram_addr_from_host(void *ptr, ram_addr_t *ram_addr);
ram_addr_t qemu_ram_addr_from_host_nofail(void *ptr);
void qemu_ram_set_idstr(ram_addr_t addr, const char *name, DeviceState *dev);

void cpu_physical_memory_rw(hwaddr addr, uint8_t *buf,
                            int len, int is_write);
static __attribute__ (( always_inline )) __inline__ void cpu_physical_memory_read(hwaddr addr,
                                            void *buf, int len)
{
    cpu_physical_memory_rw(addr, buf, len, 0);
}
static __attribute__ (( always_inline )) __inline__ void cpu_physical_memory_write(hwaddr addr,
                                             const void *buf, int len)
{
    cpu_physical_memory_rw(addr, (void *)buf, len, 1);
}
void *cpu_physical_memory_map(hwaddr addr,
                              hwaddr *plen,
                              int is_write);
void cpu_physical_memory_unmap(void *buffer, hwaddr len,
                               int is_write, hwaddr access_len);
void *cpu_register_map_client(void *opaque, void (*callback)(void *opaque));

_Bool cpu_physical_memory_is_io(hwaddr phys_addr);






void qemu_flush_coalesced_mmio_buffer(void);

uint32_t ldub_phys(hwaddr addr);
uint32_t lduw_le_phys(hwaddr addr);
uint32_t lduw_be_phys(hwaddr addr);
uint32_t ldl_le_phys(hwaddr addr);
uint32_t ldl_be_phys(hwaddr addr);
uint64_t ldq_le_phys(hwaddr addr);
uint64_t ldq_be_phys(hwaddr addr);
void stb_phys(hwaddr addr, uint32_t val);
void stw_le_phys(hwaddr addr, uint32_t val);
void stw_be_phys(hwaddr addr, uint32_t val);
void stl_le_phys(hwaddr addr, uint32_t val);
void stl_be_phys(hwaddr addr, uint32_t val);
void stq_le_phys(hwaddr addr, uint64_t val);
void stq_be_phys(hwaddr addr, uint64_t val);
# 114 "/home/gabriel/repos/qemu/include/exec/cpu-common.h"
void cpu_physical_memory_write_rom(hwaddr addr,
                                   const uint8_t *buf, int len);

extern struct MemoryRegion io_mem_ram;
extern struct MemoryRegion io_mem_rom;
extern struct MemoryRegion io_mem_unassigned;
extern struct MemoryRegion io_mem_notdirty;
# 9 "/home/gabriel/repos/qemu/include/hw/hw.h" 2


# 1 "/home/gabriel/repos/qemu/include/exec/ioport.h" 1
# 28 "/home/gabriel/repos/qemu/include/exec/ioport.h"
# 1 "/home/gabriel/repos/qemu/include/exec/iorange.h" 1





typedef struct IORange IORange;
typedef struct IORangeOps IORangeOps;

struct IORangeOps {
    void (*read)(IORange *iorange, uint64_t offset, unsigned width,
                 uint64_t *data);
    void (*write)(IORange *iorange, uint64_t offset, unsigned width,
                  uint64_t data);
    void (*destructor)(IORange *iorange);
};

struct IORange {
    const IORangeOps *ops;
    uint64_t base;
    uint64_t len;
};

static __attribute__ (( always_inline )) __inline__ void iorange_init(IORange *iorange, const IORangeOps *ops,
                                uint64_t base, uint64_t len)
{
    iorange->ops = ops;
    iorange->base = base;
    iorange->len = len;
}
# 29 "/home/gabriel/repos/qemu/include/exec/ioport.h" 2

typedef uint32_t pio_addr_t;






typedef void (IOPortWriteFunc)(void *opaque, uint32_t address, uint32_t data);
typedef uint32_t (IOPortReadFunc)(void *opaque, uint32_t address);
typedef void (IOPortDestructor)(void *opaque);

void ioport_register(IORange *iorange);
int register_ioport_read(pio_addr_t start, int length, int size,
                         IOPortReadFunc *func, void *opaque);
int register_ioport_write(pio_addr_t start, int length, int size,
                          IOPortWriteFunc *func, void *opaque);
void isa_unassign_ioport(pio_addr_t start, int length);
_Bool isa_is_ioport_assigned(pio_addr_t start);

void cpu_outb(pio_addr_t addr, uint8_t val);
void cpu_outw(pio_addr_t addr, uint16_t val);
void cpu_outl(pio_addr_t addr, uint32_t val);
uint8_t cpu_inb(pio_addr_t addr);
uint16_t cpu_inw(pio_addr_t addr);
uint32_t cpu_inl(pio_addr_t addr);

struct MemoryRegion;
struct MemoryRegionPortio;

typedef struct PortioList {
    const struct MemoryRegionPortio *ports;
    struct MemoryRegion *address_space;
    unsigned nr;
    struct MemoryRegion **regions;
    struct MemoryRegion **aliases;
    void *opaque;
    const char *name;
} PortioList;

void portio_list_init(PortioList *piolist,
                      const struct MemoryRegionPortio *callbacks,
                      void *opaque, const char *name);
void portio_list_destroy(PortioList *piolist);
void portio_list_add(PortioList *piolist,
                     struct MemoryRegion *address_space,
                     uint32_t addr);
void portio_list_del(PortioList *piolist);
# 12 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 1 "/home/gabriel/repos/qemu/include/hw/irq.h" 1





typedef struct IRQState *qemu_irq;

typedef void (*qemu_irq_handler)(void *opaque, int n, int level);

void qemu_set_irq(qemu_irq irq, int level);

static __attribute__ (( always_inline )) __inline__ void qemu_irq_raise(qemu_irq irq)
{
    qemu_set_irq(irq, 1);
}

static __attribute__ (( always_inline )) __inline__ void qemu_irq_lower(qemu_irq irq)
{
    qemu_set_irq(irq, 0);
}

static __attribute__ (( always_inline )) __inline__ void qemu_irq_pulse(qemu_irq irq)
{
    qemu_set_irq(irq, 1);
    qemu_set_irq(irq, 0);
}




qemu_irq *qemu_allocate_irqs(qemu_irq_handler handler, void *opaque, int n);




qemu_irq *qemu_extend_irqs(qemu_irq *old, int n_old, qemu_irq_handler handler,
                                void *opaque, int n);

void qemu_free_irqs(qemu_irq *s);


qemu_irq qemu_irq_invert(qemu_irq irq);


qemu_irq qemu_irq_split(qemu_irq irq1, qemu_irq irq2);




qemu_irq *qemu_irq_proxy(qemu_irq **target, int n);



void qemu_irq_intercept_in(qemu_irq *gpio_in, qemu_irq_handler handler, int n);
void qemu_irq_intercept_out(qemu_irq **gpio_out, qemu_irq_handler handler, int n);
# 13 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 1 "/home/gabriel/repos/qemu/include/block/aio.h" 1
# 19 "/home/gabriel/repos/qemu/include/block/aio.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/event_notifier.h" 1
# 22 "/home/gabriel/repos/qemu/include/qemu/event_notifier.h"
struct EventNotifier {



    int rfd;
    int wfd;

};

typedef void EventNotifierHandler(EventNotifier *);

int event_notifier_init(EventNotifier *, int active);
void event_notifier_cleanup(EventNotifier *);
int event_notifier_set(EventNotifier *);
int event_notifier_test_and_clear(EventNotifier *);
int event_notifier_set_handler(EventNotifier *, EventNotifierHandler *);


void event_notifier_init_fd(EventNotifier *, int fd);
int event_notifier_get_fd(EventNotifier *);
# 20 "/home/gabriel/repos/qemu/include/block/aio.h" 2

typedef struct BlockDriverAIOCB BlockDriverAIOCB;
typedef void BlockDriverCompletionFunc(void *opaque, int ret);

typedef struct AIOCBInfo {
    void (*cancel)(BlockDriverAIOCB *acb);
    size_t aiocb_size;
} AIOCBInfo;

struct BlockDriverAIOCB {
    const AIOCBInfo *aiocb_info;
    BlockDriverState *bs;
    BlockDriverCompletionFunc *cb;
    void *opaque;
};

void *qemu_aio_get(const AIOCBInfo *aiocb_info, BlockDriverState *bs,
                   BlockDriverCompletionFunc *cb, void *opaque);
void qemu_aio_release(void *p);

typedef struct AioHandler AioHandler;
typedef void QEMUBHFunc(void *opaque);
typedef void IOHandler(void *opaque);

typedef struct AioContext {
    GSource source;


    struct { struct AioHandler *lh_first; } aio_handlers;





    int walking_handlers;


    struct QEMUBH *first_bh;




    int walking_bh;


    EventNotifier notifier;


    GArray *pollfds;


    struct ThreadPool *thread_pool;
} AioContext;


typedef int (AioFlushEventNotifierHandler)(EventNotifier *e);
# 84 "/home/gabriel/repos/qemu/include/block/aio.h"
AioContext *aio_context_new(void);







void aio_context_ref(AioContext *ctx);







void aio_context_unref(AioContext *ctx);
# 109 "/home/gabriel/repos/qemu/include/block/aio.h"
QEMUBH *aio_bh_new(AioContext *ctx, QEMUBHFunc *cb, void *opaque);
# 124 "/home/gabriel/repos/qemu/include/block/aio.h"
void aio_notify(AioContext *ctx);






int aio_bh_poll(AioContext *ctx);
# 145 "/home/gabriel/repos/qemu/include/block/aio.h"
void qemu_bh_schedule(QEMUBH *bh);
# 158 "/home/gabriel/repos/qemu/include/block/aio.h"
void qemu_bh_cancel(QEMUBH *bh);
# 169 "/home/gabriel/repos/qemu/include/block/aio.h"
void qemu_bh_delete(QEMUBH *bh);






_Bool aio_pending(AioContext *ctx);
# 191 "/home/gabriel/repos/qemu/include/block/aio.h"
_Bool aio_poll(AioContext *ctx, _Bool blocking);



typedef int (AioFlushHandler)(void *opaque);
# 204 "/home/gabriel/repos/qemu/include/block/aio.h"
void aio_set_fd_handler(AioContext *ctx,
                        int fd,
                        IOHandler *io_read,
                        IOHandler *io_write,
                        AioFlushHandler *io_flush,
                        void *opaque);
# 219 "/home/gabriel/repos/qemu/include/block/aio.h"
void aio_set_event_notifier(AioContext *ctx,
                            EventNotifier *notifier,
                            EventNotifierHandler *io_read,
                            AioFlushEventNotifierHandler *io_flush);




GSource *aio_get_g_source(AioContext *ctx);


struct ThreadPool *aio_get_thread_pool(AioContext *ctx);



_Bool qemu_aio_wait(void);
void qemu_aio_set_event_notifier(EventNotifier *notifier,
                                 EventNotifierHandler *io_read,
                                 AioFlushEventNotifierHandler *io_flush);


void qemu_aio_set_fd_handler(int fd,
                             IOHandler *io_read,
                             IOHandler *io_write,
                             AioFlushHandler *io_flush,
                             void *opaque);
# 14 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 1 "/home/gabriel/repos/qemu/include/migration/qemu-file.h" 1
# 31 "/home/gabriel/repos/qemu/include/migration/qemu-file.h"
typedef int (QEMUFilePutBufferFunc)(void *opaque, const uint8_t *buf,
                                    int64_t pos, int size);





typedef int (QEMUFileGetBufferFunc)(void *opaque, uint8_t *buf,
                                    int64_t pos, int size);
# 48 "/home/gabriel/repos/qemu/include/migration/qemu-file.h"
typedef int (QEMUFileCloseFunc)(void *opaque);



typedef int (QEMUFileGetFD)(void *opaque);




typedef ssize_t (QEMUFileWritevBufferFunc)(void *opaque, struct iovec *iov,
                                           int iovcnt, int64_t pos);

typedef struct QEMUFileOps {
    QEMUFilePutBufferFunc *put_buffer;
    QEMUFileGetBufferFunc *get_buffer;
    QEMUFileCloseFunc *close;
    QEMUFileGetFD *get_fd;
    QEMUFileWritevBufferFunc *writev_buffer;
} QEMUFileOps;

QEMUFile *qemu_fopen_ops(void *opaque, const QEMUFileOps *ops);
QEMUFile *qemu_fopen(const char *filename, const char *mode);
QEMUFile *qemu_fdopen(int fd, const char *mode);
QEMUFile *qemu_fopen_socket(int fd, const char *mode);
QEMUFile *qemu_popen_cmd(const char *command, const char *mode);
int qemu_get_fd(QEMUFile *f);
int qemu_fclose(QEMUFile *f);
int64_t qemu_ftell(QEMUFile *f);
void qemu_put_buffer(QEMUFile *f, const uint8_t *buf, int size);
void qemu_put_byte(QEMUFile *f, int v);




void qemu_put_buffer_async(QEMUFile *f, const uint8_t *buf, int size);

static __attribute__ (( always_inline )) __inline__ void qemu_put_ubyte(QEMUFile *f, unsigned int v)
{
    qemu_put_byte(f, (int)v);
}



void qemu_put_be16(QEMUFile *f, unsigned int v);
void qemu_put_be32(QEMUFile *f, unsigned int v);
void qemu_put_be64(QEMUFile *f, uint64_t v);
int qemu_get_buffer(QEMUFile *f, uint8_t *buf, int size);
int qemu_get_byte(QEMUFile *f);

static __attribute__ (( always_inline )) __inline__ unsigned int qemu_get_ubyte(QEMUFile *f)
{
    return (unsigned int)qemu_get_byte(f);
}



unsigned int qemu_get_be16(QEMUFile *f);
unsigned int qemu_get_be32(QEMUFile *f);
uint64_t qemu_get_be64(QEMUFile *f);

int qemu_file_rate_limit(QEMUFile *f);
void qemu_file_reset_rate_limit(QEMUFile *f);
void qemu_file_set_rate_limit(QEMUFile *f, int64_t new_rate);
int64_t qemu_file_get_rate_limit(QEMUFile *f);
int qemu_file_get_error(QEMUFile *f);

static __attribute__ (( always_inline )) __inline__ void qemu_put_be64s(QEMUFile *f, const uint64_t *pv)
{
    qemu_put_be64(f, *pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_be32s(QEMUFile *f, const uint32_t *pv)
{
    qemu_put_be32(f, *pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_be16s(QEMUFile *f, const uint16_t *pv)
{
    qemu_put_be16(f, *pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_8s(QEMUFile *f, const uint8_t *pv)
{
    qemu_put_byte(f, *pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_be64s(QEMUFile *f, uint64_t *pv)
{
    *pv = qemu_get_be64(f);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_be32s(QEMUFile *f, uint32_t *pv)
{
    *pv = qemu_get_be32(f);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_be16s(QEMUFile *f, uint16_t *pv)
{
    *pv = qemu_get_be16(f);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_8s(QEMUFile *f, uint8_t *pv)
{
    *pv = qemu_get_byte(f);
}


static __attribute__ (( always_inline )) __inline__ void qemu_put_sbuffer(QEMUFile *f, const int8_t *buf, int size)
{
    qemu_put_buffer(f, (const uint8_t *)buf, size);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe16(QEMUFile *f, int v)
{
    qemu_put_be16(f, (unsigned int)v);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe32(QEMUFile *f, int v)
{
    qemu_put_be32(f, (unsigned int)v);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe64(QEMUFile *f, int64_t v)
{
    qemu_put_be64(f, (uint64_t)v);
}

static __attribute__ (( always_inline )) __inline__ size_t qemu_get_sbuffer(QEMUFile *f, int8_t *buf, int size)
{
    return qemu_get_buffer(f, (uint8_t *)buf, size);
}

static __attribute__ (( always_inline )) __inline__ int qemu_get_sbe16(QEMUFile *f)
{
    return (int)qemu_get_be16(f);
}

static __attribute__ (( always_inline )) __inline__ int qemu_get_sbe32(QEMUFile *f)
{
    return (int)qemu_get_be32(f);
}

static __attribute__ (( always_inline )) __inline__ int64_t qemu_get_sbe64(QEMUFile *f)
{
    return (int64_t)qemu_get_be64(f);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_s8s(QEMUFile *f, const int8_t *pv)
{
    qemu_put_8s(f, (const uint8_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe16s(QEMUFile *f, const int16_t *pv)
{
    qemu_put_be16s(f, (const uint16_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe32s(QEMUFile *f, const int32_t *pv)
{
    qemu_put_be32s(f, (const uint32_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_put_sbe64s(QEMUFile *f, const int64_t *pv)
{
    qemu_put_be64s(f, (const uint64_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_s8s(QEMUFile *f, int8_t *pv)
{
    qemu_get_8s(f, (uint8_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_sbe16s(QEMUFile *f, int16_t *pv)
{
    qemu_get_be16s(f, (uint16_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_sbe32s(QEMUFile *f, int32_t *pv)
{
    qemu_get_be32s(f, (uint32_t *)pv);
}

static __attribute__ (( always_inline )) __inline__ void qemu_get_sbe64s(QEMUFile *f, int64_t *pv)
{
    qemu_get_be64s(f, (uint64_t *)pv);
}
# 15 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 1 "/home/gabriel/repos/qemu/include/migration/vmstate.h" 1
# 31 "/home/gabriel/repos/qemu/include/migration/vmstate.h"
typedef void SaveStateHandler(QEMUFile *f, void *opaque);
typedef int LoadStateHandler(QEMUFile *f, void *opaque, int version_id);

typedef struct SaveVMHandlers {

    void (*set_params)(const MigrationParams *params, void * opaque);
    SaveStateHandler *save_state;

    void (*cancel)(void *opaque);
    int (*save_live_complete)(QEMUFile *f, void *opaque);


    _Bool (*is_active)(void *opaque);






    int (*save_live_iterate)(QEMUFile *f, void *opaque);


    int (*save_live_setup)(QEMUFile *f, void *opaque);
    uint64_t (*save_live_pending)(QEMUFile *f, void *opaque, uint64_t max_size);

    LoadStateHandler *load_state;
} SaveVMHandlers;

int register_savevm(DeviceState *dev,
                    const char *idstr,
                    int instance_id,
                    int version_id,
                    SaveStateHandler *save_state,
                    LoadStateHandler *load_state,
                    void *opaque);

int register_savevm_live(DeviceState *dev,
                         const char *idstr,
                         int instance_id,
                         int version_id,
                         SaveVMHandlers *ops,
                         void *opaque);

void unregister_savevm(DeviceState *dev, const char *idstr, void *opaque);
void register_device_unmigratable(DeviceState *dev, const char *idstr,
                                                                void *opaque);


typedef struct VMStateInfo VMStateInfo;
typedef struct VMStateDescription VMStateDescription;

struct VMStateInfo {
    const char *name;
    int (*get)(QEMUFile *f, void *pv, size_t size);
    void (*put)(QEMUFile *f, void *pv, size_t size);
};

enum VMStateFlags {
    VMS_SINGLE = 0x001,
    VMS_POINTER = 0x002,
    VMS_ARRAY = 0x004,
    VMS_STRUCT = 0x008,
    VMS_VARRAY_INT32 = 0x010,
    VMS_BUFFER = 0x020,
    VMS_ARRAY_OF_POINTER = 0x040,
    VMS_VARRAY_UINT16 = 0x080,
    VMS_VBUFFER = 0x100,
    VMS_MULTIPLY = 0x200,
    VMS_VARRAY_UINT8 = 0x400,
    VMS_VARRAY_UINT32 = 0x800,
};

typedef struct {
    const char *name;
    size_t offset;
    size_t size;
    size_t start;
    int num;
    size_t num_offset;
    size_t size_offset;
    const VMStateInfo *info;
    enum VMStateFlags flags;
    const VMStateDescription *vmsd;
    int version_id;
    _Bool (*field_exists)(void *opaque, int version_id);
} VMStateField;

typedef struct VMStateSubsection {
    const VMStateDescription *vmsd;
    _Bool (*needed)(void *opaque);
} VMStateSubsection;

struct VMStateDescription {
    const char *name;
    int unmigratable;
    int version_id;
    int minimum_version_id;
    int minimum_version_id_old;
    LoadStateHandler *load_state_old;
    int (*pre_load)(void *opaque);
    int (*post_load)(void *opaque, int version_id);
    void (*pre_save)(void *opaque);
    VMStateField *fields;
    const VMStateSubsection *subsections;
};





extern const VMStateInfo vmstate_info_bool;

extern const VMStateInfo vmstate_info_int8;
extern const VMStateInfo vmstate_info_int16;
extern const VMStateInfo vmstate_info_int32;
extern const VMStateInfo vmstate_info_int64;

extern const VMStateInfo vmstate_info_uint8_equal;
extern const VMStateInfo vmstate_info_uint16_equal;
extern const VMStateInfo vmstate_info_int32_equal;
extern const VMStateInfo vmstate_info_uint32_equal;
extern const VMStateInfo vmstate_info_uint64_equal;
extern const VMStateInfo vmstate_info_int32_le;

extern const VMStateInfo vmstate_info_uint8;
extern const VMStateInfo vmstate_info_uint16;
extern const VMStateInfo vmstate_info_uint32;
extern const VMStateInfo vmstate_info_uint64;

extern const VMStateInfo vmstate_info_float64;

extern const VMStateInfo vmstate_info_timer;
extern const VMStateInfo vmstate_info_buffer;
extern const VMStateInfo vmstate_info_unused_buffer;
extern const VMStateInfo vmstate_info_bitmap;
# 712 "/home/gabriel/repos/qemu/include/migration/vmstate.h"
int vmstate_load_state(QEMUFile *f, const VMStateDescription *vmsd,
                       void *opaque, int version_id);
void vmstate_save_state(QEMUFile *f, const VMStateDescription *vmsd,
                        void *opaque);

int vmstate_register_with_alias_id(DeviceState *dev, int instance_id,
                                   const VMStateDescription *vmsd,
                                   void *base, int alias_id,
                                   int required_for_version);

static __attribute__ (( always_inline )) __inline__ int vmstate_register(DeviceState *dev, int instance_id,
                                   const VMStateDescription *vmsd,
                                   void *opaque)
{
    return vmstate_register_with_alias_id(dev, instance_id, vmsd,
                                          opaque, -1, 0);
}

void vmstate_unregister(DeviceState *dev, const VMStateDescription *vmsd,
                        void *opaque);

struct MemoryRegion;
void vmstate_register_ram(struct MemoryRegion *memory, DeviceState *dev);
void vmstate_unregister_ram(struct MemoryRegion *memory, DeviceState *dev);
void vmstate_register_ram_global(struct MemoryRegion *memory);
# 16 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/log.h" 1
# 10 "/home/gabriel/repos/qemu/include/qemu/log.h"
extern FILE *qemu_logfile;
extern int qemu_loglevel;
# 22 "/home/gabriel/repos/qemu/include/qemu/log.h"
static __attribute__ (( always_inline )) __inline__ _Bool qemu_log_enabled(void)
{
    return qemu_logfile != ((void *)0);
}
# 42 "/home/gabriel/repos/qemu/include/qemu/log.h"
static __attribute__ (( always_inline )) __inline__ _Bool qemu_loglevel_mask(int mask)
{
    return (qemu_loglevel & mask) != 0;
}





void __attribute__((format(gnu_printf, 1, 2))) qemu_log(const char *fmt, ...);



static __attribute__ (( always_inline )) __inline__ void __attribute__((format(gnu_printf, 1, 0)))
qemu_log_vprintf(const char *fmt, va_list va)
{
    if (qemu_logfile) {
        vfprintf(qemu_logfile, fmt, va);
    }
}



void __attribute__((format(gnu_printf, 2, 3))) qemu_log_mask(int mask, const char *fmt, ...);
# 111 "/home/gabriel/repos/qemu/include/qemu/log.h"
static __attribute__ (( always_inline )) __inline__ void qemu_log_flush(void)
{
    fflush(qemu_logfile);
}


static __attribute__ (( always_inline )) __inline__ void qemu_log_close(void)
{
    if (qemu_logfile) {
        if (qemu_logfile != stderr) {
            fclose(qemu_logfile);
        }
        qemu_logfile = ((void *)0);
    }
}


static __attribute__ (( always_inline )) __inline__ void qemu_log_set_file(FILE *f)
{
    qemu_logfile = f;
}


typedef struct QEMULogItem {
    int mask;
    const char *name;
    const char *help;
} QEMULogItem;

extern const QEMULogItem qemu_log_items[];





void do_qemu_set_log(int log_flags, _Bool use_own_buffers);

static __attribute__ (( always_inline )) __inline__ void qemu_set_log(int log_flags)
{



    do_qemu_set_log(log_flags, 0);

}

void qemu_set_log_filename(const char *filename);
int qemu_str_to_log_mask(const char *str);




void qemu_print_log_usage(FILE *f);
# 17 "/home/gabriel/repos/qemu/include/hw/hw.h" 2
# 40 "/home/gabriel/repos/qemu/include/hw/hw.h"
typedef void QEMUResetHandler(void *opaque);

void qemu_register_reset(QEMUResetHandler *func, void *opaque);
void qemu_unregister_reset(QEMUResetHandler *func, void *opaque);



typedef int QEMUBootSetHandler(void *opaque, const char *boot_devices);
void qemu_register_boot_set(QEMUBootSetHandler *func, void *opaque);
int qemu_boot_set(const char *boot_devices);
# 22 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 1




# 1 "/home/gabriel/repos/qemu/include/exec/memory.h" 1
# 27 "/home/gabriel/repos/qemu/include/exec/memory.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/int128.h" 1



typedef struct Int128 Int128;

struct Int128 {
    uint64_t lo;
    int64_t hi;
};

static __attribute__ (( always_inline )) __inline__ Int128 int128_make64(uint64_t a)
{
    return (Int128) { a, 0 };
}

static __attribute__ (( always_inline )) __inline__ uint64_t int128_get64(Int128 a)
{
    ((!a.hi) ? (void) (0) : __assert_fail ("!a.hi", "/home/gabriel/repos/qemu/include/qemu/int128.h", 18, __PRETTY_FUNCTION__));
    return a.lo;
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_zero(void)
{
    return int128_make64(0);
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_one(void)
{
    return int128_make64(1);
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_2_64(void)
{
    return (Int128) { 0, 1 };
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_add(Int128 a, Int128 b)
{
    Int128 r = { a.lo + b.lo, a.hi + b.hi };
    r.hi += (r.lo < a.lo) || (r.lo < b.lo);
    return r;
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_neg(Int128 a)
{
    a.lo = ~a.lo;
    a.hi = ~a.hi;
    return int128_add(a, int128_one());
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_sub(Int128 a, Int128 b)
{
    return int128_add(a, int128_neg(b));
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_nonneg(Int128 a)
{
    return a.hi >= 0;
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_eq(Int128 a, Int128 b)
{
    return a.lo == b.lo && a.hi == b.hi;
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_ne(Int128 a, Int128 b)
{
    return !int128_eq(a, b);
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_ge(Int128 a, Int128 b)
{
    return int128_nonneg(int128_sub(a, b));
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_lt(Int128 a, Int128 b)
{
    return !int128_ge(a, b);
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_le(Int128 a, Int128 b)
{
    return int128_ge(b, a);
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_gt(Int128 a, Int128 b)
{
    return !int128_le(a, b);
}

static __attribute__ (( always_inline )) __inline__ _Bool int128_nz(Int128 a)
{
    return a.lo || a.hi;
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_min(Int128 a, Int128 b)
{
    return int128_le(a, b) ? a : b;
}

static __attribute__ (( always_inline )) __inline__ Int128 int128_max(Int128 a, Int128 b)
{
    return int128_ge(a, b) ? a : b;
}

static __attribute__ (( always_inline )) __inline__ void int128_addto(Int128 *a, Int128 b)
{
    *a = int128_add(*a, b);
}

static __attribute__ (( always_inline )) __inline__ void int128_subfrom(Int128 *a, Int128 b)
{
    *a = int128_sub(*a, b);
}
# 28 "/home/gabriel/repos/qemu/include/exec/memory.h" 2

typedef struct MemoryRegionOps MemoryRegionOps;
typedef struct MemoryRegionPortio MemoryRegionPortio;
typedef struct MemoryRegionMmio MemoryRegionMmio;
# 40 "/home/gabriel/repos/qemu/include/exec/memory.h"
struct MemoryRegionMmio {
    CPUReadMemoryFunc *read[3];
    CPUWriteMemoryFunc *write[3];
};


typedef struct MemoryRegionIORange MemoryRegionIORange;
struct MemoryRegionIORange {
    IORange iorange;
    MemoryRegion *mr;
    hwaddr offset;
};




struct MemoryRegionOps {


    uint64_t (*read)(void *opaque,
                     hwaddr addr,
                     unsigned size);


    void (*write)(void *opaque,
                  hwaddr addr,
                  uint64_t data,
                  unsigned size);

    enum device_endian endianness;

    struct {



        unsigned min_access_size;
        unsigned max_access_size;



         _Bool unaligned;





        _Bool (*accepts)(void *opaque, hwaddr addr,
                        unsigned size, _Bool is_write);
    } valid;

    struct {



        unsigned min_access_size;



        unsigned max_access_size;



         _Bool unaligned;
    } impl;




    const MemoryRegionPortio *old_portio;



    const MemoryRegionMmio old_mmio;
};

typedef struct CoalescedMemoryRange CoalescedMemoryRange;
typedef struct MemoryRegionIoeventfd MemoryRegionIoeventfd;

struct MemoryRegion {

    const MemoryRegionOps *ops;
    void *opaque;
    MemoryRegion *parent;
    Int128 size;
    hwaddr addr;
    void (*destructor)(MemoryRegion *mr);
    ram_addr_t ram_addr;
    _Bool subpage;
    _Bool terminates;
    _Bool readable;
    _Bool ram;
    _Bool readonly;
    _Bool enabled;
    _Bool rom_device;
    _Bool warning_printed;
    _Bool flush_coalesced_mmio;
    MemoryRegion *alias;
    hwaddr alias_offset;
    unsigned priority;
    _Bool may_overlap;
    struct subregions { struct MemoryRegion *tqh_first; struct MemoryRegion * *tqh_last; } subregions;
    struct { struct MemoryRegion *tqe_next; struct MemoryRegion * *tqe_prev; } subregions_link;
    struct coalesced_ranges { struct CoalescedMemoryRange *tqh_first; struct CoalescedMemoryRange * *tqh_last; } coalesced;
    const char *name;
    uint8_t dirty_log_mask;
    unsigned ioeventfd_nb;
    MemoryRegionIoeventfd *ioeventfds;
};

struct MemoryRegionPortio {
    uint32_t offset;
    uint32_t len;
    unsigned size;
    IOPortReadFunc *read;
    IOPortWriteFunc *write;
};






struct AddressSpace {

    const char *name;
    MemoryRegion *root;
    struct FlatView *current_map;
    int ioeventfd_nb;
    struct MemoryRegionIoeventfd *ioeventfds;
    struct AddressSpaceDispatch *dispatch;
    struct { struct AddressSpace *tqe_next; struct AddressSpace * *tqe_prev; } address_spaces_link;
};
# 184 "/home/gabriel/repos/qemu/include/exec/memory.h"
struct MemoryRegionSection {
    MemoryRegion *mr;
    AddressSpace *address_space;
    hwaddr offset_within_region;
    uint64_t size;
    hwaddr offset_within_address_space;
    _Bool readonly;
};

typedef struct MemoryListener MemoryListener;







struct MemoryListener {
    void (*begin)(MemoryListener *listener);
    void (*commit)(MemoryListener *listener);
    void (*region_add)(MemoryListener *listener, MemoryRegionSection *section);
    void (*region_del)(MemoryListener *listener, MemoryRegionSection *section);
    void (*region_nop)(MemoryListener *listener, MemoryRegionSection *section);
    void (*log_start)(MemoryListener *listener, MemoryRegionSection *section);
    void (*log_stop)(MemoryListener *listener, MemoryRegionSection *section);
    void (*log_sync)(MemoryListener *listener, MemoryRegionSection *section);
    void (*log_global_start)(MemoryListener *listener);
    void (*log_global_stop)(MemoryListener *listener);
    void (*eventfd_add)(MemoryListener *listener, MemoryRegionSection *section,
                        _Bool match_data, uint64_t data, EventNotifier *e);
    void (*eventfd_del)(MemoryListener *listener, MemoryRegionSection *section,
                        _Bool match_data, uint64_t data, EventNotifier *e);
    void (*coalesced_mmio_add)(MemoryListener *listener, MemoryRegionSection *section,
                               hwaddr addr, hwaddr len);
    void (*coalesced_mmio_del)(MemoryListener *listener, MemoryRegionSection *section,
                               hwaddr addr, hwaddr len);

    unsigned priority;
    AddressSpace *address_space_filter;
    struct { struct MemoryListener *tqe_next; struct MemoryListener * *tqe_prev; } link;
};
# 236 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init(MemoryRegion *mr,
                        const char *name,
                        uint64_t size);
# 252 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_io(MemoryRegion *mr,
                           const MemoryRegionOps *ops,
                           void *opaque,
                           const char *name,
                           uint64_t size);
# 266 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_ram(MemoryRegion *mr,
                            const char *name,
                            uint64_t size);
# 280 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_ram_ptr(MemoryRegion *mr,
                                const char *name,
                                uint64_t size,
                                void *ptr);
# 296 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_alias(MemoryRegion *mr,
                              const char *name,
                              MemoryRegion *orig,
                              hwaddr offset,
                              uint64_t size);
# 311 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_rom_device(MemoryRegion *mr,
                                   const MemoryRegionOps *ops,
                                   void *opaque,
                                   const char *name,
                                   uint64_t size);
# 329 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_init_reservation(MemoryRegion *mr,
                                    const char *name,
                                    uint64_t size);







void memory_region_destroy(MemoryRegion *mr);






uint64_t memory_region_size(MemoryRegion *mr);
# 355 "/home/gabriel/repos/qemu/include/exec/memory.h"
_Bool memory_region_is_ram(MemoryRegion *mr);
# 365 "/home/gabriel/repos/qemu/include/exec/memory.h"
static __attribute__ (( always_inline )) __inline__ _Bool memory_region_is_romd(MemoryRegion *mr)
{
    return mr->rom_device && mr->readable;
}
# 377 "/home/gabriel/repos/qemu/include/exec/memory.h"
const char *memory_region_name(MemoryRegion *mr);
# 386 "/home/gabriel/repos/qemu/include/exec/memory.h"
_Bool memory_region_is_logging(MemoryRegion *mr);
# 395 "/home/gabriel/repos/qemu/include/exec/memory.h"
_Bool memory_region_is_rom(MemoryRegion *mr);
# 406 "/home/gabriel/repos/qemu/include/exec/memory.h"
void *memory_region_get_ram_ptr(MemoryRegion *mr);
# 419 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_log(MemoryRegion *mr, _Bool log, unsigned client);
# 435 "/home/gabriel/repos/qemu/include/exec/memory.h"
_Bool memory_region_get_dirty(MemoryRegion *mr, hwaddr addr,
                             hwaddr size, unsigned client);
# 448 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_dirty(MemoryRegion *mr, hwaddr addr,
                             hwaddr size);
# 465 "/home/gabriel/repos/qemu/include/exec/memory.h"
_Bool memory_region_test_and_clear_dirty(MemoryRegion *mr, hwaddr addr,
                                        hwaddr size, unsigned client);
# 476 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_sync_dirty_bitmap(MemoryRegion *mr);
# 490 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_reset_dirty(MemoryRegion *mr, hwaddr addr,
                               hwaddr size, unsigned client);
# 502 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_readonly(MemoryRegion *mr, _Bool readonly);
# 516 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_rom_device_set_readable(MemoryRegion *mr, _Bool readable);
# 527 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_coalescing(MemoryRegion *mr);
# 540 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_add_coalescing(MemoryRegion *mr,
                                  hwaddr offset,
                                  uint64_t size);
# 553 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_clear_coalescing(MemoryRegion *mr);
# 565 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_flush_coalesced(MemoryRegion *mr);
# 578 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_clear_flush_coalesced(MemoryRegion *mr);
# 596 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_add_eventfd(MemoryRegion *mr,
                               hwaddr addr,
                               unsigned size,
                               _Bool match_data,
                               uint64_t data,
                               EventNotifier *e);
# 616 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_del_eventfd(MemoryRegion *mr,
                               hwaddr addr,
                               unsigned size,
                               _Bool match_data,
                               uint64_t data,
                               EventNotifier *e);
# 637 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_add_subregion(MemoryRegion *mr,
                                 hwaddr offset,
                                 MemoryRegion *subregion);
# 657 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_add_subregion_overlap(MemoryRegion *mr,
                                         hwaddr offset,
                                         MemoryRegion *subregion,
                                         unsigned priority);
# 669 "/home/gabriel/repos/qemu/include/exec/memory.h"
ram_addr_t memory_region_get_ram_addr(MemoryRegion *mr);
# 679 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_del_subregion(MemoryRegion *mr,
                                 MemoryRegion *subregion);
# 695 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_enabled(MemoryRegion *mr, _Bool enabled);
# 706 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_address(MemoryRegion *mr, hwaddr addr);
# 717 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_region_set_alias_offset(MemoryRegion *mr,
                                    hwaddr offset);
# 738 "/home/gabriel/repos/qemu/include/exec/memory.h"
MemoryRegionSection memory_region_find(MemoryRegion *address_space,
                                       hwaddr addr, uint64_t size);
# 749 "/home/gabriel/repos/qemu/include/exec/memory.h"
static __attribute__ (( always_inline )) __inline__ hwaddr
memory_region_section_addr(MemoryRegionSection *section,
                           hwaddr addr)
{
    addr -= section->offset_within_address_space;
    addr += section->offset_within_region;
    return addr;
}
# 765 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_global_sync_dirty_bitmap(MemoryRegion *address_space);







void memory_region_transaction_begin(void);





void memory_region_transaction_commit(void);
# 789 "/home/gabriel/repos/qemu/include/exec/memory.h"
void memory_listener_register(MemoryListener *listener, AddressSpace *filter);






void memory_listener_unregister(MemoryListener *listener);




void memory_global_dirty_log_start(void);




void memory_global_dirty_log_stop(void);

void mtree_info(fprintf_function mon_printf, void *f);







void address_space_init(AddressSpace *as, MemoryRegion *root);
# 828 "/home/gabriel/repos/qemu/include/exec/memory.h"
void address_space_destroy(AddressSpace *as);
# 838 "/home/gabriel/repos/qemu/include/exec/memory.h"
void address_space_rw(AddressSpace *as, hwaddr addr, uint8_t *buf,
                      int len, _Bool is_write);
# 848 "/home/gabriel/repos/qemu/include/exec/memory.h"
void address_space_write(AddressSpace *as, hwaddr addr,
                         const uint8_t *buf, int len);
# 858 "/home/gabriel/repos/qemu/include/exec/memory.h"
void address_space_read(AddressSpace *as, hwaddr addr, uint8_t *buf, int len);
# 873 "/home/gabriel/repos/qemu/include/exec/memory.h"
void *address_space_map(AddressSpace *as, hwaddr addr,
                        hwaddr *plen, _Bool is_write);
# 887 "/home/gabriel/repos/qemu/include/exec/memory.h"
void address_space_unmap(AddressSpace *as, void *buffer, hwaddr len,
                         int is_write, hwaddr access_len);
# 6 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 2

# 1 "/home/gabriel/repos/qemu/include/hw/isa/isa.h" 1







# 1 "/home/gabriel/repos/qemu/include/hw/qdev.h" 1




# 1 "/home/gabriel/repos/qemu/include/hw/qdev-core.h" 1




# 1 "/home/gabriel/repos/qemu/include/qemu/option.h" 1
# 31 "/home/gabriel/repos/qemu/include/qemu/option.h"
# 1 "/home/gabriel/repos/qemu/include/qapi/error.h" 1
# 15 "/home/gabriel/repos/qemu/include/qapi/error.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/compiler.h" 1
# 16 "/home/gabriel/repos/qemu/include/qapi/error.h" 2
# 1 "./qapi-types.h" 1
# 23 "./qapi-types.h"
extern const char *ErrorClass_lookup[];
typedef enum ErrorClass
{
    ERROR_CLASS_GENERIC_ERROR = 0,
    ERROR_CLASS_COMMAND_NOT_FOUND = 1,
    ERROR_CLASS_DEVICE_ENCRYPTED = 2,
    ERROR_CLASS_DEVICE_NOT_ACTIVE = 3,
    ERROR_CLASS_DEVICE_NOT_FOUND = 4,
    ERROR_CLASS_K_V_M_MISSING_CAP = 5,
    ERROR_CLASS_MAX = 6,
} ErrorClass;

typedef struct ErrorClassList
{
    ErrorClass value;
    struct ErrorClassList *next;
} ErrorClassList;

typedef struct NameInfo NameInfo;

typedef struct NameInfoList
{
    NameInfo *value;
    struct NameInfoList *next;
} NameInfoList;

typedef struct VersionInfo VersionInfo;

typedef struct VersionInfoList
{
    VersionInfo *value;
    struct VersionInfoList *next;
} VersionInfoList;

typedef struct KvmInfo KvmInfo;

typedef struct KvmInfoList
{
    KvmInfo *value;
    struct KvmInfoList *next;
} KvmInfoList;

extern const char *RunState_lookup[];
typedef enum RunState
{
    RUN_STATE_DEBUG = 0,
    RUN_STATE_INMIGRATE = 1,
    RUN_STATE_INTERNAL_ERROR = 2,
    RUN_STATE_IO_ERROR = 3,
    RUN_STATE_PAUSED = 4,
    RUN_STATE_POSTMIGRATE = 5,
    RUN_STATE_PRELAUNCH = 6,
    RUN_STATE_FINISH_MIGRATE = 7,
    RUN_STATE_RESTORE_VM = 8,
    RUN_STATE_RUNNING = 9,
    RUN_STATE_SAVE_VM = 10,
    RUN_STATE_SHUTDOWN = 11,
    RUN_STATE_SUSPENDED = 12,
    RUN_STATE_WATCHDOG = 13,
    RUN_STATE_MAX = 14,
} RunState;

typedef struct RunStateList
{
    RunState value;
    struct RunStateList *next;
} RunStateList;

typedef struct SnapshotInfo SnapshotInfo;

typedef struct SnapshotInfoList
{
    SnapshotInfo *value;
    struct SnapshotInfoList *next;
} SnapshotInfoList;

typedef struct ImageInfo ImageInfo;

typedef struct ImageInfoList
{
    ImageInfo *value;
    struct ImageInfoList *next;
} ImageInfoList;

typedef struct ImageCheck ImageCheck;

typedef struct ImageCheckList
{
    ImageCheck *value;
    struct ImageCheckList *next;
} ImageCheckList;

typedef struct StatusInfo StatusInfo;

typedef struct StatusInfoList
{
    StatusInfo *value;
    struct StatusInfoList *next;
} StatusInfoList;

typedef struct UuidInfo UuidInfo;

typedef struct UuidInfoList
{
    UuidInfo *value;
    struct UuidInfoList *next;
} UuidInfoList;

typedef struct ChardevInfo ChardevInfo;

typedef struct ChardevInfoList
{
    ChardevInfo *value;
    struct ChardevInfoList *next;
} ChardevInfoList;

extern const char *DataFormat_lookup[];
typedef enum DataFormat
{
    DATA_FORMAT_UTF8 = 0,
    DATA_FORMAT_BASE64 = 1,
    DATA_FORMAT_MAX = 2,
} DataFormat;

typedef struct DataFormatList
{
    DataFormat value;
    struct DataFormatList *next;
} DataFormatList;

typedef struct CommandInfo CommandInfo;

typedef struct CommandInfoList
{
    CommandInfo *value;
    struct CommandInfoList *next;
} CommandInfoList;

typedef struct EventInfo EventInfo;

typedef struct EventInfoList
{
    EventInfo *value;
    struct EventInfoList *next;
} EventInfoList;

typedef struct MigrationStats MigrationStats;

typedef struct MigrationStatsList
{
    MigrationStats *value;
    struct MigrationStatsList *next;
} MigrationStatsList;

typedef struct XBZRLECacheStats XBZRLECacheStats;

typedef struct XBZRLECacheStatsList
{
    XBZRLECacheStats *value;
    struct XBZRLECacheStatsList *next;
} XBZRLECacheStatsList;

typedef struct MigrationInfo MigrationInfo;

typedef struct MigrationInfoList
{
    MigrationInfo *value;
    struct MigrationInfoList *next;
} MigrationInfoList;

extern const char *MigrationCapability_lookup[];
typedef enum MigrationCapability
{
    MIGRATION_CAPABILITY_XBZRLE = 0,
    MIGRATION_CAPABILITY_MAX = 1,
} MigrationCapability;

typedef struct MigrationCapabilityList
{
    MigrationCapability value;
    struct MigrationCapabilityList *next;
} MigrationCapabilityList;

typedef struct MigrationCapabilityStatus MigrationCapabilityStatus;

typedef struct MigrationCapabilityStatusList
{
    MigrationCapabilityStatus *value;
    struct MigrationCapabilityStatusList *next;
} MigrationCapabilityStatusList;

typedef struct MouseInfo MouseInfo;

typedef struct MouseInfoList
{
    MouseInfo *value;
    struct MouseInfoList *next;
} MouseInfoList;

typedef struct CpuInfo CpuInfo;

typedef struct CpuInfoList
{
    CpuInfo *value;
    struct CpuInfoList *next;
} CpuInfoList;

typedef struct BlockDeviceInfo BlockDeviceInfo;

typedef struct BlockDeviceInfoList
{
    BlockDeviceInfo *value;
    struct BlockDeviceInfoList *next;
} BlockDeviceInfoList;

extern const char *BlockDeviceIoStatus_lookup[];
typedef enum BlockDeviceIoStatus
{
    BLOCK_DEVICE_IO_STATUS_OK = 0,
    BLOCK_DEVICE_IO_STATUS_FAILED = 1,
    BLOCK_DEVICE_IO_STATUS_NOSPACE = 2,
    BLOCK_DEVICE_IO_STATUS_MAX = 3,
} BlockDeviceIoStatus;

typedef struct BlockDeviceIoStatusList
{
    BlockDeviceIoStatus value;
    struct BlockDeviceIoStatusList *next;
} BlockDeviceIoStatusList;

typedef struct BlockDirtyInfo BlockDirtyInfo;

typedef struct BlockDirtyInfoList
{
    BlockDirtyInfo *value;
    struct BlockDirtyInfoList *next;
} BlockDirtyInfoList;

typedef struct BlockInfo BlockInfo;

typedef struct BlockInfoList
{
    BlockInfo *value;
    struct BlockInfoList *next;
} BlockInfoList;

typedef struct BlockDeviceStats BlockDeviceStats;

typedef struct BlockDeviceStatsList
{
    BlockDeviceStats *value;
    struct BlockDeviceStatsList *next;
} BlockDeviceStatsList;

typedef struct BlockStats BlockStats;

typedef struct BlockStatsList
{
    BlockStats *value;
    struct BlockStatsList *next;
} BlockStatsList;

typedef struct VncClientInfo VncClientInfo;

typedef struct VncClientInfoList
{
    VncClientInfo *value;
    struct VncClientInfoList *next;
} VncClientInfoList;

typedef struct VncInfo VncInfo;

typedef struct VncInfoList
{
    VncInfo *value;
    struct VncInfoList *next;
} VncInfoList;

typedef struct SpiceChannel SpiceChannel;

typedef struct SpiceChannelList
{
    SpiceChannel *value;
    struct SpiceChannelList *next;
} SpiceChannelList;

extern const char *SpiceQueryMouseMode_lookup[];
typedef enum SpiceQueryMouseMode
{
    SPICE_QUERY_MOUSE_MODE_CLIENT = 0,
    SPICE_QUERY_MOUSE_MODE_SERVER = 1,
    SPICE_QUERY_MOUSE_MODE_UNKNOWN = 2,
    SPICE_QUERY_MOUSE_MODE_MAX = 3,
} SpiceQueryMouseMode;

typedef struct SpiceQueryMouseModeList
{
    SpiceQueryMouseMode value;
    struct SpiceQueryMouseModeList *next;
} SpiceQueryMouseModeList;

typedef struct SpiceInfo SpiceInfo;

typedef struct SpiceInfoList
{
    SpiceInfo *value;
    struct SpiceInfoList *next;
} SpiceInfoList;

typedef struct BalloonInfo BalloonInfo;

typedef struct BalloonInfoList
{
    BalloonInfo *value;
    struct BalloonInfoList *next;
} BalloonInfoList;

typedef struct PciMemoryRange PciMemoryRange;

typedef struct PciMemoryRangeList
{
    PciMemoryRange *value;
    struct PciMemoryRangeList *next;
} PciMemoryRangeList;

typedef struct PciMemoryRegion PciMemoryRegion;

typedef struct PciMemoryRegionList
{
    PciMemoryRegion *value;
    struct PciMemoryRegionList *next;
} PciMemoryRegionList;

typedef struct PciBridgeInfo PciBridgeInfo;

typedef struct PciBridgeInfoList
{
    PciBridgeInfo *value;
    struct PciBridgeInfoList *next;
} PciBridgeInfoList;

typedef struct PciDeviceInfo PciDeviceInfo;

typedef struct PciDeviceInfoList
{
    PciDeviceInfo *value;
    struct PciDeviceInfoList *next;
} PciDeviceInfoList;

typedef struct PciInfo PciInfo;

typedef struct PciInfoList
{
    PciInfo *value;
    struct PciInfoList *next;
} PciInfoList;

extern const char *BlockdevOnError_lookup[];
typedef enum BlockdevOnError
{
    BLOCKDEV_ON_ERROR_REPORT = 0,
    BLOCKDEV_ON_ERROR_IGNORE = 1,
    BLOCKDEV_ON_ERROR_ENOSPC = 2,
    BLOCKDEV_ON_ERROR_STOP = 3,
    BLOCKDEV_ON_ERROR_MAX = 4,
} BlockdevOnError;

typedef struct BlockdevOnErrorList
{
    BlockdevOnError value;
    struct BlockdevOnErrorList *next;
} BlockdevOnErrorList;

extern const char *MirrorSyncMode_lookup[];
typedef enum MirrorSyncMode
{
    MIRROR_SYNC_MODE_TOP = 0,
    MIRROR_SYNC_MODE_FULL = 1,
    MIRROR_SYNC_MODE_NONE = 2,
    MIRROR_SYNC_MODE_MAX = 3,
} MirrorSyncMode;

typedef struct MirrorSyncModeList
{
    MirrorSyncMode value;
    struct MirrorSyncModeList *next;
} MirrorSyncModeList;

typedef struct BlockJobInfo BlockJobInfo;

typedef struct BlockJobInfoList
{
    BlockJobInfo *value;
    struct BlockJobInfoList *next;
} BlockJobInfoList;

extern const char *NewImageMode_lookup[];
typedef enum NewImageMode
{
    NEW_IMAGE_MODE_EXISTING = 0,
    NEW_IMAGE_MODE_ABSOLUTE_PATHS = 1,
    NEW_IMAGE_MODE_MAX = 2,
} NewImageMode;

typedef struct NewImageModeList
{
    NewImageMode value;
    struct NewImageModeList *next;
} NewImageModeList;

typedef struct BlockdevSnapshot BlockdevSnapshot;

typedef struct BlockdevSnapshotList
{
    BlockdevSnapshot *value;
    struct BlockdevSnapshotList *next;
} BlockdevSnapshotList;

typedef struct BlockdevAction BlockdevAction;

typedef struct BlockdevActionList
{
    BlockdevAction *value;
    struct BlockdevActionList *next;
} BlockdevActionList;

extern const char *BlockdevActionKind_lookup[];
typedef enum BlockdevActionKind
{
    BLOCKDEV_ACTION_KIND_BLOCKDEV_SNAPSHOT_SYNC = 0,
    BLOCKDEV_ACTION_KIND_MAX = 1,
} BlockdevActionKind;

typedef struct ObjectPropertyInfo ObjectPropertyInfo;

typedef struct ObjectPropertyInfoList
{
    ObjectPropertyInfo *value;
    struct ObjectPropertyInfoList *next;
} ObjectPropertyInfoList;

typedef struct ObjectTypeInfo ObjectTypeInfo;

typedef struct ObjectTypeInfoList
{
    ObjectTypeInfo *value;
    struct ObjectTypeInfoList *next;
} ObjectTypeInfoList;

typedef struct DevicePropertyInfo DevicePropertyInfo;

typedef struct DevicePropertyInfoList
{
    DevicePropertyInfo *value;
    struct DevicePropertyInfoList *next;
} DevicePropertyInfoList;

typedef struct NetdevNoneOptions NetdevNoneOptions;

typedef struct NetdevNoneOptionsList
{
    NetdevNoneOptions *value;
    struct NetdevNoneOptionsList *next;
} NetdevNoneOptionsList;

typedef struct NetLegacyNicOptions NetLegacyNicOptions;

typedef struct NetLegacyNicOptionsList
{
    NetLegacyNicOptions *value;
    struct NetLegacyNicOptionsList *next;
} NetLegacyNicOptionsList;

typedef struct String String;

typedef struct StringList
{
    String *value;
    struct StringList *next;
} StringList;

typedef struct NetdevUserOptions NetdevUserOptions;

typedef struct NetdevUserOptionsList
{
    NetdevUserOptions *value;
    struct NetdevUserOptionsList *next;
} NetdevUserOptionsList;

typedef struct NetdevTapOptions NetdevTapOptions;

typedef struct NetdevTapOptionsList
{
    NetdevTapOptions *value;
    struct NetdevTapOptionsList *next;
} NetdevTapOptionsList;

typedef struct NetdevSocketOptions NetdevSocketOptions;

typedef struct NetdevSocketOptionsList
{
    NetdevSocketOptions *value;
    struct NetdevSocketOptionsList *next;
} NetdevSocketOptionsList;

typedef struct NetdevVdeOptions NetdevVdeOptions;

typedef struct NetdevVdeOptionsList
{
    NetdevVdeOptions *value;
    struct NetdevVdeOptionsList *next;
} NetdevVdeOptionsList;

typedef struct NetdevDumpOptions NetdevDumpOptions;

typedef struct NetdevDumpOptionsList
{
    NetdevDumpOptions *value;
    struct NetdevDumpOptionsList *next;
} NetdevDumpOptionsList;

typedef struct NetdevBridgeOptions NetdevBridgeOptions;

typedef struct NetdevBridgeOptionsList
{
    NetdevBridgeOptions *value;
    struct NetdevBridgeOptionsList *next;
} NetdevBridgeOptionsList;

typedef struct NetdevHubPortOptions NetdevHubPortOptions;

typedef struct NetdevHubPortOptionsList
{
    NetdevHubPortOptions *value;
    struct NetdevHubPortOptionsList *next;
} NetdevHubPortOptionsList;

typedef struct NetClientOptions NetClientOptions;

typedef struct NetClientOptionsList
{
    NetClientOptions *value;
    struct NetClientOptionsList *next;
} NetClientOptionsList;

extern const char *NetClientOptionsKind_lookup[];
typedef enum NetClientOptionsKind
{
    NET_CLIENT_OPTIONS_KIND_NONE = 0,
    NET_CLIENT_OPTIONS_KIND_NIC = 1,
    NET_CLIENT_OPTIONS_KIND_USER = 2,
    NET_CLIENT_OPTIONS_KIND_TAP = 3,
    NET_CLIENT_OPTIONS_KIND_SOCKET = 4,
    NET_CLIENT_OPTIONS_KIND_VDE = 5,
    NET_CLIENT_OPTIONS_KIND_DUMP = 6,
    NET_CLIENT_OPTIONS_KIND_BRIDGE = 7,
    NET_CLIENT_OPTIONS_KIND_HUBPORT = 8,
    NET_CLIENT_OPTIONS_KIND_MAX = 9,
} NetClientOptionsKind;

typedef struct NetLegacy NetLegacy;

typedef struct NetLegacyList
{
    NetLegacy *value;
    struct NetLegacyList *next;
} NetLegacyList;

typedef struct Netdev Netdev;

typedef struct NetdevList
{
    Netdev *value;
    struct NetdevList *next;
} NetdevList;

typedef struct InetSocketAddress InetSocketAddress;

typedef struct InetSocketAddressList
{
    InetSocketAddress *value;
    struct InetSocketAddressList *next;
} InetSocketAddressList;

typedef struct UnixSocketAddress UnixSocketAddress;

typedef struct UnixSocketAddressList
{
    UnixSocketAddress *value;
    struct UnixSocketAddressList *next;
} UnixSocketAddressList;

typedef struct SocketAddress SocketAddress;

typedef struct SocketAddressList
{
    SocketAddress *value;
    struct SocketAddressList *next;
} SocketAddressList;

extern const char *SocketAddressKind_lookup[];
typedef enum SocketAddressKind
{
    SOCKET_ADDRESS_KIND_INET = 0,
    SOCKET_ADDRESS_KIND_UNIX = 1,
    SOCKET_ADDRESS_KIND_FD = 2,
    SOCKET_ADDRESS_KIND_MAX = 3,
} SocketAddressKind;

typedef struct MachineInfo MachineInfo;

typedef struct MachineInfoList
{
    MachineInfo *value;
    struct MachineInfoList *next;
} MachineInfoList;

typedef struct CpuDefinitionInfo CpuDefinitionInfo;

typedef struct CpuDefinitionInfoList
{
    CpuDefinitionInfo *value;
    struct CpuDefinitionInfoList *next;
} CpuDefinitionInfoList;

typedef struct AddfdInfo AddfdInfo;

typedef struct AddfdInfoList
{
    AddfdInfo *value;
    struct AddfdInfoList *next;
} AddfdInfoList;

typedef struct FdsetFdInfo FdsetFdInfo;

typedef struct FdsetFdInfoList
{
    FdsetFdInfo *value;
    struct FdsetFdInfoList *next;
} FdsetFdInfoList;

typedef struct FdsetInfo FdsetInfo;

typedef struct FdsetInfoList
{
    FdsetInfo *value;
    struct FdsetInfoList *next;
} FdsetInfoList;

extern const char *TargetType_lookup[];
typedef enum TargetType
{
    TARGET_TYPE_ALPHA = 0,
    TARGET_TYPE_ARM = 1,
    TARGET_TYPE_CRIS = 2,
    TARGET_TYPE_I386 = 3,
    TARGET_TYPE_LM32 = 4,
    TARGET_TYPE_M68K = 5,
    TARGET_TYPE_MICROBLAZEEL = 6,
    TARGET_TYPE_MICROBLAZE = 7,
    TARGET_TYPE_MIPS64EL = 8,
    TARGET_TYPE_MIPS64 = 9,
    TARGET_TYPE_MIPSEL = 10,
    TARGET_TYPE_MIPS = 11,
    TARGET_TYPE_MOXIE = 12,
    TARGET_TYPE_OR32 = 13,
    TARGET_TYPE_PPC64 = 14,
    TARGET_TYPE_PPCEMB = 15,
    TARGET_TYPE_PPC = 16,
    TARGET_TYPE_S390X = 17,
    TARGET_TYPE_SH4EB = 18,
    TARGET_TYPE_SH4 = 19,
    TARGET_TYPE_SPARC64 = 20,
    TARGET_TYPE_SPARC = 21,
    TARGET_TYPE_UNICORE32 = 22,
    TARGET_TYPE_X86_64 = 23,
    TARGET_TYPE_XTENSAEB = 24,
    TARGET_TYPE_XTENSA = 25,
    TARGET_TYPE_MAX = 26,
} TargetType;

typedef struct TargetTypeList
{
    TargetType value;
    struct TargetTypeList *next;
} TargetTypeList;

typedef struct TargetInfo TargetInfo;

typedef struct TargetInfoList
{
    TargetInfo *value;
    struct TargetInfoList *next;
} TargetInfoList;

extern const char *QKeyCode_lookup[];
typedef enum QKeyCode
{
    Q_KEY_CODE_SHIFT = 0,
    Q_KEY_CODE_SHIFT_R = 1,
    Q_KEY_CODE_ALT = 2,
    Q_KEY_CODE_ALT_R = 3,
    Q_KEY_CODE_ALTGR = 4,
    Q_KEY_CODE_ALTGR_R = 5,
    Q_KEY_CODE_CTRL = 6,
    Q_KEY_CODE_CTRL_R = 7,
    Q_KEY_CODE_MENU = 8,
    Q_KEY_CODE_ESC = 9,
    Q_KEY_CODE_1 = 10,
    Q_KEY_CODE_2 = 11,
    Q_KEY_CODE_3 = 12,
    Q_KEY_CODE_4 = 13,
    Q_KEY_CODE_5 = 14,
    Q_KEY_CODE_6 = 15,
    Q_KEY_CODE_7 = 16,
    Q_KEY_CODE_8 = 17,
    Q_KEY_CODE_9 = 18,
    Q_KEY_CODE_0 = 19,
    Q_KEY_CODE_MINUS = 20,
    Q_KEY_CODE_EQUAL = 21,
    Q_KEY_CODE_BACKSPACE = 22,
    Q_KEY_CODE_TAB = 23,
    Q_KEY_CODE_Q = 24,
    Q_KEY_CODE_W = 25,
    Q_KEY_CODE_E = 26,
    Q_KEY_CODE_R = 27,
    Q_KEY_CODE_T = 28,
    Q_KEY_CODE_Y = 29,
    Q_KEY_CODE_U = 30,
    Q_KEY_CODE_I = 31,
    Q_KEY_CODE_O = 32,
    Q_KEY_CODE_P = 33,
    Q_KEY_CODE_BRACKET_LEFT = 34,
    Q_KEY_CODE_BRACKET_RIGHT = 35,
    Q_KEY_CODE_RET = 36,
    Q_KEY_CODE_A = 37,
    Q_KEY_CODE_S = 38,
    Q_KEY_CODE_D = 39,
    Q_KEY_CODE_F = 40,
    Q_KEY_CODE_G = 41,
    Q_KEY_CODE_H = 42,
    Q_KEY_CODE_J = 43,
    Q_KEY_CODE_K = 44,
    Q_KEY_CODE_L = 45,
    Q_KEY_CODE_SEMICOLON = 46,
    Q_KEY_CODE_APOSTROPHE = 47,
    Q_KEY_CODE_GRAVE_ACCENT = 48,
    Q_KEY_CODE_BACKSLASH = 49,
    Q_KEY_CODE_Z = 50,
    Q_KEY_CODE_X = 51,
    Q_KEY_CODE_C = 52,
    Q_KEY_CODE_V = 53,
    Q_KEY_CODE_B = 54,
    Q_KEY_CODE_N = 55,
    Q_KEY_CODE_M = 56,
    Q_KEY_CODE_COMMA = 57,
    Q_KEY_CODE_DOT = 58,
    Q_KEY_CODE_SLASH = 59,
    Q_KEY_CODE_ASTERISK = 60,
    Q_KEY_CODE_SPC = 61,
    Q_KEY_CODE_CAPS_LOCK = 62,
    Q_KEY_CODE_F1 = 63,
    Q_KEY_CODE_F2 = 64,
    Q_KEY_CODE_F3 = 65,
    Q_KEY_CODE_F4 = 66,
    Q_KEY_CODE_F5 = 67,
    Q_KEY_CODE_F6 = 68,
    Q_KEY_CODE_F7 = 69,
    Q_KEY_CODE_F8 = 70,
    Q_KEY_CODE_F9 = 71,
    Q_KEY_CODE_F10 = 72,
    Q_KEY_CODE_NUM_LOCK = 73,
    Q_KEY_CODE_SCROLL_LOCK = 74,
    Q_KEY_CODE_KP_DIVIDE = 75,
    Q_KEY_CODE_KP_MULTIPLY = 76,
    Q_KEY_CODE_KP_SUBTRACT = 77,
    Q_KEY_CODE_KP_ADD = 78,
    Q_KEY_CODE_KP_ENTER = 79,
    Q_KEY_CODE_KP_DECIMAL = 80,
    Q_KEY_CODE_SYSRQ = 81,
    Q_KEY_CODE_KP_0 = 82,
    Q_KEY_CODE_KP_1 = 83,
    Q_KEY_CODE_KP_2 = 84,
    Q_KEY_CODE_KP_3 = 85,
    Q_KEY_CODE_KP_4 = 86,
    Q_KEY_CODE_KP_5 = 87,
    Q_KEY_CODE_KP_6 = 88,
    Q_KEY_CODE_KP_7 = 89,
    Q_KEY_CODE_KP_8 = 90,
    Q_KEY_CODE_KP_9 = 91,
    Q_KEY_CODE_LESS = 92,
    Q_KEY_CODE_F11 = 93,
    Q_KEY_CODE_F12 = 94,
    Q_KEY_CODE_PRINT = 95,
    Q_KEY_CODE_HOME = 96,
    Q_KEY_CODE_PGUP = 97,
    Q_KEY_CODE_PGDN = 98,
    Q_KEY_CODE_END = 99,
    Q_KEY_CODE_LEFT = 100,
    Q_KEY_CODE_UP = 101,
    Q_KEY_CODE_DOWN = 102,
    Q_KEY_CODE_RIGHT = 103,
    Q_KEY_CODE_INSERT = 104,
    Q_KEY_CODE_DELETE = 105,
    Q_KEY_CODE_STOP = 106,
    Q_KEY_CODE_AGAIN = 107,
    Q_KEY_CODE_PROPS = 108,
    Q_KEY_CODE_UNDO = 109,
    Q_KEY_CODE_FRONT = 110,
    Q_KEY_CODE_COPY = 111,
    Q_KEY_CODE_OPEN = 112,
    Q_KEY_CODE_PASTE = 113,
    Q_KEY_CODE_FIND = 114,
    Q_KEY_CODE_CUT = 115,
    Q_KEY_CODE_LF = 116,
    Q_KEY_CODE_HELP = 117,
    Q_KEY_CODE_META_L = 118,
    Q_KEY_CODE_META_R = 119,
    Q_KEY_CODE_COMPOSE = 120,
    Q_KEY_CODE_MAX = 121,
} QKeyCode;

typedef struct QKeyCodeList
{
    QKeyCode value;
    struct QKeyCodeList *next;
} QKeyCodeList;

typedef struct KeyValue KeyValue;

typedef struct KeyValueList
{
    KeyValue *value;
    struct KeyValueList *next;
} KeyValueList;

extern const char *KeyValueKind_lookup[];
typedef enum KeyValueKind
{
    KEY_VALUE_KIND_NUMBER = 0,
    KEY_VALUE_KIND_QCODE = 1,
    KEY_VALUE_KIND_MAX = 2,
} KeyValueKind;

typedef struct ChardevFile ChardevFile;

typedef struct ChardevFileList
{
    ChardevFile *value;
    struct ChardevFileList *next;
} ChardevFileList;

typedef struct ChardevHostdev ChardevHostdev;

typedef struct ChardevHostdevList
{
    ChardevHostdev *value;
    struct ChardevHostdevList *next;
} ChardevHostdevList;

typedef struct ChardevSocket ChardevSocket;

typedef struct ChardevSocketList
{
    ChardevSocket *value;
    struct ChardevSocketList *next;
} ChardevSocketList;

typedef struct ChardevDgram ChardevDgram;

typedef struct ChardevDgramList
{
    ChardevDgram *value;
    struct ChardevDgramList *next;
} ChardevDgramList;

typedef struct ChardevMux ChardevMux;

typedef struct ChardevMuxList
{
    ChardevMux *value;
    struct ChardevMuxList *next;
} ChardevMuxList;

typedef struct ChardevStdio ChardevStdio;

typedef struct ChardevStdioList
{
    ChardevStdio *value;
    struct ChardevStdioList *next;
} ChardevStdioList;

typedef struct ChardevSpiceChannel ChardevSpiceChannel;

typedef struct ChardevSpiceChannelList
{
    ChardevSpiceChannel *value;
    struct ChardevSpiceChannelList *next;
} ChardevSpiceChannelList;

typedef struct ChardevSpicePort ChardevSpicePort;

typedef struct ChardevSpicePortList
{
    ChardevSpicePort *value;
    struct ChardevSpicePortList *next;
} ChardevSpicePortList;

typedef struct ChardevVC ChardevVC;

typedef struct ChardevVCList
{
    ChardevVC *value;
    struct ChardevVCList *next;
} ChardevVCList;

typedef struct ChardevRingbuf ChardevRingbuf;

typedef struct ChardevRingbufList
{
    ChardevRingbuf *value;
    struct ChardevRingbufList *next;
} ChardevRingbufList;

typedef struct ChardevDummy ChardevDummy;

typedef struct ChardevDummyList
{
    ChardevDummy *value;
    struct ChardevDummyList *next;
} ChardevDummyList;

typedef struct ChardevBackend ChardevBackend;

typedef struct ChardevBackendList
{
    ChardevBackend *value;
    struct ChardevBackendList *next;
} ChardevBackendList;

extern const char *ChardevBackendKind_lookup[];
typedef enum ChardevBackendKind
{
    CHARDEV_BACKEND_KIND_FILE = 0,
    CHARDEV_BACKEND_KIND_SERIAL = 1,
    CHARDEV_BACKEND_KIND_PARALLEL = 2,
    CHARDEV_BACKEND_KIND_PIPE = 3,
    CHARDEV_BACKEND_KIND_SOCKET = 4,
    CHARDEV_BACKEND_KIND_DGRAM = 5,
    CHARDEV_BACKEND_KIND_PTY = 6,
    CHARDEV_BACKEND_KIND_NULL = 7,
    CHARDEV_BACKEND_KIND_MUX = 8,
    CHARDEV_BACKEND_KIND_MSMOUSE = 9,
    CHARDEV_BACKEND_KIND_BRAILLE = 10,
    CHARDEV_BACKEND_KIND_STDIO = 11,
    CHARDEV_BACKEND_KIND_CONSOLE = 12,
    CHARDEV_BACKEND_KIND_SPICEVMC = 13,
    CHARDEV_BACKEND_KIND_SPICEPORT = 14,
    CHARDEV_BACKEND_KIND_VC = 15,
    CHARDEV_BACKEND_KIND_MEMORY = 16,
    CHARDEV_BACKEND_KIND_MAX = 17,
} ChardevBackendKind;

typedef struct ChardevReturn ChardevReturn;

typedef struct ChardevReturnList
{
    ChardevReturn *value;
    struct ChardevReturnList *next;
} ChardevReturnList;

extern const char *TpmModel_lookup[];
typedef enum TpmModel
{
    TPM_MODEL_TPM_TIS = 0,
    TPM_MODEL_MAX = 1,
} TpmModel;

typedef struct TpmModelList
{
    TpmModel value;
    struct TpmModelList *next;
} TpmModelList;

extern const char *TpmType_lookup[];
typedef enum TpmType
{
    TPM_TYPE_PASSTHROUGH = 0,
    TPM_TYPE_MAX = 1,
} TpmType;

typedef struct TpmTypeList
{
    TpmType value;
    struct TpmTypeList *next;
} TpmTypeList;

typedef struct TPMPassthroughOptions TPMPassthroughOptions;

typedef struct TPMPassthroughOptionsList
{
    TPMPassthroughOptions *value;
    struct TPMPassthroughOptionsList *next;
} TPMPassthroughOptionsList;

typedef struct TpmTypeOptions TpmTypeOptions;

typedef struct TpmTypeOptionsList
{
    TpmTypeOptions *value;
    struct TpmTypeOptionsList *next;
} TpmTypeOptionsList;

extern const char *TpmTypeOptionsKind_lookup[];
typedef enum TpmTypeOptionsKind
{
    TPM_TYPE_OPTIONS_KIND_PASSTHROUGH = 0,
    TPM_TYPE_OPTIONS_KIND_MAX = 1,
} TpmTypeOptionsKind;

typedef struct TPMInfo TPMInfo;

typedef struct TPMInfoList
{
    TPMInfo *value;
    struct TPMInfoList *next;
} TPMInfoList;

typedef struct AcpiTableOptions AcpiTableOptions;

typedef struct AcpiTableOptionsList
{
    AcpiTableOptions *value;
    struct AcpiTableOptionsList *next;
} AcpiTableOptionsList;

void qapi_free_ErrorClassList(ErrorClassList * obj);

struct NameInfo
{
    _Bool has_name;
    char * name;
};

void qapi_free_NameInfoList(NameInfoList * obj);
void qapi_free_NameInfo(NameInfo * obj);

struct VersionInfo
{
    struct
    {
        int64_t major;
        int64_t minor;
        int64_t micro;
    } qemu;
    char * package;
};

void qapi_free_VersionInfoList(VersionInfoList * obj);
void qapi_free_VersionInfo(VersionInfo * obj);

struct KvmInfo
{
    _Bool enabled;
    _Bool present;
};

void qapi_free_KvmInfoList(KvmInfoList * obj);
void qapi_free_KvmInfo(KvmInfo * obj);

void qapi_free_RunStateList(RunStateList * obj);

struct SnapshotInfo
{
    char * id;
    char * name;
    int64_t vm_state_size;
    int64_t date_sec;
    int64_t date_nsec;
    int64_t vm_clock_sec;
    int64_t vm_clock_nsec;
};

void qapi_free_SnapshotInfoList(SnapshotInfoList * obj);
void qapi_free_SnapshotInfo(SnapshotInfo * obj);

struct ImageInfo
{
    char * filename;
    char * format;
    _Bool has_dirty_flag;
    _Bool dirty_flag;
    _Bool has_actual_size;
    int64_t actual_size;
    int64_t virtual_size;
    _Bool has_cluster_size;
    int64_t cluster_size;
    _Bool has_encrypted;
    _Bool encrypted;
    _Bool has_backing_filename;
    char * backing_filename;
    _Bool has_full_backing_filename;
    char * full_backing_filename;
    _Bool has_backing_filename_format;
    char * backing_filename_format;
    _Bool has_snapshots;
    SnapshotInfoList * snapshots;
};

void qapi_free_ImageInfoList(ImageInfoList * obj);
void qapi_free_ImageInfo(ImageInfo * obj);

struct ImageCheck
{
    char * filename;
    char * format;
    int64_t check_errors;
    _Bool has_image_end_offset;
    int64_t image_end_offset;
    _Bool has_corruptions;
    int64_t corruptions;
    _Bool has_leaks;
    int64_t leaks;
    _Bool has_corruptions_fixed;
    int64_t corruptions_fixed;
    _Bool has_leaks_fixed;
    int64_t leaks_fixed;
    _Bool has_total_clusters;
    int64_t total_clusters;
    _Bool has_allocated_clusters;
    int64_t allocated_clusters;
    _Bool has_fragmented_clusters;
    int64_t fragmented_clusters;
    _Bool has_compressed_clusters;
    int64_t compressed_clusters;
};

void qapi_free_ImageCheckList(ImageCheckList * obj);
void qapi_free_ImageCheck(ImageCheck * obj);

struct StatusInfo
{
    _Bool running;
    _Bool singlestep;
    RunState status;
};

void qapi_free_StatusInfoList(StatusInfoList * obj);
void qapi_free_StatusInfo(StatusInfo * obj);

struct UuidInfo
{
    char * UUID;
};

void qapi_free_UuidInfoList(UuidInfoList * obj);
void qapi_free_UuidInfo(UuidInfo * obj);

struct ChardevInfo
{
    char * label;
    char * filename;
};

void qapi_free_ChardevInfoList(ChardevInfoList * obj);
void qapi_free_ChardevInfo(ChardevInfo * obj);

void qapi_free_DataFormatList(DataFormatList * obj);

struct CommandInfo
{
    char * name;
};

void qapi_free_CommandInfoList(CommandInfoList * obj);
void qapi_free_CommandInfo(CommandInfo * obj);

struct EventInfo
{
    char * name;
};

void qapi_free_EventInfoList(EventInfoList * obj);
void qapi_free_EventInfo(EventInfo * obj);

struct MigrationStats
{
    int64_t transferred;
    int64_t remaining;
    int64_t total;
    int64_t duplicate;
    int64_t skipped;
    int64_t normal;
    int64_t normal_bytes;
    int64_t dirty_pages_rate;
};

void qapi_free_MigrationStatsList(MigrationStatsList * obj);
void qapi_free_MigrationStats(MigrationStats * obj);

struct XBZRLECacheStats
{
    int64_t cache_size;
    int64_t bytes;
    int64_t pages;
    int64_t cache_miss;
    int64_t overflow;
};

void qapi_free_XBZRLECacheStatsList(XBZRLECacheStatsList * obj);
void qapi_free_XBZRLECacheStats(XBZRLECacheStats * obj);

struct MigrationInfo
{
    _Bool has_status;
    char * status;
    _Bool has_ram;
    MigrationStats * ram;
    _Bool has_disk;
    MigrationStats * disk;
    _Bool has_xbzrle_cache;
    XBZRLECacheStats * xbzrle_cache;
    _Bool has_total_time;
    int64_t total_time;
    _Bool has_expected_downtime;
    int64_t expected_downtime;
    _Bool has_downtime;
    int64_t downtime;
};

void qapi_free_MigrationInfoList(MigrationInfoList * obj);
void qapi_free_MigrationInfo(MigrationInfo * obj);

void qapi_free_MigrationCapabilityList(MigrationCapabilityList * obj);

struct MigrationCapabilityStatus
{
    MigrationCapability capability;
    _Bool state;
};

void qapi_free_MigrationCapabilityStatusList(MigrationCapabilityStatusList * obj);
void qapi_free_MigrationCapabilityStatus(MigrationCapabilityStatus * obj);

struct MouseInfo
{
    char * name;
    int64_t index;
    _Bool current;
    _Bool absolute;
};

void qapi_free_MouseInfoList(MouseInfoList * obj);
void qapi_free_MouseInfo(MouseInfo * obj);

struct CpuInfo
{
    int64_t CPU;
    _Bool current;
    _Bool halted;
    _Bool has_pc;
    int64_t pc;
    _Bool has_nip;
    int64_t nip;
    _Bool has_npc;
    int64_t npc;
    _Bool has_PC;
    int64_t PC;
    int64_t thread_id;
};

void qapi_free_CpuInfoList(CpuInfoList * obj);
void qapi_free_CpuInfo(CpuInfo * obj);

struct BlockDeviceInfo
{
    char * file;
    _Bool ro;
    char * drv;
    _Bool has_backing_file;
    char * backing_file;
    int64_t backing_file_depth;
    _Bool encrypted;
    _Bool encryption_key_missing;
    int64_t bps;
    int64_t bps_rd;
    int64_t bps_wr;
    int64_t iops;
    int64_t iops_rd;
    int64_t iops_wr;
};

void qapi_free_BlockDeviceInfoList(BlockDeviceInfoList * obj);
void qapi_free_BlockDeviceInfo(BlockDeviceInfo * obj);

void qapi_free_BlockDeviceIoStatusList(BlockDeviceIoStatusList * obj);

struct BlockDirtyInfo
{
    int64_t count;
    int64_t granularity;
};

void qapi_free_BlockDirtyInfoList(BlockDirtyInfoList * obj);
void qapi_free_BlockDirtyInfo(BlockDirtyInfo * obj);

struct BlockInfo
{
    char * device;
    char * type;
    _Bool removable;
    _Bool locked;
    _Bool has_inserted;
    BlockDeviceInfo * inserted;
    _Bool has_tray_open;
    _Bool tray_open;
    _Bool has_io_status;
    BlockDeviceIoStatus io_status;
    _Bool has_dirty;
    BlockDirtyInfo * dirty;
};

void qapi_free_BlockInfoList(BlockInfoList * obj);
void qapi_free_BlockInfo(BlockInfo * obj);

struct BlockDeviceStats
{
    int64_t rd_bytes;
    int64_t wr_bytes;
    int64_t rd_operations;
    int64_t wr_operations;
    int64_t flush_operations;
    int64_t flush_total_time_ns;
    int64_t wr_total_time_ns;
    int64_t rd_total_time_ns;
    int64_t wr_highest_offset;
};

void qapi_free_BlockDeviceStatsList(BlockDeviceStatsList * obj);
void qapi_free_BlockDeviceStats(BlockDeviceStats * obj);

struct BlockStats
{
    _Bool has_device;
    char * device;
    BlockDeviceStats * stats;
    _Bool has_parent;
    BlockStats * parent;
};

void qapi_free_BlockStatsList(BlockStatsList * obj);
void qapi_free_BlockStats(BlockStats * obj);

struct VncClientInfo
{
    char * host;
    char * family;
    char * service;
    _Bool has_x509_dname;
    char * x509_dname;
    _Bool has_sasl_username;
    char * sasl_username;
};

void qapi_free_VncClientInfoList(VncClientInfoList * obj);
void qapi_free_VncClientInfo(VncClientInfo * obj);

struct VncInfo
{
    _Bool enabled;
    _Bool has_host;
    char * host;
    _Bool has_family;
    char * family;
    _Bool has_service;
    char * service;
    _Bool has_auth;
    char * auth;
    _Bool has_clients;
    VncClientInfoList * clients;
};

void qapi_free_VncInfoList(VncInfoList * obj);
void qapi_free_VncInfo(VncInfo * obj);

struct SpiceChannel
{
    char * host;
    char * family;
    char * port;
    int64_t connection_id;
    int64_t channel_type;
    int64_t channel_id;
    _Bool tls;
};

void qapi_free_SpiceChannelList(SpiceChannelList * obj);
void qapi_free_SpiceChannel(SpiceChannel * obj);

void qapi_free_SpiceQueryMouseModeList(SpiceQueryMouseModeList * obj);

struct SpiceInfo
{
    _Bool enabled;
    _Bool migrated;
    _Bool has_host;
    char * host;
    _Bool has_port;
    int64_t port;
    _Bool has_tls_port;
    int64_t tls_port;
    _Bool has_auth;
    char * auth;
    _Bool has_compiled_version;
    char * compiled_version;
    SpiceQueryMouseMode mouse_mode;
    _Bool has_channels;
    SpiceChannelList * channels;
};

void qapi_free_SpiceInfoList(SpiceInfoList * obj);
void qapi_free_SpiceInfo(SpiceInfo * obj);

struct BalloonInfo
{
    int64_t actual;
};

void qapi_free_BalloonInfoList(BalloonInfoList * obj);
void qapi_free_BalloonInfo(BalloonInfo * obj);

struct PciMemoryRange
{
    int64_t base;
    int64_t limit;
};

void qapi_free_PciMemoryRangeList(PciMemoryRangeList * obj);
void qapi_free_PciMemoryRange(PciMemoryRange * obj);

struct PciMemoryRegion
{
    int64_t bar;
    char * type;
    int64_t address;
    int64_t size;
    _Bool has_prefetch;
    _Bool prefetch;
    _Bool has_mem_type_64;
    _Bool mem_type_64;
};

void qapi_free_PciMemoryRegionList(PciMemoryRegionList * obj);
void qapi_free_PciMemoryRegion(PciMemoryRegion * obj);

struct PciBridgeInfo
{
    struct
    {
        int64_t number;
        int64_t secondary;
        int64_t subordinate;
        PciMemoryRange * io_range;
        PciMemoryRange * memory_range;
        PciMemoryRange * prefetchable_range;
    } bus;
    _Bool has_devices;
    PciDeviceInfoList * devices;
};

void qapi_free_PciBridgeInfoList(PciBridgeInfoList * obj);
void qapi_free_PciBridgeInfo(PciBridgeInfo * obj);

struct PciDeviceInfo
{
    int64_t bus;
    int64_t slot;
    int64_t function;
    struct
    {
        _Bool has_desc;
        char * desc;
        int64_t class;
    } class_info;
    struct
    {
        int64_t device;
        int64_t vendor;
    } id;
    _Bool has_irq;
    int64_t irq;
    char * qdev_id;
    _Bool has_pci_bridge;
    PciBridgeInfo * pci_bridge;
    PciMemoryRegionList * regions;
};

void qapi_free_PciDeviceInfoList(PciDeviceInfoList * obj);
void qapi_free_PciDeviceInfo(PciDeviceInfo * obj);

struct PciInfo
{
    int64_t bus;
    PciDeviceInfoList * devices;
};

void qapi_free_PciInfoList(PciInfoList * obj);
void qapi_free_PciInfo(PciInfo * obj);

void qapi_free_BlockdevOnErrorList(BlockdevOnErrorList * obj);

void qapi_free_MirrorSyncModeList(MirrorSyncModeList * obj);

struct BlockJobInfo
{
    char * type;
    char * device;
    int64_t len;
    int64_t offset;
    _Bool busy;
    _Bool paused;
    int64_t speed;
    BlockDeviceIoStatus io_status;
};

void qapi_free_BlockJobInfoList(BlockJobInfoList * obj);
void qapi_free_BlockJobInfo(BlockJobInfo * obj);

void qapi_free_NewImageModeList(NewImageModeList * obj);

struct BlockdevSnapshot
{
    char * device;
    char * snapshot_file;
    _Bool has_format;
    char * format;
    _Bool has_mode;
    NewImageMode mode;
};

void qapi_free_BlockdevSnapshotList(BlockdevSnapshotList * obj);
void qapi_free_BlockdevSnapshot(BlockdevSnapshot * obj);

struct BlockdevAction
{
    BlockdevActionKind kind;
    union {
        void *data;
        BlockdevSnapshot * blockdev_snapshot_sync;
    };
};
void qapi_free_BlockdevActionList(BlockdevActionList * obj);
void qapi_free_BlockdevAction(BlockdevAction * obj);

struct ObjectPropertyInfo
{
    char * name;
    char * type;
};

void qapi_free_ObjectPropertyInfoList(ObjectPropertyInfoList * obj);
void qapi_free_ObjectPropertyInfo(ObjectPropertyInfo * obj);

struct ObjectTypeInfo
{
    char * name;
};

void qapi_free_ObjectTypeInfoList(ObjectTypeInfoList * obj);
void qapi_free_ObjectTypeInfo(ObjectTypeInfo * obj);

struct DevicePropertyInfo
{
    char * name;
    char * type;
};

void qapi_free_DevicePropertyInfoList(DevicePropertyInfoList * obj);
void qapi_free_DevicePropertyInfo(DevicePropertyInfo * obj);

struct NetdevNoneOptions
{
};

void qapi_free_NetdevNoneOptionsList(NetdevNoneOptionsList * obj);
void qapi_free_NetdevNoneOptions(NetdevNoneOptions * obj);

struct NetLegacyNicOptions
{
    _Bool has_netdev;
    char * netdev;
    _Bool has_macaddr;
    char * macaddr;
    _Bool has_model;
    char * model;
    _Bool has_addr;
    char * addr;
    _Bool has_vectors;
    uint32_t vectors;
};

void qapi_free_NetLegacyNicOptionsList(NetLegacyNicOptionsList * obj);
void qapi_free_NetLegacyNicOptions(NetLegacyNicOptions * obj);

struct String
{
    char * str;
};

void qapi_free_StringList(StringList * obj);
void qapi_free_String(String * obj);

struct NetdevUserOptions
{
    _Bool has_hostname;
    char * hostname;
    _Bool has_q_restrict;
    _Bool q_restrict;
    _Bool has_ip;
    char * ip;
    _Bool has_net;
    char * net;
    _Bool has_host;
    char * host;
    _Bool has_tftp;
    char * tftp;
    _Bool has_bootfile;
    char * bootfile;
    _Bool has_dhcpstart;
    char * dhcpstart;
    _Bool has_dns;
    char * dns;
    _Bool has_dnssearch;
    StringList * dnssearch;
    _Bool has_smb;
    char * smb;
    _Bool has_smbserver;
    char * smbserver;
    _Bool has_hostfwd;
    StringList * hostfwd;
    _Bool has_guestfwd;
    StringList * guestfwd;
};

void qapi_free_NetdevUserOptionsList(NetdevUserOptionsList * obj);
void qapi_free_NetdevUserOptions(NetdevUserOptions * obj);

struct NetdevTapOptions
{
    _Bool has_ifname;
    char * ifname;
    _Bool has_fd;
    char * fd;
    _Bool has_fds;
    char * fds;
    _Bool has_script;
    char * script;
    _Bool has_downscript;
    char * downscript;
    _Bool has_helper;
    char * helper;
    _Bool has_sndbuf;
    uint64_t sndbuf;
    _Bool has_vnet_hdr;
    _Bool vnet_hdr;
    _Bool has_vhost;
    _Bool vhost;
    _Bool has_vhostfd;
    char * vhostfd;
    _Bool has_vhostfds;
    char * vhostfds;
    _Bool has_vhostforce;
    _Bool vhostforce;
    _Bool has_queues;
    uint32_t queues;
};

void qapi_free_NetdevTapOptionsList(NetdevTapOptionsList * obj);
void qapi_free_NetdevTapOptions(NetdevTapOptions * obj);

struct NetdevSocketOptions
{
    _Bool has_fd;
    char * fd;
    _Bool has_listen;
    char * listen;
    _Bool has_connect;
    char * connect;
    _Bool has_mcast;
    char * mcast;
    _Bool has_localaddr;
    char * localaddr;
    _Bool has_udp;
    char * udp;
};

void qapi_free_NetdevSocketOptionsList(NetdevSocketOptionsList * obj);
void qapi_free_NetdevSocketOptions(NetdevSocketOptions * obj);

struct NetdevVdeOptions
{
    _Bool has_sock;
    char * sock;
    _Bool has_port;
    uint16_t port;
    _Bool has_group;
    char * group;
    _Bool has_mode;
    uint16_t mode;
};

void qapi_free_NetdevVdeOptionsList(NetdevVdeOptionsList * obj);
void qapi_free_NetdevVdeOptions(NetdevVdeOptions * obj);

struct NetdevDumpOptions
{
    _Bool has_len;
    uint64_t len;
    _Bool has_file;
    char * file;
};

void qapi_free_NetdevDumpOptionsList(NetdevDumpOptionsList * obj);
void qapi_free_NetdevDumpOptions(NetdevDumpOptions * obj);

struct NetdevBridgeOptions
{
    _Bool has_br;
    char * br;
    _Bool has_helper;
    char * helper;
};

void qapi_free_NetdevBridgeOptionsList(NetdevBridgeOptionsList * obj);
void qapi_free_NetdevBridgeOptions(NetdevBridgeOptions * obj);

struct NetdevHubPortOptions
{
    int32_t hubid;
};

void qapi_free_NetdevHubPortOptionsList(NetdevHubPortOptionsList * obj);
void qapi_free_NetdevHubPortOptions(NetdevHubPortOptions * obj);

struct NetClientOptions
{
    NetClientOptionsKind kind;
    union {
        void *data;
        NetdevNoneOptions * none;
        NetLegacyNicOptions * nic;
        NetdevUserOptions * user;
        NetdevTapOptions * tap;
        NetdevSocketOptions * socket;
        NetdevVdeOptions * vde;
        NetdevDumpOptions * dump;
        NetdevBridgeOptions * bridge;
        NetdevHubPortOptions * hubport;
    };
};
void qapi_free_NetClientOptionsList(NetClientOptionsList * obj);
void qapi_free_NetClientOptions(NetClientOptions * obj);

struct NetLegacy
{
    _Bool has_vlan;
    int32_t vlan;
    _Bool has_id;
    char * id;
    _Bool has_name;
    char * name;
    NetClientOptions * opts;
};

void qapi_free_NetLegacyList(NetLegacyList * obj);
void qapi_free_NetLegacy(NetLegacy * obj);

struct Netdev
{
    char * id;
    NetClientOptions * opts;
};

void qapi_free_NetdevList(NetdevList * obj);
void qapi_free_Netdev(Netdev * obj);

struct InetSocketAddress
{
    char * host;
    char * port;
    _Bool has_to;
    uint16_t to;
    _Bool has_ipv4;
    _Bool ipv4;
    _Bool has_ipv6;
    _Bool ipv6;
};

void qapi_free_InetSocketAddressList(InetSocketAddressList * obj);
void qapi_free_InetSocketAddress(InetSocketAddress * obj);

struct UnixSocketAddress
{
    char * path;
};

void qapi_free_UnixSocketAddressList(UnixSocketAddressList * obj);
void qapi_free_UnixSocketAddress(UnixSocketAddress * obj);

struct SocketAddress
{
    SocketAddressKind kind;
    union {
        void *data;
        InetSocketAddress * inet;
        UnixSocketAddress * q_unix;
        String * fd;
    };
};
void qapi_free_SocketAddressList(SocketAddressList * obj);
void qapi_free_SocketAddress(SocketAddress * obj);

struct MachineInfo
{
    char * name;
    _Bool has_alias;
    char * alias;
    _Bool has_is_default;
    _Bool is_default;
    int64_t cpu_max;
};

void qapi_free_MachineInfoList(MachineInfoList * obj);
void qapi_free_MachineInfo(MachineInfo * obj);

struct CpuDefinitionInfo
{
    char * name;
};

void qapi_free_CpuDefinitionInfoList(CpuDefinitionInfoList * obj);
void qapi_free_CpuDefinitionInfo(CpuDefinitionInfo * obj);

struct AddfdInfo
{
    int64_t fdset_id;
    int64_t fd;
};

void qapi_free_AddfdInfoList(AddfdInfoList * obj);
void qapi_free_AddfdInfo(AddfdInfo * obj);

struct FdsetFdInfo
{
    int64_t fd;
    _Bool has_opaque;
    char * opaque;
};

void qapi_free_FdsetFdInfoList(FdsetFdInfoList * obj);
void qapi_free_FdsetFdInfo(FdsetFdInfo * obj);

struct FdsetInfo
{
    int64_t fdset_id;
    FdsetFdInfoList * fds;
};

void qapi_free_FdsetInfoList(FdsetInfoList * obj);
void qapi_free_FdsetInfo(FdsetInfo * obj);

void qapi_free_TargetTypeList(TargetTypeList * obj);

struct TargetInfo
{
    TargetType arch;
};

void qapi_free_TargetInfoList(TargetInfoList * obj);
void qapi_free_TargetInfo(TargetInfo * obj);

void qapi_free_QKeyCodeList(QKeyCodeList * obj);

struct KeyValue
{
    KeyValueKind kind;
    union {
        void *data;
        int64_t number;
        QKeyCode qcode;
    };
};
void qapi_free_KeyValueList(KeyValueList * obj);
void qapi_free_KeyValue(KeyValue * obj);

struct ChardevFile
{
    _Bool has_in;
    char * in;
    char * out;
};

void qapi_free_ChardevFileList(ChardevFileList * obj);
void qapi_free_ChardevFile(ChardevFile * obj);

struct ChardevHostdev
{
    char * device;
};

void qapi_free_ChardevHostdevList(ChardevHostdevList * obj);
void qapi_free_ChardevHostdev(ChardevHostdev * obj);

struct ChardevSocket
{
    SocketAddress * addr;
    _Bool has_server;
    _Bool server;
    _Bool has_wait;
    _Bool wait;
    _Bool has_nodelay;
    _Bool nodelay;
    _Bool has_telnet;
    _Bool telnet;
};

void qapi_free_ChardevSocketList(ChardevSocketList * obj);
void qapi_free_ChardevSocket(ChardevSocket * obj);

struct ChardevDgram
{
    SocketAddress * remote;
    _Bool has_local;
    SocketAddress * local;
};

void qapi_free_ChardevDgramList(ChardevDgramList * obj);
void qapi_free_ChardevDgram(ChardevDgram * obj);

struct ChardevMux
{
    char * chardev;
};

void qapi_free_ChardevMuxList(ChardevMuxList * obj);
void qapi_free_ChardevMux(ChardevMux * obj);

struct ChardevStdio
{
    _Bool has_signal;
    _Bool signal;
};

void qapi_free_ChardevStdioList(ChardevStdioList * obj);
void qapi_free_ChardevStdio(ChardevStdio * obj);

struct ChardevSpiceChannel
{
    char * type;
};

void qapi_free_ChardevSpiceChannelList(ChardevSpiceChannelList * obj);
void qapi_free_ChardevSpiceChannel(ChardevSpiceChannel * obj);

struct ChardevSpicePort
{
    char * fqdn;
};

void qapi_free_ChardevSpicePortList(ChardevSpicePortList * obj);
void qapi_free_ChardevSpicePort(ChardevSpicePort * obj);

struct ChardevVC
{
    _Bool has_width;
    int64_t width;
    _Bool has_height;
    int64_t height;
    _Bool has_cols;
    int64_t cols;
    _Bool has_rows;
    int64_t rows;
};

void qapi_free_ChardevVCList(ChardevVCList * obj);
void qapi_free_ChardevVC(ChardevVC * obj);

struct ChardevRingbuf
{
    _Bool has_size;
    int64_t size;
};

void qapi_free_ChardevRingbufList(ChardevRingbufList * obj);
void qapi_free_ChardevRingbuf(ChardevRingbuf * obj);

struct ChardevDummy
{
};

void qapi_free_ChardevDummyList(ChardevDummyList * obj);
void qapi_free_ChardevDummy(ChardevDummy * obj);

struct ChardevBackend
{
    ChardevBackendKind kind;
    union {
        void *data;
        ChardevFile * file;
        ChardevHostdev * serial;
        ChardevHostdev * parallel;
        ChardevHostdev * pipe;
        ChardevSocket * socket;
        ChardevDgram * dgram;
        ChardevDummy * pty;
        ChardevDummy * null;
        ChardevMux * mux;
        ChardevDummy * msmouse;
        ChardevDummy * braille;
        ChardevStdio * stdio;
        ChardevDummy * console;
        ChardevSpiceChannel * spicevmc;
        ChardevSpicePort * spiceport;
        ChardevVC * vc;
        ChardevRingbuf * memory;
    };
};
void qapi_free_ChardevBackendList(ChardevBackendList * obj);
void qapi_free_ChardevBackend(ChardevBackend * obj);

struct ChardevReturn
{
    _Bool has_pty;
    char * pty;
};

void qapi_free_ChardevReturnList(ChardevReturnList * obj);
void qapi_free_ChardevReturn(ChardevReturn * obj);

void qapi_free_TpmModelList(TpmModelList * obj);

void qapi_free_TpmTypeList(TpmTypeList * obj);

struct TPMPassthroughOptions
{
    _Bool has_path;
    char * path;
    _Bool has_cancel_path;
    char * cancel_path;
};

void qapi_free_TPMPassthroughOptionsList(TPMPassthroughOptionsList * obj);
void qapi_free_TPMPassthroughOptions(TPMPassthroughOptions * obj);

struct TpmTypeOptions
{
    TpmTypeOptionsKind kind;
    union {
        void *data;
        TPMPassthroughOptions * passthrough;
    };
};
void qapi_free_TpmTypeOptionsList(TpmTypeOptionsList * obj);
void qapi_free_TpmTypeOptions(TpmTypeOptions * obj);

struct TPMInfo
{
    char * id;
    TpmModel model;
    TpmTypeOptions * options;
};

void qapi_free_TPMInfoList(TPMInfoList * obj);
void qapi_free_TPMInfo(TPMInfo * obj);

struct AcpiTableOptions
{
    _Bool has_sig;
    char * sig;
    _Bool has_rev;
    uint8_t rev;
    _Bool has_oem_id;
    char * oem_id;
    _Bool has_oem_table_id;
    char * oem_table_id;
    _Bool has_oem_rev;
    uint32_t oem_rev;
    _Bool has_asl_compiler_id;
    char * asl_compiler_id;
    _Bool has_asl_compiler_rev;
    uint32_t asl_compiler_rev;
    _Bool has_file;
    char * file;
    _Bool has_data;
    char * data;
};

void qapi_free_AcpiTableOptionsList(AcpiTableOptionsList * obj);
void qapi_free_AcpiTableOptions(AcpiTableOptions * obj);
# 17 "/home/gabriel/repos/qemu/include/qapi/error.h" 2






typedef struct Error Error;






void error_set(Error **err, ErrorClass err_class, const char *fmt, ...) __attribute__((format(gnu_printf, 3, 4)));






void error_set_errno(Error **err, int os_error, ErrorClass err_class, const char *fmt, ...) __attribute__((format(gnu_printf, 4, 5)));
# 51 "/home/gabriel/repos/qemu/include/qapi/error.h"
_Bool error_is_set(Error **err);




ErrorClass error_get_class(const Error *err);




Error *error_copy(const Error *err);




const char *error_get_pretty(Error *err);






void error_propagate(Error **dst_err, Error *local_err);




void error_free(Error *err);
# 32 "/home/gabriel/repos/qemu/include/qemu/option.h" 2
# 1 "/home/gabriel/repos/qemu/include/qapi/qmp/qdict.h" 1
# 16 "/home/gabriel/repos/qemu/include/qapi/qmp/qdict.h"
# 1 "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h" 1
# 35 "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h"
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 36 "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h" 2
# 1 "/usr/include/assert.h" 1 3 4
# 37 "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h" 2

typedef enum {
    QTYPE_NONE,
    QTYPE_QINT,
    QTYPE_QSTRING,
    QTYPE_QDICT,
    QTYPE_QLIST,
    QTYPE_QFLOAT,
    QTYPE_QBOOL,
    QTYPE_QERROR,
} qtype_code;

struct QObject;

typedef struct QType {
    qtype_code code;
    void (*destroy)(struct QObject *);
} QType;

typedef struct QObject {
    const QType *type;
    size_t refcnt;
} QObject;
# 84 "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h"
static __attribute__ (( always_inline )) __inline__ void qobject_incref(QObject *obj)
{
    if (obj)
        obj->refcnt++;
}





static __attribute__ (( always_inline )) __inline__ void qobject_decref(QObject *obj)
{
    if (obj && --obj->refcnt == 0) {
        ((obj->type != ((void *)0)) ? (void) (0) : __assert_fail ("obj->type != ((void *)0)", "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h", 97, __PRETTY_FUNCTION__));
        ((obj->type->destroy != ((void *)0)) ? (void) (0) : __assert_fail ("obj->type->destroy != ((void *)0)", "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h", 98, __PRETTY_FUNCTION__));
        obj->type->destroy(obj);
    }
}




static __attribute__ (( always_inline )) __inline__ qtype_code qobject_type(const QObject *obj)
{
    ((obj->type != ((void *)0)) ? (void) (0) : __assert_fail ("obj->type != ((void *)0)", "/home/gabriel/repos/qemu/include/qapi/qmp/qobject.h", 108, __PRETTY_FUNCTION__));
    return obj->type->code;
}
# 17 "/home/gabriel/repos/qemu/include/qapi/qmp/qdict.h" 2
# 1 "/home/gabriel/repos/qemu/include/qapi/qmp/qlist.h" 1
# 20 "/home/gabriel/repos/qemu/include/qapi/qmp/qlist.h"
typedef struct QListEntry {
    QObject *value;
    struct { struct QListEntry *tqe_next; struct QListEntry * *tqe_prev; } next;
} QListEntry;

typedef struct QList {
    QObject base;
    struct { struct QListEntry *tqh_first; struct QListEntry * *tqh_last; } head;
} QList;
# 38 "/home/gabriel/repos/qemu/include/qapi/qmp/qlist.h"
static __attribute__ (( always_inline )) __inline__ QObject *qlist_entry_obj(const QListEntry *entry)
{
    return entry->value;
}

QList *qlist_new(void);
QList *qlist_copy(QList *src);
void qlist_append_obj(QList *qlist, QObject *obj);
void qlist_iter(const QList *qlist,
                void (*iter)(QObject *obj, void *opaque), void *opaque);
QObject *qlist_pop(QList *qlist);
QObject *qlist_peek(QList *qlist);
int qlist_empty(const QList *qlist);
size_t qlist_size(const QList *qlist);
QList *qobject_to_qlist(const QObject *obj);

static __attribute__ (( always_inline )) __inline__ const QListEntry *qlist_first(const QList *qlist)
{
    return ((&qlist->head)->tqh_first);
}

static __attribute__ (( always_inline )) __inline__ const QListEntry *qlist_next(const QListEntry *entry)
{
    return ((entry)->next.tqe_next);
}
# 18 "/home/gabriel/repos/qemu/include/qapi/qmp/qdict.h" 2





typedef struct QDictEntry {
    char *key;
    QObject *value;
    struct { struct QDictEntry *le_next; struct QDictEntry **le_prev; } next;
} QDictEntry;

typedef struct QDict {
    QObject base;
    size_t size;
    struct { struct QDictEntry *lh_first; } table[512];
} QDict;


QDict *qdict_new(void);
const char *qdict_entry_key(const QDictEntry *entry);
QObject *qdict_entry_value(const QDictEntry *entry);
size_t qdict_size(const QDict *qdict);
void qdict_put_obj(QDict *qdict, const char *key, QObject *value);
void qdict_del(QDict *qdict, const char *key);
int qdict_haskey(const QDict *qdict, const char *key);
QObject *qdict_get(const QDict *qdict, const char *key);
QDict *qobject_to_qdict(const QObject *obj);
void qdict_iter(const QDict *qdict,
                void (*iter)(const char *key, QObject *obj, void *opaque),
                void *opaque);
const QDictEntry *qdict_first(const QDict *qdict);
const QDictEntry *qdict_next(const QDict *qdict, const QDictEntry *entry);






double qdict_get_double(const QDict *qdict, const char *key);
int64_t qdict_get_int(const QDict *qdict, const char *key);
int qdict_get_bool(const QDict *qdict, const char *key);
QList *qdict_get_qlist(const QDict *qdict, const char *key);
QDict *qdict_get_qdict(const QDict *qdict, const char *key);
const char *qdict_get_str(const QDict *qdict, const char *key);
int64_t qdict_get_try_int(const QDict *qdict, const char *key,
                          int64_t def_value);
int qdict_get_try_bool(const QDict *qdict, const char *key, int def_value);
const char *qdict_get_try_str(const QDict *qdict, const char *key);

QDict *qdict_clone_shallow(const QDict *src);
# 33 "/home/gabriel/repos/qemu/include/qemu/option.h" 2

enum QEMUOptionParType {
    OPT_FLAG,
    OPT_NUMBER,
    OPT_SIZE,
    OPT_STRING,
};

typedef struct QEMUOptionParameter {
    const char *name;
    enum QEMUOptionParType type;
    union {
        uint64_t n;
        char* s;
    } value;
    const char *help;
} QEMUOptionParameter;


const char *get_opt_name(char *buf, int buf_size, const char *p, char delim);
const char *get_opt_value(char *buf, int buf_size, const char *p);
int get_next_param_value(char *buf, int buf_size,
                         const char *tag, const char **pstr);
int get_param_value(char *buf, int buf_size,
                    const char *tag, const char *str);
int check_params(char *buf, int buf_size,
                 const char * const *params, const char *str);
# 68 "/home/gabriel/repos/qemu/include/qemu/option.h"
QEMUOptionParameter *get_option_parameter(QEMUOptionParameter *list,
    const char *name);
int set_option_parameter(QEMUOptionParameter *list, const char *name,
    const char *value);
int set_option_parameter_int(QEMUOptionParameter *list, const char *name,
    uint64_t value);
QEMUOptionParameter *append_option_parameters(QEMUOptionParameter *dest,
    QEMUOptionParameter *list);
QEMUOptionParameter *parse_option_parameters(const char *param,
    QEMUOptionParameter *list, QEMUOptionParameter *dest);
void free_option_parameters(QEMUOptionParameter *list);
void print_option_parameters(QEMUOptionParameter *list);
void print_option_help(QEMUOptionParameter *list);



typedef struct QemuOpt QemuOpt;
typedef struct QemuOpts QemuOpts;
typedef struct QemuOptsList QemuOptsList;

enum QemuOptType {
    QEMU_OPT_STRING = 0,
    QEMU_OPT_BOOL,
    QEMU_OPT_NUMBER,
    QEMU_OPT_SIZE,
};

typedef struct QemuOptDesc {
    const char *name;
    enum QemuOptType type;
    const char *help;
} QemuOptDesc;

struct QemuOptsList {
    const char *name;
    const char *implied_opt_name;
    _Bool merge_lists;
    struct { struct QemuOpts *tqh_first; struct QemuOpts * *tqh_last; } head;
    QemuOptDesc desc[];
};

const char *qemu_opt_get(QemuOpts *opts, const char *name);
# 121 "/home/gabriel/repos/qemu/include/qemu/option.h"
_Bool qemu_opt_has_help_opt(QemuOpts *opts);
_Bool qemu_opt_get_bool(QemuOpts *opts, const char *name, _Bool defval);
uint64_t qemu_opt_get_number(QemuOpts *opts, const char *name, uint64_t defval);
uint64_t qemu_opt_get_size(QemuOpts *opts, const char *name, uint64_t defval);
int qemu_opt_set(QemuOpts *opts, const char *name, const char *value);
void qemu_opt_set_err(QemuOpts *opts, const char *name, const char *value,
                      Error **errp);
int qemu_opt_set_bool(QemuOpts *opts, const char *name, _Bool val);
int qemu_opt_set_number(QemuOpts *opts, const char *name, int64_t val);
typedef int (*qemu_opt_loopfunc)(const char *name, const char *value, void *opaque);
int qemu_opt_foreach(QemuOpts *opts, qemu_opt_loopfunc func, void *opaque,
                     int abort_on_failure);

QemuOpts *qemu_opts_find(QemuOptsList *list, const char *id);
QemuOpts *qemu_opts_create(QemuOptsList *list, const char *id,
                           int fail_if_exists, Error **errp);
QemuOpts *qemu_opts_create_nofail(QemuOptsList *list);
void qemu_opts_reset(QemuOptsList *list);
void qemu_opts_loc_restore(QemuOpts *opts);
int qemu_opts_set(QemuOptsList *list, const char *id,
                  const char *name, const char *value);
const char *qemu_opts_id(QemuOpts *opts);
void qemu_opts_del(QemuOpts *opts);
void qemu_opts_validate(QemuOpts *opts, const QemuOptDesc *desc, Error **errp);
int qemu_opts_do_parse(QemuOpts *opts, const char *params, const char *firstname);
QemuOpts *qemu_opts_parse(QemuOptsList *list, const char *params, int permit_abbrev);
void qemu_opts_set_defaults(QemuOptsList *list, const char *params,
                            int permit_abbrev);
QemuOpts *qemu_opts_from_qdict(QemuOptsList *list, const QDict *qdict,
                               Error **errp);
QDict *qemu_opts_to_qdict(QemuOpts *opts, QDict *qdict);
void qemu_opts_absorb_qdict(QemuOpts *opts, QDict *qdict, Error **errp);

typedef int (*qemu_opts_loopfunc)(QemuOpts *opts, void *opaque);
int qemu_opts_print(QemuOpts *opts, void *dummy);
int qemu_opts_foreach(QemuOptsList *list, qemu_opts_loopfunc func, void *opaque,
                      int abort_on_failure);
# 6 "/home/gabriel/repos/qemu/include/hw/qdev-core.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/typedefs.h" 1
# 7 "/home/gabriel/repos/qemu/include/hw/qdev-core.h" 2
# 1 "/home/gabriel/repos/qemu/include/qom/object.h" 1
# 22 "/home/gabriel/repos/qemu/include/qom/object.h"
struct Visitor;
struct Error;

struct TypeImpl;
typedef struct TypeImpl *Type;

typedef struct ObjectClass ObjectClass;
typedef struct Object Object;

typedef struct TypeInfo TypeInfo;

typedef struct InterfaceClass InterfaceClass;
typedef struct InterfaceInfo InterfaceInfo;
# 300 "/home/gabriel/repos/qemu/include/qom/object.h"
typedef void (ObjectPropertyAccessor)(Object *obj,
                                      struct Visitor *v,
                                      void *opaque,
                                      const char *name,
                                      struct Error **errp);
# 314 "/home/gabriel/repos/qemu/include/qom/object.h"
typedef void (ObjectPropertyRelease)(Object *obj,
                                     const char *name,
                                     void *opaque);

typedef struct ObjectProperty
{
    gchar *name;
    gchar *type;
    ObjectPropertyAccessor *get;
    ObjectPropertyAccessor *set;
    ObjectPropertyRelease *release;
    void *opaque;

    struct { struct ObjectProperty *tqe_next; struct ObjectProperty * *tqe_prev; } node;
} ObjectProperty;
# 337 "/home/gabriel/repos/qemu/include/qom/object.h"
typedef void (ObjectUnparent)(Object *obj);







typedef void (ObjectFree)(void *obj);







struct ObjectClass
{

    Type type;
    GSList *interfaces;

    ObjectUnparent *unparent;
};
# 377 "/home/gabriel/repos/qemu/include/qom/object.h"
struct Object
{

    ObjectClass *class;
    ObjectFree *free;
    struct { struct ObjectProperty *tqh_first; struct ObjectProperty * *tqh_last; } properties;
    uint32_t ref;
    Object *parent;
};
# 425 "/home/gabriel/repos/qemu/include/qom/object.h"
struct TypeInfo
{
    const char *name;
    const char *parent;

    size_t instance_size;
    void (*instance_init)(Object *obj);
    void (*instance_finalize)(Object *obj);

    _Bool abstract;
    size_t class_size;

    void (*class_init)(ObjectClass *klass, void *data);
    void (*class_base_init)(ObjectClass *klass, void *data);
    void (*class_finalize)(ObjectClass *klass, void *data);
    void *class_data;

    InterfaceInfo *interfaces;
};
# 513 "/home/gabriel/repos/qemu/include/qom/object.h"
struct InterfaceInfo {
    const char *type;
};
# 524 "/home/gabriel/repos/qemu/include/qom/object.h"
struct InterfaceClass
{
    ObjectClass parent_class;

    ObjectClass *concrete_class;
};
# 562 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_new(const char *typename);
# 574 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_new_with_type(Type type);
# 585 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_initialize_with_type(void *data, Type type);
# 596 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_initialize(void *obj, const char *typename);
# 608 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_dynamic_cast(Object *obj, const char *typename);
# 617 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_dynamic_cast_assert(Object *obj, const char *typename);







ObjectClass *object_get_class(Object *obj);







const char *object_get_typename(Object *obj);
# 644 "/home/gabriel/repos/qemu/include/qom/object.h"
Type type_register_static(const TypeInfo *info);
# 655 "/home/gabriel/repos/qemu/include/qom/object.h"
Type type_register(const TypeInfo *info);
# 664 "/home/gabriel/repos/qemu/include/qom/object.h"
ObjectClass *object_class_dynamic_cast_assert(ObjectClass *klass,
                                              const char *typename);

ObjectClass *object_class_dynamic_cast(ObjectClass *klass,
                                       const char *typename);







ObjectClass *object_class_get_parent(ObjectClass *klass);







const char *object_class_get_name(ObjectClass *klass);







_Bool object_class_is_abstract(ObjectClass *klass);







ObjectClass *object_class_by_name(const char *typename);

void object_class_foreach(void (*fn)(ObjectClass *klass, void *opaque),
                          const char *implements_type, _Bool include_abstract,
                          void *opaque);
# 713 "/home/gabriel/repos/qemu/include/qom/object.h"
GSList *object_class_get_list(const char *implements_type,
                              _Bool include_abstract);
# 723 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_ref(Object *obj);
# 732 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_unref(Object *obj);
# 754 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_add(Object *obj, const char *name, const char *type,
                         ObjectPropertyAccessor *get,
                         ObjectPropertyAccessor *set,
                         ObjectPropertyRelease *release,
                         void *opaque, struct Error **errp);

void object_property_del(Object *obj, const char *name, struct Error **errp);
# 770 "/home/gabriel/repos/qemu/include/qom/object.h"
ObjectProperty *object_property_find(Object *obj, const char *name,
                                     struct Error **errp);

void object_unparent(Object *obj);
# 785 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_get(Object *obj, struct Visitor *v, const char *name,
                         struct Error **errp);
# 796 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_set_str(Object *obj, const char *value,
                             const char *name, struct Error **errp);
# 809 "/home/gabriel/repos/qemu/include/qom/object.h"
char *object_property_get_str(Object *obj, const char *name,
                              struct Error **errp);
# 820 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_set_link(Object *obj, Object *value,
                              const char *name, struct Error **errp);
# 833 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_property_get_link(Object *obj, const char *name,
                                 struct Error **errp);
# 844 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_set_bool(Object *obj, _Bool value,
                              const char *name, struct Error **errp);
# 856 "/home/gabriel/repos/qemu/include/qom/object.h"
_Bool object_property_get_bool(Object *obj, const char *name,
                              struct Error **errp);
# 867 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_set_int(Object *obj, int64_t value,
                             const char *name, struct Error **errp);
# 879 "/home/gabriel/repos/qemu/include/qom/object.h"
int64_t object_property_get_int(Object *obj, const char *name,
                                struct Error **errp);
# 893 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_set(Object *obj, struct Visitor *v, const char *name,
                         struct Error **errp);
# 905 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_parse(Object *obj, const char *string,
                           const char *name, struct Error **errp);
# 917 "/home/gabriel/repos/qemu/include/qom/object.h"
char *object_property_print(Object *obj, const char *name,
                            struct Error **errp);
# 928 "/home/gabriel/repos/qemu/include/qom/object.h"
const char *object_property_get_type(Object *obj, const char *name,
                                     struct Error **errp);






Object *object_get_root(void);







gchar *object_get_canonical_path(Object *obj);
# 969 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_resolve_path(const char *path, _Bool *ambiguous);
# 989 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_resolve_path_type(const char *path, const char *typename,
                                 _Bool *ambiguous);
# 1002 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *object_resolve_path_component(Object *parent, const gchar *part);
# 1021 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_add_child(Object *obj, const char *name,
                               Object *child, struct Error **errp);
# 1043 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_add_link(Object *obj, const char *name,
                              const char *type, Object **child,
                              struct Error **errp);
# 1059 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_add_str(Object *obj, const char *name,
                             char *(*get)(Object *, struct Error **),
                             void (*set)(Object *, const char *, struct Error **),
                             struct Error **errp);
# 1075 "/home/gabriel/repos/qemu/include/qom/object.h"
void object_property_add_bool(Object *obj, const char *name,
                              _Bool (*get)(Object *, struct Error **),
                              void (*set)(Object *, _Bool, struct Error **),
                              struct Error **errp);
# 1091 "/home/gabriel/repos/qemu/include/qom/object.h"
int object_child_foreach(Object *obj, int (*fn)(Object *child, void *opaque),
                         void *opaque);
# 1104 "/home/gabriel/repos/qemu/include/qom/object.h"
Object *container_get(Object *root, const char *path);
# 8 "/home/gabriel/repos/qemu/include/hw/qdev-core.h" 2



enum {
    DEV_NVECTORS_UNSPECIFIED = -1,
};






typedef int (*qdev_initfn)(DeviceState *dev);
typedef int (*qdev_event)(DeviceState *dev);
typedef void (*qdev_resetfn)(DeviceState *dev);
typedef void (*DeviceRealize)(DeviceState *dev, Error **errp);
typedef void (*DeviceUnrealize)(DeviceState *dev, Error **errp);

struct VMStateDescription;
# 78 "/home/gabriel/repos/qemu/include/hw/qdev-core.h"
typedef struct DeviceClass {

    ObjectClass parent_class;


    const char *fw_name;
    const char *desc;
    Property *props;
    int no_user;


    void (*reset)(DeviceState *dev);
    DeviceRealize realize;
    DeviceUnrealize unrealize;


    const struct VMStateDescription *vmsd;


    qdev_initfn init;
    qdev_event unplug;
    qdev_event exit;
    const char *bus_type;
} DeviceClass;
# 110 "/home/gabriel/repos/qemu/include/hw/qdev-core.h"
struct DeviceState {

    Object parent_obj;


    const char *id;
    _Bool realized;
    QemuOpts *opts;
    int hotplugged;
    BusState *parent_bus;
    int num_gpio_out;
    qemu_irq *gpio_out;
    int num_gpio_in;
    qemu_irq *gpio_in;
    struct { struct BusState *lh_first; } child_bus;
    int num_child_bus;
    int instance_id_alias;
    int alias_required_for_version;
};






struct BusClass {
    ObjectClass parent_class;


    void (*print_dev)(Monitor *mon, DeviceState *dev, int indent);
    char *(*get_dev_path)(DeviceState *dev);





    char *(*get_fw_dev_path)(DeviceState *dev);
    int (*reset)(BusState *bus);

    int max_dev;
};

typedef struct BusChild {
    DeviceState *child;
    int index;
    struct { struct BusChild *tqe_next; struct BusChild * *tqe_prev; } sibling;
} BusChild;




struct BusState {
    Object obj;
    DeviceState *parent;
    const char *name;
    int allow_hotplug;
    int max_index;
    struct ChildrenHead { struct BusChild *tqh_first; struct BusChild * *tqh_last; } children;
    struct { struct BusState *le_next; struct BusState **le_prev; } sibling;
};

struct Property {
    const char *name;
    PropertyInfo *info;
    int offset;
    uint8_t bitnr;
    uint8_t qtype;
    int64_t defval;
    int arrayoffset;
    PropertyInfo *arrayinfo;
    int arrayfieldsize;
};

struct PropertyInfo {
    const char *name;
    const char *legacy_name;
    const char **enum_table;
    int (*parse)(DeviceState *dev, Property *prop, const char *str);
    int (*print)(DeviceState *dev, Property *prop, char *dest, size_t len);
    ObjectPropertyAccessor *get;
    ObjectPropertyAccessor *set;
    ObjectPropertyRelease *release;
};

typedef struct GlobalProperty {
    const char *driver;
    const char *property;
    const char *value;
    struct { struct GlobalProperty *tqe_next; struct GlobalProperty * *tqe_prev; } next;
} GlobalProperty;



DeviceState *qdev_create(BusState *bus, const char *name);
DeviceState *qdev_try_create(BusState *bus, const char *name);
int qdev_init(DeviceState *dev) __attribute__((warn_unused_result));
void qdev_init_nofail(DeviceState *dev);
void qdev_set_legacy_instance_id(DeviceState *dev, int alias_id,
                                 int required_for_version);
void qdev_unplug(DeviceState *dev, Error **errp);
void qdev_free(DeviceState *dev);
int qdev_simple_unplug_cb(DeviceState *dev);
void qdev_machine_creation_done(void);
_Bool qdev_machine_modified(void);

qemu_irq qdev_get_gpio_in(DeviceState *dev, int n);
void qdev_connect_gpio_out(DeviceState *dev, int n, qemu_irq pin);

BusState *qdev_get_child_bus(DeviceState *dev, const char *name);





void qdev_init_gpio_in(DeviceState *dev, qemu_irq_handler handler, int n);
void qdev_init_gpio_out(DeviceState *dev, qemu_irq *pins, int n);

BusState *qdev_get_parent_bus(DeviceState *dev);



DeviceState *qdev_find_recursive(BusState *bus, const char *id);


typedef int (qbus_walkerfn)(BusState *bus, void *opaque);
typedef int (qdev_walkerfn)(DeviceState *dev, void *opaque);

void qbus_create_inplace(void *bus, const char *typename,
                         DeviceState *parent, const char *name);
BusState *qbus_create(const char *typename, DeviceState *parent, const char *name);



int qbus_walk_children(BusState *bus, qdev_walkerfn *devfn,
                       qbus_walkerfn *busfn, void *opaque);
int qdev_walk_children(DeviceState *dev, qdev_walkerfn *devfn,
                       qbus_walkerfn *busfn, void *opaque);
void qdev_reset_all(DeviceState *dev);
# 259 "/home/gabriel/repos/qemu/include/hw/qdev-core.h"
void qbus_reset_all(BusState *bus);
void qbus_reset_all_fn(void *opaque);

void qbus_free(BusState *bus);




BusState *sysbus_get_default(void);

char *qdev_get_fw_dev_path(DeviceState *dev);







void qdev_machine_init(void);






void device_reset(DeviceState *dev);

const struct VMStateDescription *qdev_get_vmsd(DeviceState *dev);

const char *qdev_fw_name(DeviceState *dev);

Object *qdev_get_machine(void);


void qdev_set_parent_bus(DeviceState *dev, BusState *bus);

extern int qdev_hotplug;

char *qdev_get_dev_path(DeviceState *dev);
# 6 "/home/gabriel/repos/qemu/include/hw/qdev.h" 2
# 1 "/home/gabriel/repos/qemu/include/hw/qdev-properties.h" 1







extern PropertyInfo qdev_prop_bit;
extern PropertyInfo qdev_prop_bool;
extern PropertyInfo qdev_prop_uint8;
extern PropertyInfo qdev_prop_uint16;
extern PropertyInfo qdev_prop_uint32;
extern PropertyInfo qdev_prop_int32;
extern PropertyInfo qdev_prop_uint64;
extern PropertyInfo qdev_prop_hex8;
extern PropertyInfo qdev_prop_hex32;
extern PropertyInfo qdev_prop_hex64;
extern PropertyInfo qdev_prop_string;
extern PropertyInfo qdev_prop_chr;
extern PropertyInfo qdev_prop_ptr;
extern PropertyInfo qdev_prop_macaddr;
extern PropertyInfo qdev_prop_losttickpolicy;
extern PropertyInfo qdev_prop_bios_chs_trans;
extern PropertyInfo qdev_prop_drive;
extern PropertyInfo qdev_prop_netdev;
extern PropertyInfo qdev_prop_vlan;
extern PropertyInfo qdev_prop_pci_devfn;
extern PropertyInfo qdev_prop_blocksize;
extern PropertyInfo qdev_prop_pci_host_devaddr;
extern PropertyInfo qdev_prop_arraylen;
# 150 "/home/gabriel/repos/qemu/include/hw/qdev-properties.h"
void *qdev_get_prop_ptr(DeviceState *dev, Property *prop);
int qdev_prop_parse(DeviceState *dev, const char *name, const char *value);
void qdev_prop_set_bit(DeviceState *dev, const char *name, _Bool value);
void qdev_prop_set_uint8(DeviceState *dev, const char *name, uint8_t value);
void qdev_prop_set_uint16(DeviceState *dev, const char *name, uint16_t value);
void qdev_prop_set_uint32(DeviceState *dev, const char *name, uint32_t value);
void qdev_prop_set_int32(DeviceState *dev, const char *name, int32_t value);
void qdev_prop_set_uint64(DeviceState *dev, const char *name, uint64_t value);
void qdev_prop_set_string(DeviceState *dev, const char *name, const char *value);
void qdev_prop_set_chr(DeviceState *dev, const char *name, CharDriverState *value);
void qdev_prop_set_netdev(DeviceState *dev, const char *name, NetClientState *value);
int qdev_prop_set_drive(DeviceState *dev, const char *name, BlockDriverState *value) __attribute__((warn_unused_result));
void qdev_prop_set_drive_nofail(DeviceState *dev, const char *name, BlockDriverState *value);
void qdev_prop_set_macaddr(DeviceState *dev, const char *name, uint8_t *value);
void qdev_prop_set_enum(DeviceState *dev, const char *name, int value);

void qdev_prop_set_ptr(DeviceState *dev, const char *name, void *value);

void qdev_prop_register_global(GlobalProperty *prop);
void qdev_prop_register_global_list(GlobalProperty *props);
void qdev_prop_set_globals(DeviceState *dev);
void error_set_from_qdev_prop_error(Error **errp, int ret, DeviceState *dev,
                                    Property *prop, const char *value);





void qdev_property_add_static(DeviceState *dev, Property *prop, Error **errp);
# 190 "/home/gabriel/repos/qemu/include/hw/qdev-properties.h"
void qdev_prop_set_after_realize(DeviceState *dev, const char *name,
                                 Error **errp);
# 7 "/home/gabriel/repos/qemu/include/hw/qdev.h" 2
# 9 "/home/gabriel/repos/qemu/include/hw/isa/isa.h" 2
# 23 "/home/gabriel/repos/qemu/include/hw/isa/isa.h"
typedef struct ISADeviceClass {
    DeviceClass parent_class;
    int (*init)(ISADevice *dev);
} ISADeviceClass;

struct ISABus {
    BusState qbus;
    MemoryRegion *address_space_io;
    qemu_irq *irqs;
};

struct ISADevice {
    DeviceState qdev;
    uint32_t isairq[2];
    int nirqs;
    int ioport_id;
};

ISABus *isa_bus_new(DeviceState *dev, MemoryRegion *address_space_io);
void isa_bus_irqs(ISABus *bus, qemu_irq *irqs);
qemu_irq isa_get_irq(ISADevice *dev, int isairq);
void isa_init_irq(ISADevice *dev, qemu_irq *p, int isairq);
MemoryRegion *isa_address_space(ISADevice *dev);
MemoryRegion *isa_address_space_io(ISADevice *dev);
ISADevice *isa_create(ISABus *bus, const char *name);
ISADevice *isa_try_create(ISABus *bus, const char *name);
ISADevice *isa_create_simple(ISABus *bus, const char *name);

ISADevice *isa_vga_init(ISABus *bus);
# 63 "/home/gabriel/repos/qemu/include/hw/isa/isa.h"
void isa_register_ioport(ISADevice *dev, MemoryRegion *io, uint16_t start);
# 79 "/home/gabriel/repos/qemu/include/hw/isa/isa.h"
void isa_register_portio_list(ISADevice *dev, uint16_t start,
                              const MemoryRegionPortio *portio,
                              void *opaque, const char *name);

static __attribute__ (( always_inline )) __inline__ ISABus *isa_bus_from_device(ISADevice *d)
{
    return ((ISABus *)object_dynamic_cast_assert(((Object *)((qdev_get_parent_bus(((DeviceState *)object_dynamic_cast_assert(((Object *)((d))), ("device"))))))), ("ISA")));
}

extern hwaddr isa_mem_base;

void isa_mmio_setup(MemoryRegion *mr, hwaddr size);
void isa_mmio_init(hwaddr base, hwaddr size);


int DMA_get_channel_mode (int nchan);
int DMA_read_memory (int nchan, void *buf, int pos, int size);
int DMA_write_memory (int nchan, void *buf, int pos, int size);
void DMA_hold_DREQ (int nchan);
void DMA_release_DREQ (int nchan);
void DMA_schedule(int nchan);
void DMA_init(int high_page_enable, qemu_irq *cpu_request_exit);
void DMA_register_channel (int nchan,
                           DMA_transfer_handler transfer_handler,
                           void *opaque);
# 8 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 2
# 1 "/home/gabriel/repos/qemu/include/hw/block/fdc.h" 1
# 9 "/home/gabriel/repos/qemu/include/hw/block/fdc.h"
typedef enum FDriveType {
    FDRIVE_DRV_144 = 0x00,
    FDRIVE_DRV_288 = 0x01,
    FDRIVE_DRV_120 = 0x02,
    FDRIVE_DRV_NONE = 0x03,
} FDriveType;

ISADevice *fdctrl_init_isa(ISABus *bus, DriveInfo **fds);
void fdctrl_init_sysbus(qemu_irq irq, int dma_chann,
                        hwaddr mmio_base, DriveInfo **fds);
void sun4m_fdctrl_init(qemu_irq irq, hwaddr io_base,
                       DriveInfo **fds, qemu_irq *fdc_tc);

FDriveType isa_fdc_get_drive_type(ISADevice *fdc, int i);
# 9 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 2
# 1 "/home/gabriel/repos/qemu/include/net/net.h" 1







# 1 "/home/gabriel/repos/qemu/include/net/queue.h" 1
# 29 "/home/gabriel/repos/qemu/include/net/queue.h"
typedef struct NetPacket NetPacket;
typedef struct NetQueue NetQueue;

typedef void (NetPacketSent) (NetClientState *sender, ssize_t ret);




NetQueue *qemu_new_net_queue(void *opaque);

void qemu_del_net_queue(NetQueue *queue);

ssize_t qemu_net_queue_send(NetQueue *queue,
                            NetClientState *sender,
                            unsigned flags,
                            const uint8_t *data,
                            size_t size,
                            NetPacketSent *sent_cb);

ssize_t qemu_net_queue_send_iov(NetQueue *queue,
                                NetClientState *sender,
                                unsigned flags,
                                const struct iovec *iov,
                                int iovcnt,
                                NetPacketSent *sent_cb);

void qemu_net_queue_purge(NetQueue *queue, NetClientState *from);
_Bool qemu_net_queue_flush(NetQueue *queue);
# 9 "/home/gabriel/repos/qemu/include/net/net.h" 2
# 19 "/home/gabriel/repos/qemu/include/net/net.h"
struct MACAddr {
    uint8_t a[6];
};



typedef struct NICPeers {
    NetClientState *ncs[1024];
} NICPeers;

typedef struct NICConf {
    MACAddr macaddr;
    NICPeers peers;
    int32_t bootindex;
    int32_t queues;
} NICConf;
# 45 "/home/gabriel/repos/qemu/include/net/net.h"
typedef void (NetPoll)(NetClientState *, _Bool enable);
typedef int (NetCanReceive)(NetClientState *);
typedef ssize_t (NetReceive)(NetClientState *, const uint8_t *, size_t);
typedef ssize_t (NetReceiveIOV)(NetClientState *, const struct iovec *, int);
typedef void (NetCleanup) (NetClientState *);
typedef void (LinkStatusChanged)(NetClientState *);
typedef void (NetClientDestructor)(NetClientState *);

typedef struct NetClientInfo {
    NetClientOptionsKind type;
    size_t size;
    NetReceive *receive;
    NetReceive *receive_raw;
    NetReceiveIOV *receive_iov;
    NetCanReceive *can_receive;
    NetCleanup *cleanup;
    LinkStatusChanged *link_status_changed;
    NetPoll *poll;
} NetClientInfo;

struct NetClientState {
    NetClientInfo *info;
    int link_down;
    struct { struct NetClientState *tqe_next; struct NetClientState * *tqe_prev; } next;
    NetClientState *peer;
    NetQueue *send_queue;
    char *model;
    char *name;
    char info_str[256];
    unsigned receive_disabled : 1;
    NetClientDestructor *destructor;
    unsigned int queue_index;
};

typedef struct NICState {
    NetClientState *ncs;
    NICConf *conf;
    void *opaque;
    _Bool peer_deleted;
} NICState;

NetClientState *qemu_find_netdev(const char *id);
int qemu_find_net_clients_except(const char *id, NetClientState **ncs,
                                 NetClientOptionsKind type, int max);
NetClientState *qemu_new_net_client(NetClientInfo *info,
                                    NetClientState *peer,
                                    const char *model,
                                    const char *name);
NICState *qemu_new_nic(NetClientInfo *info,
                       NICConf *conf,
                       const char *model,
                       const char *name,
                       void *opaque);
void qemu_del_nic(NICState *nic);
NetClientState *qemu_get_subqueue(NICState *nic, int queue_index);
NetClientState *qemu_get_queue(NICState *nic);
NICState *qemu_get_nic(NetClientState *nc);
void *qemu_get_nic_opaque(NetClientState *nc);
void qemu_del_net_client(NetClientState *nc);
NetClientState *qemu_find_vlan_client_by_name(Monitor *mon, int vlan_id,
                                              const char *client_str);
typedef void (*qemu_nic_foreach)(NICState *nic, void *opaque);
void qemu_foreach_nic(qemu_nic_foreach func, void *opaque);
int qemu_can_send_packet(NetClientState *nc);
ssize_t qemu_sendv_packet(NetClientState *nc, const struct iovec *iov,
                          int iovcnt);
ssize_t qemu_sendv_packet_async(NetClientState *nc, const struct iovec *iov,
                                int iovcnt, NetPacketSent *sent_cb);
void qemu_send_packet(NetClientState *nc, const uint8_t *buf, int size);
ssize_t qemu_send_packet_raw(NetClientState *nc, const uint8_t *buf, int size);
ssize_t qemu_send_packet_async(NetClientState *nc, const uint8_t *buf,
                               int size, NetPacketSent *sent_cb);
void qemu_purge_queued_packets(NetClientState *nc);
void qemu_flush_queued_packets(NetClientState *nc);
void qemu_format_nic_info_str(NetClientState *nc, uint8_t macaddr[6]);
void qemu_macaddr_default_if_unset(MACAddr *macaddr);
int qemu_show_nic_models(const char *arg, const char *const *models);
void qemu_check_nic_model(NICInfo *nd, const char *model);
int qemu_find_nic_model(NICInfo *nd, const char * const *models,
                        const char *default_model);

ssize_t qemu_deliver_packet(NetClientState *sender,
                            unsigned flags,
                            const uint8_t *data,
                            size_t size,
                            void *opaque);
ssize_t qemu_deliver_packet_iov(NetClientState *sender,
                            unsigned flags,
                            const struct iovec *iov,
                            int iovcnt,
                            void *opaque);

void print_net_client(Monitor *mon, NetClientState *nc);
void do_info_network(Monitor *mon, const QDict *qdict);





struct NICInfo {
    MACAddr macaddr;
    char *model;
    char *name;
    char *devaddr;
    NetClientState *netdev;
    int used;
    int instantiated;
    int nvectors;
};

extern int nb_nics;
extern NICInfo nd_table[8];
extern int default_net;


extern const char *legacy_tftp_prefix;
extern const char *legacy_bootp_filename;

int net_client_init(QemuOpts *opts, int is_netdev, Error **errp);
int net_client_parse(QemuOptsList *opts_list, const char *str);
int net_init_clients(void);
void net_check_clients(void);
void net_cleanup(void);
void net_host_device_add(Monitor *mon, const QDict *qdict);
void net_host_device_remove(Monitor *mon, const QDict *qdict);
void netdev_add(QemuOpts *opts, Error **errp);
int qmp_netdev_add(Monitor *mon, const QDict *qdict, QObject **ret);

int net_hub_id_for_client(NetClientState *nc, int *id);
NetClientState *net_hub_port_find(int hub_id);






void qdev_set_nic_properties(DeviceState *dev, NICInfo *nd);


unsigned compute_mcast_idx(const uint8_t *ep);
# 10 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 2

# 1 "/home/gabriel/repos/qemu/include/hw/i386/ioapic.h" 1
# 25 "/home/gabriel/repos/qemu/include/hw/i386/ioapic.h"
void ioapic_eoi_broadcast(int vector);
# 12 "/home/gabriel/repos/qemu/include/hw/i386/pc.h" 2




static __attribute__ (( always_inline )) __inline__ _Bool parallel_init(ISABus *bus, int index, CharDriverState *chr)
{
    ISADevice *dev;

    dev = isa_try_create(bus, "isa-parallel");
    if (!dev) {
        return 0;
    }
    qdev_prop_set_uint32(&dev->qdev, "index", index);
    qdev_prop_set_chr(&dev->qdev, "chardev", chr);
    if (qdev_init(&dev->qdev) < 0) {
        return 0;
    }
    return 1;
}

_Bool parallel_mm_init(MemoryRegion *address_space,
                      hwaddr base, int it_shift, qemu_irq irq,
                      CharDriverState *chr);



extern DeviceState *isa_pic;
qemu_irq *i8259_init(ISABus *bus, qemu_irq parent_irq);
qemu_irq *kvm_i8259_init(ISABus *bus);
int pic_read_irq(DeviceState *d);
int pic_get_output(DeviceState *d);
void pic_info(Monitor *mon, const QDict *qdict);
void irq_info(Monitor *mon, const QDict *qdict);





typedef struct GSIState {
    qemu_irq i8259_irq[16];
    qemu_irq ioapic_irq[24];
} GSIState;

void gsi_handler(void *opaque, int n, int level);


static __attribute__ (( always_inline )) __inline__ void vmport_init(ISABus *bus)
{
    isa_create_simple(bus, "vmport");
}
void vmport_register(unsigned char command, IOPortReadFunc *func, void *opaque);
void vmmouse_get_data(uint32_t *data);
void vmmouse_set_data(const uint32_t *data);



void i8042_init(qemu_irq kbd_irq, qemu_irq mouse_irq, uint32_t io_base);
void i8042_mm_init(qemu_irq kbd_irq, qemu_irq mouse_irq,
                   MemoryRegion *region, ram_addr_t size,
                   hwaddr mask);
void i8042_isa_mouse_fake_event(void *opaque);
void i8042_setup_a20_line(ISADevice *dev, qemu_irq *a20_out);


extern int fd_bootchk;

void pc_register_ferr_irq(qemu_irq irq);
void pc_acpi_smi_interrupt(void *opaque, int irq, int level);

void pc_cpus_init(const char *cpu_model);
void pc_acpi_init(const char *default_dsdt);
void *pc_memory_init(MemoryRegion *system_memory,
                    const char *kernel_filename,
                    const char *kernel_cmdline,
                    const char *initrd_filename,
                    ram_addr_t below_4g_mem_size,
                    ram_addr_t above_4g_mem_size,
                    MemoryRegion *rom_memory,
                    MemoryRegion **ram_memory);
qemu_irq *pc_allocate_cpu_irq(void);
DeviceState *pc_vga_init(ISABus *isa_bus, PCIBus *pci_bus);
void pc_basic_device_init(ISABus *isa_bus, qemu_irq *gsi,
                          ISADevice **rtc_state,
                          ISADevice **floppy,
                          _Bool no_vmport);
void pc_init_ne2k_isa(ISABus *bus, NICInfo *nd);
void pc_cmos_init(ram_addr_t ram_size, ram_addr_t above_4g_mem_size,
                  const char *boot_device,
                  ISADevice *floppy, BusState *ide0, BusState *ide1,
                  ISADevice *s);
void pc_nic_init(ISABus *isa_bus, PCIBus *pci_bus);
void pc_pci_device_init(PCIBus *pci_bus);

typedef void (*cpu_set_smm_t)(int smm, void *arg);
void cpu_smm_register(cpu_set_smm_t callback, void *arg);

void ioapic_init_gsi(GSIState *gsi_state, const char *parent_name);



i2c_bus *piix4_pm_init(PCIBus *bus, int devfn, uint32_t smb_io_base,
                       qemu_irq sci_irq, qemu_irq smi_irq,
                       int kvm_enabled, void *fw_cfg);
void piix4_smbus_register_device(SMBusDevice *dev, uint8_t addr);


extern int no_hpet;


struct PCII440FXState;
typedef struct PCII440FXState PCII440FXState;

PCIBus *i440fx_init(PCII440FXState **pi440fx_state, int *piix_devfn,
                    ISABus **isa_bus, qemu_irq *pic,
                    MemoryRegion *address_space_mem,
                    MemoryRegion *address_space_io,
                    ram_addr_t ram_size,
                    hwaddr pci_hole_start,
                    hwaddr pci_hole_size,
                    hwaddr pci_hole64_start,
                    hwaddr pci_hole64_size,
                    MemoryRegion *pci_memory,
                    MemoryRegion *ram_memory);


extern PCIDevice *piix4_dev;
int piix4_init(PCIBus *bus, ISABus **isa_bus, int devfn);


enum vga_retrace_method {
    VGA_RETRACE_DUMB,
    VGA_RETRACE_PRECISE
};

extern enum vga_retrace_method vga_retrace_method;

int isa_vga_mm_init(hwaddr vram_base,
                    hwaddr ctrl_base, int it_shift,
                    MemoryRegion *address_space);


static __attribute__ (( always_inline )) __inline__ _Bool isa_ne2000_init(ISABus *bus, int base, int irq, NICInfo *nd)
{
    ISADevice *dev;

    qemu_check_nic_model(nd, "ne2k_isa");

    dev = isa_try_create(bus, "ne2k_isa");
    if (!dev) {
        return 0;
    }
    qdev_prop_set_uint32(&dev->qdev, "iobase", base);
    qdev_prop_set_uint32(&dev->qdev, "irq", irq);
    qdev_set_nic_properties(&dev->qdev, nd);
    qdev_init_nofail(&dev->qdev);
    return 1;
}


void pc_system_firmware_init(MemoryRegion *rom_memory);
# 180 "/home/gabriel/repos/qemu/include/hw/i386/pc.h"
int e820_add_entry(uint64_t, uint64_t, uint32_t);
# 23 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/hw/isa/apm.h" 1
# 9 "/home/gabriel/repos/qemu/include/hw/isa/apm.h"
typedef void (*apm_ctrl_changed_t)(uint32_t val, void *arg);

typedef struct APMState {
    uint8_t apmc;
    uint8_t apms;

    apm_ctrl_changed_t callback;
    void *arg;
    MemoryRegion io;
} APMState;

void apm_init(PCIDevice *dev, APMState *s, apm_ctrl_changed_t callback,
              void *arg);

extern const VMStateDescription vmstate_apm;
# 24 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/hw/i2c/pm_smbus.h" 1



typedef struct PMSMBus {
    i2c_bus *smbus;
    MemoryRegion io;

    uint8_t smb_stat;
    uint8_t smb_ctl;
    uint8_t smb_cmd;
    uint8_t smb_addr;
    uint8_t smb_data0;
    uint8_t smb_data1;
    uint8_t smb_data[32];
    uint8_t smb_index;
} PMSMBus;

void pm_smbus_init(DeviceState *parent, PMSMBus *smb);
# 25 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/hw/pci/pci.h" 1







# 1 "/home/gabriel/repos/qemu/include/sysemu/dma.h" 1
# 16 "/home/gabriel/repos/qemu/include/sysemu/dma.h"
# 1 "/home/gabriel/repos/qemu/include/block/block.h" 1






# 1 "/home/gabriel/repos/qemu/include/block/coroutine.h" 1
# 20 "/home/gabriel/repos/qemu/include/block/coroutine.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/timer.h" 1




# 1 "/home/gabriel/repos/qemu/include/qemu/main-loop.h" 1
# 45 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
int qemu_init_main_loop(void);
# 82 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
int main_loop_wait(int nonblocking);




AioContext *qemu_get_aio_context(void);
# 102 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
void qemu_notify_event(void);
# 171 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
typedef void IOReadHandler(void *opaque, const uint8_t *buf, int size);
typedef int IOCanReadHandler(void *opaque);
# 214 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
int qemu_set_fd_handler2(int fd,
                         IOCanReadHandler *fd_read_poll,
                         IOHandler *fd_read,
                         IOHandler *fd_write,
                         void *opaque);
# 248 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
int qemu_set_fd_handler(int fd,
                        IOHandler *fd_read,
                        IOHandler *fd_write,
                        void *opaque);
# 269 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
int qemu_add_child_watch(pid_t pid);
# 285 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
void qemu_mutex_lock_iothread(void);
# 300 "/home/gabriel/repos/qemu/include/qemu/main-loop.h"
void qemu_mutex_unlock_iothread(void);



void qemu_fd_register(int fd);
void qemu_iohandler_fill(GArray *pollfds);
void qemu_iohandler_poll(GArray *pollfds, int rc);

QEMUBH *qemu_bh_new(QEMUBHFunc *cb, void *opaque);
void qemu_bh_schedule_idle(QEMUBH *bh);
# 6 "/home/gabriel/repos/qemu/include/qemu/timer.h" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/notify.h" 1
# 19 "/home/gabriel/repos/qemu/include/qemu/notify.h"
typedef struct Notifier Notifier;

struct Notifier
{
    void (*notify)(Notifier *notifier, void *data);
    struct { struct Notifier *le_next; struct Notifier **le_prev; } node;
};

typedef struct NotifierList
{
    struct { struct Notifier *lh_first; } notifiers;
} NotifierList;




void notifier_list_init(NotifierList *list);

void notifier_list_add(NotifierList *list, Notifier *notifier);

void notifier_remove(Notifier *notifier);

void notifier_list_notify(NotifierList *list, void *data);
# 7 "/home/gabriel/repos/qemu/include/qemu/timer.h" 2
# 18 "/home/gabriel/repos/qemu/include/qemu/timer.h"
typedef struct QEMUClock QEMUClock;
typedef void QEMUTimerCB(void *opaque);





extern QEMUClock *rt_clock;




extern QEMUClock *vm_clock;






extern QEMUClock *host_clock;

int64_t qemu_get_clock_ns(QEMUClock *clock);
int64_t qemu_clock_has_timers(QEMUClock *clock);
int64_t qemu_clock_expired(QEMUClock *clock);
int64_t qemu_clock_deadline(QEMUClock *clock);
void qemu_clock_enable(QEMUClock *clock, _Bool enabled);
void qemu_clock_warp(QEMUClock *clock);

void qemu_register_clock_reset_notifier(QEMUClock *clock, Notifier *notifier);
void qemu_unregister_clock_reset_notifier(QEMUClock *clock,
                                          Notifier *notifier);

QEMUTimer *qemu_new_timer(QEMUClock *clock, int scale,
                          QEMUTimerCB *cb, void *opaque);
void qemu_free_timer(QEMUTimer *ts);
void qemu_del_timer(QEMUTimer *ts);
void qemu_mod_timer_ns(QEMUTimer *ts, int64_t expire_time);
void qemu_mod_timer(QEMUTimer *ts, int64_t expire_time);
_Bool qemu_timer_pending(QEMUTimer *ts);
_Bool qemu_timer_expired(QEMUTimer *timer_head, int64_t current_time);
uint64_t qemu_timer_expire_time_ns(QEMUTimer *ts);

void qemu_run_timers(QEMUClock *clock);
void qemu_run_all_timers(void);
void configure_alarms(char const *opt);
void init_clocks(void);
int init_timer_alarm(void);

int64_t cpu_get_ticks(void);
void cpu_enable_ticks(void);
void cpu_disable_ticks(void);

static __attribute__ (( always_inline )) __inline__ QEMUTimer *qemu_new_timer_ns(QEMUClock *clock, QEMUTimerCB *cb,
                                           void *opaque)
{
    return qemu_new_timer(clock, 1, cb, opaque);
}

static __attribute__ (( always_inline )) __inline__ QEMUTimer *qemu_new_timer_ms(QEMUClock *clock, QEMUTimerCB *cb,
                                           void *opaque)
{
    return qemu_new_timer(clock, 1000000, cb, opaque);
}

static __attribute__ (( always_inline )) __inline__ int64_t qemu_get_clock_ms(QEMUClock *clock)
{
    return qemu_get_clock_ns(clock) / 1000000;
}

static __attribute__ (( always_inline )) __inline__ int64_t get_ticks_per_sec(void)
{
    return 1000000000LL;
}


static __attribute__ (( always_inline )) __inline__ int64_t get_clock_realtime(void)
{
    struct timeval tv;

    gettimeofday(&tv, ((void *)0));
    return tv.tv_sec * 1000000000LL + (tv.tv_usec * 1000);
}
# 116 "/home/gabriel/repos/qemu/include/qemu/timer.h"
extern int use_rt_clock;

static __attribute__ (( always_inline )) __inline__ int64_t get_clock(void)
{

    if (use_rt_clock) {
        struct timespec ts;
        clock_gettime(1, &ts);
        return ts.tv_sec * 1000000000LL + ts.tv_nsec;
    } else

    {


        return get_clock_realtime();
    }
}


void qemu_get_timer(QEMUFile *f, QEMUTimer *ts);
void qemu_put_timer(QEMUFile *f, QEMUTimer *ts);


int64_t cpu_get_icount(void);
int64_t cpu_get_clock(void);
# 182 "/home/gabriel/repos/qemu/include/qemu/timer.h"
static __attribute__ (( always_inline )) __inline__ int64_t cpu_get_real_ticks(void)
{
    uint32_t low,high;
    int64_t val;
    asm volatile("rdtsc" : "=a" (low), "=d" (high));
    val = high;
    val <<= 32;
    val |= low;
    return val;
}
# 21 "/home/gabriel/repos/qemu/include/block/coroutine.h" 2

# 1 "/home/gabriel/repos/qemu/include/cpc/cpc_runtime.h" 1
# 29 "/home/gabriel/repos/qemu/include/cpc/cpc_runtime.h"
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.7/include/stddef.h" 1 3 4
# 30 "/home/gabriel/repos/qemu/include/cpc/cpc_runtime.h" 2
# 57 "/home/gabriel/repos/qemu/include/cpc/cpc_runtime.h"
typedef struct cpc_condvar cpc_condvar;
typedef struct cpc_sched cpc_sched;
extern cpc_sched *cpc_default_threadpool;


typedef struct cpc_continuation {
    unsigned short length;
    unsigned short size;



    char c[1];
} cpc_continuation;

extern void cpc_print_continuation(struct cpc_continuation *c, char *s);

typedef cpc_continuation *cpc_function(void*);

struct cpc_continuation *cpc_continuation_expand(struct cpc_continuation *c,
                                                 int n);

static __attribute__ (( always_inline )) __inline__ void*
cpc_alloc(struct cpc_continuation **cp, int s)
{
    struct cpc_continuation *c;
    void *p;

    c = *cp;
    if(c == (void*)0 || s > c->size - c->length)
        c = cpc_continuation_expand(c, s);
    p = c->c + c->length;
    c->length += s;
    *cp = c;
    return p;
}

static __attribute__ (( always_inline )) __inline__ void*
cpc_dealloc(struct cpc_continuation *c, int s)
{
    c->length -= s;
    return c->c + c->length;
}






static __attribute__ (( always_inline )) __inline__ struct cpc_continuation *
cpc_continuation_push(cpc_continuation *c, cpc_function *f)
{

    if(c == (void*)0 || 16 > c->size - c->length)
        c = cpc_continuation_expand(c, 16);

    *(cpc_function**)(c->c + c->length) = f;
    c->length += 16;
    return c;
}

static __attribute__ (( always_inline )) __inline__ void
cpc_continuation_patch(cpc_continuation *cont, size_t size, const void *value)
{
  void *cpc_arg;
  cpc_arg =




    ((cont)->c + (cont)->length - 16 - ((size - 1) / 16 + 1) * 16);

  __builtin_memcpy(cpc_arg, value, size);
  return;
}
# 23 "/home/gabriel/repos/qemu/include/block/coroutine.h" 2
# 50 "/home/gabriel/repos/qemu/include/block/coroutine.h"
typedef struct Coroutine Coroutine;
# 61 "/home/gabriel/repos/qemu/include/block/coroutine.h"
typedef void CoroutineEntry(void *opaque);






Coroutine *qemu_coroutine_create(CoroutineEntry *entry);







void qemu_coroutine_enter(Coroutine *coroutine, void *opaque);







void qemu_coroutine_yield(void);




Coroutine * qemu_coroutine_self(void);
# 98 "/home/gabriel/repos/qemu/include/block/coroutine.h"
_Bool qemu_in_coroutine(void);
# 107 "/home/gabriel/repos/qemu/include/block/coroutine.h"
typedef struct CoQueue {
    struct { struct Coroutine *tqh_first; struct Coroutine * *tqh_last; } entries;
    AioContext *ctx;
} CoQueue;





void qemu_co_queue_init(CoQueue *queue);





void qemu_co_queue_wait(CoQueue *queue);





void qemu_co_queue_wait_insert_head(CoQueue *queue);






_Bool qemu_co_queue_next(CoQueue *queue);




void qemu_co_queue_restart_all(CoQueue *queue);




_Bool qemu_co_queue_empty(CoQueue *queue);





typedef struct CoMutex {
    _Bool locked;
    CoQueue queue;
} CoMutex;





void qemu_co_mutex_init(CoMutex *mutex);





void qemu_co_mutex_lock(CoMutex *mutex);





void qemu_co_mutex_unlock(CoMutex *mutex);

typedef struct CoRwlock {
    _Bool writer;
    int reader;
    CoQueue queue;
} CoRwlock;





void qemu_co_rwlock_init(CoRwlock *lock);






void qemu_co_rwlock_rdlock(CoRwlock *lock);






void qemu_co_rwlock_wrlock(CoRwlock *lock);





void qemu_co_rwlock_unlock(CoRwlock *lock);







void co_sleep_ns(QEMUClock *clock, int64_t ns);
# 8 "/home/gabriel/repos/qemu/include/block/block.h" 2




typedef struct BlockDriver BlockDriver;
typedef struct BlockJob BlockJob;

typedef struct BlockDriverInfo {

    int cluster_size;

    int64_t vm_state_offset;
    _Bool is_dirty;
} BlockDriverInfo;

typedef struct BlockFragInfo {
    uint64_t allocated_clusters;
    uint64_t total_clusters;
    uint64_t fragmented_clusters;
    uint64_t compressed_clusters;
} BlockFragInfo;

typedef struct QEMUSnapshotInfo {
    char id_str[128];


    char name[256];
    uint64_t vm_state_size;
    uint32_t date_sec;
    uint32_t date_nsec;
    uint64_t vm_clock_nsec;
} QEMUSnapshotInfo;


typedef struct BlockDevOps {







    void (*change_media_cb)(void *opaque, _Bool load);
# 59 "/home/gabriel/repos/qemu/include/block/block.h"
    void (*eject_request_cb)(void *opaque, _Bool force);




    _Bool (*is_tray_open)(void *opaque);




    _Bool (*is_medium_locked)(void *opaque);



    void (*resize_cb)(void *opaque);
} BlockDevOps;
# 95 "/home/gabriel/repos/qemu/include/block/block.h"
typedef enum {
    BDRV_ACTION_REPORT, BDRV_ACTION_IGNORE, BDRV_ACTION_STOP
} BlockErrorAction;

typedef struct BlockReopenQueue { struct BlockReopenQueueEntry *sqh_first; struct BlockReopenQueueEntry **sqh_last; } BlockReopenQueue;

typedef struct BDRVReopenState {
    BlockDriverState *bs;
    int flags;
    void *opaque;
} BDRVReopenState;


void bdrv_iostatus_enable(BlockDriverState *bs);
void bdrv_iostatus_reset(BlockDriverState *bs);
void bdrv_iostatus_disable(BlockDriverState *bs);
_Bool bdrv_iostatus_is_enabled(const BlockDriverState *bs);
void bdrv_iostatus_set_err(BlockDriverState *bs, int error);
void bdrv_info_print(Monitor *mon, const QObject *data);
void bdrv_info(Monitor *mon, QObject **ret_data);
void bdrv_stats_print(Monitor *mon, const QObject *data);
void bdrv_info_stats(Monitor *mon, QObject **ret_data);


void bdrv_io_limits_enable(BlockDriverState *bs);
void bdrv_io_limits_disable(BlockDriverState *bs);
_Bool bdrv_io_limits_enabled(BlockDriverState *bs);

void bdrv_init(void);
void bdrv_init_with_whitelist(void);
BlockDriver *bdrv_find_protocol(const char *filename);
BlockDriver *bdrv_find_format(const char *format_name);
BlockDriver *bdrv_find_whitelisted_format(const char *format_name);
int bdrv_create(BlockDriver *drv, const char* filename,
    QEMUOptionParameter *options);
int bdrv_create_file(const char* filename, QEMUOptionParameter *options);
BlockDriverState *bdrv_new(const char *device_name);
void bdrv_make_anon(BlockDriverState *bs);
void bdrv_swap(BlockDriverState *bs_new, BlockDriverState *bs_old);
void bdrv_append(BlockDriverState *bs_new, BlockDriverState *bs_top);
void bdrv_delete(BlockDriverState *bs);
int bdrv_parse_cache_flags(const char *mode, int *flags);
int bdrv_parse_discard_flags(const char *mode, int *flags);
int bdrv_file_open(BlockDriverState **pbs, const char *filename,
                   QDict *options, int flags);
int bdrv_open_backing_file(BlockDriverState *bs);
int bdrv_open(BlockDriverState *bs, const char *filename, QDict *options,
              int flags, BlockDriver *drv);
BlockReopenQueue *bdrv_reopen_queue(BlockReopenQueue *bs_queue,
                                    BlockDriverState *bs, int flags);
int bdrv_reopen_multiple(BlockReopenQueue *bs_queue, Error **errp);
int bdrv_reopen(BlockDriverState *bs, int bdrv_flags, Error **errp);
int bdrv_reopen_prepare(BDRVReopenState *reopen_state,
                        BlockReopenQueue *queue, Error **errp);
void bdrv_reopen_commit(BDRVReopenState *reopen_state);
void bdrv_reopen_abort(BDRVReopenState *reopen_state);
void bdrv_close(BlockDriverState *bs);
void bdrv_add_close_notifier(BlockDriverState *bs, Notifier *notify);
int bdrv_attach_dev(BlockDriverState *bs, void *dev);
void bdrv_attach_dev_nofail(BlockDriverState *bs, void *dev);
void bdrv_detach_dev(BlockDriverState *bs, void *dev);
void *bdrv_get_attached_dev(BlockDriverState *bs);
void bdrv_set_dev_ops(BlockDriverState *bs, const BlockDevOps *ops,
                      void *opaque);
void bdrv_dev_eject_request(BlockDriverState *bs, _Bool force);
_Bool bdrv_dev_has_removable_media(BlockDriverState *bs);
_Bool bdrv_dev_is_tray_open(BlockDriverState *bs);
_Bool bdrv_dev_is_medium_locked(BlockDriverState *bs);
int bdrv_read(BlockDriverState *bs, int64_t sector_num,
              uint8_t *buf, int nb_sectors);
int bdrv_read_unthrottled(BlockDriverState *bs, int64_t sector_num,
                          uint8_t *buf, int nb_sectors);
int bdrv_write(BlockDriverState *bs, int64_t sector_num,
               const uint8_t *buf, int nb_sectors);
int bdrv_writev(BlockDriverState *bs, int64_t sector_num, QEMUIOVector *qiov);
int bdrv_pread(BlockDriverState *bs, int64_t offset,
               void *buf, int count);
int bdrv_pwrite(BlockDriverState *bs, int64_t offset,
                const void *buf, int count);
int bdrv_pwritev(BlockDriverState *bs, int64_t offset, QEMUIOVector *qiov);
int bdrv_pwrite_sync(BlockDriverState *bs, int64_t offset,
    const void *buf, int count);
int bdrv_co_readv(BlockDriverState *bs, int64_t sector_num,
    int nb_sectors, QEMUIOVector *qiov);
int bdrv_co_copy_on_readv(BlockDriverState *bs,
    int64_t sector_num, int nb_sectors, QEMUIOVector *qiov);
int bdrv_co_writev(BlockDriverState *bs, int64_t sector_num,
    int nb_sectors, QEMUIOVector *qiov);






int bdrv_co_write_zeroes(BlockDriverState *bs, int64_t sector_num,
    int nb_sectors);
int bdrv_co_is_allocated(BlockDriverState *bs, int64_t sector_num,
    int nb_sectors, int *pnum);
int bdrv_co_is_allocated_above(BlockDriverState *top,
                                            BlockDriverState *base,
                                            int64_t sector_num,
                                            int nb_sectors, int *pnum);
BlockDriverState *bdrv_find_backing_image(BlockDriverState *bs,
    const char *backing_file);
int bdrv_get_backing_file_depth(BlockDriverState *bs);
int bdrv_truncate(BlockDriverState *bs, int64_t offset);
int64_t bdrv_getlength(BlockDriverState *bs);
int64_t bdrv_get_allocated_file_size(BlockDriverState *bs);
void bdrv_get_geometry(BlockDriverState *bs, uint64_t *nb_sectors_ptr);
int bdrv_commit(BlockDriverState *bs);
int bdrv_commit_all(void);
int bdrv_change_backing_file(BlockDriverState *bs,
    const char *backing_file, const char *backing_fmt);
void bdrv_register(BlockDriver *bdrv);
int bdrv_drop_intermediate(BlockDriverState *active, BlockDriverState *top,
                           BlockDriverState *base);
BlockDriverState *bdrv_find_overlay(BlockDriverState *active,
                                    BlockDriverState *bs);
BlockDriverState *bdrv_find_base(BlockDriverState *bs);


typedef struct BdrvCheckResult {
    int corruptions;
    int leaks;
    int check_errors;
    int corruptions_fixed;
    int leaks_fixed;
    int64_t image_end_offset;
    BlockFragInfo bfi;
} BdrvCheckResult;

typedef enum {
    BDRV_FIX_LEAKS = 1,
    BDRV_FIX_ERRORS = 2,
} BdrvCheckMode;

int bdrv_check(BlockDriverState *bs, BdrvCheckResult *res, BdrvCheckMode fix);


typedef void BlockDriverDirtyHandler(BlockDriverState *bs, int64_t sector,
                                     int sector_num);
BlockDriverAIOCB *bdrv_aio_readv(BlockDriverState *bs, int64_t sector_num,
                                 QEMUIOVector *iov, int nb_sectors,
                                 BlockDriverCompletionFunc *cb, void *opaque);
BlockDriverAIOCB *bdrv_aio_writev(BlockDriverState *bs, int64_t sector_num,
                                  QEMUIOVector *iov, int nb_sectors,
                                  BlockDriverCompletionFunc *cb, void *opaque);
BlockDriverAIOCB *bdrv_aio_flush(BlockDriverState *bs,
                                 BlockDriverCompletionFunc *cb, void *opaque);
BlockDriverAIOCB *bdrv_aio_discard(BlockDriverState *bs,
                                   int64_t sector_num, int nb_sectors,
                                   BlockDriverCompletionFunc *cb, void *opaque);
void bdrv_aio_cancel(BlockDriverAIOCB *acb);

typedef struct BlockRequest {

    int64_t sector;
    int nb_sectors;
    QEMUIOVector *qiov;
    BlockDriverCompletionFunc *cb;
    void *opaque;


    int error;
} BlockRequest;

int bdrv_aio_multiwrite(BlockDriverState *bs, BlockRequest *reqs,
    int num_reqs);


int bdrv_ioctl(BlockDriverState *bs, unsigned long int req, void *buf);
BlockDriverAIOCB *bdrv_aio_ioctl(BlockDriverState *bs,
        unsigned long int req, void *buf,
        BlockDriverCompletionFunc *cb, void *opaque);


void bdrv_invalidate_cache(BlockDriverState *bs);
void bdrv_invalidate_cache_all(void);

void bdrv_clear_incoming_migration_all(void);


int bdrv_flush(BlockDriverState *bs);
int bdrv_co_flush(BlockDriverState *bs);
void bdrv_flush_all(void);
void bdrv_close_all(void);
void bdrv_drain_all(void);

int bdrv_discard(BlockDriverState *bs, int64_t sector_num, int nb_sectors);
int bdrv_co_discard(BlockDriverState *bs, int64_t sector_num, int nb_sectors);
int bdrv_has_zero_init(BlockDriverState *bs);
int bdrv_is_allocated(BlockDriverState *bs, int64_t sector_num, int nb_sectors,
                      int *pnum);
int bdrv_is_allocated_above(BlockDriverState *top, BlockDriverState *base,
                            int64_t sector_num, int nb_sectors, int *pnum);

void bdrv_set_on_error(BlockDriverState *bs, BlockdevOnError on_read_error,
                       BlockdevOnError on_write_error);
BlockdevOnError bdrv_get_on_error(BlockDriverState *bs, _Bool is_read);
BlockErrorAction bdrv_get_error_action(BlockDriverState *bs, _Bool is_read, int error);
void bdrv_error_action(BlockDriverState *bs, BlockErrorAction action,
                       _Bool is_read, int error);
int bdrv_is_read_only(BlockDriverState *bs);
int bdrv_is_sg(BlockDriverState *bs);
int bdrv_enable_write_cache(BlockDriverState *bs);
void bdrv_set_enable_write_cache(BlockDriverState *bs, _Bool wce);
int bdrv_is_inserted(BlockDriverState *bs);
int bdrv_media_changed(BlockDriverState *bs);
void bdrv_lock_medium(BlockDriverState *bs, _Bool locked);
void bdrv_eject(BlockDriverState *bs, _Bool eject_flag);
const char *bdrv_get_format_name(BlockDriverState *bs);
BlockDriverState *bdrv_find(const char *name);
BlockDriverState *bdrv_next(BlockDriverState *bs);
void bdrv_iterate(void (*it)(void *opaque, BlockDriverState *bs),
                  void *opaque);
int bdrv_is_encrypted(BlockDriverState *bs);
int bdrv_key_required(BlockDriverState *bs);
int bdrv_set_key(BlockDriverState *bs, const char *key);
int bdrv_query_missing_keys(void);
void bdrv_iterate_format(void (*it)(void *opaque, const char *name),
                         void *opaque);
const char *bdrv_get_device_name(BlockDriverState *bs);
int bdrv_get_flags(BlockDriverState *bs);
int bdrv_write_compressed(BlockDriverState *bs, int64_t sector_num,
                          const uint8_t *buf, int nb_sectors);
int bdrv_get_info(BlockDriverState *bs, BlockDriverInfo *bdi);
void bdrv_round_to_clusters(BlockDriverState *bs,
                            int64_t sector_num, int nb_sectors,
                            int64_t *cluster_sector_num,
                            int *cluster_nb_sectors);

const char *bdrv_get_encrypted_filename(BlockDriverState *bs);
void bdrv_get_backing_filename(BlockDriverState *bs,
                               char *filename, int filename_size);
void bdrv_get_full_backing_filename(BlockDriverState *bs,
                                    char *dest, size_t sz);
BlockInfo *bdrv_query_info(BlockDriverState *s);
BlockStats *bdrv_query_stats(const BlockDriverState *bs);
int bdrv_can_snapshot(BlockDriverState *bs);
int bdrv_is_snapshot(BlockDriverState *bs);
BlockDriverState *bdrv_snapshots(void);
int bdrv_snapshot_create(BlockDriverState *bs,
                         QEMUSnapshotInfo *sn_info);
int bdrv_snapshot_goto(BlockDriverState *bs,
                       const char *snapshot_id);
int bdrv_snapshot_delete(BlockDriverState *bs, const char *snapshot_id);
int bdrv_snapshot_list(BlockDriverState *bs,
                       QEMUSnapshotInfo **psn_info);
int bdrv_snapshot_load_tmp(BlockDriverState *bs,
                           const char *snapshot_name);
char *bdrv_snapshot_dump(char *buf, int buf_size, QEMUSnapshotInfo *sn);

char *get_human_readable_size(char *buf, int buf_size, int64_t size);
int path_is_absolute(const char *path);
void path_combine(char *dest, int dest_size,
                  const char *base_path,
                  const char *filename);

int bdrv_writev_vmstate(BlockDriverState *bs, QEMUIOVector *qiov, int64_t pos);
int bdrv_save_vmstate(BlockDriverState *bs, const uint8_t *buf,
                      int64_t pos, int size);

int bdrv_load_vmstate(BlockDriverState *bs, uint8_t *buf,
                      int64_t pos, int size);

void bdrv_img_create(const char *filename, const char *fmt,
                     const char *base_filename, const char *base_fmt,
                     char *options, uint64_t img_size, int flags,
                     Error **errp, _Bool quiet);

void bdrv_set_buffer_alignment(BlockDriverState *bs, int align);
void *qemu_blockalign(BlockDriverState *bs, size_t size);
_Bool bdrv_qiov_is_aligned(BlockDriverState *bs, QEMUIOVector *qiov);

struct HBitmapIter;
void bdrv_set_dirty_tracking(BlockDriverState *bs, int granularity);
int bdrv_get_dirty(BlockDriverState *bs, int64_t sector);
void bdrv_set_dirty(BlockDriverState *bs, int64_t cur_sector, int nr_sectors);
void bdrv_reset_dirty(BlockDriverState *bs, int64_t cur_sector, int nr_sectors);
void bdrv_dirty_iter_init(BlockDriverState *bs, struct HBitmapIter *hbi);
int64_t bdrv_get_dirty_count(BlockDriverState *bs);

void bdrv_enable_copy_on_read(BlockDriverState *bs);
void bdrv_disable_copy_on_read(BlockDriverState *bs);

void bdrv_set_in_use(BlockDriverState *bs, int in_use);
int bdrv_in_use(BlockDriverState *bs);




static __attribute__ (( always_inline )) __inline__ int raw_get_aio_fd(BlockDriverState *bs)
{
    return -95;
}


enum BlockAcctType {
    BDRV_ACCT_READ,
    BDRV_ACCT_WRITE,
    BDRV_ACCT_FLUSH,
    BDRV_MAX_IOTYPE,
};

typedef struct BlockAcctCookie {
    int64_t bytes;
    int64_t start_time_ns;
    enum BlockAcctType type;
} BlockAcctCookie;

void bdrv_acct_start(BlockDriverState *bs, BlockAcctCookie *cookie,
        int64_t bytes, enum BlockAcctType type);
void bdrv_acct_done(BlockDriverState *bs, BlockAcctCookie *cookie);

typedef enum {
    BLKDBG_L1_UPDATE,

    BLKDBG_L1_GROW_ALLOC_TABLE,
    BLKDBG_L1_GROW_WRITE_TABLE,
    BLKDBG_L1_GROW_ACTIVATE_TABLE,

    BLKDBG_L2_LOAD,
    BLKDBG_L2_UPDATE,
    BLKDBG_L2_UPDATE_COMPRESSED,
    BLKDBG_L2_ALLOC_COW_READ,
    BLKDBG_L2_ALLOC_WRITE,

    BLKDBG_READ_AIO,
    BLKDBG_READ_BACKING_AIO,
    BLKDBG_READ_COMPRESSED,

    BLKDBG_WRITE_AIO,
    BLKDBG_WRITE_COMPRESSED,

    BLKDBG_VMSTATE_LOAD,
    BLKDBG_VMSTATE_SAVE,

    BLKDBG_COW_READ,
    BLKDBG_COW_WRITE,

    BLKDBG_REFTABLE_LOAD,
    BLKDBG_REFTABLE_GROW,

    BLKDBG_REFBLOCK_LOAD,
    BLKDBG_REFBLOCK_UPDATE,
    BLKDBG_REFBLOCK_UPDATE_PART,
    BLKDBG_REFBLOCK_ALLOC,
    BLKDBG_REFBLOCK_ALLOC_HOOKUP,
    BLKDBG_REFBLOCK_ALLOC_WRITE,
    BLKDBG_REFBLOCK_ALLOC_WRITE_BLOCKS,
    BLKDBG_REFBLOCK_ALLOC_WRITE_TABLE,
    BLKDBG_REFBLOCK_ALLOC_SWITCH_TABLE,

    BLKDBG_CLUSTER_ALLOC,
    BLKDBG_CLUSTER_ALLOC_BYTES,
    BLKDBG_CLUSTER_FREE,

    BLKDBG_EVENT_MAX,
} BlkDebugEvent;


void bdrv_debug_event(BlockDriverState *bs, BlkDebugEvent event);

int bdrv_debug_breakpoint(BlockDriverState *bs, const char *event,
                           const char *tag);
int bdrv_debug_resume(BlockDriverState *bs, const char *tag);
_Bool bdrv_debug_is_suspended(BlockDriverState *bs, const char *tag);
# 17 "/home/gabriel/repos/qemu/include/sysemu/dma.h" 2
# 1 "/home/gabriel/repos/qemu/include/sysemu/kvm.h" 1
# 17 "/home/gabriel/repos/qemu/include/sysemu/kvm.h"
# 1 "/usr/include/errno.h" 1 3 4
# 18 "/home/gabriel/repos/qemu/include/sysemu/kvm.h" 2
# 1 "./config-host.h" 1
# 19 "/home/gabriel/repos/qemu/include/sysemu/kvm.h" 2

# 1 "/home/gabriel/repos/qemu/include/qom/cpu.h" 1
# 25 "/home/gabriel/repos/qemu/include/qom/cpu.h"
# 1 "/home/gabriel/repos/qemu/include/qemu/thread.h" 1






typedef struct QemuMutex QemuMutex;
typedef struct QemuCond QemuCond;
typedef struct QemuSemaphore QemuSemaphore;
typedef struct QemuThread QemuThread;




# 1 "/home/gabriel/repos/qemu/include/qemu/thread-posix.h" 1



# 1 "/usr/include/semaphore.h" 1 3 4
# 30 "/usr/include/semaphore.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/semaphore.h" 1 3 4
# 24 "/usr/include/x86_64-linux-gnu/bits/semaphore.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 25 "/usr/include/x86_64-linux-gnu/bits/semaphore.h" 2 3 4
# 37 "/usr/include/x86_64-linux-gnu/bits/semaphore.h" 3 4
typedef union
{
  char __size[32];
  long int __align;
} sem_t;
# 31 "/usr/include/semaphore.h" 2 3 4






extern int sem_init (sem_t *__sem, int __pshared, unsigned int __value)
     __attribute__ ((__nothrow__));

extern int sem_destroy (sem_t *__sem) __attribute__ ((__nothrow__));


extern sem_t *sem_open (__const char *__name, int __oflag, ...) __attribute__ ((__nothrow__));


extern int sem_close (sem_t *__sem) __attribute__ ((__nothrow__));


extern int sem_unlink (__const char *__name) __attribute__ ((__nothrow__));





extern int sem_wait (sem_t *__sem);






extern int sem_timedwait (sem_t *__restrict __sem,
     __const struct timespec *__restrict __abstime);



extern int sem_trywait (sem_t *__sem) __attribute__ ((__nothrow__));


extern int sem_post (sem_t *__sem) __attribute__ ((__nothrow__));


extern int sem_getvalue (sem_t *__restrict __sem, int *__restrict __sval)
     __attribute__ ((__nothrow__));



# 5 "/home/gabriel/repos/qemu/include/qemu/thread-posix.h" 2

struct QemuMutex {
    pthread_mutex_t lock;
};

struct QemuCond {
    pthread_cond_t cond;
};

struct QemuSemaphore {





    sem_t sem;

};

struct QemuThread {
    pthread_t thread;
};
# 16 "/home/gabriel/repos/qemu/include/qemu/thread.h" 2





void qemu_mutex_init(QemuMutex *mutex);
void qemu_mutex_destroy(QemuMutex *mutex);
void qemu_mutex_lock(QemuMutex *mutex);
int qemu_mutex_trylock(QemuMutex *mutex);
void qemu_mutex_unlock(QemuMutex *mutex);




void qemu_cond_init(QemuCond *cond);
void qemu_cond_destroy(QemuCond *cond);






void qemu_cond_signal(QemuCond *cond);
void qemu_cond_broadcast(QemuCond *cond);
void qemu_cond_wait(QemuCond *cond, QemuMutex *mutex);

void qemu_sem_init(QemuSemaphore *sem, int init);
void qemu_sem_post(QemuSemaphore *sem);
void qemu_sem_wait(QemuSemaphore *sem);
int qemu_sem_timedwait(QemuSemaphore *sem, int ms);
void qemu_sem_destroy(QemuSemaphore *sem);

void qemu_thread_create(QemuThread *thread,
                        void *(*start_routine)(void *),
                        void *arg, int mode);
void *qemu_thread_join(QemuThread *thread);
void qemu_thread_get_self(QemuThread *thread);
_Bool qemu_thread_is_self(QemuThread *thread);
void qemu_thread_exit(void *retval);
# 26 "/home/gabriel/repos/qemu/include/qom/cpu.h" 2
# 40 "/home/gabriel/repos/qemu/include/qom/cpu.h"
typedef struct CPUState CPUState;
# 52 "/home/gabriel/repos/qemu/include/qom/cpu.h"
typedef struct CPUClass {

    DeviceClass parent_class;


    ObjectClass *(*class_by_name)(const char *cpu_model);

    void (*reset)(CPUState *cpu);
    void (*do_interrupt)(CPUState *cpu);

    const struct VMStateDescription *vmsd;
} CPUClass;

struct KVMState;
struct kvm_run;
# 89 "/home/gabriel/repos/qemu/include/qom/cpu.h"
struct CPUState {

    DeviceState parent_obj;


    int nr_cores;
    int nr_threads;
    int numa_node;

    struct QemuThread *thread;



    int thread_id;
    uint32_t host_tid;
    _Bool running;
    struct QemuCond *halt_cond;
    struct qemu_work_item *queued_work_first, *queued_work_last;
    _Bool thread_kicked;
    _Bool created;
    _Bool stop;
    _Bool stopped;
    volatile sig_atomic_t exit_request;
    volatile sig_atomic_t tcg_exit_req;
    uint32_t interrupt_request;

    void *env_ptr;
    struct TranslationBlock *current_tb;

    int kvm_fd;
    _Bool kvm_vcpu_dirty;
    struct KVMState *kvm_state;
    struct kvm_run *kvm_run;


    int cpu_index;
    uint32_t halted;
};






void cpu_reset(CPUState *cpu);
# 144 "/home/gabriel/repos/qemu/include/qom/cpu.h"
ObjectClass *cpu_class_by_name(const char *typename, const char *cpu_model);
# 158 "/home/gabriel/repos/qemu/include/qom/cpu.h"
static __attribute__ (( always_inline )) __inline__ void cpu_class_set_vmsd(CPUClass *cc,
                                      const struct VMStateDescription *value)
{
    cc->vmsd = value;
}
# 175 "/home/gabriel/repos/qemu/include/qom/cpu.h"
_Bool qemu_cpu_has_work(CPUState *cpu);
# 185 "/home/gabriel/repos/qemu/include/qom/cpu.h"
_Bool qemu_cpu_is_self(CPUState *cpu);







void qemu_cpu_kick(CPUState *cpu);
# 204 "/home/gabriel/repos/qemu/include/qom/cpu.h"
_Bool cpu_is_stopped(CPUState *cpu);
# 214 "/home/gabriel/repos/qemu/include/qom/cpu.h"
void run_on_cpu(CPUState *cpu, void (*func)(void *data), void *data);
# 224 "/home/gabriel/repos/qemu/include/qom/cpu.h"
CPUState *qemu_get_cpu(int index);



typedef void (*CPUInterruptHandler)(CPUState *, int);

extern CPUInterruptHandler cpu_interrupt_handler;
# 239 "/home/gabriel/repos/qemu/include/qom/cpu.h"
static __attribute__ (( always_inline )) __inline__ void cpu_interrupt(CPUState *cpu, int mask)
{
    cpu_interrupt_handler(cpu, mask);
}
# 257 "/home/gabriel/repos/qemu/include/qom/cpu.h"
void cpu_reset_interrupt(CPUState *cpu, int mask);
# 21 "/home/gabriel/repos/qemu/include/sysemu/kvm.h" 2
# 42 "/home/gabriel/repos/qemu/include/sysemu/kvm.h"
extern _Bool kvm_allowed;
extern _Bool kvm_kernel_irqchip;
extern _Bool kvm_async_interrupts_allowed;
extern _Bool kvm_irqfds_allowed;
extern _Bool kvm_msi_via_irqfd_allowed;
extern _Bool kvm_gsi_routing_allowed;
# 109 "/home/gabriel/repos/qemu/include/sysemu/kvm.h"
struct kvm_run;
struct kvm_lapic_state;

typedef struct KVMCapabilityInfo {
    const char *name;
    int value;
} KVMCapabilityInfo;




struct KVMState;
typedef struct KVMState KVMState;
extern KVMState *kvm_state;



int kvm_init(void);

int kvm_has_sync_mmu(void);
int kvm_has_vcpu_events(void);
int kvm_has_robust_singlestep(void);
int kvm_has_debugregs(void);
int kvm_has_xsave(void);
int kvm_has_xcrs(void);
int kvm_has_pit_state2(void);
int kvm_has_many_ioeventfds(void);
int kvm_has_gsi_routing(void);
int kvm_has_intx_set_mask(void);

int kvm_init_vcpu(CPUState *cpu);
# 286 "/home/gabriel/repos/qemu/include/sysemu/kvm.h"
int kvm_set_ioeventfd_mmio(int fd, uint32_t adr, uint32_t val, _Bool assign,
                           uint32_t size);

int kvm_set_ioeventfd_pio_word(int fd, uint16_t adr, uint16_t val, _Bool assign);

int kvm_irqchip_add_msi_route(KVMState *s, MSIMessage msg);
int kvm_irqchip_update_msi_route(KVMState *s, int virq, MSIMessage msg);
void kvm_irqchip_release_virq(KVMState *s, int virq);

int kvm_irqchip_add_irqfd_notifier(KVMState *s, EventNotifier *n, int virq);
int kvm_irqchip_remove_irqfd_notifier(KVMState *s, EventNotifier *n, int virq);
void kvm_pc_gsi_handler(void *opaque, int n, int level);
void kvm_pc_setup_irq_routing(_Bool pci_enabled);
# 18 "/home/gabriel/repos/qemu/include/sysemu/dma.h" 2

typedef struct DMAContext DMAContext;
typedef struct ScatterGatherEntry ScatterGatherEntry;

typedef enum {
    DMA_DIRECTION_TO_DEVICE = 0,
    DMA_DIRECTION_FROM_DEVICE = 1,
} DMADirection;

struct QEMUSGList {
    ScatterGatherEntry *sg;
    int nsg;
    int nalloc;
    size_t size;
    DMAContext *dma;
};
# 44 "/home/gabriel/repos/qemu/include/sysemu/dma.h"
typedef uint64_t dma_addr_t;




typedef int DMATranslateFunc(DMAContext *dma,
                             dma_addr_t addr,
                             hwaddr *paddr,
                             hwaddr *len,
                             DMADirection dir);
typedef void* DMAMapFunc(DMAContext *dma,
                         dma_addr_t addr,
                         dma_addr_t *len,
                         DMADirection dir);
typedef void DMAUnmapFunc(DMAContext *dma,
                          void *buffer,
                          dma_addr_t len,
                          DMADirection dir,
                          dma_addr_t access_len);

struct DMAContext {
    AddressSpace *as;
    DMATranslateFunc *translate;
    DMAMapFunc *map;
    DMAUnmapFunc *unmap;
};




extern DMAContext dma_context_memory;

static __attribute__ (( always_inline )) __inline__ void dma_barrier(DMAContext *dma, DMADirection dir)
{
# 96 "/home/gabriel/repos/qemu/include/sysemu/dma.h"
    if ((kvm_allowed)) {
        asm volatile("mfence" ::: "memory");
    }
}

static __attribute__ (( always_inline )) __inline__ _Bool dma_has_iommu(DMAContext *dma)
{
    return dma && dma->translate;
}




_Bool iommu_dma_memory_valid(DMAContext *dma, dma_addr_t addr, dma_addr_t len,
                            DMADirection dir);
static __attribute__ (( always_inline )) __inline__ _Bool dma_memory_valid(DMAContext *dma,
                                    dma_addr_t addr, dma_addr_t len,
                                    DMADirection dir)
{
    if (!dma_has_iommu(dma)) {
        return 1;
    } else {
        return iommu_dma_memory_valid(dma, addr, len, dir);
    }
}

int iommu_dma_memory_rw(DMAContext *dma, dma_addr_t addr,
                        void *buf, dma_addr_t len, DMADirection dir);
static __attribute__ (( always_inline )) __inline__ int dma_memory_rw_relaxed(DMAContext *dma, dma_addr_t addr,
                                        void *buf, dma_addr_t len,
                                        DMADirection dir)
{
    if (!dma_has_iommu(dma)) {

        address_space_rw(dma->as, addr, buf, len, dir == DMA_DIRECTION_FROM_DEVICE);
        return 0;
    } else {
        return iommu_dma_memory_rw(dma, addr, buf, len, dir);
    }
}

static __attribute__ (( always_inline )) __inline__ int dma_memory_read_relaxed(DMAContext *dma, dma_addr_t addr,
                                          void *buf, dma_addr_t len)
{
    return dma_memory_rw_relaxed(dma, addr, buf, len, DMA_DIRECTION_TO_DEVICE);
}

static __attribute__ (( always_inline )) __inline__ int dma_memory_write_relaxed(DMAContext *dma, dma_addr_t addr,
                                           const void *buf, dma_addr_t len)
{
    return dma_memory_rw_relaxed(dma, addr, (void *)buf, len,
                                 DMA_DIRECTION_FROM_DEVICE);
}

static __attribute__ (( always_inline )) __inline__ int dma_memory_rw(DMAContext *dma, dma_addr_t addr,
                                void *buf, dma_addr_t len,
                                DMADirection dir)
{
    dma_barrier(dma, dir);

    return dma_memory_rw_relaxed(dma, addr, buf, len, dir);
}

static __attribute__ (( always_inline )) __inline__ int dma_memory_read(DMAContext *dma, dma_addr_t addr,
                                  void *buf, dma_addr_t len)
{
    return dma_memory_rw(dma, addr, buf, len, DMA_DIRECTION_TO_DEVICE);
}

static __attribute__ (( always_inline )) __inline__ int dma_memory_write(DMAContext *dma, dma_addr_t addr,
                                   const void *buf, dma_addr_t len)
{
    return dma_memory_rw(dma, addr, (void *)buf, len,
                         DMA_DIRECTION_FROM_DEVICE);
}

int iommu_dma_memory_set(DMAContext *dma, dma_addr_t addr, uint8_t c,
    dma_addr_t len);

int dma_memory_set(DMAContext *dma, dma_addr_t addr, uint8_t c, dma_addr_t len);

void *iommu_dma_memory_map(DMAContext *dma,
                           dma_addr_t addr, dma_addr_t *len,
                           DMADirection dir);
static __attribute__ (( always_inline )) __inline__ void *dma_memory_map(DMAContext *dma,
                                   dma_addr_t addr, dma_addr_t *len,
                                   DMADirection dir)
{
    if (!dma_has_iommu(dma)) {
        hwaddr xlen = *len;
        void *p;

        p = address_space_map(dma->as, addr, &xlen, dir == DMA_DIRECTION_FROM_DEVICE);
        *len = xlen;
        return p;
    } else {
        return iommu_dma_memory_map(dma, addr, len, dir);
    }
}

void iommu_dma_memory_unmap(DMAContext *dma,
                            void *buffer, dma_addr_t len,
                            DMADirection dir, dma_addr_t access_len);
static __attribute__ (( always_inline )) __inline__ void dma_memory_unmap(DMAContext *dma,
                                    void *buffer, dma_addr_t len,
                                    DMADirection dir, dma_addr_t access_len)
{
    if (!dma_has_iommu(dma)) {
        address_space_unmap(dma->as, buffer, (hwaddr)len,
                            dir == DMA_DIRECTION_FROM_DEVICE, access_len);
    } else {
        iommu_dma_memory_unmap(dma, buffer, len, dir, access_len);
    }
}
# 227 "/home/gabriel/repos/qemu/include/sysemu/dma.h"
static __attribute__ (( always_inline )) __inline__ uint8_t ldub_dma(DMAContext *dma, dma_addr_t addr)
{
    uint8_t val;

    dma_memory_read(dma, addr, &val, 1);
    return val;
}

static __attribute__ (( always_inline )) __inline__ void stb_dma(DMAContext *dma, dma_addr_t addr, uint8_t val)
{
    dma_memory_write(dma, addr, &val, 1);
}

static __attribute__ (( always_inline )) __inline__ uint16_t lduw_le_dma(DMAContext *dma, dma_addr_t addr) { uint16_t val; dma_memory_read(dma, addr, &val, (16) / 8); return le16_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stw_le_dma(DMAContext *dma, dma_addr_t addr, uint16_t val) { val = cpu_to_le16(val); dma_memory_write(dma, addr, &val, (16) / 8); };
static __attribute__ (( always_inline )) __inline__ uint32_t ldl_le_dma(DMAContext *dma, dma_addr_t addr) { uint32_t val; dma_memory_read(dma, addr, &val, (32) / 8); return le32_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stl_le_dma(DMAContext *dma, dma_addr_t addr, uint32_t val) { val = cpu_to_le32(val); dma_memory_write(dma, addr, &val, (32) / 8); };
static __attribute__ (( always_inline )) __inline__ uint64_t ldq_le_dma(DMAContext *dma, dma_addr_t addr) { uint64_t val; dma_memory_read(dma, addr, &val, (64) / 8); return le64_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stq_le_dma(DMAContext *dma, dma_addr_t addr, uint64_t val) { val = cpu_to_le64(val); dma_memory_write(dma, addr, &val, (64) / 8); };
static __attribute__ (( always_inline )) __inline__ uint16_t lduw_be_dma(DMAContext *dma, dma_addr_t addr) { uint16_t val; dma_memory_read(dma, addr, &val, (16) / 8); return be16_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stw_be_dma(DMAContext *dma, dma_addr_t addr, uint16_t val) { val = cpu_to_be16(val); dma_memory_write(dma, addr, &val, (16) / 8); };
static __attribute__ (( always_inline )) __inline__ uint32_t ldl_be_dma(DMAContext *dma, dma_addr_t addr) { uint32_t val; dma_memory_read(dma, addr, &val, (32) / 8); return be32_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stl_be_dma(DMAContext *dma, dma_addr_t addr, uint32_t val) { val = cpu_to_be32(val); dma_memory_write(dma, addr, &val, (32) / 8); };
static __attribute__ (( always_inline )) __inline__ uint64_t ldq_be_dma(DMAContext *dma, dma_addr_t addr) { uint64_t val; dma_memory_read(dma, addr, &val, (64) / 8); return be64_to_cpu(val); } static __attribute__ (( always_inline )) __inline__ void stq_be_dma(DMAContext *dma, dma_addr_t addr, uint64_t val) { val = cpu_to_be64(val); dma_memory_write(dma, addr, &val, (64) / 8); };



void dma_context_init(DMAContext *dma, AddressSpace *as, DMATranslateFunc translate,
                      DMAMapFunc map, DMAUnmapFunc unmap);

struct ScatterGatherEntry {
    dma_addr_t base;
    dma_addr_t len;
};

void qemu_sglist_init(QEMUSGList *qsg, int alloc_hint, DMAContext *dma);
void qemu_sglist_add(QEMUSGList *qsg, dma_addr_t base, dma_addr_t len);
void qemu_sglist_destroy(QEMUSGList *qsg);


typedef BlockDriverAIOCB *DMAIOFunc(BlockDriverState *bs, int64_t sector_num,
                                 QEMUIOVector *iov, int nb_sectors,
                                 BlockDriverCompletionFunc *cb, void *opaque);

BlockDriverAIOCB *dma_bdrv_io(BlockDriverState *bs,
                              QEMUSGList *sg, uint64_t sector_num,
                              DMAIOFunc *io_func, BlockDriverCompletionFunc *cb,
                              void *opaque, DMADirection dir);
BlockDriverAIOCB *dma_bdrv_read(BlockDriverState *bs,
                                QEMUSGList *sg, uint64_t sector,
                                BlockDriverCompletionFunc *cb, void *opaque);
BlockDriverAIOCB *dma_bdrv_write(BlockDriverState *bs,
                                 QEMUSGList *sg, uint64_t sector,
                                 BlockDriverCompletionFunc *cb, void *opaque);
uint64_t dma_buf_read(uint8_t *ptr, int32_t len, QEMUSGList *sg);
uint64_t dma_buf_write(uint8_t *ptr, int32_t len, QEMUSGList *sg);

void dma_acct_start(BlockDriverState *bs, BlockAcctCookie *cookie,
                    QEMUSGList *sg, enum BlockAcctType type);
# 9 "/home/gabriel/repos/qemu/include/hw/pci/pci.h" 2




# 1 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h" 1
# 25 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h"
# 1 "/home/gabriel/repos/qemu/include/hw/pci/pci_regs.h" 1
# 26 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h" 2
# 1 "/home/gabriel/repos/qemu/include/hw/pci/pcie_regs.h" 1
# 27 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h" 2
# 1 "/home/gabriel/repos/qemu/include/hw/pci/pcie_aer.h" 1
# 29 "/home/gabriel/repos/qemu/include/hw/pci/pcie_aer.h"
struct PCIEAERLog {




    uint16_t log_num;
# 48 "/home/gabriel/repos/qemu/include/hw/pci/pcie_aer.h"
    uint16_t log_max;


    PCIEAERErr *log;
};



struct PCIEAERMsg {




    uint32_t severity;

    uint16_t source_id;
};

static __attribute__ (( always_inline )) __inline__ _Bool
pcie_aer_msg_is_uncor(const PCIEAERMsg *msg)
{
    return msg->severity == 0x00000002 ||
        msg->severity == 0x00000004;
}


struct PCIEAERErr {
    uint32_t status;
    uint16_t source_id;





    uint16_t flags;

    uint32_t header[4];
    uint32_t prefix[4];
};

extern const VMStateDescription vmstate_pcie_aer_log;

int pcie_aer_init(PCIDevice *dev, uint16_t offset);
void pcie_aer_exit(PCIDevice *dev);
void pcie_aer_write_config(PCIDevice *dev,
                           uint32_t addr, uint32_t val, int len);


void pcie_aer_root_set_vector(PCIDevice *dev, unsigned int vector);
void pcie_aer_root_init(PCIDevice *dev);
void pcie_aer_root_reset(PCIDevice *dev);
void pcie_aer_root_write_config(PCIDevice *dev,
                                uint32_t addr, uint32_t val, int len,
                                uint32_t root_cmd_prev);


int pcie_aer_inject_error(PCIDevice *dev, const PCIEAERErr *err);
# 28 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h" 2

typedef enum {

    PCI_EXP_HP_IND_RESERVED = 0x0,
    PCI_EXP_HP_IND_ON = 0x1,
    PCI_EXP_HP_IND_BLINK = 0x2,
    PCI_EXP_HP_IND_OFF = 0x3,
} PCIExpressIndicator;

typedef enum {
# 47 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h"
    PCI_EXP_HP_EV_ABP = 0x0001,

    PCI_EXP_HP_EV_PDC = 0x0008,

    PCI_EXP_HP_EV_CCI = 0x0010,


    PCI_EXP_HP_EV_SUPPORTED = PCI_EXP_HP_EV_ABP |
                                  PCI_EXP_HP_EV_PDC |
                                  PCI_EXP_HP_EV_CCI,



} PCIExpressHotPlugEvent;

struct PCIExpressDevice {

    uint8_t exp_cap;


    unsigned int hpev_intx;
# 76 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h"
    _Bool hpev_notified;






    uint16_t aer_cap;
    PCIEAERLog aer_log;
    unsigned int aer_intx;
# 94 "/home/gabriel/repos/qemu/include/hw/pci/pcie.h"
};


int pcie_cap_init(PCIDevice *dev, uint8_t offset, uint8_t type, uint8_t port);
int pcie_endpoint_cap_init(PCIDevice *dev, uint8_t offset);
void pcie_cap_exit(PCIDevice *dev);
uint8_t pcie_cap_get_type(const PCIDevice *dev);
void pcie_cap_flags_set_vector(PCIDevice *dev, uint8_t vector);
uint8_t pcie_cap_flags_get_vector(PCIDevice *dev);

void pcie_cap_deverr_init(PCIDevice *dev);
void pcie_cap_deverr_reset(PCIDevice *dev);

void pcie_cap_slot_init(PCIDevice *dev, uint16_t slot);
void pcie_cap_slot_reset(PCIDevice *dev);
void pcie_cap_slot_write_config(PCIDevice *dev,
                                uint32_t addr, uint32_t val, int len);
int pcie_cap_slot_post_load(void *opaque, int version_id);
void pcie_cap_slot_push_attention_button(PCIDevice *dev);

void pcie_cap_root_init(PCIDevice *dev);
void pcie_cap_root_reset(PCIDevice *dev);

void pcie_cap_flr_init(PCIDevice *dev);
void pcie_cap_flr_write_config(PCIDevice *dev,
                           uint32_t addr, uint32_t val, int len);

void pcie_cap_ari_init(PCIDevice *dev);
void pcie_cap_ari_reset(PCIDevice *dev);
_Bool pcie_cap_is_ari_enabled(const PCIDevice *dev);


uint16_t pcie_find_capability(PCIDevice *dev, uint16_t cap_id);
void pcie_add_capability(PCIDevice *dev,
                         uint16_t cap_id, uint8_t cap_ver,
                         uint16_t offset, uint16_t size);

void pcie_ari_init(PCIDevice *dev, uint16_t offset, uint16_t nextfn);

extern const VMStateDescription vmstate_pcie_device;
# 14 "/home/gabriel/repos/qemu/include/hw/pci/pci.h" 2
# 24 "/home/gabriel/repos/qemu/include/hw/pci/pci.h"
# 1 "/home/gabriel/repos/qemu/include/hw/pci/pci_ids.h" 1
# 25 "/home/gabriel/repos/qemu/include/hw/pci/pci.h" 2
# 92 "/home/gabriel/repos/qemu/include/hw/pci/pci.h"
typedef void PCIConfigWriteFunc(PCIDevice *pci_dev,
                                uint32_t address, uint32_t data, int len);
typedef uint32_t PCIConfigReadFunc(PCIDevice *pci_dev,
                                   uint32_t address, int len);
typedef void PCIMapIORegionFunc(PCIDevice *pci_dev, int region_num,
                                pcibus_t addr, pcibus_t size, int type);
typedef void PCIUnregisterFunc(PCIDevice *pci_dev);

typedef struct PCIIORegion {
    pcibus_t addr;

    pcibus_t size;
    uint8_t type;
    MemoryRegion *memory;
    MemoryRegion *address_space;
} PCIIORegion;




enum {
    QEMU_PCI_VGA_MEM,
    QEMU_PCI_VGA_IO_LO,
    QEMU_PCI_VGA_IO_HI,
    QEMU_PCI_VGA_NUM_REGIONS,
};
# 141 "/home/gabriel/repos/qemu/include/hw/pci/pci.h"
enum {
    QEMU_PCI_CAP_MSI = 0x1,
    QEMU_PCI_CAP_MSIX = 0x2,
    QEMU_PCI_CAP_EXPRESS = 0x4,



    QEMU_PCI_CAP_MULTIFUNCTION = (1 << 3),



    QEMU_PCI_CAP_SERR = (1 << 4),


    QEMU_PCI_CAP_SHPC = (1 << 5),

    QEMU_PCI_CAP_SLOTID = (1 << 6),
};
# 168 "/home/gabriel/repos/qemu/include/hw/pci/pci.h"
typedef struct PCIINTxRoute {
    enum {
        PCI_INTX_ENABLED,
        PCI_INTX_INVERTED,
        PCI_INTX_DISABLED,
    } mode;
    int irq;
} PCIINTxRoute;

typedef struct PCIDeviceClass {
    DeviceClass parent_class;

    int (*init)(PCIDevice *dev);
    PCIUnregisterFunc *exit;
    PCIConfigReadFunc *config_read;
    PCIConfigWriteFunc *config_write;

    uint16_t vendor_id;
    uint16_t device_id;
    uint8_t revision;
    uint16_t class_id;
    uint16_t subsystem_vendor_id;
    uint16_t subsystem_id;






    int is_bridge;


    int is_express;


    int no_hotplug;


    const char *romfile;
} PCIDeviceClass;

typedef void (*PCIINTxRoutingNotifier)(PCIDevice *dev);
typedef int (*MSIVectorUseNotifier)(PCIDevice *dev, unsigned int vector,
                                      MSIMessage msg);
typedef void (*MSIVectorReleaseNotifier)(PCIDevice *dev, unsigned int vector);
typedef void (*MSIVectorPollNotifier)(PCIDevice *dev,
                                      unsigned int vector_start,
                                      unsigned int vector_end);

struct PCIDevice {
    DeviceState qdev;


    uint8_t *config;



    uint8_t *cmask;


    uint8_t *wmask;


    uint8_t *w1cmask;


    uint8_t *used;


    PCIBus *bus;
    int32_t devfn;
    char name[64];
    PCIIORegion io_regions[7];
    AddressSpace bus_master_as;
    MemoryRegion bus_master_enable_region;
    DMAContext *dma;


    PCIConfigReadFunc *config_read;
    PCIConfigWriteFunc *config_write;


    qemu_irq *irq;


    MemoryRegion *vga_regions[QEMU_PCI_VGA_NUM_REGIONS];
    _Bool has_vga;


    uint8_t irq_state;


    uint32_t cap_present;


    uint8_t msix_cap;


    int msix_entries_nr;


    uint8_t *msix_table;
    uint8_t *msix_pba;

    MemoryRegion msix_exclusive_bar;

    MemoryRegion msix_table_mmio;
    MemoryRegion msix_pba_mmio;

    unsigned *msix_entry_used;

    _Bool msix_function_masked;

    int32_t version_id;


    uint8_t msi_cap;


    PCIExpressDevice exp;


    SHPCDevice *shpc;


    char *romfile;
    _Bool has_rom;
    MemoryRegion rom;
    uint32_t rom_bar;


    PCIINTxRoutingNotifier intx_routing_notifier;


    MSIVectorUseNotifier msix_vector_use_notifier;
    MSIVectorReleaseNotifier msix_vector_release_notifier;
    MSIVectorPollNotifier msix_vector_poll_notifier;
};

void pci_register_bar(PCIDevice *pci_dev, int region_num,
                      uint8_t attr, MemoryRegion *memory);
void pci_register_vga(PCIDevice *pci_dev, MemoryRegion *mem,
                      MemoryRegion *io_lo, MemoryRegion *io_hi);
void pci_unregister_vga(PCIDevice *pci_dev);
pcibus_t pci_get_bar_addr(PCIDevice *pci_dev, int region_num);

int pci_add_capability(PCIDevice *pdev, uint8_t cap_id,
                       uint8_t offset, uint8_t size);

void pci_del_capability(PCIDevice *pci_dev, uint8_t cap_id, uint8_t cap_size);

uint8_t pci_find_capability(PCIDevice *pci_dev, uint8_t cap_id);


uint32_t pci_default_read_config(PCIDevice *d,
                                 uint32_t address, int len);
void pci_default_write_config(PCIDevice *d,
                              uint32_t address, uint32_t val, int len);
void pci_device_save(PCIDevice *s, QEMUFile *f);
int pci_device_load(PCIDevice *s, QEMUFile *f);
MemoryRegion *pci_address_space(PCIDevice *dev);
MemoryRegion *pci_address_space_io(PCIDevice *dev);

typedef void (*pci_set_irq_fn)(void *opaque, int irq_num, int level);
typedef int (*pci_map_irq_fn)(PCIDevice *pci_dev, int irq_num);
typedef PCIINTxRoute (*pci_route_irq_fn)(void *opaque, int pin);

typedef enum {
    PCI_HOTPLUG_DISABLED,
    PCI_HOTPLUG_ENABLED,
    PCI_COLDPLUG_ENABLED,
} PCIHotplugState;

typedef int (*pci_hotplug_fn)(DeviceState *qdev, PCIDevice *pci_dev,
                              PCIHotplugState state);





_Bool pci_bus_is_express(PCIBus *bus);
_Bool pci_bus_is_root(PCIBus *bus);
void pci_bus_new_inplace(PCIBus *bus, DeviceState *parent,
                         const char *name,
                         MemoryRegion *address_space_mem,
                         MemoryRegion *address_space_io,
                         uint8_t devfn_min, const char *typename);
PCIBus *pci_bus_new(DeviceState *parent, const char *name,
                    MemoryRegion *address_space_mem,
                    MemoryRegion *address_space_io,
                    uint8_t devfn_min, const char *typename);
void pci_bus_irqs(PCIBus *bus, pci_set_irq_fn set_irq, pci_map_irq_fn map_irq,
                  void *irq_opaque, int nirq);
int pci_bus_get_irq_level(PCIBus *bus, int irq_num);
void pci_bus_hotplug(PCIBus *bus, pci_hotplug_fn hotplug, DeviceState *dev);

int pci_swizzle_map_irq_fn(PCIDevice *pci_dev, int pin);
PCIBus *pci_register_bus(DeviceState *parent, const char *name,
                         pci_set_irq_fn set_irq, pci_map_irq_fn map_irq,
                         void *irq_opaque,
                         MemoryRegion *address_space_mem,
                         MemoryRegion *address_space_io,
                         uint8_t devfn_min, int nirq, const char *typename);
void pci_bus_set_route_irq_fn(PCIBus *, pci_route_irq_fn);
PCIINTxRoute pci_device_route_intx_to_irq(PCIDevice *dev, int pin);
_Bool pci_intx_route_changed(PCIINTxRoute *old, PCIINTxRoute *new);
void pci_bus_fire_intx_routing_notifier(PCIBus *bus);
void pci_device_set_intx_routing_notifier(PCIDevice *dev,
                                          PCIINTxRoutingNotifier notifier);
void pci_device_reset(PCIDevice *dev);
void pci_bus_reset(PCIBus *bus);

PCIDevice *pci_nic_init(NICInfo *nd, const char *default_model,
                        const char *default_devaddr);
PCIDevice *pci_nic_init_nofail(NICInfo *nd, const char *default_model,
                               const char *default_devaddr);

PCIDevice *pci_vga_init(PCIBus *bus);

int pci_bus_num(PCIBus *s);
void pci_for_each_device(PCIBus *bus, int bus_num,
                         void (*fn)(PCIBus *bus, PCIDevice *d, void *opaque),
                         void *opaque);
PCIBus *pci_find_root_bus(int domain);
int pci_find_domain(const PCIBus *bus);
PCIDevice *pci_find_device(PCIBus *bus, int bus_num, uint8_t devfn);
int pci_qdev_find_device(const char *id, PCIDevice **pdev);
PCIBus *pci_get_bus_devfn(int *devfnp, const char *devaddr);

int pci_read_devaddr(Monitor *mon, const char *addr, int *domp, int *busp,
                     unsigned *slotp);

void pci_device_deassert_intx(PCIDevice *dev);

typedef DMAContext *(*PCIDMAContextFunc)(PCIBus *, void *, int);

void pci_setup_iommu(PCIBus *bus, PCIDMAContextFunc fn, void *opaque);

static __attribute__ (( always_inline )) __inline__ void
pci_set_byte(uint8_t *config, uint8_t val)
{
    *config = val;
}

static __attribute__ (( always_inline )) __inline__ uint8_t
pci_get_byte(const uint8_t *config)
{
    return *config;
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_word(uint8_t *config, uint16_t val)
{
    cpu_to_le16wu((uint16_t *)config, val);
}

static __attribute__ (( always_inline )) __inline__ uint16_t
pci_get_word(const uint8_t *config)
{
    return le16_to_cpupu((const uint16_t *)config);
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_long(uint8_t *config, uint32_t val)
{
    cpu_to_le32wu((uint32_t *)config, val);
}

static __attribute__ (( always_inline )) __inline__ uint32_t
pci_get_long(const uint8_t *config)
{
    return le32_to_cpupu((const uint32_t *)config);
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_quad(uint8_t *config, uint64_t val)
{
    cpu_to_le64w((uint64_t *)config, val);
}

static __attribute__ (( always_inline )) __inline__ uint64_t
pci_get_quad(const uint8_t *config)
{
    return le64_to_cpup((const uint64_t *)config);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_vendor_id(uint8_t *pci_config, uint16_t val)
{
    pci_set_word(&pci_config[0x00], val);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_device_id(uint8_t *pci_config, uint16_t val)
{
    pci_set_word(&pci_config[0x02], val);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_revision(uint8_t *pci_config, uint8_t val)
{
    pci_set_byte(&pci_config[0x08], val);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_class(uint8_t *pci_config, uint16_t val)
{
    pci_set_word(&pci_config[0x0a], val);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_prog_interface(uint8_t *pci_config, uint8_t val)
{
    pci_set_byte(&pci_config[0x09], val);
}

static __attribute__ (( always_inline )) __inline__ void
pci_config_set_interrupt_pin(uint8_t *pci_config, uint8_t val)
{
    pci_set_byte(&pci_config[0x3d], val);
}







static __attribute__ (( always_inline )) __inline__ uint8_t
pci_byte_test_and_clear_mask(uint8_t *config, uint8_t mask)
{
    uint8_t val = pci_get_byte(config);
    pci_set_byte(config, val & ~mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint8_t
pci_byte_test_and_set_mask(uint8_t *config, uint8_t mask)
{
    uint8_t val = pci_get_byte(config);
    pci_set_byte(config, val | mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint16_t
pci_word_test_and_clear_mask(uint8_t *config, uint16_t mask)
{
    uint16_t val = pci_get_word(config);
    pci_set_word(config, val & ~mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint16_t
pci_word_test_and_set_mask(uint8_t *config, uint16_t mask)
{
    uint16_t val = pci_get_word(config);
    pci_set_word(config, val | mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint32_t
pci_long_test_and_clear_mask(uint8_t *config, uint32_t mask)
{
    uint32_t val = pci_get_long(config);
    pci_set_long(config, val & ~mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint32_t
pci_long_test_and_set_mask(uint8_t *config, uint32_t mask)
{
    uint32_t val = pci_get_long(config);
    pci_set_long(config, val | mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint64_t
pci_quad_test_and_clear_mask(uint8_t *config, uint64_t mask)
{
    uint64_t val = pci_get_quad(config);
    pci_set_quad(config, val & ~mask);
    return val & mask;
}

static __attribute__ (( always_inline )) __inline__ uint64_t
pci_quad_test_and_set_mask(uint8_t *config, uint64_t mask)
{
    uint64_t val = pci_get_quad(config);
    pci_set_quad(config, val | mask);
    return val & mask;
}


static __attribute__ (( always_inline )) __inline__ void
pci_set_byte_by_mask(uint8_t *config, uint8_t mask, uint8_t reg)
{
    uint8_t val = pci_get_byte(config);
    uint8_t rval = reg << (ffs(mask) - 1);
    pci_set_byte(config, (~mask & val) | (mask & rval));
}

static __attribute__ (( always_inline )) __inline__ uint8_t
pci_get_byte_by_mask(uint8_t *config, uint8_t mask)
{
    uint8_t val = pci_get_byte(config);
    return (val & mask) >> (ffs(mask) - 1);
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_word_by_mask(uint8_t *config, uint16_t mask, uint16_t reg)
{
    uint16_t val = pci_get_word(config);
    uint16_t rval = reg << (ffs(mask) - 1);
    pci_set_word(config, (~mask & val) | (mask & rval));
}

static __attribute__ (( always_inline )) __inline__ uint16_t
pci_get_word_by_mask(uint8_t *config, uint16_t mask)
{
    uint16_t val = pci_get_word(config);
    return (val & mask) >> (ffs(mask) - 1);
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_long_by_mask(uint8_t *config, uint32_t mask, uint32_t reg)
{
    uint32_t val = pci_get_long(config);
    uint32_t rval = reg << (ffs(mask) - 1);
    pci_set_long(config, (~mask & val) | (mask & rval));
}

static __attribute__ (( always_inline )) __inline__ uint32_t
pci_get_long_by_mask(uint8_t *config, uint32_t mask)
{
    uint32_t val = pci_get_long(config);
    return (val & mask) >> (ffs(mask) - 1);
}

static __attribute__ (( always_inline )) __inline__ void
pci_set_quad_by_mask(uint8_t *config, uint64_t mask, uint64_t reg)
{
    uint64_t val = pci_get_quad(config);
    uint64_t rval = reg << (ffs(mask) - 1);
    pci_set_quad(config, (~mask & val) | (mask & rval));
}

static __attribute__ (( always_inline )) __inline__ uint64_t
pci_get_quad_by_mask(uint8_t *config, uint64_t mask)
{
    uint64_t val = pci_get_quad(config);
    return (val & mask) >> (ffs(mask) - 1);
}

PCIDevice *pci_create_multifunction(PCIBus *bus, int devfn, _Bool multifunction,
                                    const char *name);
PCIDevice *pci_create_simple_multifunction(PCIBus *bus, int devfn,
                                           _Bool multifunction,
                                           const char *name);
PCIDevice *pci_create(PCIBus *bus, int devfn, const char *name);
PCIDevice *pci_create_simple(PCIBus *bus, int devfn, const char *name);

static __attribute__ (( always_inline )) __inline__ int pci_is_express(const PCIDevice *d)
{
    return d->cap_present & QEMU_PCI_CAP_EXPRESS;
}

static __attribute__ (( always_inline )) __inline__ uint32_t pci_config_size(const PCIDevice *d)
{
    return pci_is_express(d) ? 0x1000 : 0x100;
}


static __attribute__ (( always_inline )) __inline__ DMAContext *pci_dma_context(PCIDevice *dev)
{
    return dev->dma;
}

static __attribute__ (( always_inline )) __inline__ int pci_dma_rw(PCIDevice *dev, dma_addr_t addr,
                             void *buf, dma_addr_t len, DMADirection dir)
{
    dma_memory_rw(pci_dma_context(dev), addr, buf, len, dir);
    return 0;
}

static __attribute__ (( always_inline )) __inline__ int pci_dma_read(PCIDevice *dev, dma_addr_t addr,
                               void *buf, dma_addr_t len)
{
    return pci_dma_rw(dev, addr, buf, len, DMA_DIRECTION_TO_DEVICE);
}

static __attribute__ (( always_inline )) __inline__ int pci_dma_write(PCIDevice *dev, dma_addr_t addr,
                                const void *buf, dma_addr_t len)
{
    return pci_dma_rw(dev, addr, (void *) buf, len, DMA_DIRECTION_FROM_DEVICE);
}
# 676 "/home/gabriel/repos/qemu/include/hw/pci/pci.h"
static __attribute__ (( always_inline )) __inline__ uint8_t ldub_pci_dma(PCIDevice *dev, dma_addr_t addr) { return ldub_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stb_pci_dma(PCIDevice *dev, dma_addr_t addr, uint8_t val) { stb_dma(pci_dma_context(dev), addr, val); };
static __attribute__ (( always_inline )) __inline__ uint16_t lduw_le_pci_dma(PCIDevice *dev, dma_addr_t addr) { return lduw_le_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stw_le_pci_dma(PCIDevice *dev, dma_addr_t addr, uint16_t val) { stw_le_dma(pci_dma_context(dev), addr, val); }
static __attribute__ (( always_inline )) __inline__ uint32_t ldl_le_pci_dma(PCIDevice *dev, dma_addr_t addr) { return ldl_le_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stl_le_pci_dma(PCIDevice *dev, dma_addr_t addr, uint32_t val) { stl_le_dma(pci_dma_context(dev), addr, val); };
static __attribute__ (( always_inline )) __inline__ uint64_t ldq_le_pci_dma(PCIDevice *dev, dma_addr_t addr) { return ldq_le_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stq_le_pci_dma(PCIDevice *dev, dma_addr_t addr, uint64_t val) { stq_le_dma(pci_dma_context(dev), addr, val); };
static __attribute__ (( always_inline )) __inline__ uint16_t lduw_be_pci_dma(PCIDevice *dev, dma_addr_t addr) { return lduw_be_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stw_be_pci_dma(PCIDevice *dev, dma_addr_t addr, uint16_t val) { stw_be_dma(pci_dma_context(dev), addr, val); }
static __attribute__ (( always_inline )) __inline__ uint32_t ldl_be_pci_dma(PCIDevice *dev, dma_addr_t addr) { return ldl_be_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stl_be_pci_dma(PCIDevice *dev, dma_addr_t addr, uint32_t val) { stl_be_dma(pci_dma_context(dev), addr, val); };
static __attribute__ (( always_inline )) __inline__ uint64_t ldq_be_pci_dma(PCIDevice *dev, dma_addr_t addr) { return ldq_be_dma(pci_dma_context(dev), addr); } static __attribute__ (( always_inline )) __inline__ void stq_be_pci_dma(PCIDevice *dev, dma_addr_t addr, uint64_t val) { stq_be_dma(pci_dma_context(dev), addr, val); };



static __attribute__ (( always_inline )) __inline__ void *pci_dma_map(PCIDevice *dev, dma_addr_t addr,
                                dma_addr_t *plen, DMADirection dir)
{
    void *buf;

    buf = dma_memory_map(pci_dma_context(dev), addr, plen, dir);
    return buf;
}

static __attribute__ (( always_inline )) __inline__ void pci_dma_unmap(PCIDevice *dev, void *buffer, dma_addr_t len,
                                 DMADirection dir, dma_addr_t access_len)
{
    dma_memory_unmap(pci_dma_context(dev), buffer, len, dir, access_len);
}

static __attribute__ (( always_inline )) __inline__ void pci_dma_sglist_init(QEMUSGList *qsg, PCIDevice *dev,
                                       int alloc_hint)
{
    qemu_sglist_init(qsg, alloc_hint, pci_dma_context(dev));
}

extern const VMStateDescription vmstate_pci_device;
# 26 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/hw/acpi/acpi.h" 1
# 83 "/home/gabriel/repos/qemu/include/hw/acpi/acpi.h"
typedef struct ACPIPMTimer ACPIPMTimer;
typedef struct ACPIPM1EVT ACPIPM1EVT;
typedef struct ACPIPM1CNT ACPIPM1CNT;
typedef struct ACPIGPE ACPIGPE;
typedef struct ACPIREGS ACPIREGS;

typedef void (*acpi_update_sci_fn)(ACPIREGS *ar);

struct ACPIPMTimer {
    QEMUTimer *timer;
    MemoryRegion io;
    int64_t overflow_time;

    acpi_update_sci_fn update_sci;
};

struct ACPIPM1EVT {
    MemoryRegion io;
    uint16_t sts;
    uint16_t en;
    acpi_update_sci_fn update_sci;
};

struct ACPIPM1CNT {
    MemoryRegion io;
    uint16_t cnt;
    uint8_t s4_val;
};

struct ACPIGPE {
    uint8_t len;

    uint8_t *sts;
    uint8_t *en;
};

struct ACPIREGS {
    ACPIPMTimer tmr;
    ACPIGPE gpe;
    struct {
        ACPIPM1EVT evt;
        ACPIPM1CNT cnt;
    } pm1;
    Notifier wakeup;
};


void acpi_pm_tmr_update(ACPIREGS *ar, _Bool enable);
void acpi_pm_tmr_calc_overflow_time(ACPIREGS *ar);
void acpi_pm_tmr_init(ACPIREGS *ar, acpi_update_sci_fn update_sci,
                      MemoryRegion *parent);
void acpi_pm_tmr_reset(ACPIREGS *ar);


static __attribute__ (( always_inline )) __inline__ int64_t acpi_pm_tmr_get_clock(void)
{
    return muldiv64(qemu_get_clock_ns(vm_clock), 3579545,
                    get_ticks_per_sec());
}


uint16_t acpi_pm1_evt_get_sts(ACPIREGS *ar);
void acpi_pm1_evt_power_down(ACPIREGS *ar);
void acpi_pm1_evt_reset(ACPIREGS *ar);
void acpi_pm1_evt_init(ACPIREGS *ar, acpi_update_sci_fn update_sci,
                       MemoryRegion *parent);


void acpi_pm1_cnt_init(ACPIREGS *ar, MemoryRegion *parent, uint8_t s4_val);
void acpi_pm1_cnt_update(ACPIREGS *ar,
                         _Bool sci_enable, _Bool sci_disable);
void acpi_pm1_cnt_reset(ACPIREGS *ar);


void acpi_gpe_init(ACPIREGS *ar, uint8_t len);
void acpi_gpe_reset(ACPIREGS *ar);

void acpi_gpe_ioport_writeb(ACPIREGS *ar, uint32_t addr, uint32_t val);
uint32_t acpi_gpe_ioport_readb(ACPIREGS *ar, uint32_t addr);


extern int acpi_enabled;
extern char unsigned *acpi_tables;
extern size_t acpi_tables_len;

void acpi_table_add(const QemuOpts *opts, Error **errp);
# 27 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/sysemu/sysemu.h" 1
# 15 "/home/gabriel/repos/qemu/include/sysemu/sysemu.h"
extern const char *bios_name;

extern const char *qemu_name;
extern uint8_t qemu_uuid[];
int qemu_uuid_parse(const char *str, uint8_t *uuid);


_Bool runstate_check(RunState state);
void runstate_set(RunState new_state);
int runstate_is_running(void);
typedef struct vm_change_state_entry VMChangeStateEntry;
typedef void VMChangeStateHandler(void *opaque, int running, RunState state);

VMChangeStateEntry *qemu_add_vm_change_state_handler(VMChangeStateHandler *cb,
                                                     void *opaque);
void qemu_del_vm_change_state_handler(VMChangeStateEntry *e);
void vm_state_notify(int running, RunState state);




void vm_start(void);
void vm_stop(RunState state);
void vm_stop_force_state(RunState state);

typedef enum WakeupReason {
    QEMU_WAKEUP_REASON_OTHER = 0,
    QEMU_WAKEUP_REASON_RTC,
    QEMU_WAKEUP_REASON_PMTIMER,
} WakeupReason;

void qemu_system_reset_request(void);
void qemu_system_suspend_request(void);
void qemu_register_suspend_notifier(Notifier *notifier);
void qemu_system_wakeup_request(WakeupReason reason);
void qemu_system_wakeup_enable(WakeupReason reason, _Bool enabled);
void qemu_register_wakeup_notifier(Notifier *notifier);
void qemu_system_shutdown_request(void);
void qemu_system_powerdown_request(void);
void qemu_register_powerdown_notifier(Notifier *notifier);
void qemu_system_debug_request(void);
void qemu_system_vmstop_request(RunState reason);
int qemu_shutdown_requested_get(void);
int qemu_reset_requested_get(void);
void qemu_system_killed(int signal, pid_t pid);
void qemu_devices_reset(void);
void qemu_system_reset(_Bool report);

void qemu_add_exit_notifier(Notifier *notify);
void qemu_remove_exit_notifier(Notifier *notify);

void qemu_add_machine_init_done_notifier(Notifier *notify);

void do_savevm(Monitor *mon, const QDict *qdict);
int load_vmstate(const char *name);
void do_delvm(Monitor *mon, const QDict *qdict);
void do_info_snapshots(Monitor *mon, const QDict *qdict);

void qemu_announce_self(void);

_Bool qemu_savevm_state_blocked(Error **errp);
void qemu_savevm_state_begin(QEMUFile *f,
                             const MigrationParams *params);
int qemu_savevm_state_iterate(QEMUFile *f);
void qemu_savevm_state_complete(QEMUFile *f);
void qemu_savevm_state_cancel(void);
uint64_t qemu_savevm_state_pending(QEMUFile *f, uint64_t max_size);
int qemu_loadvm_state(QEMUFile *f);


void do_info_slirp(Monitor *mon);

typedef enum DisplayType
{
    DT_DEFAULT,
    DT_CURSES,
    DT_SDL,
    DT_GTK,
    DT_NOGRAPHIC,
    DT_NONE,
} DisplayType;

extern int autostart;

typedef enum {
    VGA_NONE, VGA_STD, VGA_CIRRUS, VGA_VMWARE, VGA_XENFB, VGA_QXL,
} VGAInterfaceType;

extern int vga_interface_type;



extern int graphic_width;
extern int graphic_height;
extern int graphic_depth;
extern DisplayType display_type;
extern const char *keyboard_layout;
extern int win2k_install_hack;
extern int alt_grab;
extern int ctrl_grab;
extern int smp_cpus;
extern int max_cpus;
extern int cursor_hide;
extern int graphic_rotate;
extern int no_quit;
extern int no_shutdown;
extern int semihosting_enabled;
extern int old_param;
extern int boot_menu;
extern uint8_t *boot_splash_filedata;
extern size_t boot_splash_filedata_size;
extern uint8_t qemu_extra_params_fw[2];
extern QEMUClock *rtc_clock;



extern int nb_numa_nodes;
extern uint64_t node_mem[64];
extern unsigned long *node_cpumask[64];


typedef struct QEMUOptionRom {
    const char *name;
    int32_t bootindex;
} QEMUOptionRom;
extern QEMUOptionRom option_rom[16];
extern int nb_option_roms;


extern const char *prom_envs[128];
extern unsigned int nb_prom_envs;


void pci_device_hot_add(Monitor *mon, const QDict *qdict);
int pci_drive_hot_add(Monitor *mon, const QDict *qdict, DriveInfo *dinfo);
void do_pci_device_hot_remove(Monitor *mon, const QDict *qdict);


void drive_hot_add(Monitor *mon, const QDict *qdict);


void pcie_aer_inject_error_print(Monitor *mon, const QObject *data);
int do_pcie_aer_inject_error(Monitor *mon,
                             const QDict *qdict, QObject **ret_data);





extern CharDriverState *serial_hds[4];





extern CharDriverState *parallel_hds[3];

void do_usb_add(Monitor *mon, const QDict *qdict);
void do_usb_del(Monitor *mon, const QDict *qdict);
void usb_info(Monitor *mon, const QDict *qdict);

void rtc_change_mon_event(struct tm *tm);

void register_devices(void);

void add_boot_device_path(int32_t bootindex, DeviceState *dev,
                          const char *suffix);
char *get_boot_devices_list(size_t *size);

_Bool usb_enabled(_Bool default_usb);

extern QemuOptsList qemu_drive_opts;
extern QemuOptsList qemu_chardev_opts;
extern QemuOptsList qemu_device_opts;
extern QemuOptsList qemu_netdev_opts;
extern QemuOptsList qemu_net_opts;
extern QemuOptsList qemu_global_opts;
extern QemuOptsList qemu_mon_opts;
# 28 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/qemu/range.h" 1





static __attribute__ (( always_inline )) __inline__ uint64_t range_get_last(uint64_t offset, uint64_t len)
{
    return offset + len - 1;
}


static __attribute__ (( always_inline )) __inline__ int range_covers_byte(uint64_t offset, uint64_t len,
                                    uint64_t byte)
{
    return offset <= byte && byte <= range_get_last(offset, len);
}



static __attribute__ (( always_inline )) __inline__ int ranges_overlap(uint64_t first1, uint64_t len1,
                                 uint64_t first2, uint64_t len2)
{
    uint64_t last1 = range_get_last(first1, len1);
    uint64_t last2 = range_get_last(first2, len2);

    return !(last2 < first1 || last1 < first2);
}
# 29 "hw/acpi/piix4.c" 2

# 1 "/home/gabriel/repos/qemu/include/hw/nvram/fw_cfg.h" 1
# 42 "/home/gabriel/repos/qemu/include/hw/nvram/fw_cfg.h"
typedef struct FWCfgFile {
    uint32_t size;
    uint16_t select;
    uint16_t reserved;
    char name[56];
} FWCfgFile;

typedef struct FWCfgFiles {
    uint32_t count;
    FWCfgFile f[];
} FWCfgFiles;

typedef void (*FWCfgCallback)(void *opaque, uint8_t *data);

typedef struct FWCfgState FWCfgState;
void fw_cfg_add_bytes(FWCfgState *s, uint16_t key, void *data, size_t len);
void fw_cfg_add_string(FWCfgState *s, uint16_t key, const char *value);
void fw_cfg_add_i16(FWCfgState *s, uint16_t key, uint16_t value);
void fw_cfg_add_i32(FWCfgState *s, uint16_t key, uint32_t value);
void fw_cfg_add_i64(FWCfgState *s, uint16_t key, uint64_t value);
void fw_cfg_add_callback(FWCfgState *s, uint16_t key, FWCfgCallback callback,
                         void *callback_opaque, void *data, size_t len);
void fw_cfg_add_file(FWCfgState *s, const char *filename, void *data,
                     size_t len);
FWCfgState *fw_cfg_init(uint32_t ctl_port, uint32_t data_port,
                        hwaddr crl_addr, hwaddr data_addr);
# 31 "hw/acpi/piix4.c" 2
# 1 "/home/gabriel/repos/qemu/include/exec/address-spaces.h" 1
# 29 "/home/gabriel/repos/qemu/include/exec/address-spaces.h"
MemoryRegion *get_system_memory(void);




MemoryRegion *get_system_io(void);

extern AddressSpace address_space_memory;
extern AddressSpace address_space_io;
# 32 "hw/acpi/piix4.c" 2
# 53 "hw/acpi/piix4.c"
struct pci_status {
    uint32_t up;
    uint32_t down;
};

typedef struct PIIX4PMState {
    PCIDevice dev;

    MemoryRegion io;
    MemoryRegion io_gpe;
    MemoryRegion io_pci;
    ACPIREGS ar;

    APMState apm;

    PMSMBus smb;
    uint32_t smb_io_base;

    qemu_irq irq;
    qemu_irq smi_irq;
    int kvm_enabled;
    Notifier machine_ready;
    Notifier powerdown_notifier;


    struct pci_status pci0_status;
    uint32_t pci0_hotplug_enable;
    uint32_t pci0_slot_device_present;

    uint8_t disable_s3;
    uint8_t disable_s4;
    uint8_t s4_val;
} PIIX4PMState;

static void piix4_acpi_system_hot_add_init(MemoryRegion *parent,
                                           PCIBus *bus, PIIX4PMState *s);




static void pm_update_sci(PIIX4PMState *s)
{
    int sci_level, pmsts;

    pmsts = acpi_pm1_evt_get_sts(&s->ar);
    sci_level = (((pmsts & s->ar.pm1.evt.en) &
                  (0x0400 |
                   0x0100 |
                   0x0020 |
                   0x0001)) != 0) ||
        (((s->ar.gpe.sts[0] & s->ar.gpe.en[0])
          & 2) != 0);

    qemu_set_irq(s->irq, sci_level);

    acpi_pm_tmr_update(&s->ar, (s->ar.pm1.evt.en & 0x0001) &&
                       !(pmsts & 0x0001));
}

static void pm_tmr_timer(ACPIREGS *ar)
{
    PIIX4PMState *s = ({ const typeof(((PIIX4PMState *) 0)->ar) *__mptr = (ar); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, ar));});
    pm_update_sci(s);
}

static void apm_ctrl_changed(uint32_t val, void *arg)
{
    PIIX4PMState *s = arg;


    acpi_pm1_cnt_update(&s->ar, val == 0xf1, val == 0xf0);

    if (s->dev.config[0x5b] & (1 << 1)) {
        if (s->smi_irq) {
            qemu_irq_raise(s->smi_irq);
        }
    }
}

static void pm_io_space_update(PIIX4PMState *s)
{
    uint32_t pm_io_base;

    pm_io_base = le32_to_cpu(*(uint32_t *)(s->dev.config + 0x40));
    pm_io_base &= 0xffc0;

    memory_region_transaction_begin();
    memory_region_set_enabled(&s->io, s->dev.config[0x80] & 1);
    memory_region_set_address(&s->io, pm_io_base);
    memory_region_transaction_commit();
}

static void smbus_io_space_update(PIIX4PMState *s)
{
    s->smb_io_base = le32_to_cpu(*(uint32_t *)(s->dev.config + 0x90));
    s->smb_io_base &= 0xffc0;

    memory_region_transaction_begin();
    memory_region_set_enabled(&s->smb.io, s->dev.config[0xd2] & 1);
    memory_region_set_address(&s->smb.io, s->smb_io_base);
    memory_region_transaction_commit();
}

static void pm_write_config(PCIDevice *d,
                            uint32_t address, uint32_t val, int len)
{
    pci_default_write_config(d, address, val, len);
    if (range_covers_byte(address, len, 0x80) ||
        ranges_overlap(address, len, 0x40, 4)) {
        pm_io_space_update((PIIX4PMState *)d);
    }
    if (range_covers_byte(address, len, 0xd2) ||
        ranges_overlap(address, len, 0x90, 4)) {
        smbus_io_space_update((PIIX4PMState *)d);
    }
}

static void vmstate_pci_status_pre_save(void *opaque)
{
    struct pci_status *pci0_status = opaque;
    PIIX4PMState *s = ({ const typeof(((PIIX4PMState *) 0)->pci0_status) *__mptr = (pci0_status); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, pci0_status));});




    pci0_status->up = s->pci0_slot_device_present & s->pci0_hotplug_enable;
}

static int vmstate_acpi_post_load(void *opaque, int version_id)
{
    PIIX4PMState *s = opaque;

    pm_io_space_update(s);
    return 0;
}
# 199 "hw/acpi/piix4.c"
static const VMStateDescription vmstate_gpe = {
    .name = "gpe",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .fields = (VMStateField []) {
        { .name = ("sts"), .version_id = 0, .info = &vmstate_info_uint16, .size = sizeof(uint16_t), .flags = VMS_SINGLE | VMS_POINTER, .offset = (__builtin_offsetof (ACPIGPE, sts) + ((uint8_t**)0 - (typeof(((ACPIGPE *)0)->sts)*)0)), },
        { .name = ("en"), .version_id = 0, .info = &vmstate_info_uint16, .size = sizeof(uint16_t), .flags = VMS_SINGLE | VMS_POINTER, .offset = (__builtin_offsetof (ACPIGPE, en) + ((uint8_t**)0 - (typeof(((ACPIGPE *)0)->en)*)0)), },
        {}
    }
};

static const VMStateDescription vmstate_pci_status = {
    .name = "pci_status",
    .version_id = 1,
    .minimum_version_id = 1,
    .minimum_version_id_old = 1,
    .pre_save = vmstate_pci_status_pre_save,
    .fields = (VMStateField []) {
        { .name = ("up"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(uint32_t), .info = &(vmstate_info_uint32), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (struct pci_status, up) + ((uint32_t*)0 - (typeof(((struct pci_status *)0)->up)*)0)), },
        { .name = ("down"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(uint32_t), .info = &(vmstate_info_uint32), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (struct pci_status, down) + ((uint32_t*)0 - (typeof(((struct pci_status *)0)->down)*)0)), },
        {}
    }
};

static int acpi_load_old(QEMUFile *f, void *opaque, int version_id)
{
    PIIX4PMState *s = opaque;
    int ret, i;
    uint16_t temp;

    ret = pci_device_load(&s->dev, f);
    if (ret < 0) {
        return ret;
    }
    qemu_get_be16s(f, &s->ar.pm1.evt.sts);
    qemu_get_be16s(f, &s->ar.pm1.evt.en);
    qemu_get_be16s(f, &s->ar.pm1.cnt.cnt);

    ret = vmstate_load_state(f, &vmstate_apm, &s->apm, 1);
    if (ret) {
        return ret;
    }

    qemu_get_timer(f, s->ar.tmr.timer);
    qemu_get_sbe64s(f, &s->ar.tmr.overflow_time);

    qemu_get_be16s(f, (uint16_t *)s->ar.gpe.sts);
    for (i = 0; i < 3; i++) {
        qemu_get_be16s(f, &temp);
    }

    qemu_get_be16s(f, (uint16_t *)s->ar.gpe.en);
    for (i = 0; i < 3; i++) {
        qemu_get_be16s(f, &temp);
    }

    ret = vmstate_load_state(f, &vmstate_pci_status, &s->pci0_status, 1);
    return ret;
}







static const VMStateDescription vmstate_acpi = {
    .name = "piix4_pm",
    .version_id = 3,
    .minimum_version_id = 3,
    .minimum_version_id_old = 1,
    .load_state_old = acpi_load_old,
    .post_load = vmstate_acpi_post_load,
    .fields = (VMStateField []) {
        { .name = ("dev"), .size = sizeof(PCIDevice), .vmsd = &vmstate_pci_device, .flags = VMS_STRUCT, .offset = (__builtin_offsetof (PIIX4PMState, dev) + ((PCIDevice*)0 - (typeof(((PIIX4PMState *)0)->dev)*)0)), },
        { .name = ("ar.pm1.evt.sts"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(uint16_t), .info = &(vmstate_info_uint16), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (PIIX4PMState, ar.pm1.evt.sts) + ((uint16_t*)0 - (typeof(((PIIX4PMState *)0)->ar.pm1.evt.sts)*)0)), },
        { .name = ("ar.pm1.evt.en"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(uint16_t), .info = &(vmstate_info_uint16), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (PIIX4PMState, ar.pm1.evt.en) + ((uint16_t*)0 - (typeof(((PIIX4PMState *)0)->ar.pm1.evt.en)*)0)), },
        { .name = ("ar.pm1.cnt.cnt"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(uint16_t), .info = &(vmstate_info_uint16), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (PIIX4PMState, ar.pm1.cnt.cnt) + ((uint16_t*)0 - (typeof(((PIIX4PMState *)0)->ar.pm1.cnt.cnt)*)0)), },
        { .name = ("apm"), .version_id = (0), .field_exists = (((void *)0)), .vmsd = &(vmstate_apm), .size = sizeof(APMState), .flags = VMS_STRUCT, .offset = (__builtin_offsetof (PIIX4PMState, apm) + ((APMState*)0 - (typeof(((PIIX4PMState *)0)->apm)*)0)), },
        { .name = ("ar.tmr.timer"), .version_id = (0), .info = &(vmstate_info_timer), .size = sizeof(QEMUTimer *), .flags = VMS_SINGLE|VMS_POINTER, .offset = (__builtin_offsetof (PIIX4PMState, ar.tmr.timer) + ((QEMUTimer **)0 - (typeof(((PIIX4PMState *)0)->ar.tmr.timer)*)0)), },
        { .name = ("ar.tmr.overflow_time"), .version_id = (0), .field_exists = (((void *)0)), .size = sizeof(int64_t), .info = &(vmstate_info_int64), .flags = VMS_SINGLE, .offset = (__builtin_offsetof (PIIX4PMState, ar.tmr.overflow_time) + ((int64_t*)0 - (typeof(((PIIX4PMState *)0)->ar.tmr.overflow_time)*)0)), },
        { .name = ("ar.gpe"), .version_id = (2), .field_exists = (((void *)0)), .vmsd = &(vmstate_gpe), .size = sizeof(ACPIGPE), .flags = VMS_STRUCT, .offset = (__builtin_offsetof (PIIX4PMState, ar.gpe) + ((ACPIGPE*)0 - (typeof(((PIIX4PMState *)0)->ar.gpe)*)0)), },
        { .name = ("pci0_status"), .version_id = (2), .field_exists = (((void *)0)), .vmsd = &(vmstate_pci_status), .size = sizeof(struct pci_status), .flags = VMS_STRUCT, .offset = (__builtin_offsetof (PIIX4PMState, pci0_status) + ((struct pci_status*)0 - (typeof(((PIIX4PMState *)0)->pci0_status)*)0)), }
                                         ,
        {}
    }
};

static void acpi_piix_eject_slot(PIIX4PMState *s, unsigned slots)
{
    BusChild *kid, *next;
    BusState *bus = qdev_get_parent_bus(&s->dev.qdev);
    int slot = ffs(slots) - 1;
    _Bool slot_free = 1;


    s->pci0_status.down &= ~(1U << slot);

    for ((kid) = ((&bus->children)->tqh_first); (kid) && ((next) = ((kid)->sibling.tqe_next), 1); (kid) = (next)) {
        DeviceState *qdev = kid->child;
        PCIDevice *dev = ((PCIDevice *)object_dynamic_cast_assert(((Object *)((qdev))), ("pci-device")));
        PCIDeviceClass *pc = ((PCIDeviceClass *)object_class_dynamic_cast_assert(((ObjectClass *)(object_get_class(((Object *)((dev)))))), ("pci-device")));
        if ((((dev->devfn) >> 3) & 0x1f) == slot) {
            if (pc->no_hotplug) {
                slot_free = 0;
            } else {
                qdev_free(qdev);
            }
        }
    }
    if (slot_free) {
        s->pci0_slot_device_present &= ~(1U << slot);
    }
}

static void piix4_update_hotplug(PIIX4PMState *s)
{
    PCIDevice *dev = &s->dev;
    BusState *bus = qdev_get_parent_bus(&dev->qdev);
    BusChild *kid, *next;


    while (s->pci0_status.down) {
        acpi_piix_eject_slot(s, s->pci0_status.down);
    }

    s->pci0_hotplug_enable = ~0;
    s->pci0_slot_device_present = 0;

    for ((kid) = ((&bus->children)->tqh_first); (kid) && ((next) = ((kid)->sibling.tqe_next), 1); (kid) = (next)) {
        DeviceState *qdev = kid->child;
        PCIDevice *pdev = ((PCIDevice *)object_dynamic_cast_assert(((Object *)((qdev))), ("pci-device")));
        PCIDeviceClass *pc = ((PCIDeviceClass *)object_class_dynamic_cast_assert(((ObjectClass *)(object_get_class(((Object *)((pdev)))))), ("pci-device")));
        int slot = (((pdev->devfn) >> 3) & 0x1f);

        if (pc->no_hotplug) {
            s->pci0_hotplug_enable &= ~(1U << slot);
        }

        s->pci0_slot_device_present |= (1U << slot);
    }
}

static void piix4_reset(void *opaque)
{
    PIIX4PMState *s = opaque;
    uint8_t *pci_conf = s->dev.config;

    pci_conf[0x58] = 0;
    pci_conf[0x59] = 0;
    pci_conf[0x5a] = 0;
    pci_conf[0x5b] = 0;

    pci_conf[0x40] = 0x01;
    pci_conf[0x80] = 0;

    if (s->kvm_enabled) {

        pci_conf[0x5B] = 0x02;
    }
    piix4_update_hotplug(s);
}

static void piix4_pm_powerdown_req(Notifier *n, void *opaque)
{
    PIIX4PMState *s = ({ const typeof(((PIIX4PMState *) 0)->powerdown_notifier) *__mptr = (n); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, powerdown_notifier));});

    ((s != ((void *)0)) ? (void) (0) : __assert_fail ("s != ((void *)0)", "hw/acpi/piix4.c", 367, __PRETTY_FUNCTION__));
    acpi_pm1_evt_power_down(&s->ar);
}

static void piix4_pm_machine_ready(Notifier *n, void *opaque)
{
    PIIX4PMState *s = ({ const typeof(((PIIX4PMState *) 0)->machine_ready) *__mptr = (n); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, machine_ready));});
    uint8_t *pci_conf;

    pci_conf = s->dev.config;
    pci_conf[0x5f] = (isa_is_ioport_assigned(0x378) ? 0x80 : 0) | 0x10;
    pci_conf[0x63] = 0x60;
    pci_conf[0x67] = (isa_is_ioport_assigned(0x3f8) ? 0x08 : 0) |
 (isa_is_ioport_assigned(0x2f8) ? 0x90 : 0);

}

static int piix4_pm_initfn(PCIDevice *dev)
{
    PIIX4PMState *s = ( __extension__ ( { char __attribute__((unused)) offset_must_be_zero[ -__builtin_offsetof (PIIX4PMState, dev)]; ({ const typeof(((PIIX4PMState *) 0)->dev) *__mptr = (dev); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, dev));});}));
    uint8_t *pci_conf;

    pci_conf = s->dev.config;
    pci_conf[0x06] = 0x80;
    pci_conf[0x07] = 0x02;
    pci_conf[0x09] = 0x00;
    pci_conf[0x3d] = 0x01;


    apm_init(dev, &s->apm, apm_ctrl_changed, s);

    if (s->kvm_enabled) {


        pci_conf[0x5B] = 0x02;
    }



    pci_conf[0x90] = s->smb_io_base | 1;
    pci_conf[0x91] = s->smb_io_base >> 8;
    pci_conf[0xd2] = 0x09;
    pm_smbus_init(&s->dev.qdev, &s->smb);
    memory_region_set_enabled(&s->smb.io, pci_conf[0xd2] & 1);
    memory_region_add_subregion(pci_address_space_io(dev),
                                s->smb_io_base, &s->smb.io);

    memory_region_init(&s->io, "piix4-pm", 64);
    memory_region_set_enabled(&s->io, 0);
    memory_region_add_subregion(pci_address_space_io(dev),
                                0, &s->io);

    acpi_pm_tmr_init(&s->ar, pm_tmr_timer, &s->io);
    acpi_pm1_evt_init(&s->ar, pm_tmr_timer, &s->io);
    acpi_pm1_cnt_init(&s->ar, &s->io, s->s4_val);
    acpi_gpe_init(&s->ar, 4);

    s->powerdown_notifier.notify = piix4_pm_powerdown_req;
    qemu_register_powerdown_notifier(&s->powerdown_notifier);

    s->machine_ready.notify = piix4_pm_machine_ready;
    qemu_add_machine_init_done_notifier(&s->machine_ready);
    qemu_register_reset(piix4_reset, s);

    piix4_acpi_system_hot_add_init(pci_address_space_io(dev), dev->bus, s);

    return 0;
}

i2c_bus *piix4_pm_init(PCIBus *bus, int devfn, uint32_t smb_io_base,
                       qemu_irq sci_irq, qemu_irq smi_irq,
                       int kvm_enabled, void *fw_cfg)
{
    PCIDevice *dev;
    PIIX4PMState *s;

    dev = pci_create(bus, devfn, "PIIX4_PM");
    qdev_prop_set_uint32(&dev->qdev, "smb_io_base", smb_io_base);

    s = ( __extension__ ( { char __attribute__((unused)) offset_must_be_zero[ -__builtin_offsetof (PIIX4PMState, dev)]; ({ const typeof(((PIIX4PMState *) 0)->dev) *__mptr = (dev); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, dev));});}));
    s->irq = sci_irq;
    s->smi_irq = smi_irq;
    s->kvm_enabled = kvm_enabled;

    qdev_init_nofail(&dev->qdev);

    if (fw_cfg) {
        uint8_t suspend[6] = {128, 0, 0, 129, 128, 128};
        suspend[3] = 1 | ((!s->disable_s3) << 7);
        suspend[4] = s->s4_val | ((!s->disable_s4) << 7);

        fw_cfg_add_file(fw_cfg, "etc/system-states", g_memdup(suspend, 6), 6);
    }

    return s->smb.smbus;
}

static Property piix4_pm_properties[] = {
    { .name = ("smb_io_base"), .info = &(qdev_prop_uint32), .offset = __builtin_offsetof (PIIX4PMState, smb_io_base) + ((uint32_t*)0 - (typeof(((PIIX4PMState *)0)->smb_io_base)*)0), .qtype = QTYPE_QINT, .defval = (uint32_t)0, },
    { .name = ("disable_s3"), .info = &(qdev_prop_uint8), .offset = __builtin_offsetof (PIIX4PMState, disable_s3) + ((uint8_t*)0 - (typeof(((PIIX4PMState *)0)->disable_s3)*)0), .qtype = QTYPE_QINT, .defval = (uint8_t)0, },
    { .name = ("disable_s4"), .info = &(qdev_prop_uint8), .offset = __builtin_offsetof (PIIX4PMState, disable_s4) + ((uint8_t*)0 - (typeof(((PIIX4PMState *)0)->disable_s4)*)0), .qtype = QTYPE_QINT, .defval = (uint8_t)0, },
    { .name = ("s4_val"), .info = &(qdev_prop_uint8), .offset = __builtin_offsetof (PIIX4PMState, s4_val) + ((uint8_t*)0 - (typeof(((PIIX4PMState *)0)->s4_val)*)0), .qtype = QTYPE_QINT, .defval = (uint8_t)2, },
    {},
};

static void piix4_pm_class_init(ObjectClass *klass, void *data)
{
    DeviceClass *dc = ((DeviceClass *)object_class_dynamic_cast_assert(((ObjectClass *)((klass))), ("device")));
    PCIDeviceClass *k = ((PCIDeviceClass *)object_class_dynamic_cast_assert(((ObjectClass *)((klass))), ("pci-device")));

    k->no_hotplug = 1;
    k->init = piix4_pm_initfn;
    k->config_write = pm_write_config;
    k->vendor_id = 0x8086;
    k->device_id = 0x7113;
    k->revision = 0x03;
    k->class_id = 0x0680;
    dc->desc = "PM";
    dc->no_user = 1;
    dc->vmsd = &vmstate_acpi;
    dc->props = piix4_pm_properties;
}

static const TypeInfo piix4_pm_info = {
    .name = "PIIX4_PM",
    .parent = "pci-device",
    .instance_size = sizeof(PIIX4PMState),
    .class_init = piix4_pm_class_init,
};

static void piix4_pm_register_types(void)
{
    type_register_static(&piix4_pm_info);
}

static void __attribute__((constructor)) do_qemu_init_piix4_pm_register_types(void) { register_module_init(piix4_pm_register_types, MODULE_INIT_QOM); }

static uint64_t gpe_readb(void *opaque, hwaddr addr, unsigned width)
{
    PIIX4PMState *s = opaque;
    uint32_t val = acpi_gpe_ioport_readb(&s->ar, addr);

    do { } while (0);
    return val;
}

static void gpe_writeb(void *opaque, hwaddr addr, uint64_t val,
                       unsigned width)
{
    PIIX4PMState *s = opaque;

    acpi_gpe_ioport_writeb(&s->ar, addr, val);
    pm_update_sci(s);

    do { } while (0);
}

static const MemoryRegionOps piix4_gpe_ops = {
    .read = gpe_readb,
    .write = gpe_writeb,
    .valid.min_access_size = 1,
    .valid.max_access_size = 4,
    .impl.min_access_size = 1,
    .impl.max_access_size = 1,
    .endianness = DEVICE_LITTLE_ENDIAN,
};

static uint64_t pci_read(void *opaque, hwaddr addr, unsigned int size)
{
    PIIX4PMState *s = opaque;
    uint32_t val = 0;

    switch (addr) {
    case 0xae00 - 0xae00:


        val = s->pci0_slot_device_present & s->pci0_hotplug_enable;
        do { } while (0);
        break;
    case 0xae04 - 0xae00:
        val = s->pci0_status.down;
        do { } while (0);
        break;
    case 0xae08 - 0xae00:

        do { } while (0);
        break;
    case 0xae0c - 0xae00:
        val = s->pci0_hotplug_enable;
        break;
    default:
        break;
    }

    return val;
}

static void pci_write(void *opaque, hwaddr addr, uint64_t data,
                      unsigned int size)
{
    switch (addr) {
    case 0xae08 - 0xae00:
        acpi_piix_eject_slot(opaque, (uint32_t)data);
        do { } while (0)
                                 ;
        break;
    default:
        break;
    }
}

static const MemoryRegionOps piix4_pci_ops = {
    .read = pci_read,
    .write = pci_write,
    .endianness = DEVICE_LITTLE_ENDIAN,
    .valid = {
        .min_access_size = 4,
        .max_access_size = 4,
    },
};

static int piix4_device_hotplug(DeviceState *qdev, PCIDevice *dev,
                                PCIHotplugState state);

static void piix4_acpi_system_hot_add_init(MemoryRegion *parent,
                                           PCIBus *bus, PIIX4PMState *s)
{
    memory_region_init_io(&s->io_gpe, &piix4_gpe_ops, s, "apci-gpe0",
                          4);
    memory_region_add_subregion(parent, 0xafe0, &s->io_gpe);

    memory_region_init_io(&s->io_pci, &piix4_pci_ops, s, "apci-pci-hotplug",
                          0x000f);
    memory_region_add_subregion(parent, 0xae00,
                                &s->io_pci);
    pci_bus_hotplug(bus, piix4_device_hotplug, &s->dev.qdev);
}

static void enable_device(PIIX4PMState *s, int slot)
{
    s->ar.gpe.sts[0] |= 2;
    s->pci0_slot_device_present |= (1U << slot);
}

static void disable_device(PIIX4PMState *s, int slot)
{
    s->ar.gpe.sts[0] |= 2;
    s->pci0_status.down |= (1U << slot);
}

static int piix4_device_hotplug(DeviceState *qdev, PCIDevice *dev,
    PCIHotplugState state)
{
    int slot = (((dev->devfn) >> 3) & 0x1f);
    PIIX4PMState *s = ( __extension__ ( { char __attribute__((unused)) offset_must_be_zero[ -__builtin_offsetof (PIIX4PMState, dev)]; ({ const typeof(((PIIX4PMState *) 0)->dev) *__mptr = (((PCIDevice *)object_dynamic_cast_assert(((Object *)((qdev))), ("pci-device")))); (PIIX4PMState *) ((char *) __mptr - __builtin_offsetof (PIIX4PMState, dev));});}))
                                                 ;




    if (state == PCI_COLDPLUG_ENABLED) {
        s->pci0_slot_device_present |= (1U << slot);
        return 0;
    }

    if (state == PCI_HOTPLUG_ENABLED) {
        enable_device(s, slot);
    } else {
        disable_device(s, slot);
    }

    pm_update_sci(s);

    return 0;
}
