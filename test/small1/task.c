/* this obscure structure definition actually comes up in the linux kernel
 * ... */

  typedef struct { } spinlock_t;

  struct task_struct {
      spinlock_t sigmask_lock; 
  };

  struct task_struct my_task;

