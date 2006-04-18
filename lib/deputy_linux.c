#include <linux/config.h>    /* has to be first!  */
#include <linux/init.h>
#include <linux/module.h>
#ifdef CONFIG_KRECOVER
#include <linux/krecover.h>
#endif

asmlinkage void deputy_fail(const char *what, const char *check,
                 const char *file, int line) {
    printk(KERN_ALERT
           "Assertion \"%s\" failed in %s check at %s:%d.\n", 
           what, check, file, line);
#ifdef CONFIG_KRECOVER
    /* This will trigger real krecover recovery */
    if (kr_recovery_enabled)
        kr_trigger_fault();
#endif
}

asmlinkage int deputy_strlen(const char *str) {
    return strlen(str);
}

asmlinkage char *deputy_strcpy(char *dest, const char *src) {
    char *tmp = dest;
    while ((*dest++ = *src++) != '\0') {
        // do nothing
    }
    return tmp;
}

asmlinkage char *deputy_strncpy(char *dest, const char *src, size_t count) {
    char *tmp = dest;
    while (count >= 0) {
        if ((*tmp = *src) != 0) src++;
        tmp++;
        count--;
    }
    return dest;
}
