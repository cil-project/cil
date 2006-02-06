#include <linux/init.h>
#include <linux/module.h>

void deputy_fail(const char *what, const char *check,
                 const char *file, int line) {
    printk(KERN_ALERT
           "Assertion \"%s\" failed in %s check at %s:%d.\n", 
           what, check, file, line);
}

int deputy_strlen(const char *str) {
    return strlen(str);
}
