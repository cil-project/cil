typedef struct {
    char x;             // this field is only important for automating
                        // the test, we fail even without it
    unsigned dns_resolved:1;
} uri_components;

int main() {
    if (sizeof(uri_components) != 4) {
        printf("ERROR: we dropped the bitfield\n");
        return 1;
    } else {
        printf("Passed.\n");
        return 0;
    } 
}
