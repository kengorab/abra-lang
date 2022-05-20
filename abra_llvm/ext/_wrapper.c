#include "gc.h"

typedef union {
    long long i64;
    double d;
} double_int64_transmute;

long long transmute_double_int64(double value) {
    double_int64_transmute t = {.d = value};
    return t.i64;
}

double transmute_int64_double(long long value) {
    double_int64_transmute t = {.i64 = value};
    return t.d;
}

void __mod_entry();

int main() {
    GC_init();

    __mod_entry();
    return 0;
}
