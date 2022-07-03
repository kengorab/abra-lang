#include "gc.h"
#include "time.h"
#include "stdlib.h"

void __mod_entry();

int main() {
  GC_init();

  srand(time(NULL));

  __mod_entry();

  return 0;
}
