#include "gc.h"
#include "time.h"
#include "stdlib.h"

void __mod_entry(int argc, char** argv, char** envp);

int main(int argc, char** argv, char** envp) {
  GC_init();

  srand(time(NULL));

  __mod_entry(argc, argv, envp);

  return 0;
}
