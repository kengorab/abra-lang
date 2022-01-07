#ifndef __ABRA_MODULE_IO_H
#define __ABRA_MODULE_IO_H

#include "io_fns.h"

ABRA_MODULE(io) {
  FN_SETUP(getCurrentDir, 0, io__getCurrentDir);
  FN_SETUP(readFile, 1, io__readFile);
}

#endif
