#ifndef __ABRA_MODULE_IO_H
#define __ABRA_MODULE_IO_H

#include "abra_module.h"

#include "limits.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
#include "unistd.h"

ABRA_DEFINE_FN_0(io, getCurrentDir) {
  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd)) != NULL) {
    size_t len = strlen(cwd);
    return alloc_string(cwd, len);
  }
  return alloc_string("", 0);
}

ABRA_DEFINE_FN(io, readFile, _path) {
  AbraString* path = (AbraString*) AS_OBJ(_path);
  char* path_str = path->data;

  char abs_path[PATH_MAX];
  if (realpath(path_str, abs_path) == NULL) return ABRA_NONE;

  FILE* f = fopen(abs_path, "r");
  if (!f) return ABRA_NONE;

  int fd = fileno(f);
  struct stat st;
  fstat(fd, &st);

  off_t str_size = st.st_size;
  char* buf = (char*) malloc(sizeof(char) * (str_size + 1));
  int read_size = fread(buf, sizeof(char), str_size, f);
  buf[str_size] = 0;

  AbraValue ret;
  if (str_size != read_size) {
    ret = ABRA_NONE;
  } else {
    ret = alloc_string(buf, str_size);
  }

  free(buf);
  fclose(f);
  return ret;
}

ABRA_MODULE(io) {
  FN_SETUP(getCurrentDir, 0, io__getCurrentDir);
  FN_SETUP(readFile, 1, io__readFile);
}

#endif
