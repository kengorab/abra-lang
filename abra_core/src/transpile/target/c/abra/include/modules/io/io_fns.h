#ifndef __ABRA_IO_FNS_H
#define __ABRA_IO_FNS_H

#include "../../abra_module.h"

#include "limits.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"
#include "unistd.h"

// getCurrentDir(): String
ABRA_DEFINE_FN_0(io, getCurrentDir) {
  char cwd[PATH_MAX];
  if (getcwd(cwd, sizeof(cwd)) != NULL) {
    size_t len = strlen(cwd);
    return alloc_string(cwd, len);
  }
  return alloc_string("", 0);
}

// readFile(path: String): String?
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

// prompt(msg: String?): String
ABRA_DEFINE_FN(io, prompt, _msg) {
    if (!IS_NONE(_msg)) {
        AbraString* msg = (AbraString*)AS_OBJ(_msg);
        printf("%s", msg->data);
    }

    int capacity = 16;
    int size = 0;
    char* str = malloc(sizeof(char) * capacity);

    char c = getchar();
    while (c && c != '\n') {
        str[size++] = c;
        if (size == capacity) {
            capacity *= 2;
            str = realloc(str, sizeof(char) * capacity);
        }
        c = getchar();
    }

    AbraValue ret = alloc_string(str, size);
    free(str);
    return ret;
}

#endif