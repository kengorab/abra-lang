import crypto from "crypto";
import fs from "fs";
import { $decodestr, $encodestr } from "./builtins.mjs";

export function write(fd, buf, _count) {
  const str = $decodestr(buf)

  if (fd === 1) {
    process.stdout.write(str);
  } else if (fd === 2) {
    process.stderr.write(str);
  }
}

export function strlen(str) {
  return str.indexOf(0);
}

export function getenv(key) {
  const keyStr = $decodestr(key)

  const val = process.env[keyStr];
  if (!val) return null;

  return $encodestr(val)
}

const EBADF = 1;
const EINVAL = 2;
let err = 0;
export function errno() {
  return err;
}

export function strerror(errno) {
  switch (errno) {
    case EBADF: return $encodestr("not an open file descriptor");
    case EINVAL: return $encodestr("invalid");
    default: return $encodestr("");
  }
}

export function exit(status) {
  process.exit(status);
}

export function getcwd(outbuf, count) {
  const cwd = $encodestr(process.cwd());

  outbuf.set(cwd)
}

const openFiles = []
export function open(buf, flags, mode) {
  const fd = openFiles.length;
  const pathName = $decodestr(buf)
  openFiles.push(pathName);
  return fd;
}

export function close(fd) {
  openFiles[fd] = null;
}

const LSEEK_SEEK_SET = 0;
const LSEEK_SEEK_CUR = 1;
const LSEEK_SEEK_END = 2;
export function lseek(fd, offset, whence) {
  if (whence === LSEEK_SEEK_SET) {
    return 0;
  }
  if (whence === LSEEK_SEEK_CUR) {
    // TODO
    return -1;
  }
  if (whence !== LSEEK_SEEK_END) {
    err = EINVAL;
    return -1;
  }

  const filePath = openFiles[fd]
  if (!filePath) {
    err = EBADF;
    return -1;
  }
  const stats = fs.statSync(filePath)

  if (offset >= stats.size) {
    err = EINVAL;
    return -1;
  }

  return stats.size - offset;
}

export function read(fd, outbuf, count) {
  const filePath = openFiles[fd]
  if (!filePath) {
    err = EBADF;
    return -1;
  }

  const fileBuf = fs.readFileSync(filePath)
  outbuf.set(fileBuf)
}

export function rand() {
  return crypto.randomInt(Number.MAX_SAFE_INTEGER)
}