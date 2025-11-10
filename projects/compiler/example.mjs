import main from "./._abra/_main.mjs";

const $td = new TextDecoder();
const $te = new TextEncoder();

function _decodeString(buf) {
  let i = buf.length - 1;
  while (i >= 0 && buf[i] === 0) { i--; }
  return $td.decode(buf.slice(0, i + 1));
}

function write(fd, buf, _count) {
  const str = _decodeString(buf)

  if (fd === 1) {
    process.stdout.write(str);
  } else if (fd === 2) {
    process.stderr.write(str);
  }
}

function strlen(str) {
  return str.length;
}

function getenv(key) {
  const keyStr = _decodeString(key)

  const val = process.env[keyStr];
  if (!val) return null;

  return $te.encode(val)
}

const externs = {
  write,
  strlen,
  getenv,
};

main(externs, process.argv.slice(1));
