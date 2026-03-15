const $td = new TextDecoder();
const $te = new TextEncoder();

export function $mkstr(length, _buffer) {
  return { length, _buffer: $encodestr(_buffer) };
}

export function $interp(strs, ...exprs) {
  let totalLength = 0;
  const bufs = []
  for (let i = 0; i < strs.length; i++) {
    const strBuf = $encodestr(strs[i]);
    const len = strBuf.indexOf(0);
    totalLength += len;
    bufs.push([len, strBuf]);
    if (exprs[i]) {
      const { length, _buffer } = exprs[i];
      totalLength += length;
      bufs.push([length, _buffer]);
    }
  }
  const res = new Uint8Array(totalLength);
  let offset = 0;
  for (const [len, buf] of bufs) {
    res.set(buf.subarray(0, len), offset);
    offset += len;
  }
  return res
}

export function $mkargs(args) {
  return args.map(a => $encodestr(a));
}

export function $decodestr(buf) {
  let i = buf.length - 1;
  while (i >= 0 && buf[i] === 0) { i--; }
  return $td.decode(buf.slice(0, i + 1));
}

export function $encodestr(str) {
  return $te.encode(str + '\0');
}
