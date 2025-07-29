import main from "./._abra/_main.mjs";

function write(fd, buf, _count) {
  let i = buf.length - 1;
  while (i >= 0 && buf[i] === 0) { i--; }

  const str = new TextDecoder().decode(buf.slice(0, i + 1))
  if (fd === 1) {
    process.stdout.write(str);
  } else if (fd === 2) {
    process.stderr.write(str);
  }
}

const externs = {
  write,
};

main(externs);
