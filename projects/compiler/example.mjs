import main from './._abra/_main.mjs';

export function write(fd, buf, _count) {
  if (fd === 1) {
    process.stdout.write(buf);
  } else if (fd === 2) {
    process.stderr.write(buf);
  }
}

const externs = {
  write,
};

main(externs);
