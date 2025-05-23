import main from './._abra/_main.mjs';

function write(fd, buf, _count) {
  const str = buf.join('')
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
