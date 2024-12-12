const spawn = require('child_process').spawn;
const child = spawn('/Users/kengorab/Desktop/abra-lang/projects/lsp/._abra/abra-lsp');

child.stdin.setEncoding('utf-8');
child.stdout.pipe(process.stdout);

child.stdin.write('Content-Length: 8\r\n\r\n{"id":');
child.stdin.write('0}');
