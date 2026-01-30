import * as path from 'path';
import * as externs from '../src/target-js/externs.node.mjs';
import * as builtins from '../src/target-js/builtins.mjs';

const args = process.argv;
const [, , target] = args.splice(0, 3);

if (!target) {
    console.error('Path to target file required');
    process.exit(1);
}

let filePath = target
if (!path.isAbsolute(filePath)) {
    filePath = path.join(process.cwd(), filePath);
}

const { default: main } = await import(filePath);
main(externs, builtins, args);
