import Abra, { ModuleReader } from 'abra_wasm';

const defaultPrintln = (str: string) => console.log(str);

export function run(input: string, println = defaultPrintln) {
    const result = Abra.run(input, { println });
    if (!result.success) {
        throw new Error(`Failed to execute input:\n${input}\nError: ${result.errorMessage}`);
    }
    return result.dataToString;
}

export function runModule(
    moduleName: string,
    moduleReader: ModuleReader,
    println = defaultPrintln
) {
    const result = Abra.runModule(moduleName, moduleReader, { println });
    if (!result.success) {
        throw new Error(`Failed to execute module ${moduleName}\nError: ${result.errorMessage}`);
    }
    return result.dataToString;
}
