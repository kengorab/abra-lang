import { runModule, typecheckModule } from 'abra_wasm';

describe('module loading', () => {
    test('runModule', () => {
        const modules: Record<string, string> = {
            './utils': `
              export func concat(a: String, b: String): String = a + b
            `,
            './strings': `
              export val a = "hello"
              export val b = "world"
            `,
            './main': `
              import concat from "./utils"
              import a, b from "./strings"

              concat(concat(a, " "), b)
            `
        }
        const moduleLoader = {
            resolveModulePath(moduleId: string, _withRespectTo: string) { return moduleId },
            readModule(moduleName: string) { return modules[moduleName] },
            getModuleName(moduleName: string) { return moduleName }
        }
        const result = runModule('./main', moduleLoader);
        expect(result.success && result.dataToString).toEqual('hello world');
    });

    test('typecheckModule', () => {
        const modules: Record<string, string> = {
            './person': `
              type Person { name: String }
              export val me = Person(name: "Ken")
            `,
            './main': `
              import me from "./person"

              me.name
            `
        }
        const moduleLoader = {
            resolveModulePath(moduleId: string, _withRespectTo: string) { return moduleId },
            readModule(moduleName: string) { return modules[moduleName] },
            getModuleName(moduleName: string) { return moduleName }
        }
        const result = typecheckModule('./main', moduleLoader);
        expect(result && result.success).toEqual(true);
    });
});