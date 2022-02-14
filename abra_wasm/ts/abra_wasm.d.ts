/* tslint:disable */

export interface Context {
    println(value: string): void;
}

export interface ModuleReader {
    resolveModulePath(moduleId: string, withRespectTo: string): string;
    readModule(moduleName: string): string | null;
    getModuleName(moduleName: string): string;
}

export interface Range {
    start: [number, number];
    end: [number, number];
}

export interface TypecheckSuccess {
    success: true;
}

export interface TypecheckFailure {
    success: false;
    errorMessage: string;
    range: Range;
}

export type TypecheckResult = TypecheckSuccess | TypecheckFailure;

/**
 * Reads the input string as Abra code, and typechecks it.
 */
export function typecheck(input: string): TypecheckResult | null;

/**
 * Performs typechecking on the given module, using the moduleReader to resolve it. Imported
 * modules are resolved via the moduleReader as well.
 */
export function typecheckModule(moduleName: string, moduleReader: ModuleReader): TypecheckResult | null;

export interface RunSuccess {
    success: true;
    dataToString: string;
}

export interface RunFailure {
    success: false;
    errorMessage: string;
}

export type RunResult = RunSuccess | RunFailure;

/**
 * Compiles and executes the input string as Abra code, returning the result. This could
 * result in a runtime error. An optional Context may be provided,
 * but sane defaults are used within runModule itself.
 */
export function run(input: string, context?: Context): RunResult;

/**
 * Compiles and executes the given module, using the moduleReader to resolve it. Imported
 * modules are resolved via the moduleReader as well. An optional Context may be provided,
 * but sane defaults are used within runModule itself.
 */
export function runModule(moduleName: string, moduleReader: ModuleReader, context?: Context): RunResult;

export interface DisassembleSuccess {
    success: true;
    disassembled: string;
}

export interface DisassembleFailure {
    success: false;
    errorMessage: string;
    range: Range;
}

export type DisassembleResult = DisassembleSuccess | DisassembleFailure;

/**
 * Compiles the input and returns a stringified disassembly
 */
export function disassemble(input: string): DisassembleResult | null;

/**
 * Compiles and disassembles the given module, using the moduleReader to resolve it. Imported
 * modules are resolved via the moduleReader as well.
 */
export function disassembleModule(moduleName: string, moduleReader: ModuleReader): DisassembleResult | null;
