/* tslint:disable */

import { CompiledModule } from './types/compiled-module'
import { Error } from './types/error'

export interface CompileSuccess {
    success: true,
    compiledModule: CompiledModule
}

export interface CompileFailure {
    success: false,
    error: Error
}

export type CompileResult = CompileSuccess | CompileFailure

/**
 * Compiles the input string as Abra code, using the wasm implementation of the compiler.
 * This will either return an error if unsuccessful, or a compiled module if successful.
 */
export function compile(input: string): CompileResult | null;

/**
 * Compiles and executes the input string as Abra code, returning the result. This could
 * result in a runtime error.
 */
export function runSync(input: string): Error | any;

/**
 * Compiles and executes the input string as Abra code, invoking the callback with the
 * result. This could result in a runtime error, which will be passed to the callback.
 */
export function runAsync(input: string, callback: (result: any) => void): void;
