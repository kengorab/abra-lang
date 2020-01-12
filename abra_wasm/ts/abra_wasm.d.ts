/* tslint:disable */

import { Error } from './types/error'
import { ObjFunction } from './types/obj-function'

export interface CompileSuccess {
    success: true,
    topLevelFunction: ObjFunction
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
 * Compiles and executes the input string as Abra code, resolving with the
 * result. This could result in a runtime error, which will also resolve as a successful Promise
 */
export function runAsync(input: string): Promise<any>;

export interface DisassembleSuccess {
    success: true,
    disassembled: string
}

export interface DisassembleFailure {
    success: false,
    error: Error
}

export type DisassembleResult = DisassembleSuccess | DisassembleFailure

/**
 * Compiles the input and returns a stringified disassembly
 */
export function disassemble(input: string): DisassembleResult | null;
