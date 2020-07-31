/* tslint:disable */

import { Error } from './types/error'
import { Module } from './types/module'

export interface TypecheckSuccess {
    success: true,
}

export interface TypecheckFailure {
    success: false,
    error: Error,
    errorMessage: string
}

export type TypecheckResult = TypecheckSuccess | TypecheckFailure

/**
 * Reads the input string as Abra code, and typechecks it, using the wasm implementation
 * of the compiler.
 * This will either return an error if unsuccessful, or nothing if successful.
 */
export function typecheck(input: string): TypecheckResult | null;

export interface CompileSuccess {
    success: true,
    module: Module
}

export interface CompileFailure {
    success: false,
    error: Error,
    errorMessage: string
}

export type CompileResult = CompileSuccess | CompileFailure

/**
 * Compiles the input string as Abra code, using the wasm implementation of the compiler.
 * This will either return an error if unsuccessful, or a compiled module if successful.
 */
export function compile(input: string): CompileResult | null;

export interface RunSuccess {
    success: true,
    data: any
}

export interface RunFailure {
    success: false,
    error: Error,
    errorMessage: string
}

export type RunResult = RunSuccess | RunFailure

/**
 * Compiles and executes the input string as Abra code, returning the result. This could
 * result in a runtime error.
 */
export function runSync(input: string): RunResult;

/**
 * Compiles and executes the input string as Abra code, resolving with the
 * result. This could result in a runtime error, which will also resolve as a successful Promise
 */
export function runAsync(input: string): Promise<RunResult>;

export interface DisassembleSuccess {
    success: true,
    disassembled: string
}

export interface DisassembleFailure {
    success: false,
    error: Error,
    errorMessage: string
}

export type DisassembleResult = DisassembleSuccess | DisassembleFailure

/**
 * Compiles the input and returns a stringified disassembly
 */
export function disassemble(input: string): DisassembleResult | null;
