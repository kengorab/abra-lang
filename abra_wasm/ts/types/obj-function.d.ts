import { Value } from './value'

export interface ObjFunction {
    code: [string, number | null][],
    constants: Value[],
}
