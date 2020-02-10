import { Value } from './value'

export interface Module {
    code: [string, number[] | null][],
    constants: Value[],
}
