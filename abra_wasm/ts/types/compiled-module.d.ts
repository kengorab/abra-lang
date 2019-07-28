import { Chunk } from './chunk'
import { Value } from './value'

export interface CompiledModule {
    name: string,
    chunks: { [chunkName: string]: Chunk },
    constants: Value[],
}