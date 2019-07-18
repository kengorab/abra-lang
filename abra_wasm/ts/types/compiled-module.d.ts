import { Chunk } from './chunk'
import { Value } from './value'
import { BindingDescriptor } from './binding-descriptor'

export interface CompiledModule {
    name: string,
    chunks: { [chunkName: string]: Chunk },
    constants: Value[],
    bindingDescriptors: BindingDescriptor[]
}