export type Type
    = Types.Int
    | Types.Unit
    | Types.Any
    | Types.Or
    | Types.Float
    | Types.String
    | Types.Bool
    | Types.Array
    | Types.Option
    | Types.Fn
    | Types._Type
    | Types.Unknown
    | Types.Struct
    | Types.Placeholder
    | Types.Reference
    | Types.Generic


export namespace Types {
    interface Int {
        kind: 'Int'
    }

    interface Unit {
        kind: 'Unit'
    }

    interface Any {
        kind: 'Any'
    }

    interface Or {
        kind: 'Or',
        options: Type[]
    }

    interface Float {
        kind: 'Float'
    }

    interface String {
        kind: 'String'
    }

    interface Bool {
        kind: 'Bool'
    }

    interface Array {
        kind: 'Array',
        innerType: Type
    }

    interface Option {
        kind: 'Option',
        innerType: Type
    }

    interface Fn {
        kind: 'Fn',
        typeArgs: string[],
        args: [string, Type][],
        returnType: Type
    }

    interface _Type {
        kind: 'type',
        name: string
    }

    interface Unknown {
        kind: 'unknown'
    }

    interface Struct {
        kind: 'unknown',
        name: string,
        typeArgs: string[],
        fields: [string, Type][],
        staticFields: [string, Type][],
        methods: [string, Type][]
    }

    interface Placeholder {
        kind: 'placeholder'
    }

    interface Reference {
        kind: 'reference',
        name: string,
        typeArgs: string[],
    }

    interface Generic {
        kind: 'generic',
        name: string
    }
}
