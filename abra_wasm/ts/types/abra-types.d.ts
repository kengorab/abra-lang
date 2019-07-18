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
        args: [string, Type][],
        returnType: Type
    }
}
