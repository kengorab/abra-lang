export type Value
    = Values.Int
    | Values.Float
    | Values.Bool
    | Values.Obj
    | Values.Fn
    | Values.Nil

export namespace Values {
    interface Int {
        kind: 'int',
        value: number
    }

    interface Float {
        kind: 'float',
        value: number
    }

    interface Bool {
        kind: 'bool',
        value: boolean
    }

    interface Obj {
        kind: 'obj',
        value: ObjectValue
    }

    interface Fn {
        kind: 'fn',
        name: string
    }

    interface Nil {
        kind: 'nil'
    }
}

export type ObjectValue
    = ObjectValues.StringObj
    | ObjectValues.ArrayObj
    | ObjectValues.MapObj

export namespace ObjectValues {
    interface StringObj {
        kind: 'stringObj',
        value: string
    }

    interface ArrayObj {
        kind: 'arrayObj',
        value: Value[]
    }

    interface MapObj {
        kind: 'mapObj',
        value: [string, Value][]
    }
}