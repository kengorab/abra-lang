import Abra, { CompileFailure, CompileResult } from 'abra_wasm'

describe('errors', () => {
    describe('parse errors', () => {
        test('ExpectedToken error', () => {
            const result: CompileResult = Abra.compile('func 1')!
            const expected: CompileFailure = {
                success: false,
                error: {
                    kind: 'parseError',
                    subKind: 'expectedToken',
                    expectedType: 'Ident',
                    token: {
                        kind: 'int',
                        pos: [1, 6],
                        val: 1
                    }
                }
            }
            expect(result).toEqual(expected)
        })
    })
})
