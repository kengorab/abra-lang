import Abra from 'abra_wasm'

describe('errors', () => {
    describe('parse errors', () => {
        test('ExpectedToken error', () => {
            const result = Abra.typecheck('func 1')!
            const expected = {
                success: false,
                error: {
                    kind: 'parseError',
                    subKind: 'expectedToken',
                    expectedType: 'Ident',
                    token: {
                        kind: 'int',
                        pos: [1, 6],
                        val: 1
                    },
                    range: {
                        start: [1, 6],
                        end: [1, 6]
                    }
                },
                errorMessage: `Expected token 'identifier', saw 'int' (1:6)
  |  func 1
          ^`
            }
            expect(result).toEqual(expected)
        })
    })
})
