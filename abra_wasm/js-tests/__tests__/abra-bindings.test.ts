import Abra from 'abra_wasm';

describe('bindings', () => {
    test('setting variables', () => {
        const input = `
            val a = 1
            val b = 2 * a
            val c = a + b + 3
            c
        `;
        expect(Abra.run(input)).toEqual(6);
    });

    test('reassigning to variables', () => {
        const input = `
            var a: Int
            a = 1
            a = 2 * a
            a = a * 3
            a
        `;
        expect(Abra.run(input)).toEqual(6);
    });

    test('chained reassignment', () => {
        const input = `
            var a = 5
            var b = 6
            var c = 7
            val d = c = b = a = a + b + c + 1
            val e = a + b + c + d
            e
        `;
        expect(Abra.run(input)).toEqual(76);
    });
});