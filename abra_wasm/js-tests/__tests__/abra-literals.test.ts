import Abra from 'abra_wasm';

describe('literals', () => {
    test('evaluates integers', () => {
        expect(Abra.runSync('24')).toEqual(24);
        expect(Abra.runSync('0')).toEqual(0);
    });

    test('evaluates floats', () => {
        expect(Abra.runSync('24.0')).toEqual(24.0);
        expect(Abra.runSync('0.24')).toEqual(0.24);
    });

    test('evaluates booleans', () => {
        expect(Abra.runSync('true')).toEqual(true);
        expect(Abra.runSync('false')).toEqual(false);
    });

    test('evaluates strings', () => {
        expect(Abra.runSync('"hello world"')).toEqual('hello world');
        expect(Abra.runSync('"false"')).toEqual('false');
    });

    test('evaluates arrays', () => {
        expect(Abra.runSync('[1, 2, 3]')).toEqual([1, 2, 3]);
        expect(Abra.runSync('["a", "b", "c"]')).toEqual(['a', 'b', 'c']);
        expect(Abra.runSync('[[true, false], [false, true, true]]')).toEqual([[true, false], [false, true, true]]);
        expect(Abra.runSync('[1, true, "3", [4]]')).toEqual([1, true, '3', [4]]);
    });
});