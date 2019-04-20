import Abra from 'abra_wasm';

describe('literals', () => {
    test('evaluates integers', () => {
        expect(Abra.run('24')).toEqual(24);
        expect(Abra.run('0')).toEqual(0);
    });

    test('evaluates floats', () => {
        expect(Abra.run('24.0')).toEqual(24.0);
        expect(Abra.run('0.24')).toEqual(0.24);
    });

    test('evaluates booleans', () => {
        expect(Abra.run('true')).toEqual(true);
        expect(Abra.run('false')).toEqual(false);
    });

    test('evaluates strings', () => {
        expect(Abra.run('"hello world"')).toEqual('hello world');
        expect(Abra.run('"false"')).toEqual('false');
    });

    test('evaluates arrays', () => {
        expect(Abra.run('[1, 2, 3]')).toEqual([1, 2, 3]);
        expect(Abra.run('["a", "b", "c"]')).toEqual(['a', 'b', 'c']);
        expect(Abra.run('[[true, false], [false, true, true]]')).toEqual([[true, false], [false, true, true]]);
        expect(Abra.run('[1, true, "3", [4]]')).toEqual([1, true, '3', [4]]);
    });
});