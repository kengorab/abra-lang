import { runSync } from '../test-utils'

describe('literals', () => {

    test('evaluates integers', () => {
        expect(runSync('24')).toEqual(24);
        expect(runSync('0')).toEqual(0);
    });

    test('evaluates floats', () => {
        expect(runSync('24.0')).toEqual(24.0);
        expect(runSync('0.24')).toEqual(0.24);
    });

    test('evaluates booleans', () => {
        expect(runSync('true')).toEqual(true);
        expect(runSync('false')).toEqual(false);
    });

    test('evaluates strings', () => {
        expect(runSync('"hello world"')).toEqual('hello world');
        expect(runSync('"false"')).toEqual('false');
    });

    test('evaluates arrays', () => {
        expect(runSync('[1, 2, 3]')).toEqual([1, 2, 3]);
        expect(runSync('["a", "b", "c"]')).toEqual(['a', 'b', 'c']);
        expect(runSync('[[true, false], [false, true, true]]')).toEqual([[true, false], [false, true, true]]);
        expect(runSync('[1, true, "3", [4]]')).toEqual([1, true, '3', [4]]);
    });
});