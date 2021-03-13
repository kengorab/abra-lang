import { run } from '../test-utils'

describe('literals', () => {

    test('evaluates integers', () => {
        expect(run('24')).toEqual('24');
        expect(run('0')).toEqual('0');
    });

    test('evaluates floats', () => {
        expect(run('24.0')).toEqual('24');
        expect(run('0.24')).toEqual('0.24');
    });

    test('evaluates booleans', () => {
        expect(run('true')).toEqual('true');
        expect(run('false')).toEqual('false');
    });

    test('evaluates strings', () => {
        expect(run('"hello world"')).toEqual('hello world');
        expect(run('"false"')).toEqual('false');
    });

    test('evaluates arrays', () => {
        expect(run('[1, 2, 3]')).toEqual('[1, 2, 3]');
        expect(run('["a", "b", "c"]')).toEqual('[a, b, c]');
        expect(run('[[true, false], [false, true, true]]')).toEqual('[[true, false], [false, true, true]]');
        expect(run('[1, true, "3", [4]]')).toEqual('[1, true, 3, [4]]');
    });
});