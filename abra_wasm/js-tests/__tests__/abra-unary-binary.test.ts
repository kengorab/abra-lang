import { run } from '../test-utils'

describe('unary & binary expressions', () => {
    test('unary expressions', () => {
        expect(run('!true')).toEqual(false);
        expect(run('!false')).toEqual(true);

        expect(run('-4')).toEqual(-4);
        expect(run('-3.1')).toEqual(-3.1);
    });

    test('simple arithmetic', () => {
        expect(run('23 + 1')).toEqual(24);
        expect(run('25 - 1')).toEqual(24);
        expect(run('12 * 2')).toEqual(24);
        expect(run('48 / 2')).toEqual(24);
    });

    test('string concatenation', () => {
        expect(run('"hello" + "world"')).toEqual('helloworld');
        expect(run('"hello" + 3')).toEqual('hello3');
        expect(run('3.14 +"hello"')).toEqual('3.14hello');
        expect(run('"hello" + true')).toEqual('hellotrue');
        expect(run('false + "world"')).toEqual('falseworld');
        expect(run('[1, 2, 3] + "world"')).toEqual('[1, 2, 3]world');
        expect(run('"hello" + [" ", "world"]')).toEqual('hello[ , world]');
    });

    test('comparisons', () => {
        expect(run('3.0 < 3')).toEqual(false);
        expect(run('2.999 <= 3')).toEqual(true);
        expect(run('3 > 3.0')).toEqual(false);
        expect(run('2.999 >= 3')).toEqual(false);

        expect(run('3 == 3')).toEqual(true);
        expect(run('3 + 3 != 3 + 1')).toEqual(true);

        expect(run('[1, [2, 3]] == [1, [2, 3]]')).toEqual(true);
        expect(run('"hello" == "world"')).toEqual(false);
        expect(run('"hello" != "hello"')).toEqual(false);
        expect(run('"hello" != 17')).toEqual(true);
    });

    test('boolean operations', () => {
        expect(run('1 < 3 && 3 > 2')).toEqual(true);
        expect(run('2 > 3 || 3 > 2')).toEqual(true);
    });
});