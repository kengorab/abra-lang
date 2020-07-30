import { runSync } from '../test-utils'

describe('unary & binary expressions', () => {
    test('unary expressions', () => {
        expect(runSync('!true')).toEqual(false);
        expect(runSync('!false')).toEqual(true);

        expect(runSync('-4')).toEqual(-4);
        expect(runSync('-3.1')).toEqual(-3.1);
    });

    test('simple arithmetic', () => {
        expect(runSync('23 + 1')).toEqual(24);
        expect(runSync('25 - 1')).toEqual(24);
        expect(runSync('12 * 2')).toEqual(24);
        expect(runSync('48 / 2')).toEqual(24);
    });

    test('string concatenation', () => {
        expect(runSync('"hello" + "world"')).toEqual('helloworld');
        expect(runSync('"hello" + 3')).toEqual('hello3');
        expect(runSync('3.14 +"hello"')).toEqual('3.14hello');
        expect(runSync('"hello" + true')).toEqual('hellotrue');
        expect(runSync('false + "world"')).toEqual('falseworld');
        expect(runSync('[1, 2, 3] + "world"')).toEqual('1,2,3world');
        expect(runSync('"hello" + [" ", "world"]')).toEqual('hello ,world');
    });

    test('comparisons', () => {
        expect(runSync('3.0 < 3')).toEqual(false);
        expect(runSync('2.999 <= 3')).toEqual(true);
        expect(runSync('3 > 3.0')).toEqual(false);
        expect(runSync('2.999 >= 3')).toEqual(false);

        expect(runSync('3 == 3')).toEqual(true);
        expect(runSync('3 + 3 != 3 + 1')).toEqual(true);

        expect(runSync('[1, [2, 3]] == [1, [2, 3]]')).toEqual(true);
        expect(runSync('"hello" == "world"')).toEqual(false);
        expect(runSync('"hello" != "hello"')).toEqual(false);
        expect(runSync('"hello" != 17')).toEqual(true);
    });

    test('boolean operations', () => {
        expect(runSync('1 < 3 && 3 > 2')).toEqual(true);
        expect(runSync('2 > 3 || 3 > 2')).toEqual(true);
    });
});