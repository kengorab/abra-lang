import Abra from 'abra_wasm';

describe('unary & binary expressions', () => {
    test('unary expressions', () => {
        expect(Abra.runSync('!true')).toEqual(false);
        expect(Abra.runSync('!false')).toEqual(true);

        expect(Abra.runSync('-4')).toEqual(-4);
        expect(Abra.runSync('-3.1')).toEqual(-3.1);
    });

    test('simple arithmetic', () => {
        expect(Abra.runSync('23 + 1')).toEqual(24);
        expect(Abra.runSync('25 - 1')).toEqual(24);
        expect(Abra.runSync('12 * 2')).toEqual(24);
        expect(Abra.runSync('48 / 2')).toEqual(24);
    });

    test('string concatenation', () => {
        expect(Abra.runSync('"hello" + "world"')).toEqual('helloworld');
        expect(Abra.runSync('"hello" + 3')).toEqual('hello3');
        expect(Abra.runSync('3.14 +"hello"')).toEqual('3.14hello');
        expect(Abra.runSync('"hello" + true')).toEqual('hellotrue');
        expect(Abra.runSync('false + "world"')).toEqual('falseworld');
        expect(Abra.runSync('[1, 2, 3] + "world"')).toEqual('[1, 2, 3]world');
        expect(Abra.runSync('"hello" + [" ", "world"]')).toEqual('hello[" ", "world"]');
    });

    test('comparisons', () => {
        expect(Abra.runSync('3.0 < 3')).toEqual(false);
        expect(Abra.runSync('2.999 <= 3')).toEqual(true);
        expect(Abra.runSync('3 > 3.0')).toEqual(false);
        expect(Abra.runSync('2.999 >= 3')).toEqual(false);

        expect(Abra.runSync('3 == 3')).toEqual(true);
        expect(Abra.runSync('3 + 3 != 3 + 1')).toEqual(true);

        expect(Abra.runSync('[1, [2, 3]] == [1, [2, 3]]')).toEqual(true);
        expect(Abra.runSync('"hello" == "world"')).toEqual(false);
        expect(Abra.runSync('"hello" != "hello"')).toEqual(false);
        expect(Abra.runSync('"hello" != 17')).toEqual(true);
    });

    test('boolean operations', () => {
        expect(Abra.runSync('1 < 3 && 3 > 2')).toEqual(true);
        expect(Abra.runSync('2 > 3 || 3 > 2')).toEqual(true);
    });
});