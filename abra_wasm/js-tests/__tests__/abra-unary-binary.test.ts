import Abra from 'abra_wasm';

describe('unary & binary expressions', () => {
    test('unary expressions', () => {
        expect(Abra.run('!true')).toEqual(false);
        expect(Abra.run('!false')).toEqual(true);

        expect(Abra.run('-4')).toEqual(-4);
        expect(Abra.run('-3.1')).toEqual(-3.1);
    });

    test('simple arithmetic', () => {
        expect(Abra.run('23 + 1')).toEqual(24);
        expect(Abra.run('25 - 1')).toEqual(24);
        expect(Abra.run('12 * 2')).toEqual(24);
        expect(Abra.run('48 / 2')).toEqual(24);
    });

    test('string concatenation', () => {
        expect(Abra.run('"hello" + "world"')).toEqual('helloworld');
        expect(Abra.run('"hello" + 3')).toEqual('hello3');
        expect(Abra.run('3.14 +"hello"')).toEqual('3.14hello');
        expect(Abra.run('"hello" + true')).toEqual('hellotrue');
        expect(Abra.run('false + "world"')).toEqual('falseworld');
        expect(Abra.run('[1, 2, 3] + "world"')).toEqual('[1, 2, 3]world');
        expect(Abra.run('"hello" + [" ", "world"]')).toEqual('hello[" ", "world"]');
    });

    test('comparisons', () => {
        expect(Abra.run('3.0 < 3')).toEqual(false);
        expect(Abra.run('2.999 <= 3')).toEqual(true);
        expect(Abra.run('3 > 3.0')).toEqual(false);
        expect(Abra.run('2.999 >= 3')).toEqual(false);

        expect(Abra.run('3 == 3')).toEqual(true);
        expect(Abra.run('3 + 3 != 3 + 1')).toEqual(true);

        expect(Abra.run('[1, [2, 3]] == [1, [2, 3]]')).toEqual(true);
        expect(Abra.run('"hello" == "world"')).toEqual(false);
        expect(Abra.run('"hello" != "hello"')).toEqual(false);
        expect(Abra.run('"hello" != 17')).toEqual(true);
    });

    test('boolean operations', () => {
        expect(Abra.run('1 < 3 && 3 > 2')).toEqual(true);
        expect(Abra.run('2 > 3 || 3 > 2')).toEqual(true);
    });
});