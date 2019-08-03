import Abra from 'abra_wasm';

describe('builtin functions', () => {
    test('range', () => {
        const input = `
          val arr = range(0, 4)
          arr
        `;

        expect(Abra.runSync(input)).toEqual([0, 1, 2, 3]);
    });

    test('println', () => {
        // @ts-ignore
        const oldConsole = global.console;
        // @ts-ignore
        global.console = {
            log: jest.fn()
        };

        const input = `
          println("Hello world")
        `;
        Abra.runSync(input);
        // @ts-ignore
        expect(global.console.log).toHaveBeenCalledWith('Hello world');

        // @ts-ignore
        global.console = oldConsole;
    });
});