import { run } from '../test-utils'

describe('builtin functions', () => {
    test('range', () => {
        const input = `
          val arr = range(0, 4)
          arr
        `;

        expect(run(input)).toEqual('[0, 1, 2, 3]');
    });

    test('println', () => {
        const input = 'println("Hello world")';
        const printlnFn = jest.fn();
        run(input, printlnFn);
        expect(printlnFn).toHaveBeenCalledWith('Hello world');
    });
});