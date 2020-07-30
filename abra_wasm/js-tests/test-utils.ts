import Abra from 'abra_wasm';

export const runSync = (input: string) => {
    const result = Abra.runSync(input);
    if (!result.success) {
        throw new Error(`Failed to execute input:\n${input}\nError: ${result.error}`);
    }
    return result.data;
};
