export {}

declare global {
    module NodeJS {
        interface Global {
            __abra_func__println: (...args: any[]) => void
        }
    }
}
