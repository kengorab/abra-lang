use wasm_bindgen::prelude::*;
use std::io::{Write, Error};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

pub struct Console;

impl Write for Console {
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        let mut buf = buf.to_vec();

        // This is all a bit of a hack... basically since console.log always appends a newline,
        // but we use println! within the native println function, our stdout receives 2 writes:
        // one for the message, and then one for the newlines. Since we don't want to have duplicate
        // newlines, if a message begins with a newline, trim it. This is, of course, janky and will
        // probably need to be changed in the future.
        let len = buf.len();
        if len >= 1 && buf[0] == ('\n' as u8) {
            buf.remove(0);
        }
        let len = buf.len();
        if len == 0 {
            // Returning Ok(0) indicates some underlying error... returning 1 is a bit of a hack
            return Ok(1);
        }

        let string = match String::from_utf8(buf.to_vec()) {
            Ok(string) => string,
            Err(e) => panic!("Error writing buffer: {}", e)
        };
        log(&string);
        Ok(len)
    }

    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
