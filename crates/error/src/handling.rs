use crate::Error;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref ERRORS: Mutex<Vec<Error>> = Mutex::new(Vec::new());
}

#[macro_export]
macro_rules! impl_error_handling {
    ($ident:ident, $location:path) => {
        impl $ident {
            fn add_error(&mut self, kind: error::ErrorKind) {
                let mut errors = error::handling::ERRORS.lock().unwrap();
                errors.push(Error::new(kind, $location))
            }
        }
    };
}

pub fn report_errors() {
    let errors = ERRORS.lock().unwrap();
    if errors.is_empty() {
        return;
    }

    for error in errors.clone() {
        eprint!("{}", error)
    }
    std::process::exit(1)
}
