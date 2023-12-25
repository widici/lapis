#[macro_export]
macro_rules! impl_error_handling {
    ($ident:ident, $location:path) => {
        impl $ident {
            fn add_error(&mut self, kind: error::ErrorKind) {
                self.errors.push(Error::new(kind, $location))
            }

            fn report_errors(&self) {
                if self.errors.is_empty() {
                    return;
                }

                for error in self.errors.clone() {
                    eprint!("{:?}", error.to_report())
                }
                std::process::exit(1)
            }
        }
    };
}
