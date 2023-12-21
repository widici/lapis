#[macro_export]
macro_rules! impl_error_handling {
    ($ident:ident, $location:ident) => {
        impl $ident {
            fn add_error(&mut self, kind: error::ErrorKind) {
                self.errors.push(Error::new(kind, $location))
            }

            fn report_errors(&self) {
                if self.errors.is_empty() {
                    return;
                }

                for error in self.errors.clone() {
                    print!("{:?}", error.to_report())
                }
                std::process::exit(0)
            }
        }
    };
}