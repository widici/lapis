use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref FILE_PATH: Mutex<Option<String>> = Mutex::new(None);
}

pub fn set_file_path(path: String) {
    let mut fp = FILE_PATH.lock().unwrap();
    *fp = Some(path);
}

pub(crate) fn get_file_path() -> String {
    return match FILE_PATH.lock().unwrap().clone() {
        Some(fp) => fp,
        None => unreachable!("FILE_PATH not initialized")
    }
}

#[cfg(test)]
mod tests {
    use crate::file::{set_file_path, get_file_path};

    #[test]
    fn file_path_test() {
        set_file_path("test".to_owned());
        assert_eq!(get_file_path(), "test".to_owned())
    }
}