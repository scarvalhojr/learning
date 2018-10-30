use std::io;
use std::io::Read;
use std::io::ErrorKind;
use std::fs::File;

fn main() {
   match_result();
   closures();
   expect_error();

   let username = match read_username_from_file() {
       Ok(s) => s,
       Err(err) => panic!("Failed to read file: {}", err)
   };

   println!("Username: {}", username);

   let username = match question_mark_operator() {
       Ok(s) => s,
       Err(err) => panic!("Failed to read file: {}", err)
   };

   println!("Username: {}", username);
}

fn match_result() {
    let _f = match File::open("hello.txt") {
        Ok(file) => file,
        Err(error) => match error.kind() {
            ErrorKind::NotFound => match File::create("hello.txt") {
                Ok(fc) => fc,
                Err(e) => panic!("Tried to create file but there was a problem: {:?}", e),
            },
            other_error => panic!("There was a problem opening the file: {:?}", other_error),
        },
    };
}

fn closures() {
    let _f = File::open("hello.txt").map_err(|error| {
        if error.kind() == ErrorKind::NotFound {
            File::create("hello.txt").unwrap_or_else(|error| {
                panic!("Tried to create file but there was a problem: {:?}", error);
            })
        } else {
            panic!("There was a problem opening the file: {:?}", error);
        }
    });
}

fn expect_error() {
    let _f = File::open("hello.txt").expect("Failed to open hello.txt");
}

fn read_username_from_file() -> Result<String, io::Error> {
    let mut f = match File::open("hello.txt") {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}

fn question_mark_operator() -> Result<String, io::Error> {
    let mut s = String::new();
    File::open("hello.txt")?.read_to_string(&mut s)?;
    Ok(s)
}