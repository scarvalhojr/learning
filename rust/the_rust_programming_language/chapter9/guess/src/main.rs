extern crate guess;

// use guess::Guess;

fn main() {
    let myguess = guess::Guess::new(15);

    // Can't access private field
    // myguess.value = 101;
    
    println!("Guess is {}", myguess.value());
}
