#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
    California,
}

#[derive(Debug)]
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

impl Coin {
    fn value(&self) -> u32 {
        match self {
            Coin::Penny => 1,
            Coin::Nickel => 5,
            Coin::Dime => 10,
            Coin::Quarter(UsState::California) => 100,
            Coin::Quarter(_) => 25,
        }
    }
}

fn main() {
    let coins = [Coin::Penny,
                 Coin::Nickel,
                 Coin::Dime,
                 Coin::Quarter(UsState::Alabama),
                 Coin::Quarter(UsState::Alaska),
                 Coin::Quarter(UsState::California)];

    for (idx, coin) in coins.iter().enumerate() {
        if let Coin::Quarter(UsState::California) = coin {
            println!(
                "Coin {} is a special {:?} and is hence worth {} cents",
                idx, coin, coin.value())
            ;
        } else {
            println!(
                "Coin {} is a {:?} and is worth {} cents",
                idx, coin, coin.value()
            );
        }
    }
}