struct ImportantExcerpt<'a> {
    part: &'a str,
}

fn main() {
    // Scenario 1 okay
    let string1 = String::from("abcd");
    let string2 = "xyz";
    let result1 = longest(string1.as_str(), string2);
    println!("result1: The longest string is '{}'", result1);

    // Scenario 2 okay
    let string3 = String::from("long string is long");
    {
        let string4 = String::from("xyz");
        let result2 = longest(string3.as_str(), string4.as_str());
        println!("result2: The longest string is '{}'", result2);
    }

    // Scenario 3 fails
    // let string5 = String::from("long string is long");
    // let result3;
    // {
    //     let string6 = String::from("xyz");
    //     result3 = longest(string5.as_str(), string6.as_str());
    //     println!("result3: The longest string is '{}'", result3);
    // }

    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.')
        .next()
        .expect("Could not find a '.'");
    println!("first_sentence = {}", first_sentence);
    let i = ImportantExcerpt { part: first_sentence };
    println!("i.part = {}", i.part);
}

fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}