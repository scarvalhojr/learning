use std::collections::HashMap;

fn main() {
    let teams  = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50, 60];
    let scores: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();

    for score in scores {
        println!("{:?}", score);
    }

    let field_name = String::from("Favorite color");
    let field_value = String::from("Blue");

    let mut prefs = HashMap::new();
    prefs.insert(field_name, field_value);

    for (k, v) in &prefs {
        println!("{} = {}", k, v);
    }

    // This won't compile
    // println!("{} = {}", field_name, field_value);

    let mut scores2 = HashMap::new();
    scores2.insert(String::from("Blue"), 20);
    scores2.insert(String::from("Blue"), 30);
    scores2.entry(String::from("Yellow")).or_insert(50);
    scores2.entry(String::from("Blue")).or_insert(50);
    println!("{:?}", scores2);

    let text = "hello world wonderful world hello life hello hell";
    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0);
        *count += 1;
    }

    println!("{:?}", map);
}
