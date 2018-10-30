fn main() {
    let strings = vec![String::from("السلام عليكم"),
        String::from("Dobrý den"),
        String::from("Hello"),
        String::from("שָׁלוֹם"),
        String::from("नमस्ते"),
        String::from("こんにちは"),
        String::from("안녕하세요"),
        String::from("你好"),
        String::from("Olá"),
        String::from("Здравствуйте"),
        String::from("Hola"),
        String::from("नमस्ते")];
    
    let mut all_in_one = String::new();

    for s in &strings {
        for c in s.chars() {
            print!("[ {} ]", c);
            all_in_one.push(c);
        }
        print!(" => ");
        all_in_one.push_str(", ");
        for c in s.chars() {
            print!("[ ");
            for u in c.escape_unicode() {
                print!("{}", u);
            }
            print!(" ]");
        }
        println!();
    }

    all_in_one.pop();
    all_in_one.pop();
    all_in_one.push('!');
    println!("All together now: {}", all_in_one);

    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2; // Note s1 has been moved here and can no longer be used
    println!("s1 {} = {}", s2, s3);

    let s4 = String::from("tic");
    let s5 = String::from("tac");
    let s6 = String::from("toe");
    let s7 = s4 + "-" + &s5 + "-" + &s6;
    println!("s4-{}-{} = {}", s5, s6, s7);

    for b in "नमस्ते".bytes() {
        println!("{}", b);
    }
}
