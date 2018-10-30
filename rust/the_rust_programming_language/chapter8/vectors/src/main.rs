
fn main() {
    let mut v0 = Vec::new();
    v0.push(5);
    v0.push(6);
    v0.push(7);

    let mut v = vec![100, 32, 57];
    for i in &mut v {
        *i += 50;
        println!("{}", i);
    }
    println!("v = {:?}", &v);
}
