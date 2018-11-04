struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }

    fn y(&self) -> &T {
        &self.y
    }
}

impl Point<char> {
    fn reset(&mut self) {
        self.x = 'x';
        self.y = 'y';
    }
}

fn main() {
    let p1 = Point { x: 5, y: 10 };
    let mut p2 = Point { x: 'a', y: 'b' };

    println!("p1.x = {}, p1.y = {}", p1.x(), p1.y());
    println!("p2.x = {}, p2.y = {}", p2.x(), p2.y());

    p2.reset();
    println!("After reset: p2.x = {}, p2.y = {}", p2.x(), p2.y());
}