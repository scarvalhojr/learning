enum TrafficLight {
    Red,
    Yellow,
    Green,
}

use TrafficLight::{Red, Yellow};

pub mod a {
    pub mod series {
        pub mod of {
            pub fn nested_modules() {}
        }
    }
}

use a::series::of;

fn main() {
    let _red = Red;
    let _yellow = Yellow;
    let _green = TrafficLight::Green;
    of::nested_modules();
}