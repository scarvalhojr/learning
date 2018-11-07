use std::thread;
use std::time::Duration;
use std::marker;

fn main() {
    let example_closure = |x| x;

    let _s = example_closure(String::from("hello"));
    // let _n = example_closure(5);

    environ_closure();

    let simulated_user_specified_value = 10;
    let simulated_random_number = 7;

    generate_workout(
        simulated_user_specified_value,
        simulated_random_number
    );
}

struct GenericCacher<F, B, C>
    where F: Fn(&B) -> C
{
    calculation: F,
    value: Option<C>,
    phantom: marker::PhantomData<B>
}

impl<F, B, C> GenericCacher<F, B, C>
    where F: Fn(&B) -> C, C: Copy
{
    fn new(calculation: F) -> GenericCacher<F, B, C> {
        GenericCacher {
            calculation,
            value: None,
            phantom: marker::PhantomData,
        }
    }

    fn value(&mut self, arg: &B) -> C {
        match self.value {
            Some(v) => v,
            None => {
                let v = (self.calculation)(arg);
                self.value = Some(v);
                v
            },
        }
    }
}

fn generate_workout(intensity: u32, random_number: u32) {
    let mut expensive_result = GenericCacher::new(|&num| {
        println!("calculating slowly...");
        thread::sleep(Duration::from_secs(2));
        num
    });

    let mut cheap_result = GenericCacher::new(|num: &u32| {
        *num > 10
    });

    let mut num = 20;
    println!("cheap_result({}) = {}", num, cheap_result.value(&num));
    num = 5;
    println!("cheap_result({}) = {}", num, cheap_result.value(&num));

    if intensity < 25 {
        println!(
            "Today, do {} pushups!",
            expensive_result.value(&intensity)
        );
        println!(
            "Next, do {} situps!",
            expensive_result.value(&intensity)
        );
    } else {
        if random_number == 3 {
            println!("Take a break today! Remember to stay hydrated!");
        } else {
            println!(
                "Today, run for {} minutes!",
                expensive_result.value(&intensity)
            );
        }
    }
}

fn environ_closure() {
    let x = vec![1, 2, 3];

    let equal_to_x = move |z| z == x;

    // println!("can't use x here: {:?}", x);

    let y = vec![1, 2, 3];

    println!("equal_to_x(y) = {}", equal_to_x(y));
}