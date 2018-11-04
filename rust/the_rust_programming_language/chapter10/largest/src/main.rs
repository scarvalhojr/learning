fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest_with_copy<T>(list: &[T]) -> T
    where T: PartialOrd + Copy
{
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest_with_clone<T>(list: &[T]) -> T
    where T: PartialOrd + Clone
{
    let mut largest = list[0].clone();

    for item in list.iter() {
        if *item > largest {
            largest = item.clone();
        }
    }

    largest
}

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];
    let result = largest_with_copy(&number_list);
    println!("largest_with_copy({:?}) returned {}", number_list, result);

    let char_list = vec!['y', 'm', 'a', 'q'];
    let result = largest_with_clone(&char_list);
    println!("largest_with_clone({:?}) returned {}", char_list, result);

    let bool_list = vec![true, false, true, false];
    let result = largest(&bool_list);
    println!("largest({:?}) returned {}", bool_list, result);
}