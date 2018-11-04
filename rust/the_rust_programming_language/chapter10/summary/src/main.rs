extern crate summary;

use summary::Summary;
use summary::NewsArticle;
use summary::Tweet;

fn main() {
    let tweet = Tweet {
        username: String::from("scarvalhojr"),
        content: String::from("WTF, World?"),
        reply: false,
        retweet: false,
    };

    let article = NewsArticle {
        headline: String::from("Penguins win the Stanley Cup Championship!"),
        location: String::from("Pittsburgh, PA, USA"),
        author: String::from("Iceburgh"),
        content: String::from("The Pittsburgh Penguins once again are the best \
            hockey team in the NHL."),
    };

    println!("1 new tweet: {}", tweet.summarize());
    println!("Tweet contents: {}", tweet.summarize_content());
    println!("New article available! {}", article.summarize());
    println!("Article contents: {}", article.summarize_content());
}
