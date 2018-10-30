pub mod client;

pub mod network;

#[cfg(test)]
mod tests {
    use super::network::server;
    #[test]
    fn it_works () {
        ::client::connect();
        server::connect();
        super::network::connect();
        assert_eq!(2 + 2, 4);
    }
}