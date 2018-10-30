mod outermost {
    pub fn middle_function() {
        middle_secret_function()
    }

    fn middle_secret_function() {}

    pub mod inside {
        pub fn inner_function() {
            ::outermost::middle_function();
            ::outermost::middle_secret_function();
            secret_function()
        }

        fn secret_function() {}
    }
}

pub fn try_me() {
    outermost::middle_function();
    // outermost::middle_secret_function();
    outermost::inside::inner_function();
    // outermost::inside::secret_function();
}