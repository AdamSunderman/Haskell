foo :: Int -> Int -> Int
foo x 0 = x + 2
foo x y = x + y

foo (foo 0 0) 0
=> foo 0 0 + 2
=> (0 + 2) + 2
=>* 4

foo (foo 0 0) 0
=> foo (0 + 2) 0
=> (0 + 2) + 2
=>* 4

foo (foo 0 0) 0
=> foo (0 + 2) 0
=> foo 2 0
=> 2 + 2
=> 4

-- ALL THE SAME!
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) :: (b -> c) -> ((a -> b) -> (a -> c))

-- NOT THE SAME
(.) :: b -> c -> a -> b -> a -> c
(.) :: b -> c -> (a -> b) -> a -> c