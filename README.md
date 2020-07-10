# exemplar demo

```haskell
λ> :set -XTemplateHaskell
λ> $(exemplarsOf 'foldr)
------------------------------------
-- foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int
------------------------------------
foldr (+) 1 [3, 4] = 8
foldr (*) (-1) [10] = -10
foldr const 3 [17, 100, 19] = 17
foldr (+) 4 [] = 4
foldr (*) 10 [0, 1, -1] = 0
foldr const 17 [3, 4] = 3


------------------------------------
-- foldr :: (Symbolic -> Symbolic -> Symbolic) -> Symbolic -> [Symbolic] -> Symbolic
------------------------------------
foldr (+) b [d, x] = (d + (x + b))
foldr (*) c [y] = (y * c)
foldr const d [z, p, w] = z
foldr (+) x [] = x
foldr (*) y [a, b, c] = (a * (b * (c * y)))
foldr const z [d, x] = d


------------------------------------
-- foldr :: (IO () -> IO () -> IO ()) -> IO () -> [IO ()] -> IO ()
------------------------------------
foldr (>>) (putStrLn "Hello") [putStrLn "1234", putStrLn "foo"] = [[Output "1234",Output "foo",Output "Hello"]]()
foldr (>>) (putStrLn "world!") [putStrLn "bar"] = [[Output "bar",Output "world!"]]()
foldr (>>) (putStrLn "1234") [putStrLn "The quick brown fox", putStrLn "Hello", pure ()] = [[Output "The quick brown fox",Output "Hello",Output "1234"]]()
foldr (>>) (putStrLn "foo") [] = [[Output "foo"]]()
foldr (>>) (putStrLn "bar") [pure (), putStrLn "Hello", putStrLn "world!"] = [[Output "Hello",Output "world!",Output "bar"]]()
foldr (>>) (putStrLn "The quick brown fox") [putStrLn "1234", putStrLn "foo"] = [[Output "1234",Output "foo",Output "The quick brown fox"]]()


------------------------------------
-- foldr :: (String -> String -> String) -> String -> [String] -> String
------------------------------------
foldr (++) "world!" [] = "world!"
foldr (flip (++)) "1234" ["Hello", "world!"] = "1234world!Hello"
foldr const "foo" [] = "foo"
foldr (++) "bar" ["Hello", "world!"] = "Helloworld!bar"
foldr (flip (++)) "The quick brown fox" [] = "The quick brown fox"
foldr const "Hello" ["Hello", "world!"] = "Hello"


------------------------------------
-- foldr :: (Int -> Int -> Int) -> Int -> Maybe Int -> Int
------------------------------------
foldr (+) 1 (Just 1) = 2
foldr (*) (-1) (Just (-1)) = 1
foldr const 3 (Just 3) = 3
foldr (+) 4 (Just 4) = 8
foldr (*) 10 (Just 10) = 100
foldr const 17 (Just 17) = 17


------------------------------------
-- foldr :: (Symbolic -> Symbolic -> Symbolic) -> Symbolic -> Maybe Symbolic -> Symbolic
------------------------------------
foldr (+) b (Just b) = (b + b)
foldr (*) c (Just c) = (c * c)
foldr const d (Just d) = d
foldr (+) x (Just x) = (x + x)
foldr (*) y (Just y) = (y * y)
foldr const z (Just z) = z


------------------------------------
-- foldr :: (IO () -> IO () -> IO ()) -> IO () -> Maybe IO () -> IO ()
------------------------------------
foldr (>>) (putStrLn "Hello") (Just (putStrLn "Hello")) = [[Output "Hello",Output "Hello"]]()
foldr (>>) (putStrLn "world!") (Just (putStrLn "world!")) = [[Output "world!",Output "world!"]]()
foldr (>>) (putStrLn "1234") (Just (putStrLn "1234")) = [[Output "1234",Output "1234"]]()
foldr (>>) (putStrLn "foo") (Just (putStrLn "foo")) = [[Output "foo",Output "foo"]]()
foldr (>>) (putStrLn "bar") (Just (putStrLn "bar")) = [[Output "bar",Output "bar"]]()
foldr (>>) (putStrLn "The quick brown fox") (Just (putStrLn "The quick brown fox")) = [[Output "The quick brown fox",Output "The quick brown fox"]]()


------------------------------------
-- foldr :: (String -> String -> String) -> String -> Maybe String -> String
------------------------------------
foldr (++) "world!" (Just "world!") = "world!world!"
foldr (flip (++)) "1234" (Just "1234") = "12341234"
foldr const "foo" (Just "foo") = "foo"
foldr (++) "bar" (Just "bar") = "barbar"
foldr (flip (++)) "The quick brown fox" (Just "The quick brown fox") = "The quick brown foxThe quick brown fox"
foldr const "Hello" Nothing = "Hello"
```

