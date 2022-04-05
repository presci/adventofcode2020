module Main where
    import qualified Data.Set as Set
    import Text.Printf (printf)

    
    solve01::[Int] -> Int
    solve01 xss = solve01' Set.empty xss
        where 
            solve01' :: Set.Set Int -> [Int] -> Int
            solve01' _ [] = error "not found"
            solve01' set (x:xs) = case Set.member (2020 - x) set of
                True -> x * (2020 - x) 
                _ -> solve01' (Set.insert x set)  xs

    solve02 ::Set.Set Int -> [Int] -> Int
    solve02 set (x:xs) = solve02' set (2020 - x) xs
        where 
            solve02' :: Set.Set Int -> Int -> [Int] -> Int
            solve02' aset  _ [] = solve02 aset xs 
            solve02' aset val (l:lx) = case Set.member (val - l) aset of
                True -> (2020 - val) * l * (val - l)
                _ -> solve02' (Set.insert l aset) val lx


    main::IO()
    main = do
        arr <-  map ( read::String-> Int). lines <$> readFile "input.txt"
        putStrLn .printf "%d " $ solve01 arr
        putStrLn .printf "%d " $ solve02 Set.empty arr
