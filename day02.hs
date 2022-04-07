module Main where 
    import Text.Parsec
    import Data.Char

    rights :: [Either a b] -> [b]
    rights [] = []
    rights (Right x: xs) = x: rights xs
    rights (_:xs) = rights xs


    test:: (Int, Int, Char, String) -> Bool
    test (low, high, c, str) = let n = filter (==c) str 
                                in length n >= low && length n <= high

    solve02 :: (Int, Int, Char, String) -> Bool
    solve02 (low, high, c, str) = l `xor` h 
        where
            l = str !! (low - 1) == c
            h = str !! (high -1) == c 

    xor :: Bool -> Bool -> Bool
    xor True False = True
    xor False True = True
    xor _ _ = False
    {--
    1-3 a: abcde
    1-3 b: cdefg
    2-9 c: ccccccccc
    --}
    myparser :: Parsec String () (Int, Int, Char, String)
    myparser = do
        low <- many1 digit
        char '-'
        high <- many1 digit
        char ' '
        mychar <- letter
        string ": "
        pass <- many1 letter
        return $ (read low, read high, mychar, pass)

    main::IO()
    main = do
        rows <-  rights . map (parse myparser "")  . lines <$> readFile "input.txt"
        putStrLn $ show . length . filter (== True) . map test $ rows
        putStrLn $ show . length . filter ( == True) . map solve02 $ rows
        
