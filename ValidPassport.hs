module ValidPassport
    (validatePassport)
where
-- https://adventofcode.com/2020/day/4

-- fields
-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID) Optional
-- iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929

contains::String  -> Bool
contains xse = let passportdetail = words xse
                   mandatory = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
                   ct = length $ filter (== True) $ [startsWith y x | x<- mandatory, y <- passportdetail]
                   in
                       if ct == 7 then True else False



-- byr:1020 byr
startsWith::String -> String -> Bool
startsWith _ [] = True
startsWith (x:xs) (v:vs) = if x /= v then False else startsWith xs vs

mylines ::[String] -> [String]
mylines [] = []
mylines ("":xs) = mylines xs
mylines xs =
    let (content, rest) = span (/= "") xs
    in unwords content : mylines rest

validatePassport::IO()
validatePassport = do
    content <- readFile "validpassport.txt"
    let rows = mylines $ lines content
    let k = length $ filter (== True) $ map contains rows
    print k
