module Day4 where

import Control.Arrow (second)
import Data.Char (isDigit)
import Data.Map.Strict (Map, fromList, member, toList)
import Data.List (span)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)


fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Document = Map String String

testdata = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
\byr:1937 iyr:2017 cid:147 hgt:183cm\n\
\\n\
\iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
\hcl:#cfa07d byr:1929\n\
\\n\
\hcl:#ae17e1 iyr:2013\n\
\eyr:2024\n\
\ecl:brn pid:760753108 byr:1931\n\
\hgt:179cm\n\
\\n\
\hcl:#cfa07d eyr:2025 pid:166559648\n\
\iyr:2011 ecl:brn hgt:59in\n"

regroup :: String -> [String]
regroup = map unwords . splitOn [""] . lines

parse :: String -> [Document]
parse =  map (fromList . map (second (drop 1) . span (/= ':')) . words) . regroup

validFields :: Document -> Bool
validFields m = all (`member` m) fields

valid :: String -> String -> Bool
valid "byr" = number (1920,2002)
valid "iyr" = number (2010,2020)
valid "eyr" = number (2020,2030)
valid "hgt" = height
valid "hcl" = haircolor
valid "ecl" = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
valid "pid" = passportid
valid "cid" = const True
valid _ = const False

validDoc :: Document -> Bool
validDoc d = validFields d && all (uncurry valid) (toList d)

height :: String -> Bool
height x = case span isDigit x of
            (x, "cm") -> number (150,193) x
            (x, "in") -> number (56,76) x
            _         -> False

passportid :: String -> Bool
passportid pid = length pid == 9 && all isDigit pid

haircolor :: String -> Bool
haircolor ('#':xs) = length xs == 6 && all (`elem` "abcdef0123456789") xs
haircolor _ = False

number :: (Int,Int) -> String -> Bool
number (a,b) s = case readMaybe s of
                Nothing -> False
                Just x -> x >= a && x <= b


test_invalid_passports = "eyr:1972 cid:100\n\
\hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
\\n\
\iyr:2019\n\
\hcl:#602927 eyr:1967 hgt:170cm\n\
\ecl:grn pid:012533040 byr:1946\n\
\\n\
\hcl:dab227 iyr:2012\n\
\ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
\\n\
\hgt:59cm ecl:zzz\n\
\eyr:2038 hcl:74454a iyr:2023\n\
\pid:3556412378 byr:2007"

test_valid_passports = "\
\pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
\hcl:#623a2f\n\
\\n\
\eyr:2029 ecl:blu cid:129 byr:1989\n\
\iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
\\n\
\hcl:#888785\n\
\hgt:164cm byr:2001 iyr:2015 cid:88\n\
\pid:545766238 ecl:hzl\n\
\eyr:2022\n\
\\n\
\iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"