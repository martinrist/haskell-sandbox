{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Use guards" #-}
module EffectiveHaskell.Chapter01.Examples where

message :: String
message = "Hello George"

makeGreeting salutation person =
    salutation <> " " <> person

greetPerson = makeGreeting "Hello"

enthusiasticGreeting salutation =
    makeGreeting (salutation <> "!")

half = (/2)

extendedGreeting person =
    let hello = makeGreeting helloStr person
        goodDay = makeGreeting "I hope you have a nice afternoon" person
        goodBye = makeGreeting "See you later" person
        helloStr = "Hello"
    in hello <> "\n" <> goodDay <> "\n" <> goodBye

letWhereGreeting name place =
    let
        salutation = "Hello " <> name
        meetingInfo = location "Tuesday"
    in salutation <> " " <> meetingInfo
    where
        location day = "we met at " <> place <> " on a " <> day

printSmallNumber num =
    if num < 10
    then print num
    else print "the number is too big!"

sizeNumber num =
    if num < 3
    then "that's a small number"
    else
        if num < 3
        then "that's a medium sized number"
        else "that's a big number"

guardSize num
    | num < 3 = "that's a small number"
    | num < 10 = "that's a medium number"
    | num < 100 = "that's a pretty big number"
    | num < 1000 = "wow, that's a giant number"
    | otherwise = "that's an unfathomably big number"

fizzBuzzFor number
    | 0 == number `rem` 15 = "fizzbuzz"
    | 0 == number `rem` 5 = "buzz"
    | 0 == number `rem` 3 = "fizz"
    | otherwise = show number

naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString =
    if curNum > fizzBuzzCount
    then fizzBuzzString
    else
        let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
            nextNumber = curNum + 1
        in naiveFizzBuzz fizzBuzzCount nextNumber nextFizzBuzzString

main = print "Hello, World!"

