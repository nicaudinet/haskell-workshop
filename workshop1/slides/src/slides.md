---
title: "Haskell WorkShop"
subtitle: "Writing your first Haskell programm"
author: "Nicolas Audinet"
institute: "RELEX Solutions Dev Day"
date: 2019-04-12
#background-image: src/solarized-mountains.png
#background-image: vector/relex-logo-black-rgb.pdf
titlegraphic: vector/relex-logo-black-rgb.pdf
classoption: "aspectratio=169"
classoption: "13pt"
outertheme: "infolines"
innertheme: "circles"
colortheme: "dove"
navigation: "frame"
lang: "en-US"
---

## The Goal

Develop a complete Haskell program that converts CSV data to JSON.

![](images/goal.pdf)

##

![](images/data.png){ height=90% }

## The Plan

1) Haskell Development Environment
2) Hello World and some Syntax
3) The Data Model
4) Data Transformation
5) Connecting to the Outside

# The Haskell Development Environment

## Setting up your environment

- Go to [the Github repo](https://github.com/relex/haskell-workshop) for the workshop
(https://github.com/relex/haskell-workshop)

- Clone it locally

- Go to workshop1/Exercise.md

- Complete "Set up tooling" section

## Stack

* One of Haskell's build tools.
* To build a project: `stack build`
* To access the REPL: `stack repl`

![Basic repl](images/basic-repl.png)

# Hello Syntax

## Playing with the REPL

* Useful tool to try things out parts of your program
* Extensively used in the workshop

<!-- Live REPL Coding
* Let binding
* Numbers & mathematical operators
* Strings
* Defining a function
-->

## Hello World

* Open src/Main.hs
* Run in the REPL by typing `main`

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

* `main` is special:
  - Present in every Haskell program
  - No arguments
  - Performs an IO action

## Functions and Types

* The primary way of defining computation is with **functions**
* **Types** describe the inputs and outputs of functions
* Working with two languages at once :)

```haskell
identity :: Int -> Int
identity x = x

hello :: String -> String
hello name = "Hello, " ++ name

addInt :: Int -> Int -> Int
addInt x y = x + y
```

## A First Function

* A function that converts (some) integers to words
* Pattern matching

```haskell
intToWord :: Int -> String
intToWord 1 = "one"
intToWord 2 = "two"
intToWord 3 = "three"
intToWord _ = "dunno"
```

## Polymorphic Types

* Some functions have the same behaviour for values of different types
  * E.g. the `identity` function
* Can generalise functions by using *type variables*

```haskell
identity :: a -> a
identity x = x
```

* Can also *constrain* the type variables

```haskell
add :: Num a => a -> a -> a
add x y = x + y
```

* These constraints can be defined using *type classes*

## Composition

* Using the output of one function as the input of another function: *"chaining functions together"*
* Use the dot operator for composition

```haskell
f :: Int -> Int
f x = x + 1

g :: Int -> Int
g x = x * 2

h :: Int -> Int
h = g . f

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = g (f x)
```


# The Data Model

## Goal

* Create a data structure that accurately models the data
* Introduce `Maybe`
* Introduce lists

### Approach

* Define a model for a single row of data (a candidate)
* Apply this model to all the rows

![](images/data-model.pdf)

## A Single Candidate

![](images/small-data.png)

* A candidate has 4 attributes:
  - constituency
  - party
  - sex
  - average age

* Need a way of grouping these together

## Data Types

A simple data type:

```haskell
data Person = MakePerson
  { name :: String
  , age  :: Int
  } deriving Show
```

* `Person` is the type being defined
* `MakePerson` is the constructor function
* `name, age` are fields with associated types
* `deriving Show` allows the use of the `show` function
  * `show` converts structure to a string

## Working with Data Types

* Creating a new data type:

```haskell
MakePerson :: String -> String -> Person
```
* Accessing the data type:

```haskell
name :: Person -> String
age :: Person -> Int
```
![](images/simple-datatype.png)

## The Maybe Type

* What happens if we cannot parse a candidate?
* Need some way to represent the failure case:

```haskell
data Maybe a = Nothing | Just a
```

* `Maybe` is the type being defined
* `a` is a *type variable* (could be any type)
* `Nothing, Just` are *both* constructors

## The List Type

* How do we represent a list of candidates?
* Use singly-linked lists:
  * `[]` is the empty list
  * `:` lets you append values to the front
  * `[a,b,c]` == `(a : b : c : [])`

![](images/list.png){ height=40% }

## The Data Model

* Define our basic candidate type:

```haskell
data Candidate = Candidate
  { constituency :: String
  , party        :: String
  , sex          :: String
  , averageAge   :: Double
  } deriving Show
```

* Add a notion of failure: `Maybe Candidate`
* Create a list of candidates: `[Maybe Candidate]`

# Data Transformation

## Goal

* Write some functions to:
  * Convert CSV into data model
  * Convert data model into JSON
* Demonstrate power of `Generic`
* Introduce `map`

### Approach

* Write a function `parseLine :: CSV -> Candidate`
* Generate a function `encode :: Candidate -> JSON`
* Apply these to every row

![](images/data-transformation.pdf)

## Writing `parseLine :: CSV -> Candidate`

```haskell
parseLine :: String -> Maybe Candidate
parseLine line =
  case splitOn ',' line of
    [c, p, s, a] ->
      case readMaybe a of
        Nothing -> Nothing
        Just a' -> Candidate c p s a'
    _ -> Nothing
```

* Use case statement to pattern match on possible outcomes
* `_` means "for every other value"
* `splitOn` splits a string into a list of strings on a character
* `readMaybe` decodes a double from a string (may fail)

ps: there's an error in the code above, try and find it!

## Stripping Quotes

```haskell
stripQ :: String -> String
stripQ = leftStrip . rightStrip
  where
    leftStrip :: String -> String
    leftStrip ('"' : xs) = xs
    leftStrip xs         = xs

    rightStrip :: String -> String
    rightStrip = undefined
```

- Use `leftStrip` and `reverse` to implement `rightStrip`
* A string is literally a list of characters (`Char`)
* `where` used to define local functions
* Point-free style
* composition FTW!

## parseLine v2

* Using `stripQ` to improve `parseLine`

```haskell
parseLine :: String -> Maybe Candidate
parseLine line =
  case splitOn ',' line of
    [c, p, s, a] ->
      case readMaybe a of
        Nothing -> Nothing
        Just a' ->
          let c' = stripQ c
              p' = stripQ p
              s' = stripQ s
          in Just (Candidate c' p' s' a')
    _ -> Nothing
```

## Generating `encode :: Candidate -> JSON`

```haskell
{-# LANGUAGE DeriveGeneric #-}
main Main where

import qualified Data.Aeson      as JSON
import qualified Data.ByteString as B

data Candidate =
  ...
  deriving (Show, Generic)

instance JSON.ToJSON Candidate
```

* Language extensions expand the Haskell language
* `Data.Aeson` provides encoding to and from JSON
* `Data.ByteString` provides efficient binary strings
* Ta-da! We can use JSON.encode now

## Using `JSON.encode`

* Try it out in the REPL!

![](images/encoding.png)

## The `map` function

* How do we apply `parseLine` to every row?
* Use `map`, a *higher-order function*
* Takes a function and a list as input and applies the function to every item in the list

```haskell
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

* Defined using *recursion*
* Can apply `parseLine` to structures of type `[String]`

## The Full Parser

* We are ready to write the full parser function:
* `lines` splits strings into lines
* `tail` removes the first item in a list

```haskell
parseFile :: String -> [Maybe Candidate]
parseFile = map parseLine . tail . lines
```

* We don't have to write this for encoding

# Connecting to the Outside

## Goal

* Write a complete program!
* Introduce IO and do notation

### Approach

* Introduce purity and impurity
* Write main function

## Purity in Haskell

* All functions we have written in the previous section are *pure*
* A pure function **always** returns the same *output* when given the same *input*
* This behaviour is *enforced by the compiler*
* Why?
  * Code is easier to refactor and reason about
  * Interactions with the outside world are more explicit
  * Easy to test
* Pure functions cannot interact with the world
* So how do we communicate with the outside world?

## IO and do notation

* Communication with the outside handled through IO
* Impurity is always explicitly encoded in the types

![](images/io.pdf){ width=100% }

## Read / Write

## The Complete App

# Conclusion

## Conclusion

## Challenges

## Further Reading

##

![](images/thats-all-folks.png)
