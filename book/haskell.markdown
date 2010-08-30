---
title: Mini-introduction to Haskell
---
This chapter should give you a bare minimum introduction to Haskell to feel comfortable with the rest of the code in this book. As is often the case with web programming, you can suffice with a subset of the language to get started, and pick up more as you go along. Obviously, the more you know the better. If you already know Haskell, do yourself a favor and skip to the next chapter.

## Data types

Haskell has a concept of **algebraic data types**. I think examples will illustrate better than anything:

    data Color = Red | Green | Blue

Here we create a new **data type** called Color, which has three **constructors**: Red, Green and Blue. Notice how all of these start with a capital letter: that's a rule in Haskel. In this case, the constructors are all **nullary**, meaning they take no arguments. Another possibility could be:

    data Person = Person String Int

Here there is a single constructor, Person, that takes two arguments: a String and an Int. These might represent a name and an age. If we wanted to be more explicit about what they represent, we could use record syntax:

    data Person = Person { name :: String, age :: Int }

Record syntax creates some functions to more easily access the values in a datatype and allows you to construct values using record syntax, ie:

    let michael = Person { name = "Michael", age = 25 }
    let hisName = name michael

That second line used function application, which we'll see in a little bit. Also notice that the variable and function names are all lowercase; once again, this is a rule in Haskell.

You are able to combine different features of the above data type declarations, eg:

    data Animal = Cat { catName :: String } | Dog String

### Pattern Matching

One of the great features in Haskell is pattern matching. For example:

    case color of
        Red -> print "The color is Red"
        Green -> print "The color is Green"
        Blue -> print "The color is Blue"

With warnings turned on, the compiler can check to make sure your code accounts for every possible value in your datatype. This is one small example of Haskell's type system making your life easier.

## Functions


