// F# is a multi-paradigm language. It combines aspects of imperative, functions, OO, and scripting
// languages.

// Functions are invoked by their name followed by their arguments, with no commas.
printfn "Hello world."
printfn "Welcome to F#!"
// printfn takes printf-style arguments (from C)
printfn "%d is an integer" 10
// Most useful: %d for int, %f for double, %s for string

// Note that we haven't defined a "main" method. Like most "scripting" languages, F# files execute 
// from top to bottom.

// Use "let" to declare a variable or a function.
let pi = 3.14159
// Notice what's missing?

// F# makes heavy use of type inference to determine types. In many cases, you will never specify
// the type of a variable or function.
// The variable "pi" does have a type: it is of type "float". In F#, float is a 64-bit floating
// point number, akin to "double" in Java/C#. How was this type determined?

let x = 10
// What type is x? How was that determined?

// F# runs on the "Common Language Runtime", which is Microsoft's equivalent of the 
// Java Virtual Machine. F# supports all types that othe "CLR" languages like C# support,
// including 1-, 2-, 4-, and 8-byte integers; 4- and 8-byte floating points; booleans;
// and 2-byte character values.

// Then how do we declare a 2-byte "short" integer?
let s = 10s

// F# defines suffixes for literal values that specify their type:
// s = "short"; "L" = long; f = "float 32"

// F# has a VERY STRICT type system that does almost NO automatic coercions.
// let f = x * 1.5
// Error: type "float" does not match type "int"
// In F#, math operators are only valid when the types of the operands match EXACTLY. 
// To fix this example, we call the "float" function on x to turn it into a float.
let f = float x * 1.5
// If it makes more sense, this line can be rewritten as
let f2 = (float x) * 1.5


// The funny thing about F# variables is that they are immutable.
s = 100s
printfn "s is now %d" s // what does this print?

// Outside of a declaration, = is used for comparison; it is not for assignment.
// F# variables cannot be assigned new values after they are declared...
// What???


// They actually can, if you declare them as "mutable":
let mutable m = 1
// And then mutate them with <-
m <- m + 1

// Example: calculate and print the amount of money in a bank account that receives
// 5% interest compounded yearly.
let mutable balance = 1000.0
let rate = 0.05 // this doesn't need to change
let mutable year = 1
let numYears = 5
while year <= numYears do
    // In F#, blocks are not denoted by braces; they are denoted by indentation after the "do" keyword.
    // Each of the following three statements is inside the "while" because they are indented
    // past the "while"
    balance <- balance * (1.0 + rate)
    printfn "After %d years, $%0.2f in the bank" year balance
    year <- year + 1


// Conditional statements
if x < 10 then
    printfn "s is less than 10"
elif x > 10 then // note: "elif"
    printfn "s is greater than 10"
else
    printfn "s is equal to 10"

// Again, we use = for comparison
let rating = "R"
let age = 14
if rating = "R" && not (age >= 18) then
    printfn "Can't see the movie!"