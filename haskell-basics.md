# haskell basics

### overview

- functional, modular, static typing
- lazy (expressions evaluate only when needed)

### environment

Compiler is `ghc` and interactive terminal is `ghci` (similar to the Python3
interactive terminal). Type `:h` for helpful commands. Type `:t <expr>` for the
type of an expression. Type `:l <file>` to load a Haskell file/module.

### hello world

```haskell
main :: IO()
main = do
    putStrLn "Hello World"
```

### names

Variables and type variables begin with `_` or lowercase letters. All other
names begin with uppercase letters. All names are case sensitive. The name `_`
is reserved as a wildcard in patterns. By convention, names begin with `_` if
they are expected to be unused.

- Variables:
- Constructors:
- Type variables:
- Type constructors:
- Type classes:
- Module names:

### comments

Begin with `--` for single line (like `//` in C/C++). Use `{-` and `-}` for
multi line (like `/*` and `*/` in C/C++). Legal lexemes `-->` and `|--` do not
begin a comment. Unlike C/C++, multi line comments can be nested to high depth,
requiring each `{-` to be matched with a `-}`.

### data types

- `Bool`: value is `True` or `False`, operators `&&`, `||`, `not`
- `Char`: single character, use single quotes `'`
- `String`: type is `[Char]` (character list), uses double quotes `"`. The
string `"abc"` is short for `['a','b','c']`
- `Lists`: `[]` (empty) or `a : [a]` (type a : list of type a). A list
`[e1,...,.en]` is equivalent to `e1 : e2 : ... : en : []`
- `Tuples`: comma separated items between `()`, can be empty, types may vary

`Integer` values are intuitive and have arbitrary precision. Begin with `0o` or
`0O` for octal (base 8) and `0x` or `0X` for hexadecimal (base 16).

`Int` is the fixed width signed integer type, guaranteed to be at least 30 bits.

`Fractional` values are IEEE-754 double precision. They must start with a digit,
not `.`. They can have a base 10 exponent with `e` or `E` followed by an
integer.

Exponent operators are `^` to raise a number to nonnegative integer, `^^` to
raise a fractional number to any integer, and `**` for 2 floating point numbers.

Integer division is done with the `div x y` function. Floating point division is
done with the `/` operator.

Lists can be indexed with the `!!` operator, beginning from 0. The `i`th element
of list `l` is `l !! i`.

Other builtin types:
```
data Maybe a = Nothing | Just a deriving (Eq, Ord, Read, Show)
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data Ordering = LT | EQ | GT deriving (Eq, Ord, Bounded, Enum, Read, Show)
```

Strings and lists can be concatenated with `++`.

Haskell implementations must support 0-15 size tuples, but longer is usually
supported. Standard functions are defined for up to size 7. Tuples may also be
defined by `(,) x y`, `(,,) x y z`, etc.

Errors in Haskell are denoted as ⊥ ("bottom") and indistinguishable from
non-termination.

### custom data types

Can be defined using the `data` keyword. Custom data types must start with a
uppercase letter.
```haskell
data Shape = Circle Float Float Float
areaOf :: Shape -> Float
areaOf (Circle _ _ r) = pi * r * r
```

### class interfaces

`Eq` provides equality functionality for `==` and `/=`.

`Ord` provides ordering functionality for `<`, `<=`, `>=`, `>`, and `compare`.

`Show` provides functionality to convert other types to `String`.

`Read` provides functionality to convert a `String` to other types.

`Enum` provides sequential ordering functionality, usable with the `succ x` and
`pred x` functions. Compatible types include `Bool`, `Char`, and numbers.

`Num` is used for number operations. Subclasses include `Int`, `Integer`,
`Float`, and `Double`. They can further be split up under `Integral` and
`Floating`.

### operators

Arithmetic uses standard `+`, `-`, `*`, `/`. Integer division requires using the
`div x y` function. Logical operators are `&&`, `||`, `not`. Comparisons are
intuitive with `<`, `<=`, `==`, `>=`, `>`. The `/=` operator means not equal.
Lists (and Strings) are concatenated with `++`. The ':' is used for separating
head and tail in lists. The `->` is used for function type mappings. In pattern
matching, `_` is the wildcard, `~` is for irrefutable patterns, and `@` means
"read as". The `!` forces evaluation (Haskell is lazy by default).

The `$` is infix application, with `f $ g` meaning `f g`. It enables writing
expressions like `f (g x)` without parenthesis as `f $ g x`.

### expressions

Sample parses

| expression                 | parsed as                      |
|----------------------------|--------------------------------|
| `f x + g y`                | `(f x) + (g y)`                |
| `- f x + y`                | `(- (f x)) + y`                |
| `let { ... } in x + y`     | `let { ... } in (x + y)`       |
| `z + let { ... } in x + y` | `z + (let { ... } in (x + y))` |
| `f x y :: Int`             | `(f x y) :: Int`               |
| `\ x -> a+b :: Int`        | `\ x -> ((a+b) :: Int)`        |

Lambda functions: `\ p1 .. pn -> expr` (`n` variables)

Function application is `e1 e2` and left associative so `(f x) y` can simply be
`f x y`.

Binary infix operators may be used like `a + b` or `(+) a b`. For `-e`, it can
also be written as `negate (e)`. Similar is possible with functions of 2
variables: `f x y` can be written as ``x `f` y``.

There is a `subtract exp` function equivalent to `(+ (- exp))`.

Conditionals can be written as `if e1 then e2 else e3` or `case e1 of {
True -> e2 ; False -> e3 }`

Arithmetic sequence `[a..b]` is list `[a,a+1,a+2,..,b]` and `[a,b..c]` is list
`[a,a+(b-a),a+2*(b-a),..,c]`. The value after `..` can be eliminated for an
infinite list.

List comprehensions are written as `[exp | q1, ..., qn]`. There are other ways
to use these, but the most common is having each `qi` be `xi <- [list]` which
gives values for variable `xi`. The list for `xi` may depend on previous vars.
The order of generating values is like row-major order. Use the first value for
`x1`, then go through all possibilities of the other variables before using the
next value for `x1`.

Let expressions define a value to be used in an expression. Write as
`let p = e1 in e`. Multiple values can be defined like
`let {p1=e1;...;pn=en} in e`.

Case expressions are written like `case e of {p1 -> e1;...;pn -> en}`

Do expressions are `do { stmt1 ... stmtn exp }`. Each statement can be
`pattern <- exp;` or `let ...;` or `;` (empty statement).

### functions

Defining a function is done by specifying a name (beginning in lowercase),
arguments, and types which can sometimes be inferred by the compiler. Different
cases of arguments can be specified and pattern matching is done from first to
last. Some examples below:
```haskell
-- recursive add that moves 2nd argument toward 0
add :: Integer -> Integer -> Integer
add x 0 = x
add x y = if y > 0 then add (x+1) (y-1) else add (x-1) (y+1)
```

Guards may be used, similar to pattern matching. They test for a condition on
the inputs. The `otherwise` keyword is used when none of the other cases work.
```haskell
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0 = n * (factorial (n-1))
            | otherwise = (-1) -- invalid input
```

The where clause can be helpful for more complex calculations, breaking them
into smaller parts.
```haskell
-- quadratic formula
zeroes :: (Float,Float,Float) -> (Float,Float)
zeroes (a,b,c) = (x1,x2) where
    x1 = extrema + dist
    x2 = extrema - dist
    dist = (sqrt det) / (2.0*a)
    det = b*b - 4.0*a*c
    extrema = -b / (2.0*a)
```

Higher order functions use other functions as input. For example, a definition
for `map`:
```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (h:t) = f h : map f t

-- variant to map several functions to a list, keeping the same type
multimap :: [a -> a] -> [a] -> [a]
multimap [] l = l
multimap (h:t) l = multimap t (map h l)
```

Function composition uses the '.' operator. For example, `(floor.sqrt) x` would
compute an upper bound for trial division factoring of `x`.

### exceptions

They can be handled by a case statement. An example below:
```haskell
import Control.Exception
main = do
    result <- try (evaluate (div 2 0)) :: IO (Either SomeException Int)
    case result of
        Left ex -> putStrLn $ "exception: " ++ show ex
        Right val -> putStrLn $ "success: " ++ show val
```

### monads

TODO

### modules

Module creation is done in its own file beginning with a list of things in the
module, followed by their definitions.
```haskell
module ModuleName (
    isEven,
    isOdd,
    bool2int
) where

isEven :: Integer -> Bool
isEven x = x `rem` 2 == 0

isOdd :: Integer -> Bool
isOdd x = not (isEven x)

bool2int :: Bool -> Int
bool2int b = if b then 1 else 0
```

### builtin stuff

Math stuff 
- `abs x` absolute value
- `signum x` sign of `x` (`-1` or `0` or `1`)
- `negate x` same as `-x`
- `mod x y` modular arithmetic
- `ceiling x` ceiling of a floating point
- `floor x` floor of a floating point
- `truncate x` nearest integer between `0` and `x` inclusive
- `round x` nearest integer, round to even if exactly in the middle
- `properFraction x` splits `x=n+f`, `n` is integer, `0.0 <= f < 1.0`
- `quot x y` integer division (truncated to 0)
- `rem x y` division remainder, satisfies `(quot x y)*y + (rem x y) == x`
- `quotRem x y` returns `(quot x y, rem x y)`
- `div x y` integer division (truncated to -infinity)
- `mod x y` division remainder, satisfies `(div x y)*y + (mod x y) == x`
- `divMod x y` returns `(div x y, mod x y)`
- `gcd x y`
- `sqrt x`
- `min x y`
- `max x y`

Floating point stuff
- `isNaN x`
- `isInfinite x`
- `isDenormalized x`
- `isNegativeZero x`
- `isIEEE x`

List stuff
- `length x` list length
- `head l` list head (cannot be empty)
- `tail l` list tail (all except first element, cannot be empty)
- `last l` last list element
- `init l` all but last element of list
- `null l` is list empty
- `reverse l` elements in reverse order
- `take n l` first n elements
- `drop n l` all elements after the first n
- `maximum x` max element
- `minimum x` min element
- `sum l` addition fold
- `product l` multiplication fold
- `elem e l` is `e` in `l`

### other standard modules

`Data.List`
- `intersperse '.' "abc" == "a.b.c"` inserts new element between list elements
- `intercalate " " ["a","b","c"] == "a b c"` concatenates lists with a list
inserted between each provided list
- `splitAt 5 "HelloWorld" == ["Hello","World"]` split into 2, similar to
`l[:5]` and `l[5:]` in Python3.
- `sort l` sort elements

`Data.Char`
- `toUpper c`
- `toLower c`
- `words s` split string into words

`Data.Set`

### input/output

Output to stdout
- `putChar :: Char -> IO ()`
- `putStr :: String -> IO ()`
- `putStrLn :: String -> IO ()` adds `\n` to end
- `print :: Show a => a -> IO ()`

Input from stdin
- `getChar :: IO Char` exception on EOF, identified with `isEOFError`
- `getLine :: IO String`
- `getContents :: IO String`
- `interact :: (String -> String) -> IO ()`
- `readIO :: Read a => String -> IO a`
- `readLn :: Read a => IO a`

File IO
- `writeFile f s` writes string `s` to file `f`
- `readFile f` reads contents of file `f`

Operator `a >> b` ignores the result of `a`. Operator `a >>= b` passes the
result of `a` as input to `b`.

### sources

https://www.tutorialspoint.com/haskell/index.htm
https://www.haskell.org/onlinereport/haskell2010/
https://imada.sdu.dk/~rolf/Edu/DM22/F07/haskell-operatorer.pdf
