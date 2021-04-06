# haskell basics

### overview

- functional, modular, static typing
- lazy (expressions evaluate only when needed)

### environment

Compiler is `ghc` and interactive terminal is `ghci` (similar to the Python3
interactive terminal). Type `:h` for helpful commands. Type `:t <expr>` for the
type of an expression.

### hello world

```haskell
main = putStrLn "Hello World"
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

### operators

Arithmetic uses standard `+`, `-`, `*`, `/`. Integer division requires using the
`div x y` function. Logical operators are `&&`, `||`, `not`. Comparisons are
intuitive with `<`, `<=`, `==`, `>=`, `>`. The `/=` operator means not equal.
Lists (and Strings) are concatenated with `++`. The ':' is used for separating
head and tail in lists. The `->` is used for function type mappings. In pattern
matching, `_` is the wildcard, `~` is for irrefutable patterns, and `@` means
"read as". The `!` forces evaluation (Haskell is lazy by default).

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

TODO describe creating functions and types and stuff

### modules

TODO describe creating and importing

### builtin stuff

- `abs x` absolute value
- `signum x` sign of `x` (`-1` or `0` or `1`)
- `negate x` same as `-x`
- `mod x y` modular arithmetic
- `length x` list length
- `ceiling x` ceiling of a floating point
- `floor x` floor of a floating point
- `truncate x` nearest integer between `0` and `x` inclusive
- `round x` nearest integer, round to even if exactly in the middle
- `properFraction x` splits `x=n+f`, `n` is integer, `0.0 <= f < 1.0`
- `div x y` integer division
- `gcd x y`
- `isNaN x`
- `isInfinite x`
- `isDenormalized x`
- `isNegativeZero x`
- `isIEEE x`

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
