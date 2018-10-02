# Factorial

The factorial of a non-negative integer *n*, denoted by *n!*, is
the product of all positive integers less than or equal to *n*.
For example: `5! = 5 x 4 x 3 x 2 x 1 = 120`.

## Haskell

The beauty of a functional language is that you define how the
things are rather than defining how to get the result. A factorial
can be defined as following:

```
5! = 5 x 4!
4! = 4 x 3!
...
0! = 1
```

So we can more generally describe a factorial of an integer *n*
like:

```
n! = n * (n - 1)!
0! = 1
```

Which is exactly what we have defined in the `factorial.hs` file:

```haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```
