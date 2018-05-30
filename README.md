# Haskell Book

My solutions to the exercises in [Haskell Programming from first principles](http://haskellbook.com/)

## Quick start

Make sure you have [Stack](https://docs.haskellstack.org/) installed.

Clone this repository then build the project with:

```
make build
```

Open GHCI with:

```
make ghci
```

Run tests with:

```
make test
```

To run, or work on, a single spec:

```
make ghci-test
> :l Chapter14Spec
> hspec spec
```

Run benchmarks with:

```
make bench
```
