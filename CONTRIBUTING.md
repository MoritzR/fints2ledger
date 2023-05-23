# Contributing

Contributions are welcome.
If you want to contribute to the code here is a little guide.

## Setup

The best developer experience is available by using [GHCup](https://www.haskell.org/ghcup/).
It will install the Haskell compiler, build tools and the language server (for IDE support).

You can use [VSCode](https://code.visualstudio.com/) as your IDE, which supports Haskell through the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).

Next, set up the project:

```
git clone https://github.com/MoritzR/fints2ledger
cd fints2ledger
stack test
```

## Navigating the code

The Apps entrypoint is in the [app](app/) directory. There isn't much code there, so you can also skip right to [src/Lib.ts].
You can explore the code from there.
