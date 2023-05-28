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

You can run the application with
```
stack run
```
and pass arguments with an additional `--` like this:
```
stack run -- --demo
```

To make the `fints2ledger` executable available in the bin directory run
```
stack install
```

## Navigating the code

The Apps entrypoint is in the [app](app/) directory. There isn't much code there, so you can also skip right to [Lib.ts](src/Lib.ts).
You can explore the code from there.

### Dependency Injection
Some functions, like
```haskell
transactionsToLedger :: [Transaction] -> App ()
```
have the `App` return type. Every function that returns this type has access to all the dependencies listed in the `Env` type (see [here](src/App.hs)).
This is mainly useful for testing, as it allows to provide mock dependencies instead.

### String Types
Annoyingly, there are multiple String types in use, as different libraries require different String types. There is:
* `String`: the default string type
* strict `Text`: a more efficient string type, used by many libraries
* lazy `Text`: more or less the same thing as above, but one library requires this instead of the strict `Text`
* `ByteString`: used by the JSON library 

One can convert between text and string easily by using the [pack](https://hackage.haskell.org/package/text-2.0.2/docs/Data-Text.html#v:pack) and [unpack](https://hackage.haskell.org/package/text-2.0.2/docs/Data-Text.html#v:unpack) function from the [text](https://hackage.haskell.org/package/text).

### The Symbols
One of the more scary aspects of Haskell code for me was seeing a lot of symbols like `$` and `>>=`. As I've learned about these I used and liked them more and more. I may overuse these, as there are often equivalent expressions that don't use symbols.
Here is a quick rundown of all the interesting ones.

`$` is used the reduce brackets.
```haskell
putStrLn $ show $ head someList
```
is equivalent to the following
```haskell
putStrLn (show (head someList))
```

`>>=` is known as `flatMap`, `bind` or `chain` and is also known behind the scenes by the do-notation. `=<<` is the same but with flipped arguments. All of the following are equivalent.
For example
```haskell
-- using >>=
getLine >>= print

-- using =<<
print =<< getLine

-- using do-notation
do
  line <- getLine
  print line
```

`<$>` is the same as `fmap`, so the following lines are equivalent.
```haskell
-- using <$>
takeDirectory <$> getExecutablePath

-- using fmap with infix notation
takeDirectory `fmap` getExecutablePath

-- using fmap directly
fmap takeDirectory getExecutablePath
```

`&` is equivalent to the `|>` pipe operator in other languages like F# or Elm.