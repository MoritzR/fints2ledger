# fints2ledger
[![Build Status](https://github.com/MoritzR/fints2ledger/actions/workflows/main.yml/badge.svg)](https://github.com/MoritzR/fints2ledger/actions) [![languages](https://img.shields.io/pypi/pyversions/fints.svg)](https://pypi.org/project/fints)

A tool for downloading transactions from FinTS banking APIs and sorting them into a [ledger journal](http://hledger.org/).

[pyfints](https://github.com/raphaelm/python-fints) is used to download the transactions. A list of compatible banks can be found there. This tool was tested with [ING][ing-link] and [GLS Bank][gls-link].

![](demo.gif)

There is a pure python implementation available on the [python branch](https://github.com/MoritzR/fints2ledger/tree/python).

## Contents
- [Install](#install)
- [Usage](#usage)
- [Changelog](#changelog)
- [Contributing](#contributing)

## Install
You need Python version 3.6 or higher. Install the Python dependencies using:
```
pip3 install "fints>=4,<5" "mt-940>=4.11,<5"
```

Next, you can install fints2ledger either from a pre-built binary or from source.

### from a pre-built binary
Simply grab the package from the [releases page](https://github.com/MoritzR/fints2ledger/releases).
On Unix, don't forget to make the binary executable with `chmod +x fints2ledger`.

### from source
For this you need [cabal](https://www.haskell.org/cabal/#install-upgrade) installed. Then run
```
git clone git@github.com:MoritzR/fints2ledger.git
cd fints2ledger
cabal install
```
This might take a while.

## Usage
You can try out the program with the `demo` flag, which does not call any banking API.
```
fints2ledger --demo
```

To use a real connection run
```
fints2ledger
```
and enter your banking credentials in the following form. This only needs to be done once.
The full configuration is stored in `~/.config/fints2ledger/config.yml`.

For a list of available command line arguments, run
```
fints2ledger --help
```

### Automatically matching transactions
In the `ledger` section you can use a regex match on any field of the transaction data to automatically fill other fields.
The `amount` field uses comparison symbols instead of a regex. Valid values are for example "<=90.5", "120.13", "> 200"

Example: I do not want to enter a `credit_account` and `purpose` for my monthly recurring payments for the rent of my apartment. Same for my music streaming transactions. I can change the `config.yml` like this:
```yaml
ledger:
  ...
  fills:
    - match:
        payee: "The Landlord"
        purpose: "Rent for apartment B month.*"
      fill:
        credit_account: "expenses:monthly:rent"
        purpose: "monthly rent"
    - match:
        payee: "MUSIC COMPANY 123"
      fill:
        credit_account: "expenses:monthly:musiccompany"
        purpose: "Monthly fee for music streaming"
```
To only fill out parts of the transaction while still being prompted for others, leave the value empty for the fields that you like to be prompted for.
The following will fill out `credit_account` but still prompt for `purpose` (instead of taking the purpose from the original transaction).
```yaml
ledger:
  ...
  fills:
    - match:
        payee: "The Landlord"
      fill:
        credit_account: "expenses:monthly:rent"
        purpose:
```

## Changelog
The changelog can be found in [CHANGELOG.md](CHANGELOG.md)

## Contributing
For additional information on how to work with the repository, see [CONTRIBUTING.md](CONTRIBUTING.md)

[ing-link]: https://www.ing.de
[gls-link]: https://www.gls.de
