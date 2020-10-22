# fints2ledger
[![Build Status](https://travis-ci.org/MoritzR/fints2ledger.svg?branch=master)](https://travis-ci.org/MoritzR/fints2ledger) [![Coverage Status](https://coveralls.io/repos/github/MoritzR/fints2ledger/badge.svg?branch=master)](https://coveralls.io/github/MoritzR/fints2ledger?branch=master) [![PyPI version](https://badge.fury.io/py/fints2ledger.svg)](https://badge.fury.io/py/fints2ledger) [![languages](https://img.shields.io/pypi/pyversions/fints2ledger.svg)](https://pypi.org/project/fints2ledger)

A tool for downloading transactions from FinTS banking APIs and sorting them into a [ledger journal](http://hledger.org/).

[pyfints](https://github.com/raphaelm/python-fints) is used to download the transactions. A list of compatible banks can be found there. This tool was tested with [ING][ing-link].

![](demo.gif)

## Contents
- [Install](#install)
- [Usage](#usage)
- [Developing](#developing)

## Install

```
pip install fints2ledger
```

Create config file at `~/.config/fints2ledger/config.yml` file with the following contents and replace values in the fints category:
(This file will also be automatically created if missing)
```
fints:
  blz: "<your bank's BLZ>"
  account: "<your account number>"
  password: "<your banking password>"
  endpoint: <your bank fints endpoint> # e.g.: https://fints.ing.de/fints for ING
  selectedAccount: "<account number>" # defaults to the value from "account"
                                      # useful when you have multiple accounts for the same login

ledger:
  prompts: 
    - credit_account
    - debit_account
  autocomplete:
    accounts:
      - credit_account
      - debit_account
  defaults:
    debit_account: assets:bank:checking
  md5:
    - date
    - payee
    - purpose
    - amount
```

## Usage
Run 
```
fints2ledger
```
This will download the transactions from the last year and tries to convert them to a ledger journal.

You can enter `s` on any transaction prompt to skip the transaction and return to it on the next run.

A list of available command line arguments:
```
usage: fints2ledger [-h] [--no-csv] [--no-ledger] [--csv-file CSVFILE]
                    [--ledger-file LEDGERFILE] [--files-path FILES_PATH]
                    [--date START] [--separator SEPARATOR]

Converting transactions from fints apis to ledger.

optional arguments:
  -h, --help            show this help message and exit
  --no-csv              exclude conversion from fints to csv (default: not
                        excluded)
  --no-ledger           exclude conversion from csv to ledger (default: not
                        excluded)
  --csv-file CSVFILE    file to store/load csv transactions to/from (default:
                        transactions.csv)
  --ledger-file LEDGERFILE
                        file to store ledger entries to (default:
                        ledger.journal)
  --files-path FILES_PATH
                        directory to store fints2ledger files (like
                        config.yml) (default: ~/.config/fints2ledger/)
  --date START          start date to pull the FinTS entires from (fromat:
                        2017/12/31 or 17/12/31, default: last year)
  --separator SEPARATOR
                        character used as separator in csv file (default: ;)
```

### Template File
A template file with the name `template.txt` will be automaticall generated. It will be user to create the ledger entries.
It looks something like
```
{date} {payee} {posting} {purpose}
    ; md5sum: {md5sum}
    {debit_account:<60}    {currency} {debit}
    {credit_account:<60}    {currency} {credit}
    
```
Each name inside curly brackets can specify a value that can come from either a named csv column, a default value (from the `config.yml`) or an input prompt (also from the `config.yml`).

### Automatically matching transactions
In the `ledger` category you can use a regex match on any field of the transaction data to automatically fill other fields.

Example: I do not want to enter a `credit_account` and `purpose` for my monthly recurring payments for the rent of my apparment. Same for my music streaming transactions. I can change the `config.yml` like this:
```
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

### Converting from csv to ledger without requesting a FinTS API
With the argument `--no-csv` the program will not create a csv file with banking transactions itself (default ist fints -> csv -> ledger).
Instead, it will convert directly from a csv file to ledger. This is useful when all transactions have already been downloaded or when converting from another source than FinTS to ledger.

The csv file must provide a headline which names the columns. The column names are then used to fill the values in the template file.
Example:
```
date;amount;currency;payee;posting;purpose
2017/04/26;167.31;EUR;Billy, Bill;bonus;for vacation
2017/04/27;-130;EUR;John, Smith;debit entry;monthly electricity payment
```

## Developing
You can modify the code yourself and run it with:
```
git clone https://github.com/MoritzR/fints2ledger.git
cd fints2ledger
python fints2ledger/main.py
```
Don't forget that your python version should be 3.5 or higher.

You can run the tests with:
```
pip install green
green
```

[ing-link]: https://www.ing.de