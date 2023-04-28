# fints2ledger
[![Build Status](https://github.com/MoritzR/fints2ledger/actions/workflows/main.yml/badge.svg)](https://github.com/MoritzR/fints2ledger/actions) [![Coverage Status](https://coveralls.io/repos/github/MoritzR/fints2ledger/badge.svg?branch=master)](https://coveralls.io/github/MoritzR/fints2ledger?branch=master) [![PyPI version](https://badge.fury.io/py/fints2ledger.svg)](https://badge.fury.io/py/fints2ledger) [![languages](https://img.shields.io/pypi/pyversions/fints2ledger.svg)](https://pypi.org/project/fints2ledger)

A tool for downloading transactions from FinTS banking APIs and sorting them into a [ledger journal](http://hledger.org/).

[pyfints](https://github.com/raphaelm/python-fints) is used to download the transactions. A list of compatible banks can be found there. This tool was tested with [ING][ing-link] and [GLS Bank][gls-link].

![](demo.gif)

## Contents
- [Install](#install)
- [Usage](#usage)
- [Developing](#developing)
- [Changelog](#changelog)

## Install
You need Python version 3.6 or higher. Install the Python dependencies using:
```
python3 -m pip install "fints>=3,<4" "mt-940>=4.11,<5"
```

Next, you can install fints2ledger either from a pre-built binary or from source.

### from a pre-built binary
Simply grab the package from the [releases page](https://github.com/MoritzR/fints2ledger/releases)

### from source
For the you need stack installed (or alternatively cabal). Then run
```
git clone git@github.com:MoritzR/fints2ledger.git
cd fints2ledger
stack install
```

## Usage
Run
```
fints2ledger
```
This will download the transactions from the last year and tries to convert them to a ledger journal.

A list of available command line arguments:
```
usage: fints2ledger [-h] [--no-csv] [--no-ledger] [--csv-file CSVFILE]
                    [--ledger-file LEDGERFILE] [--files-path FILES_PATH]
                    [--date START] [--separator SEPARATOR]
                    [--csv_date_format CSV_DATE_FORMAT]

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
  --date START          start date to pull the FinTS entries from (format:
                        2017/12/31 or 17/12/31, default: last year)
  --separator SEPARATOR
                        character used as separator in csv file (default: ;)
  --csv_date_format CSV_DATE_FORMAT
                        Date format used in the donwloaded csv (and
                        subsequently the ledger file). hledger supports 3 date
                        formats (https://hledger.org/1.9/journal.html#simple-
                        dates). Format needs to be compatible with pythons
                        strftime(), see https://docs.python.org/3/library/date
                        time.html#strftime-strptime-behavior
                        (fints.csv_date_format in config.yml) (default:
                        %Y/%m/%d)

```

### Automatically matching transactions
In the `ledger` category you can use a regex match on any field of the transaction data to automatically fill other fields.

Example: I do not want to enter a `credit_account` and `purpose` for my monthly recurring payments for the rent of my apartment. Same for my music streaming transactions. I can change the `config.yml` like this:
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

## Changelog
The changelog can be found in [CHANGELOG.md](CHANGELOG.md)

[ing-link]: https://www.ing.de
[gls-link]: https://www.gls.de
