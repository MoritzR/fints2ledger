# fints2ledger
[![Build Status](https://travis-ci.org/MoritzR/fints2ledger.svg?branch=master)](https://travis-ci.org/MoritzR/fints2ledger)

A tool for downloading transactions from FinTS banking APIs and sorting them into a [ledger journal](http://hledger.org/).

## Install

```
python setup.py install
```

Create a `config.yml` file with the following contents and replace values in the fints category:
(This file will also be atomatically created if missing)
```
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint> # e.g.: https://fints.ing-diba.de/fints/ for ING-Diba

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

A list of available command line arguments:
```
usage: fints2ledger [-h] [--no-csv] [--no-ledger] [--csv-file CSVFILE]
                    [--ledger-file LEDGERFILE]

Converting transactions from fints apis to ledger.

optional arguments:
  -h, --help            show this help message and exit
  --no-csv              exclude conversion from fints to csv (default: not
                        excluded)
  --no-ledger           exclude conversion from csv to ledger (default: not
                        excluded)fints2ledger -h
  --csv-file CSVFILE    file to store/load csv transactions to/from (default:
                        transactions.csv)
  --ledger-file LEDGERFILE
                        file to store ledger entries to (default:
                        ledger.journal)
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
