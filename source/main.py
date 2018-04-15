from transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from csv_converter import CsvConverter
import configparser
from ledger_writer import LedgerWriter
import csv
import os

'''
This requires a "application.config" file in the same folder, according to the following format:
[FINTS]
blz = <your bank's BLZ>
account = <your account number>
password = <your banking password>
endpoint = <your bank fints endpoint>
'''


def retrieveAndSave():
    config = configparser.ConfigParser()
    config.read('application.config')
    fintsConfig = config["FINTS"]
    client = FinTS3PinTanClient(
        fintsConfig["blz"],  # Your bank's BLZ
        fintsConfig["account"],  # your account number
        fintsConfig["password"],
        fintsConfig["endpoint"]  # e.g. 'https://fints.ing-diba.de/fints/'
    )

    retriever = TRetriever(client, fintsConfig["account"])
    converter = CsvConverter(";")
    csv_output = "\n".join(map(lambda transaction: converter.convert(
        transaction), retriever.get_hbci_transactions(Date(2018, 3, 25), Date.today())))
    with open('transaction.csv', 'w') as f:
        f.write(converter.get_headline())
        f.write("\n")
        f.write(csv_output)


def convertToLedger():
    writer = LedgerWriter()
    with open('transactions.ledger', 'r') as existing_journal:
        writer.with_existing_journal(existing_journal.readlines())
    
    accounts_file = 'resources/autocomplete/accounts.txt'
    if not os.path.exists(accounts_file):
        with open(accounts_file, 'w'): pass
    writer.with_autocomplete_file("debit_account", accounts_file)
    writer.with_autocomplete_file("credit_account", accounts_file)

    with open('transaction.csv') as csvfile, open('transactions.ledger', 'a') as ledger_journal:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            entry = writer.journal_entry(row)
            if entry:
                ledger_journal.write(entry)
                ledger_journal.write("\n")


retrieveAndSave()
convertToLedger()
