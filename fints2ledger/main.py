from fints2ledger.transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from fints2ledger.csv_converter import CsvConverter
import yaml
from fints2ledger.ledger_writer import LedgerWriter
import csv
import os

'''
This requires a "config.yml" file in the same folder, according to the following format:
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint>
'''


def retrieveAndSave(fintsConfig):
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


def convertToLedger(config):
    writer = LedgerWriter(prompts=config["prompts"])
    if os.path.exists('transactions.ledger'):
        with open('transactions.ledger', 'r') as existing_journal:
            writer.with_existing_journal(existing_journal.readlines())
    
    if "autocomplete" in config:
        for autocomplete_file in config["autocomplete"]:
            for autocomplete_key in config["autocomplete"][autocomplete_file]:
                file_with_extension = autocomplete_file + ".auto" #add extension to have a grouping for autocomplete files
                # create file if non-existent
                if not os.path.exists(file_with_extension):
                    with open(file_with_extension, 'w'):
                        pass
                writer.with_autocomplete_file(autocomplete_key, file_with_extension)

    with open('transaction.csv') as csvfile, open('transactions.ledger', 'a') as ledger_journal:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            if "defaults" in config:
                row.update(config["defaults"])
            entry = writer.journal_entry(row)
            if entry:
                ledger_journal.write(entry)
                ledger_journal.write("\n")

config = {}
with open("config.yml") as config_file:
    config = yaml.load(config_file.read())

retrieveAndSave(config["fints"])
convertToLedger(config["ledger"])
