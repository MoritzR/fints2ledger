from fints2ledger.transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from fints2ledger.csv_converter import CsvConverter
from fints2ledger.ledger_writer import LedgerWriter
import csv
import os
import argparse
from fints2ledger.config import Config

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
    today = Date.today()
    csv_output = "\n".join(map(lambda transaction: converter.convert(
        transaction), retriever.get_hbci_transactions(Date(today.year-1, today.month, today.day), Date.today())))
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
                # add extension to have a grouping for autocomplete files
                file_with_extension = autocomplete_file + ".auto"
                # create file if non-existent
                if not os.path.exists(file_with_extension):
                    with open(file_with_extension, 'w'):
                        pass
                writer.with_autocomplete_file(
                    autocomplete_key, file_with_extension)

    with open('transaction.csv') as csvfile, open('transactions.ledger', 'a') as ledger_journal:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            if "defaults" in config:
                row.update(config["defaults"])
            entry = writer.journal_entry(row)
            if entry:
                ledger_journal.write(entry)
                ledger_journal.write("\n")


def main():
    parser = argparse.ArgumentParser(
        description='Converting transactions from fints apis to ledger.')
    parser.add_argument('--no-csv', dest='convert_to_csv', action='store_const',
                        const=False, default=True,   help='exclude conversion from fints to csv (default: not excluded)')
    parser.add_argument('--no-ledger', dest='convert_to_ledger', action='store_const',
                        const=False, default=True,   help='exclude conversion from csv to ledger (default: not excluded)')
    args = parser.parse_args()

    config_setup = Config()
    config_setup.setup_files()
    config = config_setup.get_config()

    if args.convert_to_csv:
        retrieveAndSave(config["fints"])
    if args.convert_to_ledger:
        convertToLedger(config["ledger"])


if __name__ == '__main__':
    main()
