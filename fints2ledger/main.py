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


def retrieveAndSave(config):
    client = FinTS3PinTanClient(
        config["fints"]["blz"],  # Your bank's BLZ
        config["fints"]["account"],  # your account number
        config["fints"]["password"],
        config["fints"]["endpoint"]  # e.g. 'https://fints.ing-diba.de/fints/'
    )

    retriever = TRetriever(client, config["fints"]["account"])
    converter = CsvConverter(";")
    today = Date.today()
    csv_output = "\n".join(map(lambda transaction: converter.convert(
        transaction), retriever.get_hbci_transactions(Date(today.year-1, today.month, today.day), Date.today())))
    with open(config["files"]["csv_file"], 'w') as f:
        f.write(converter.get_headline())
        f.write("\n")
        f.write(csv_output)


def convertToLedger(config):
    writer = LedgerWriter(prompts=config["ledger"]["prompts"])
    if os.path.exists(config["files"]["ledger_file"]):
        with open(config["files"]["ledger_file"], 'r') as existing_journal:
            writer.with_existing_journal(existing_journal.readlines())

    if "autocomplete" in config["ledger"]:
        for autocomplete_file in config["ledger"]["autocomplete"]:
            for autocomplete_key in config["ledger"]["autocomplete"][autocomplete_file]:
                # add extension to have a grouping for autocomplete files
                file_with_extension = autocomplete_file + ".auto"
                # create file if non-existent
                if not os.path.exists(file_with_extension):
                    with open(file_with_extension, 'w'):
                        pass
                writer.with_autocomplete_file(
                    autocomplete_key, file_with_extension)

    with open(config["files"]["csv_file"]) as csvfile, open(config["files"]["ledger_file"], 'a') as ledger_journal:
        reader = csv.DictReader(csvfile, delimiter=";")
        for row in reader:
            if "defaults" in config["ledger"]:
                row.update(config["ledger"]["defaults"])
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
    parser.add_argument('--csv-file', dest='csvfile', action='store',
                        default="transactions.csv",   help='file to store/load csv transactions to/from (default: transactions.csv)')
    parser.add_argument('--ledger-file', dest='ledgerfile', action='store',
                        default="journal.ledger",   help='file to store ledger entries to (default: ledger.journal)')
    args = parser.parse_args()
    command_line_config = {
        "files": {
            "csv_file": args.csvfile,
            "ledger_file": args.ledgerfile
        }
    }

    config_setup = Config()
    config_setup.setup_files()
    config = config_setup.get_config()

    config.update(command_line_config)

    if args.convert_to_csv:
        retrieveAndSave(config)
    if args.convert_to_ledger:
        convertToLedger(config)


if __name__ == '__main__':
    main()
