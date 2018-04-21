from fints2ledger.transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from fints2ledger.csv_converter import CsvConverter
from fints2ledger.ledger_writer import LedgerWriter
import csv
import os
import argparse
from fints2ledger.config import Config
import fints2ledger.utils as utils

'''
This requires a "config.yml" file in the same folder, according to the following format:
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint>
'''

def date_string_to_mt940_date(date_string):
    parts = date_string.split("/")
    return Date(year=parts[0], month=parts[1], day=parts[2])


def retrieveAndSave(config):
    client = FinTS3PinTanClient(
        config["fints"]["blz"],  # Your bank's BLZ
        config["fints"]["account"],  # your account number
        config["fints"]["password"],
        config["fints"]["endpoint"]  # e.g. 'https://fints.ing-diba.de/fints/'
    )

    retriever = TRetriever(client, config["fints"]["account"])
    converter = CsvConverter(config["fints"]["csv_separator"])
    csv_output = "\n".join(map(lambda transaction: converter.convert(
        transaction), retriever.get_hbci_transactions(config["fints"]["start"], Date.today())))
    with open(config["files"]["csv_file"], 'w') as f:
        f.write(converter.get_headline())
        f.write("\n")
        f.write(csv_output)


def convertToLedger(config):
    writer = LedgerWriter(config)
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
        reader = csv.DictReader(csvfile, delimiter=config["fints"]["csv_separator"])
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
    parser.add_argument('--csv-file', "-c", dest='csvfile', action='store',
                        default="transactions.csv",   help='file to store/load csv transactions to/from (default: transactions.csv)')
    parser.add_argument('--ledger-file', "-l", dest='ledgerfile', action='store',
                        default="journal.ledger",   help='file to store ledger entries to (default: ledger.journal)')
    parser.add_argument('--start', "-s", dest='start', action='store',
                        default=None,   help='start date to pull the FinTS entires from (fromat: 2017/12/31 or 17/12/31, default: last year)')
    parser.add_argument('--separator', "-p", dest='separator', action='store',
                        default=";",   help='character used as separator in csv file (default: ;)')
    args = parser.parse_args()
    command_line_config = {
        "fints": {
            "start": date_string_to_mt940_date(args.start) if args.start else Date(Date.today().year-1, Date.today().month, Date.today().day),
            "csv_separator": args.separator
        },
        "files": {
            "csv_file": args.csvfile,
            "ledger_file": args.ledgerfile
        }
    }

    config_setup = Config()
    config_setup.setup_files()
    config = config_setup.get_config()

    config = utils.update_dict(config, command_line_config)

    if args.convert_to_csv:
        retrieveAndSave(config)
    if args.convert_to_ledger:
        convertToLedger(config)


if __name__ == '__main__':
    main()
