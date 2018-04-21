from mt940.models import Date
import argparse
from fints2ledger.config import Config
from fints2ledger.fints2csv import Fints2Csv
from fints2ledger.csv2ledger import Csv2Ledger
import fints2ledger.utils as utils

'''
This requires a "config.yml" file in the same folder, according to the following format:
fints:
  blz: <your bank's BLZ>
  account: <your account number>
  password: <your banking password>
  endpoint: <your bank fints endpoint>
'''


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
            "start": utils.date_string_to_mt940_date(args.start) if args.start else Date(Date.today().year-1, Date.today().month, Date.today().day),
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
        Fints2Csv(config).retrieveAndSave()
    if args.convert_to_ledger:
        Csv2Ledger(config).convertToLedger()


if __name__ == '__main__':
    main()
