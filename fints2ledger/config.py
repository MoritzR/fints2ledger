import os
import yaml
from mt940.models import Date
import argparse
import fints2ledger.utils as utils


class Config:
    DEFAULT_CONFIG = {
        "fints": {
            "blz": "<your bank's BLZ>",
            "account": "<your account number>",
            "password": "<your banking password>",
            "endpoint": "<your bank fints endpoint>"
        },
        "ledger": {
            "prompts": ["credit_account", "debit_account"],
            "autocomplete": {
                "accounts": ["credit_account", "debit_account"]
            },
            "defaults": {
                "debit_account": "assets:bank:checking"
            },
            "md5": ["date", "payee", "purpose", "amount"]
        }
    }

    DEFAULT_TEMPLATE = """\
{date} {payee} {posting} {purpose}
    ; md5sum: {md5sum}
    {debit_account:<60}    {currency} {debit}
    {credit_account:<60}    {currency} {credit}
"""

    def setup_files(self):
        if not os.path.exists('config.yml'):
            with open('config.yml', 'w') as config_file:
                config_file.write(yaml.dump(Config.DEFAULT_CONFIG, default_flow_style=False))

        if not os.path.exists('template.txt'):
            with open('template.txt', 'w') as template_file:
                template_file.write(Config.DEFAULT_TEMPLATE)

    def load_config_file(self):
        config = {}
        with open("config.yml", "r") as config_file:
            config = yaml.load(config_file.read())
        return config

    def get_config(self):
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
        parser.add_argument('--date', dest='start', action='store',
                            default=None,   help='start date to pull the FinTS entires from (fromat: 2017/12/31 or 17/12/31, default: last year)')
        parser.add_argument('--separator', dest='separator', action='store',
                            default=";",   help='character used as separator in csv file (default: ;)')
        args = parser.parse_args()
        command_line_config = {
            "fints": {
                "start": utils.date_string_to_mt940_date(args.start) if args.start else Date(Date.today().year-1, Date.today().month, Date.today().day),
                "csv_separator": args.separator,
                "convert_to_csv": args.convert_to_csv
            },
            "ledger": {
                "convert_to_ledger": args.convert_to_ledger
            },
            "files": {
                "csv_file": args.csvfile,
                "ledger_file": args.ledgerfile
            }
        }

        self.setup_files()
        config = self.load_config_file()

        return utils.update_dict(config, command_line_config)
