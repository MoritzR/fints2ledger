import os
import glob
import yaml
from mt940.models import Date
import argparse
import fints2ledger.utils as utils
from pathlib import Path


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
    
    def __init__(self):
        self.files_path = os.path.expanduser("~/.config/fints2ledger/")
        self.config_file_name = "config.yml"
        self.template_file_name = "template.txt"

    def setup_files(self):
        Path(self.files_path).mkdir(parents=True, exist_ok=True) 

        if not os.path.exists(self.getConfigFilePath()):
            with open(self.getConfigFilePath(), 'w') as config_file:
                config_file.write(yaml.dump(Config.DEFAULT_CONFIG, default_flow_style=False))

            self.moveLocalConfigFileIfPresent()
            self.moveLocalAutoFilesIfPresent()

        if not os.path.exists(self.getTemplateFilePath()):
            with open(self.getTemplateFilePath(), 'w') as template_file:
                template_file.write(Config.DEFAULT_TEMPLATE)
                
            self.moveLocalTemplateFileIfPresent()

    def getTemplateFilePath(self):
        return os.path.join(self.files_path, self.template_file_name)

    def getConfigFilePath(self):
        return os.path.join(self.files_path, self.config_file_name)

    def moveLocalConfigFileIfPresent(self):
        if os.path.exists(self.config_file_name):
            Path(self.config_file_name).replace(self.getConfigFilePath())
            print("I moved your 'config.yml' to '{}'".format(self.files_path))

    def moveLocalTemplateFileIfPresent(self):
        if os.path.exists(self.template_file_name):
            Path(self.template_file_name).replace(self.getTemplateFilePath())
            print("I moved your 'template.txt' to '{}'".format(self.files_path))

    def moveLocalAutoFilesIfPresent(self):
        for auto_file in glob.glob("*.auto"):
            Path(auto_file).replace(os.path.join(self.files_path,auto_file))
            print("I moved your '{}' to '{}'".format(auto_file, self.files_path))

    def load_config_file(self):
        config = {}
        with open(self.getConfigFilePath(), "r") as config_file:
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
        parser.add_argument('--files-path', dest='files_path', action='store',
                            default="~/.config/fints2ledger/",   help='directory to store fints2ledger files (like config.yml) (default: ~/.config/fints2ledger/)')
        parser.add_argument('--date', dest='start', action='store',
                            default=None,   help='start date to pull the FinTS entries from (format: 2017/12/31 or 17/12/31, default: last year)')
        parser.add_argument('--separator', dest='separator', action='store',
                            default=";",   help='character used as separator in csv file (default: ;)')
        parser.add_argument('--csv_date_format', dest='csv_date_format', action='store',
                            default='%Y/%m/%d',   help='Date format used in the donwloaded csv (and subsequently the ledger file). hledger supports 3 date formats (https://hledger.org/1.9/journal.html#simple-dates). Format needs to be compatible with pythons strftime(), see https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior (fints.csv_date_format in config.yml) (default: %%Y/%%m/%%d)')
        args = parser.parse_args()
        command_line_config = {
            "fints": {
                "start": utils.date_string_to_mt940_date(args.start) if args.start else Date(Date.today().year-1, Date.today().month, Date.today().day),
                "csv_separator": args.separator,
                "csv_date_format": args.csv_date_format,
                "convert_to_csv": args.convert_to_csv
            },
            "ledger": {
                "convert_to_ledger": args.convert_to_ledger
            },
            "files": {
                "csv_file": os.path.expanduser(args.csvfile),
                "ledger_file": os.path.expanduser(args.ledgerfile),
                "files_path": os.path.expanduser(args.files_path)
            }
        }

        self.files_path = command_line_config["files"]["files_path"]

        self.setup_files()
        config = self.load_config_file()

        if not "selectedAccount" in config["fints"]:
            config["fints"]["selectedAccount"] = config["fints"]["account"]

        return utils.update_dict(command_line_config, config)
