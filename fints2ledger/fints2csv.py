from fints2ledger.transaction_retriever import TRetriever
from fints.client import FinTS3PinTanClient
from fints2ledger.csv_converter import CsvConverter
from mt940.models import Date
from getpass import getpass


class Fints2Csv:
    def __init__(self, config):
        self.config = config

    def retrieveAndSave(self):
        converter = CsvConverter(self.config["fints"]["csv_separator"], self.config["fints"]["csv_date_format"])

        transactions = retrieve_transactions({
            "blz": self.config["fints"]["blz"],  # Your bank's BLZ
            "account": self.config["fints"]["account"],  # your account number
            "password": self.config["fints"]["password"] or getpass("Password: "),
            # e.g. 'https://fints.ing-diba.de/fints/'
            "endpoint": self.config["fints"]["endpoint"],
            "selected_account": self.config["fints"]["selectedAccount"],
            "start": self.config["fints"]["start"],
            "end": Date.today()
        })

        csv_output = "\n".join(map(converter.convert, transactions))

        with open(self.config["files"]["csv_file"], 'w') as f:
            f.write(converter.get_headline())
            f.write("\n")
            f.write(csv_output)

def retrieve_transactions(args):
    client = FinTS3PinTanClient(
        args["blz"],
        args["account"],
        args["password"],
        args["endpoint"],
        product_id = "EC449295201FA9BE5040B9154"
    )
    return TRetriever(client, args["selected_account"]).get_hbci_transactions(args["start"], args["end"])
