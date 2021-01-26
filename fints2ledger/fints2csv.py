from fints2ledger.transaction_retriever import TRetriever
from fints.client import FinTS3PinTanClient
from fints2ledger.csv_converter import CsvConverter
from mt940.models import Date


class Fints2Csv:
    def __init__(self, config):
        self.config = config

    def retrieveAndSave(self):
        client = FinTS3PinTanClient(
            self.config["fints"]["blz"],  # Your bank's BLZ
            self.config["fints"]["account"],  # your account number
            self.config["fints"]["password"],
            # e.g. 'https://fints.ing-diba.de/fints/'
            self.config["fints"]["endpoint"]
        )

        retriever = TRetriever(client, self.config["fints"]["selectedAccount"])
        converter = CsvConverter(self.config["fints"]["csv_separator"], self.config["fints"]["csv_date_format"])

        csv_output = "\n".join(map(lambda transaction: converter.convert(
            transaction), retriever.get_hbci_transactions(self.config["fints"]["start"], Date.today())))

        with open(self.config["files"]["csv_file"], 'w') as f:
            f.write(converter.get_headline())
            f.write("\n")
            f.write(csv_output)
