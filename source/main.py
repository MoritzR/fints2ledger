from transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from csv_converter import CsvConverter
import configparser

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
        fintsConfig["account"], # your account number
        fintsConfig["password"],
        fintsConfig["endpoint"] # e.g. 'https://fints.ing-diba.de/fints/'
    )

    retriever = TRetriever(client, fintsConfig["account"])
    converter = CsvConverter()
    today = Date.today()
    csv = "\n".join(map(lambda transaction: converter.convert(transaction), retriever.get_hbci_transactions(Date(2017,12,18), Date.today())))
    with open('transaction.csv', 'w') as f:
        f.write(csv)

retrieveAndSave()

