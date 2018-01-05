from transaction_retriever import TRetriever
from mt940.models import Date
from fints.client import FinTS3PinTanClient
from tinydb import TinyDB, Query
from ledger_writer import Classifier
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
    today = Date.today()
    retriever.retrieve_and_save(Date(2017,12,18), Date.today())

retrieveAndSave()

classifier = Classifier()

for transaction in classifier.load_unclassified():
    
    print(transaction)
    source = "assets:bank"
    target = "expenses:"+input("Expenses account: ")
    reason = transaction.details

    classifier.add_to_ledger(transaction, source, target, reason)

