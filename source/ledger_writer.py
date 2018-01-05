from tinydb import TinyDB, Query
from hconverter import LedgerConverter, HbciConverter


class Classifier:
    def __init__(self):
        self.db = TinyDB("transactions.json")
        self.ledgerConverter = LedgerConverter(self.db)

    def load_unclassified(self):
        transaction = Query()
        unclassified = map(lambda entry: HbciConverter.databaseEntryToTransaction(
            entry), self.db.search(transaction.classified == False))

        return unclassified

    def add_to_ledger(self, transaction, source, target, reason):
        hledgerEntry = self.ledgerConverter.transactionToLedger(
            transaction, source, target, reason)

        with open("hledger.journal", "a") as journal:
            journal.write(hledgerEntry + "\n")
