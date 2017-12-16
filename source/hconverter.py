import re
from tinydb import Query, operations
from mt940.models import Date, Amount


class LedgerConverter:
    def __init__(self, db):
        self.db = db
        self.markAsClassified = operations.set("classified", True)

    def transactionToLedger(self, transaction, source, target, reason):
        dateLine = transaction.date.strftime(
            "%Y/%m/%d") + " " + reason + "\n"
        sourceLine = " " + source + "\n"
        targetLine = " " + target + "        €" + \
            str(transaction.amount.amount) + "\n"

        transactionQuery = Query()
        self.db.update(self.markAsClassified,
                       transactionQuery.originalDetails == transaction.originalDetails)

        return dateLine + targetLine + sourceLine


class HbciConverter:
    @staticmethod
    def hbciDataToTransaction(hbciData):
        amount = hbciData["amount"]
        date = hbciData["date"]
        # self.transactionDetails=hbciData["transaction_details"]
        originalDetails = hbciData["transaction_details"]
        stringWithoutQuestionsMarks = re.sub(
            r"\?[0-9][0-9]", "", originalDetails)
        stringAfterLastPlus = stringWithoutQuestionsMarks[stringWithoutQuestionsMarks.rfind(
            "+") + 1:]
        transactionDetails = stringAfterLastPlus.replace("\n", "")

        return Transaction(date, amount, transactionDetails, originalDetails)
    

class Transaction:
    def __init__(self, date, amount, details, originalDetails):
        self.amount = amount
        self.date = date
        self.details = details
        self.originalDetails = originalDetails

    def __str__(self):
        return str(self.amount) + " on " + str(self.date) + ": " + self.details
