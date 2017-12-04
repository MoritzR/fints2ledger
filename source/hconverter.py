import re


class Converter:
    @staticmethod
    def transactionToLedger(transaction, source, target):
        dateLine = transaction.date.strftime(
            "%Y/%m/%d") + " spent money" + "\n"
        sourceLine = " " + source + "\n"
        targetLine = " " + target + "        €" + \
            str(transaction.amount.amount) + "\n"
        return dateLine + targetLine + sourceLine

    @staticmethod
    def hbciDataToTransaction(hbciData):
        amount = hbciData.data["amount"]
        date = hbciData.data["date"]
        # self.transactionDetails=hbciData["transaction_details"]
        originalDetails = hbciData.data["transaction_details"]
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
