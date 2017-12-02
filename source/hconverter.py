import re

class Converter:
    @staticmethod
    def hbciDataToTransaction(hbciData):
        amount = hbciData["amount"]
        date = hbciData["date"]
        # self.transactionDetails=hbciData["transaction_details"]
        stringWithoutQuestionsMarks = re.sub(
            r"\?[0-9][0-9]", "", hbciData["transaction_details"])
        stringAfterLastPlus = stringWithoutQuestionsMarks[stringWithoutQuestionsMarks.rfind(
            "+") + 1:]
        transactionDetails = stringAfterLastPlus.replace("\n", "")

        return Transaction(amount, date, transactionDetails)

class Transaction:
    def __init__(self, amount, date, transactionDetails):
        self.amount=amount
        self.date=date
        self.transactionDetails=transactionDetails

    def __str__(self):
        return str(self.amount) + " on " + str(self.date) + ": " + self.transactionDetails