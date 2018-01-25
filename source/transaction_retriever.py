from tinydb import TinyDB, Query
from hconverter import HbciConverter


class TRetriever:
    def __init__(self, client, accountnumber, db=TinyDB('transactions.json')):
        self.client = client
        self.accountnumber = accountnumber
        self.db = db

    def retrieve_and_save(self, start_date, end_date):
        transactions = self.get_transactions(start_date, end_date)
        for transaction in transactions:
            self.save_transaction(transaction)

    def get_transactions(self, start_date, end_date):
        accounts = self.client.get_sepa_accounts()

        account = self.__find_matching_account(accounts, self.accountnumber)

        statements = self.client.get_statement(account, start_date, end_date)
        print("**statements: ", statements)
        return list(map(lambda transaction: HbciConverter.hbciDataToTransaction(transaction.data), statements))

    def get_hbci_transactions(self, start_date, end_date):
        accounts = self.client.get_sepa_accounts()

        account = self.__find_matching_account(accounts, self.accountnumber)

        statements = self.client.get_statement(account, start_date, end_date)
        print("**statements: ", statements)
        return statements

    def save_transaction(self, transaction):
        insert = {
            "year": int(transaction.date.year),
            "month": int(transaction.date.month),
            "day": int(transaction.date.day),
            "amount": float(transaction.amount.amount),
            "currency": "€" if transaction.amount.currency == "EUR" else transaction.amount.currency,
            "details": transaction.details,
            "originalDetails": transaction.originalDetails,
            "classified": False
        }
        entry = Query()
        if len(self.db.search(entry.originalDetails == transaction.originalDetails)) == 0:
            self.db.insert(insert)

    def __find_matching_account(self, accounts, accountnumber):
        for account in accounts:
            if account.accountnumber == accountnumber:
                return account
        raise StopIteration
