from fints.client import FinTS3PinTanClient
from mt940.models import Date
import os
import json

def retrieve_transactions(
    account, blz, password, endpoint, selected_account, start, end
):
    client = FinTS3PinTanClient(blz, account, password, endpoint, product_id = "EC449295201FA9BE5040B9154")
    return TRetriever(client, selected_account).get_hbci_transactions(start, end)


class TRetriever:
    def __init__(self, client, accountnumber):
        self.client = client
        self.accountnumber = accountnumber

    def get_hbci_transactions(self, start_date, end_date):
        accounts = self.client.get_sepa_accounts()

        account = self.__find_matching_account(accounts, self.accountnumber)

        return self.client.get_transactions(account, start_date, end_date)

    def __find_matching_account(self, accounts, accountnumber):
        for account in accounts:
            if account.accountnumber == accountnumber:
                return account
        raise Exception(
            f"Could not find a matching account for account number '{accountnumber}'. Possible accounts: {accounts}"
        )

date_format="%Y/%m/%d"
def transaction_to_object(transaction):
    hbci_data = transaction.data

    date = hbci_data["date"].strftime(date_format)
    amount = str(hbci_data["amount"].amount)
    currency = hbci_data["amount"].currency
    posting_text = hbci_data["posting_text"]
    applicant_name = hbci_data["applicant_name"]
    purpose = hbci_data["purpose"]
    return {
        "date": date,
        "amount": amount,
        "currency": currency,
        "posting": (posting_text or "").strip(),
        "payee": (applicant_name or "").strip(),
        "purpose": (purpose or "").strip(),
    }


def main():
    args = json.loads(os.environ["FINTS2LEDGER_ARGS"])
    transactions = retrieve_transactions(
        account=args["account"],
        blz=args["blz"],
        password=args["password"],
        endpoint=args["endpoint"],
        selected_account=args["selectedAccount"],
        start=date_string_to_mt940_date(args["start"]),
        end=date_string_to_mt940_date(args["end"]),
    )
    converted = json.dumps(list(map(transaction_to_object, transactions)))
    print(converted)


def date_string_to_mt940_date(date_string):
    parts = date_string.split("/")
    return Date(year=parts[0], month=parts[1], day=parts[2])


if __name__ == "__main__":
    main()
