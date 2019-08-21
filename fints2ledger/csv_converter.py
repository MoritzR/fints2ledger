class CsvConverter:
    def __init__(self, separator):
        self.separator = separator

    def get_headline(self):
        return self.separator.join(
            ["date",
             "amount",
             "currency",
             "payee",
             "posting",
             "purpose"])

    def convert(self, transaction):
        hbciData = transaction.data

        date = hbciData["date"].strftime("%Y/%m/%d")
        amount = str(hbciData["amount"].amount)
        currency = hbciData["amount"].currency
        posting_text = hbciData["posting_text"]
        applicant_name = hbciData["applicant_name"]
        purpose = hbciData["purpose"]
        return self.separator.join(
            [date,
             amount,
             currency,
             (applicant_name or "").strip(),
             (posting_text or "").strip(),
             (purpose or "").strip()])
