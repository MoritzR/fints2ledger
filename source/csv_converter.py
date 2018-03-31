class CsvConverter:
    def convert(self, transaction):
        hbciData = transaction.data

        date = hbciData["date"].strftime("%Y/%m/%d")
        amount = str(hbciData["amount"].amount)
        currency = hbciData["amount"].currency
        posting_text = hbciData["posting_text"]
        applicant_name = hbciData["applicant_name"]
        purpose = hbciData["purpose"]
        print([date, amount, currency, applicant_name, posting_text, purpose])
        return ",".join([date, amount, currency, applicant_name, posting_text, purpose])