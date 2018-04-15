import hashlib

DEFAULT_TEMPLATE = """\
{date} {payee} {posting} {purpose}
    ; md5sum: {md5sum}
    {debit_account:<60}    {debit_currency} {debit}
    {credit_account:<60}    {credit_currency} {credit}
"""

class LedgerWriter:
    def __init__(self, csv_separator):
        self.csv_separator = csv_separator

    def journal_entry(self, csv_line, credit_account, debit_account):
        template = DEFAULT_TEMPLATE

        line_as_list = csv_line.split(self.csv_separator)

        date=line_as_list[0]
        payee=line_as_list[3]
        posting=line_as_list[4]
        purpose=line_as_list[5]
        credit_amount=line_as_list[1]

        md5=hashlib.md5()
        md5.update(date.encode("UTF-8"))
        md5.update(payee.encode("UTF-8"))
        md5.update(purpose.encode("UTF-8"))
        md5.update(credit_amount.encode("UTF-8"))

        format_data = {
            'date': date,
            'payee': payee,
            'posting': posting,
            'purpose': purpose,

            'debit_account': debit_account,
            'debit_currency': "EUR",
            'debit': float(credit_amount),

            'credit_account': credit_account,
            'credit_currency': "EUR",
            'credit': -float(line_as_list[1]),

            'md5sum': md5.hexdigest()
        }

        # generate and clean output
        output_lines = template.format(**format_data).split('\n')
        output = '\n'.join([x.rstrip()
                            for x in output_lines if x.strip()]) + '\n'

        return output
